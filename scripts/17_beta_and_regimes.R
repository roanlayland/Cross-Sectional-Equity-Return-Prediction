# =====================================================================
# Cross-Sectional-Equity-Return-Prediction — Regime analysis and beta control
#
# Q1: does the model do better or worse in DOWN markets?
# Q2: if beta is held near 1.0, does the signal beat the market?
#
# Q2 is the decisive test. Every prior configuration carried beta of
# 1.32-1.48, so returns were partly leveraged market exposure rather
# than stock selection. Holding beta at 1.0 isolates the selection.
# =====================================================================

library(tidyverse)
library(lubridate)
library(scales)

scored <- read_rds("output/all_scores.rds")
msf    <- read_rds("data/msf.rds")
dl     <- read_rds("data/delist.rds")
ff     <- read_rds("data/ff_factors.rds") |>
  mutate(month_date = ceiling_date(date, "month") - 1)

rets <- msf |>
  mutate(ym = floor_date(date, "month")) |>
  left_join(dl |> mutate(ym = floor_date(dlstdt, "month")) |>
              select(permno, ym, dlret) |>
              distinct(permno, ym, .keep_all = TRUE), by = c("permno","ym")) |>
  mutate(ret_adj = case_when(
    !is.na(ret) & !is.na(dlret) ~ (1+ret)*(1+dlret) - 1,
     is.na(ret) & !is.na(dlret) ~ dlret, TRUE ~ ret)) |>
  transmute(permno, month_date = ceiling_date(ym, "month") - 1, ret_adj) |>
  filter(!is.na(ret_adj))


# =====================================================================
# 1. Rolling 36-month beta for every stock
# =====================================================================
# Estimated on trailing data only, so no look-ahead.

cat("Computing rolling betas (a few minutes)...\n")

beta_dat <- rets |>
  left_join(ff |> select(month_date, mktrf, rf), by = "month_date") |>
  drop_na(mktrf) |>
  mutate(ex_ret = ret_adj - rf) |>
  arrange(permno, month_date)

roll_beta <- function(y, x, n = 36) {
  out <- rep(NA_real_, length(y))
  if (length(y) < n) return(out)
  for (i in n:length(y)) {
    yy <- y[(i-n+1):i]; xx <- x[(i-n+1):i]
    ok <- !is.na(yy) & !is.na(xx)
    if (sum(ok) >= 24) out[i] <- cov(yy[ok], xx[ok]) / var(xx[ok])
  }
  out
}

betas <- beta_dat |>
  group_by(permno) |>
  filter(n() >= 36) |>
  mutate(beta_est = roll_beta(ex_ret, mktrf)) |>
  ungroup() |>
  select(permno, month_date, beta_est) |>
  filter(!is.na(beta_est)) |>
  mutate(beta_est = pmin(pmax(beta_est, -1), 3))   # winsorise

cat("Betas estimated for", nrow(betas), "stock-months\n")


# =====================================================================
# 2. Backtest engine with optional beta control
# =====================================================================
fwd_ret <- rets |> rename(fwd = ret_adj)

build_port <- function(n_side, min_cap, hold_mo, cost_bps,
                       beta_neutral = FALSE, label = "") {

  d <- scored |> filter(mktcap >= min_cap) |>
    left_join(betas, by = c("permno", "form_date" = "month_date"))

  picks <- if (beta_neutral) {
    # Pick the top names WITHIN each beta quintile, so the portfolio's
    # average beta matches the universe's (~1.0) by construction.
    d |> filter(!is.na(beta_est)) |>
      group_by(form_date) |>
      mutate(bq = ntile(beta_est, 5)) |>
      group_by(form_date, bq) |>
      slice_max(score, n = ceiling(n_side/5)) |>
      ungroup()
  } else {
    d |> group_by(form_date) |> slice_max(score, n = n_side) |> ungroup()
  }

  held <- picks |> select(cohort = form_date, permno) |>
    crossing(k = 1:hold_mo) |>
    mutate(month_date = ceiling_date(cohort %m+% months(k), "month") - 1) |>
    left_join(fwd_ret, by = c("permno","month_date")) |>
    mutate(fwd = replace_na(fwd, 0)) |>
    group_by(cohort, month_date) |>
    summarise(ret = mean(fwd), .groups = "drop")

  p <- held |> group_by(month_date) |>
    summarise(ret = mean(ret), n_coh = n_distinct(cohort), .groups="drop") |>
    filter(n_coh == hold_mo) |>
    left_join(ff |> select(month_date, mktrf, smb, hml, rmw, cma, umd, rf),
              by = "month_date") |> drop_na(mktrf)

  cost <- (1/hold_mo) * 2 * cost_bps / 1e4
  p |> mutate(net_ret = ret - cost, config = label)
}

stats <- function(p) {
  ex   <- p$net_ret - p$rf
  capm <- lm(ex ~ p$mktrf)
  ff6  <- lm(ex ~ p$mktrf+p$smb+p$hml+p$rmw+p$cma+p$umd)
  tibble(
    config  = p$config[1], n_months = nrow(p),
    ann_ret = prod(1+p$net_ret)^(12/nrow(p)) - 1,
    mkt_ret = prod(1+p$mktrf+p$rf)^(12/nrow(p)) - 1,
    ann_vol = sd(p$net_ret)*sqrt(12),
    sharpe  = mean(ex)*12/(sd(p$net_ret)*sqrt(12)),
    mkt_sharpe = mean(p$mktrf)*12/(sd(p$mktrf)*sqrt(12)),
    beta    = coef(capm)[2],
    capm_a  = coef(capm)[1]*12, capm_t = summary(capm)$coef[1,3],
    ff6_a   = coef(ff6)[1]*12,  ff6_t  = summary(ff6)$coef[1,3]
  )
}

fmt <- function(x) x |>
  mutate(across(c(ann_ret, mkt_ret, ann_vol, capm_a, ff6_a),
                ~ percent(.x, accuracy=0.1)),
         across(c(sharpe, mkt_sharpe, beta, capm_t, ff6_t), ~ round(.x,2)))


# =====================================================================
# 3. THE DECISIVE TEST — beta-neutral vs unconstrained
# =====================================================================
cat("\n========== BETA CONTROL ==========\n")
cat("Both use 50 names, $2B floor, 6-month hold, 20bp.\n")
cat("The only difference is whether beta is held near 1.0.\n\n")

p_uncon <- build_port(50, 2000, 6, 20, FALSE, "Unconstrained")
p_bneut <- build_port(50, 2000, 6, 20, TRUE,  "Beta-neutral")

bind_rows(stats(p_uncon), stats(p_bneut)) |> fmt() |> print(width = Inf)

cat("\nIf beta-neutral shows POSITIVE alpha, the earlier underperformance\n")
cat("came from risk exposure, not from a bad signal.\n")
cat("If alpha stays negative, the signal itself is too weak.\n")


# =====================================================================
# 4. REGIME ANALYSIS — up markets vs down markets
# =====================================================================
regime <- p_uncon |>
  mutate(mkt_total = mktrf + rf,
         regime = if_else(mkt_total >= 0, "Market UP", "Market DOWN"))

cat("\n========== PERFORMANCE BY MARKET DIRECTION ==========\n")
regime |> group_by(regime) |>
  summarise(n_months = n(),
            avg_strat = mean(net_ret),
            avg_mkt   = mean(mkt_total),
            diff      = mean(net_ret - mkt_total),
            capture   = mean(net_ret) / mean(mkt_total),
            win_rate  = mean(net_ret > mkt_total)) |>
  mutate(across(c(avg_strat, avg_mkt, diff, win_rate),
                ~ percent(.x, accuracy=0.1)),
         capture = round(capture, 2)) |> print(width = Inf)

cat("\nCapture ratio: >1 in UP months means amplified gains;\n")
cat(">1 in DOWN months means amplified losses. Beta ~1.35 predicts\n")
cat("roughly 1.35 in both -- that is the cost of the risk exposure.\n")

cat("\n========== WORST 10 MARKET MONTHS ==========\n")
regime |> arrange(mkt_total) |> head(10) |>
  transmute(Month = format(month_date, "%Y-%m"),
            Market = percent(mkt_total, accuracy=0.1),
            Strategy = percent(net_ret, accuracy=0.1),
            Diff = percent(net_ret - mkt_total, accuracy=0.1)) |>
  print(n = 10)

cat("\n========== BY CALENDAR YEAR ==========\n")
regime |> mutate(yr = year(month_date)) |>
  group_by(yr) |>
  summarise(strat = prod(1+net_ret)-1, mkt = prod(1+mkt_total)-1,
            .groups="drop") |>
  mutate(diff = strat - mkt,
         across(c(strat, mkt, diff), ~ percent(.x, accuracy=0.1))) |>
  print(n = 20)

down_yrs <- regime |> mutate(yr = year(month_date)) |> group_by(yr) |>
  summarise(strat = prod(1+net_ret)-1, mkt = prod(1+mktrf+rf)-1) |>
  filter(mkt < 0)

cat("\nDown years:", nrow(down_yrs), "\n")
if (nrow(down_yrs) > 0) {
  cat("Strategy beat the market in", sum(down_yrs$strat > down_yrs$mkt),
      "of them.\n")
}


# =====================================================================
# 5. Beta-neutral in down markets
# =====================================================================
cat("\n========== BETA-NEUTRAL BY REGIME ==========\n")
p_bneut |> mutate(mkt_total = mktrf + rf,
                  regime = if_else(mkt_total >= 0, "Market UP", "Market DOWN")) |>
  group_by(regime) |>
  summarise(n = n(), avg_strat = mean(net_ret), avg_mkt = mean(mkt_total),
            capture = mean(net_ret)/mean(mkt_total)) |>
  mutate(across(c(avg_strat, avg_mkt), ~ percent(.x, accuracy=0.1)),
         capture = round(capture,2)) |> print()


# =====================================================================
# 6. Figure
# =====================================================================
theme_set(theme_minimal(base_size = 12))

fig <- bind_rows(p_uncon, p_bneut) |>
  group_by(config) |> arrange(month_date) |>
  mutate(value = cumprod(1 + net_ret)) |>
  ungroup() |>
  bind_rows(p_uncon |> arrange(month_date) |>
              mutate(config = "Market", value = cumprod(1+mktrf+rf))) |>
  ggplot(aes(month_date, value, colour = config)) +
  geom_line(linewidth = .9) +
  scale_y_log10(labels = dollar_format(accuracy = 0.1)) +
  scale_colour_manual(values = c("Unconstrained"="steelblue4",
                                 "Beta-neutral"="darkgreen",
                                 "Market"="firebrick")) +
  labs(title = "Beta control vs unconstrained selection",
       subtitle = "50 names, $2B floor, 6-month hold, net of 20bp. Log scale.",
       x = NULL, y = "Growth of $1", colour = NULL)

ggsave("figures/beta_control.png", fig, width = 10, height = 6, dpi = 300)

write_csv(bind_rows(stats(p_uncon), stats(p_bneut)), "output/beta_control.csv")

cat("\nDone.\n")


# =====================================================================
# WHAT THIS ANSWERS
# =====================================================================
# Q1 (down markets): capture ratios near 1.35 in BOTH directions mean
#     the portfolio simply amplifies the market. Amplified gains in up
#     months, amplified losses in down months, no defensive property.
#
# Q2 (beta control): this is the question that matters. If holding beta
#     at 1.0 produces positive alpha, the signal has genuine value and
#     the earlier results were a risk-exposure artifact. If alpha stays
#     negative at beta 1.0, the signal is simply too small -- roughly a
#     6% gross decile spread against 1-4% costs and a competitive market
#     that has traded these anomalies for fifteen years.
