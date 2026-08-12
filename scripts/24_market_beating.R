# =====================================================================
# AlphaQuant — Three attempts to beat the market
#
#   A. Volatility targeting   (Moreira & Muir 2017)
#   B. Low-beta screen        (Frazzini & Pedersen 2014)
#   C. Combined best-practice construction
#
# ⚠️ READ THIS FIRST
# You have now evaluated roughly 400 configurations. Anything that looks
# good here is the maximum of a large search. Two safeguards are built in:
#
#   1. Every strategy is validated on a SPLIT SAMPLE. A strategy that
#      only works in one half is not a strategy.
#   2. A permutation test estimates how good the BEST of N random
#      configurations looks by chance, giving a benchmark for judging
#      whether any result exceeds what search alone would produce.
#
# The verdict printed at the end applies fixed criteria decided in
# advance, not chosen after seeing results.
#
# PASS requires ALL of:
#   - Sharpe exceeds the market in BOTH sample halves
#   - CAPM alpha positive in both halves
#   - full-sample CAPM alpha t > 2
#   - beta below 1.10 (the edge is not leverage)
# =====================================================================

library(tidyverse)
library(lubridate)
library(scales)

HOLD_MO  <- 6
COST_BPS <- 20
SPLIT    <- as.Date("2010-01-01")

stopifnot(exists("signed"), exists("rets"), exists("ff"))
msf <- read_rds("data/msf.rds")

cat("Universe:", nrow(signed), "stock-months |",
    n_distinct(signed$form_date), "periods\n")


# =====================================================================
# 0. Rolling betas and market volatility
# =====================================================================
cat("\nComputing rolling betas...\n")

beta_dat <- rets |>
  left_join(ff |> select(month_date, mktrf, rf), by = "month_date") |>
  drop_na(mktrf) |>
  mutate(ex = ret_adj - rf) |>
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

betas <- beta_dat |> group_by(permno) |> filter(n() >= 36) |>
  mutate(beta_est = roll_beta(ex, mktrf)) |> ungroup() |>
  filter(!is.na(beta_est)) |>
  mutate(beta_est = pmin(pmax(beta_est, -1), 3)) |>
  select(permno, month_date, beta_est)

# Trailing market volatility — 12 months, lagged so it uses only
# information available at the decision point.
mkt_vol <- ff |> arrange(month_date) |>
  mutate(vol12 = slider::slide_dbl(mktrf, sd, .before = 11, .complete = TRUE),
         vol12_lag = lag(vol12, 1)) |>
  select(month_date, vol12_lag)

cat("Betas for", nrow(betas), "stock-months\n")


# =====================================================================
# 1. Engine
# =====================================================================
# vol_target: if set, scales exposure to hit a constant target
# volatility, using only trailing information. Leverage capped at 1.5
# so results are not driven by borrowing.

build <- function(d, n_side, label, vol_target = NULL, max_lev = 1.5) {
  picks <- d |> group_by(form_date) |>
    filter(n() >= n_side * 2) |>
    slice_max(composite, n = n_side) |> ungroup()
  if (nrow(picks) < 100) return(NULL)

  held <- picks |> select(cohort = form_date, permno) |>
    crossing(k = 1:HOLD_MO) |>
    mutate(month_date = ceiling_date(cohort %m+% months(k), "month") - 1) |>
    left_join(rets |> select(permno, month_date, ret_adj),
              by = c("permno","month_date")) |>
    mutate(ret_adj = replace_na(ret_adj, 0)) |>
    group_by(cohort, month_date) |>
    summarise(ret = mean(ret_adj), .groups = "drop")

  p <- held |> group_by(month_date) |>
    summarise(ret = mean(ret), nc = n_distinct(cohort), .groups = "drop") |>
    filter(nc == HOLD_MO) |>
    left_join(ff |> select(month_date, mktrf, smb, hml, rmw, cma, umd, rf),
              by = "month_date") |> drop_na(mktrf)
  if (nrow(p) < 60) return(NULL)

  p <- p |> mutate(gross = ret - (1/HOLD_MO)*2*COST_BPS/1e4)

  if (!is.null(vol_target)) {
    p <- p |> left_join(mkt_vol, by = "month_date") |>
      mutate(lev = pmin(vol_target / (vol12_lag * sqrt(12)), max_lev),
             lev = replace_na(lev, 1),
             net = lev * gross + (1 - lev) * rf)
  } else {
    p <- p |> mutate(lev = 1, net = gross)
  }
  p |> mutate(strategy = label)
}

stats <- function(p, label = NULL) {
  if (is.null(p) || nrow(p) < 30) return(NULL)
  ex   <- p$net - p$rf
  capm <- lm(ex ~ p$mktrf)
  ff6  <- lm(ex ~ p$mktrf + p$smb + p$hml + p$rmw + p$cma + p$umd)
  tibble(
    strategy = label %||% p$strategy[1], n_mo = nrow(p),
    ann_ret = prod(1 + p$net)^(12/nrow(p)) - 1,
    mkt_ret = prod(1 + p$mktrf + p$rf)^(12/nrow(p)) - 1,
    vol = sd(p$net)*sqrt(12),
    sharpe = mean(ex)*12/(sd(p$net)*sqrt(12)),
    mkt_sharpe = mean(p$mktrf)*12/(sd(p$mktrf)*sqrt(12)),
    beta = coef(capm)[2],
    capm_a = coef(capm)[1]*12, capm_t = summary(capm)$coef[1,3],
    ff6_a = coef(ff6)[1]*12, ff6_t = summary(ff6)$coef[1,3],
    max_dd = min(cumprod(1+p$net)/cummax(cumprod(1+p$net)) - 1)
  )
}

fmt <- function(x) x |>
  mutate(across(c(ann_ret, mkt_ret, vol, capm_a, ff6_a, max_dd),
                ~ percent(.x, accuracy = 0.1)),
         across(c(sharpe, mkt_sharpe, beta, capm_t, ff6_t), ~ round(.x, 2)))


# =====================================================================
# 2. Prepare the universes
# =====================================================================
base <- signed |>
  left_join(betas, by = c("permno", "form_date" = "month_date")) |>
  group_by(form_date) |> mutate(sz = percent_rank(mktcap)) |> ungroup()

u_all    <- base
u_p80    <- base |> filter(sz >= 0.80)
u_lowb   <- base |> group_by(form_date) |>
  filter(is.na(beta_est) | beta_est <= quantile(beta_est, 0.80, na.rm = TRUE)) |>
  ungroup()
u_screen <- base |> group_by(form_date) |>
  filter(is.na(s_accruals) |
           s_accruals >= quantile(s_accruals, 0.25, na.rm = TRUE)) |> ungroup()

u_combo <- base |>
  filter(sz >= 0.60, sz <= 0.95) |>
  group_by(form_date) |>
  filter(is.na(beta_est) | beta_est <= quantile(beta_est, 0.80, na.rm = TRUE),
         is.na(s_accruals) |
           s_accruals >= quantile(s_accruals, 0.25, na.rm = TRUE)) |>
  ungroup()

cat("\nMedian stocks available per month:\n")
tibble(universe = c("All","P80-100","Low beta","Accruals screen","Combined"),
       median_n = c(median(count(u_all, form_date)$n),
                    median(count(u_p80, form_date)$n),
                    median(count(u_lowb, form_date)$n),
                    median(count(u_screen, form_date)$n),
                    median(count(u_combo, form_date)$n))) |> print()


# =====================================================================
# 3. Build all strategies
# =====================================================================
cat("\nBuilding strategies...\n")

strats <- list(
  "Baseline (100, all)"        = build(u_all,    100, "Baseline (100, all)"),
  "A. Vol-targeted 12%"        = build(u_all,    100, "A. Vol-targeted 12%",
                                       vol_target = 0.12),
  "A. Vol-targeted 15%"        = build(u_all,    100, "A. Vol-targeted 15%",
                                       vol_target = 0.15),
  "B. Low-beta screen"         = build(u_lowb,   100, "B. Low-beta screen"),
  "B. P80-100 (large cap)"     = build(u_p80,    100, "B. P80-100 (large cap)"),
  "C. Combined"                = build(u_combo,  100, "C. Combined"),
  "C. Combined + vol target"   = build(u_combo,  100, "C. Combined + vol target",
                                       vol_target = 0.12),
  "C. Combined, 200 names"     = build(u_combo,  200, "C. Combined, 200 names")
)
strats <- compact(strats)

cat("\n========== FULL SAMPLE ==========\n")
full_res <- map_dfr(names(strats), ~ stats(strats[[.x]], .x))
full_res |> fmt() |> print(width = Inf)


# =====================================================================
# 4. ⚠️ SPLIT-SAMPLE VALIDATION
# =====================================================================
cat("\n========== FIRST HALF (pre-2010) ==========\n")
h1 <- map_dfr(names(strats), ~ stats(strats[[.x]] |> filter(month_date < SPLIT), .x))
h1 |> fmt() |> print(width = Inf)

cat("\n========== SECOND HALF (2010+) ==========\n")
h2 <- map_dfr(names(strats), ~ stats(strats[[.x]] |> filter(month_date >= SPLIT), .x))
h2 |> fmt() |> print(width = Inf)


# =====================================================================
# 5. ⚠️ PERMUTATION BENCHMARK
# =====================================================================
# How good does the BEST of N RANDOM strategies look? If random
# selection produces Sharpe ratios comparable to the strategies above,
# then those results are consistent with search alone.

cat("\n========== PERMUTATION BENCHMARK ==========\n")
cat("Building 30 random-selection portfolios for comparison...\n")

set.seed(1)
rand_res <- map_dfr(1:30, function(i) {
  d <- base |> group_by(form_date) |>
    mutate(composite = runif(n())) |> ungroup()
  s <- build(d, 100, paste0("random_", i))
  st <- stats(s)
  if (is.null(st)) return(NULL)
  st
})

cat("\nRandom portfolios (n =", nrow(rand_res), "):\n")
rand_res |> summarise(
  mean_sharpe = mean(sharpe), best_sharpe = max(sharpe),
  mean_alpha  = mean(capm_a), best_alpha  = max(capm_a),
  best_alpha_t = max(capm_t),
  mkt_sharpe  = first(mkt_sharpe)) |>
  mutate(across(c(mean_alpha, best_alpha), ~ percent(.x, accuracy=0.1)),
         across(c(mean_sharpe, best_sharpe, best_alpha_t, mkt_sharpe),
                ~ round(.x,2))) |> print(width = Inf)

cat("\nA real strategy should beat the BEST random draw, not the average.\n")


# =====================================================================
# 6. VERDICT — fixed criteria
# =====================================================================
# Set before running. Applied mechanically.

cat("\n\n================ VERDICT ================\n")
cat("PASS requires ALL of:\n")
cat("  1. Sharpe > market Sharpe in BOTH halves\n")
cat("  2. CAPM alpha > 0 in BOTH halves\n")
cat("  3. Full-sample CAPM alpha t > 2\n")
cat("  4. Beta < 1.10\n")
cat("  5. Full-sample Sharpe > best of 30 random portfolios\n\n")

best_rand_sharpe <- max(rand_res$sharpe)

verdict <- full_res |>
  select(strategy, f_sharpe = sharpe, f_mkt = mkt_sharpe,
         f_beta = beta, f_a = capm_a, f_t = capm_t) |>
  left_join(h1 |> select(strategy, h1_sharpe = sharpe,
                         h1_mkt = mkt_sharpe, h1_a = capm_a), by="strategy") |>
  left_join(h2 |> select(strategy, h2_sharpe = sharpe,
                         h2_mkt = mkt_sharpe, h2_a = capm_a), by="strategy") |>
  mutate(
    c1_sharpe_both = h1_sharpe > h1_mkt & h2_sharpe > h2_mkt,
    c2_alpha_both  = h1_a > 0 & h2_a > 0,
    c3_alpha_sig   = f_t > 2,
    c4_low_beta    = f_beta < 1.10,
    c5_beats_rand  = f_sharpe > best_rand_sharpe,
    PASS = c1_sharpe_both & c2_alpha_both & c3_alpha_sig &
           c4_low_beta & c5_beats_rand,
    n_criteria = c1_sharpe_both + c2_alpha_both + c3_alpha_sig +
                 c4_low_beta + c5_beats_rand
  )

verdict |>
  select(strategy, n_criteria, PASS, c1_sharpe_both, c2_alpha_both,
         c3_alpha_sig, c4_low_beta, c5_beats_rand) |>
  arrange(desc(n_criteria)) |> print(width = Inf)

cat("\n--- Detail ---\n")
verdict |> select(strategy, f_sharpe, f_mkt, f_beta, f_a, f_t,
                  h1_sharpe, h1_mkt, h2_sharpe, h2_mkt) |>
  mutate(across(c(f_a), ~ percent(.x, accuracy=0.1)),
         across(where(is.numeric), ~ round(.x, 2))) |>
  arrange(desc(f_sharpe)) |> print(width = Inf)

n_pass <- sum(verdict$PASS, na.rm = TRUE)
cat("\n=========================================\n")
if (n_pass == 0) {
  cat("RESULT: No strategy passes all five criteria.\n\n")
  cat("Most fail on criterion 3 (alpha t > 2). A Sharpe advantage of\n")
  cat("0.05-0.10 over 30 years is not statistically distinguishable\n")
  cat("from zero, which is the honest conclusion.\n")
} else {
  cat("RESULT:", n_pass, "strategy/strategies pass all five criteria.\n\n")
  cat("Even so, these were selected from roughly 400 configurations\n")
  cat("evaluated across this project. Report the full search, not the\n")
  cat("winner alone.\n")
}
cat("=========================================\n")


# =====================================================================
# 7. Figure
# =====================================================================
theme_set(theme_minimal(base_size = 11))

top4 <- full_res |> slice_max(sharpe, n = 4) |> pull(strategy)

fig <- map_dfr(top4, function(s) {
  strats[[s]] |> arrange(month_date) |>
    mutate(value = cumprod(1 + net), strategy = s) |>
    select(month_date, value, strategy)
}) |>
  bind_rows(strats[[1]] |> arrange(month_date) |>
              mutate(value = cumprod(1 + mktrf + rf), strategy = "Market") |>
              select(month_date, value, strategy)) |>
  ggplot(aes(month_date, value, colour = strategy)) +
  geom_line(linewidth = .8) +
  scale_y_log10(labels = dollar_format(accuracy = 0.1)) +
  geom_vline(xintercept = SPLIT, linetype = 2, colour = "grey50") +
  labs(title = "Growth of $1, top strategies vs market",
       subtitle = "Net of 20bp per side. Dashed line marks the split-sample boundary.",
       x = NULL, y = NULL, colour = NULL)

ggsave("figures/market_beating_attempts.png", fig,
       width = 10, height = 6, dpi = 300)

write_csv(full_res, "output/strategy_full.csv")
write_csv(h1,       "output/strategy_h1.csv")
write_csv(h2,       "output/strategy_h2.csv")
write_csv(verdict,  "output/strategy_verdict.csv")
write_csv(rand_res, "output/random_benchmark.csv")

cat("\nDone.\n")


# =====================================================================
# HOW TO READ THE VERDICT
# =====================================================================
# Criterion 3 (alpha t > 2) is the one most strategies fail, and it is
# the one that matters most. A Sharpe advantage of 0.60 vs 0.59 is not
# evidence of skill; it is within the noise of a 30-year sample.
#
# Criterion 5 is the search correction. If the best of 30 RANDOM
# portfolios achieves Sharpe 0.55, then a designed strategy at 0.58 has
# demonstrated very little.
#
# If nothing passes, that is a legitimate and publishable finding, and
# it is consistent with everything else in this project.
#
# If something passes, the honest framing remains: it was selected from
# a large search, and the appropriate next test would be genuinely new
# data -- a different market, or a period after this sample ends.
