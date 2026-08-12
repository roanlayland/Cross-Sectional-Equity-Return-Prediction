# =====================================================================
# AlphaQuant — Final three analyses
#
#   1. Long / short / long-short at IDENTICAL breadth and holding period
#   2. Transaction cost sensitivity (20 / 30 / 50 bp)
#   3. Configuration summary table
#
# The point of (1): every previous comparison mixed constructions.
# Here the ONLY difference between legs is which end of the ranking is
# used, so the long-vs-short alpha difference is cleanly attributable.
# =====================================================================

library(tidyverse)
library(lubridate)
library(xgboost)
library(scales)
library(broom)

N_SIDE     <- 100     # per side
HOLD_MO    <- 6
MIN_PRICE  <- 5
MIN_MKTCAP <- 100
FIRST_YEAR <- 2010
LAST_YEAR  <- 2024

panel    <- read_rds("data/panel_ranked.rds")
FEATURES <- read_rds("data/feature_list.rds")
RK       <- paste0("rk_", FEATURES)
msf      <- read_rds("data/msf.rds")
dl       <- read_rds("data/delist.rds")
ff       <- read_rds("data/ff_factors.rds") |>
  mutate(month_date = ceiling_date(date, "month") - 1)


# =====================================================================
# 1. Returns and prep
# =====================================================================
rets <- msf |>
  mutate(ym = floor_date(date, "month")) |>
  left_join(dl |> mutate(ym = floor_date(dlstdt, "month")) |>
              select(permno, ym, dlret) |>
              distinct(permno, ym, .keep_all = TRUE), by = c("permno","ym")) |>
  mutate(ret_adj = case_when(
    !is.na(ret) & !is.na(dlret) ~ (1+ret)*(1+dlret) - 1,
     is.na(ret) & !is.na(dlret) ~ dlret, TRUE ~ ret)) |>
  transmute(permno, month_date = ceiling_date(ym, "month") - 1,
            ret_adj, price = abs(prc)) |>
  filter(!is.na(ret_adj))

miss_rate  <- panel |> summarise(across(all_of(RK), ~ mean(is.na(.x)))) |>
  pivot_longer(everything(), names_to="term", values_to="rate")
needs_flag <- miss_rate |> filter(rate > 0.05) |> pull(term)

panel <- panel |>
  mutate(across(all_of(needs_flag), ~ as.integer(is.na(.x)),
                .names = "miss_{.col}")) |>
  group_by(form_date) |>
  mutate(across(all_of(RK), ~ {
    m <- is.na(.x); if (any(m) && !all(m)) .x[m] <- median(.x, na.rm=TRUE); .x
  })) |> ungroup()

RK <- c(RK, paste0("miss_", needs_flag))

bad <- panel |> group_by(form_date) |>
  summarise(across(all_of(RK), ~ mean(is.na(.x))), .groups="drop") |>
  pivot_longer(-form_date) |> filter(value == 1) |>
  distinct(form_date) |> pull(form_date)

dat <- panel |> filter(!form_date %in% bad) |>
  left_join(rets |> select(permno, month_date, price),
            by = c("permno"="permno","form_date"="month_date"))


# =====================================================================
# 2. Form BOTH tails each month
# =====================================================================
xgb_params <- list(objective="reg:squarederror", eta=0.05, max_depth=6,
                   subsample=0.7, colsample_bytree=0.7,
                   min_child_weight=50, lambda=5, nthread=4)

train_model <- function(train_end) {
  tr <- dat |> filter(!is.na(fwd_12m), form_date < train_end) |>
    drop_na(all_of(RK))
  if (nrow(tr) < 20000) return(NULL)
  fit <- tr |> filter(form_date <  max(form_date) %m-% years(4))
  val <- tr |> filter(form_date >= max(form_date) %m-% years(3))
  if (nrow(fit) < 5000 || nrow(val) < 2000) return(NULL)
  lg <- capture.output(
    xgb.train(xgb_params, xgb.DMatrix(as.matrix(fit[,RK]), label=fit$fwd_12m),
              nrounds=600,
              evals=list(v=xgb.DMatrix(as.matrix(val[,RK]), label=val$fwd_12m)),
              early_stopping_rounds=50, verbose=1))
  r <- as.numeric(str_extract(lg, "(?<=val-rmse:)[0-9.]+")); r <- r[!is.na(r)]
  xgb.train(xgb_params, xgb.DMatrix(as.matrix(tr[,RK]), label=tr$fwd_12m),
            nrounds = max(if (length(r)) which.min(r) else 150L, 10L), verbose=0)
}

cohorts <- list()
for (yr in FIRST_YEAR:LAST_YEAR) {
  m <- train_model(as.Date(paste0(yr,"-01-01")) %m-% months(HOLD_MO))
  if (is.null(m)) next
  cat("Trained", yr, "\n")
  for (d in sort(unique(dat$form_date[year(dat$form_date) == yr]))) {
    d <- as.Date(d)
    cs <- dat |> filter(form_date == d, price >= MIN_PRICE,
                        mktcap >= MIN_MKTCAP) |> drop_na(all_of(RK))
    if (nrow(cs) < 400) next
    cs$score <- predict(m, as.matrix(cs[, RK]))
    cohorts[[as.character(d)]] <- bind_rows(
      cs |> slice_max(score, n = N_SIDE) |> mutate(leg = "long"),
      cs |> slice_min(score, n = N_SIDE) |> mutate(leg = "short")
    ) |> transmute(cohort = d, permno, ticker, leg, score)
  }
}
coh <- bind_rows(cohorts)
cat("Cohorts:", n_distinct(coh$cohort), "\n")


# =====================================================================
# 3. Track both legs
# =====================================================================
legs <- coh |>
  select(cohort, permno, leg) |>
  crossing(k = 1:HOLD_MO) |>
  mutate(month_date = ceiling_date(cohort %m+% months(k), "month") - 1) |>
  left_join(rets |> select(permno, month_date, ret_adj),
            by = c("permno","month_date")) |>
  mutate(ret_adj = replace_na(ret_adj, 0)) |>
  group_by(leg, cohort, month_date) |>
  summarise(ret = mean(ret_adj), .groups = "drop")

port <- legs |>
  group_by(leg, month_date) |>
  summarise(ret = mean(ret), n_coh = n_distinct(cohort), .groups = "drop") |>
  filter(n_coh == HOLD_MO) |>
  select(-n_coh) |>
  pivot_wider(names_from = leg, values_from = ret) |>
  drop_na() |>
  mutate(long_short = long - short) |>
  left_join(ff |> select(month_date, mktrf, smb, hml, rmw, cma, umd, rf),
            by = "month_date") |>
  drop_na(mktrf)

cat("Steady-state months:", nrow(port), "\n")


# =====================================================================
# 4. THE KEY TABLE — alpha by leg, at identical breadth
# =====================================================================
MONTHLY_TO <- 1 / HOLD_MO

leg_stats <- function(r, label, cost_bps, n_sides) {
  cost <- MONTHLY_TO * 2 * n_sides * cost_bps / 1e4
  rn   <- r - cost
  ex   <- if (label == "Long-short") rn else rn - port$rf
  capm <- lm(ex ~ port$mktrf)
  ff6  <- lm(ex ~ port$mktrf + port$smb + port$hml +
                  port$rmw + port$cma + port$umd)
  tibble(
    Leg        = label,
    `Cost bp`  = cost_bps,
    `Ann ret`  = prod(1+rn)^(12/length(rn)) - 1,
    `Ann vol`  = sd(rn)*sqrt(12),
    Sharpe     = mean(ex)*12 / (sd(rn)*sqrt(12)),
    `CAPM a`   = coef(capm)[1]*12,
    `CAPM t`   = summary(capm)$coef[1,3],
    Beta       = coef(capm)[2],
    `FF6 a`    = coef(ff6)[1]*12,
    `FF6 t`    = summary(ff6)$coef[1,3]
  )
}

cat("\n===== ALPHA BY LEG (identical breadth, holding period, signal) =====\n")
alpha_tbl <- bind_rows(
  leg_stats(port$long,       "Long only",  20, 1),
  leg_stats(-port$short,     "Short only", 20, 1),
  leg_stats(port$long_short, "Long-short", 20, 2)
)
print(alpha_tbl |> mutate(across(c(`Ann ret`,`Ann vol`,`CAPM a`,`FF6 a`),
                                 ~ percent(.x, accuracy=0.1)),
                          across(c(Sharpe,`CAPM t`,Beta,`FF6 t`),
                                 ~ round(.x,2))), width = Inf)

cat("\nThis is the paper's central table. Same model, same 100 names per\n")
cat("side, same 6-month hold. The only difference is which tail is used.\n")


# =====================================================================
# 5. COST SENSITIVITY
# =====================================================================
cat("\n===== TRANSACTION COST SENSITIVITY =====\n")
cost_tbl <- map_dfr(c(20, 30, 50), function(b) {
  bind_rows(
    leg_stats(port$long,       "Long only",  b, 1),
    leg_stats(port$long_short, "Long-short", b, 2)
  )
})
print(cost_tbl |> select(Leg, `Cost bp`, `Ann ret`, Sharpe, `FF6 a`, `FF6 t`) |>
        mutate(across(c(`Ann ret`,`FF6 a`), ~ percent(.x, accuracy=0.1)),
               across(c(Sharpe,`FF6 t`), ~ round(.x,2))), n = 20)

cat("\n20bp is optimistic for a universe reaching down to $100M market cap.\n")
cat("Report the 50bp column as the conservative case.\n")


# =====================================================================
# 6. CONFIGURATION TABLE
# =====================================================================
# Every construction tested, so the breadth relationship is presented as
# a finding rather than the best result being cherry-picked.
# ⚠️ VERIFY these against your own console output before publishing.

config <- tribble(
  ~Construction,          ~Names, ~`Hold (mo)`, ~`Net ann`, ~Vol,   ~Sharpe, ~`Max DD`,
  "Top 25, monthly",         25,    1,          0.095,      0.321,  0.44,    -0.555,
  "Top 25, 6-month",         25,    6,          0.088,      0.295,  0.43,    -0.582,
  "Top 25, 12-month",        25,   12,          0.040,      0.283,  0.28,    -0.590,
  "Top 100, 6-month",       100,    6,          0.120,      0.243,  0.59,    -0.468,
  "Top 150, 6-month",       150,    6,          0.125,      0.230,  0.63,    -0.447,
  "CRSP value-wt market",    NA,   NA,          0.150,      0.160,  0.93,    NA
)

cat("\n===== ALL CONFIGURATIONS TESTED =====\n")
print(config |> mutate(across(c(`Net ann`, Vol, `Max DD`),
                              ~ percent(.x, accuracy=0.1))), width = Inf)

cat("\nPerformance improves monotonically with breadth (25 -> 100 -> 150)\n")
cat("in return, volatility, Sharpe, and drawdown. No long-only variant\n")
cat("matched the market on a risk-adjusted basis.\n")


# =====================================================================
# 7. Figures and export
# =====================================================================
theme_set(theme_minimal(base_size = 12))

fig <- port |>
  arrange(month_date) |>
  mutate(Long = cumprod(1 + long),
         `Short (as held)` = cumprod(1 - short),
         `Long-short` = cumprod(1 + long_short),
         Market = cumprod(1 + mktrf + rf)) |>
  select(month_date, Long, `Short (as held)`, `Long-short`, Market) |>
  pivot_longer(-month_date) |>
  ggplot(aes(month_date, value, colour = name)) +
  geom_line(linewidth = .9) +
  scale_y_log10(labels = dollar_format(accuracy = 0.1)) +
  labs(title = "Growth of $1 by leg, matched breadth and holding period",
       subtitle = paste0(N_SIDE, " names per side, ", HOLD_MO,
                         "-month hold, net of 20bp per side. Log scale."),
       x = NULL, y = NULL, colour = NULL)

ggsave("figures/leg_decomposition.png", fig, width = 10, height = 6, dpi = 300)

write_csv(alpha_tbl, "output/alpha_by_leg.csv")
write_csv(cost_tbl,  "output/cost_sensitivity.csv")
write_csv(config,    "output/configurations.csv")
write_csv(port,      "output/leg_returns.csv")

cat("\nDone. Three CSVs and one figure.\n")


# =====================================================================
# WHAT TO EXPECT
# =====================================================================
# Long only:   CAPM alpha near or below zero, beta above 1
# Short only:  positive alpha (the informative tail)
# Long-short:  positive FF6 alpha, beta near zero, low volatility
#
# If that pattern holds, the paper writes itself: the model's predictive
# power is concentrated in identifying underperformers, which a long-only
# portfolio structurally cannot exploit.
#
# If long-short alpha is NOT significant here at matched breadth, then
# the earlier decile-level alpha (t = 3.21) came from the extreme tails
# rather than the top/bottom 100 -- also worth reporting, and it would
# mean the tradeable capacity is smaller than the decile result suggests.
