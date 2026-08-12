# =====================================================================
# AlphaQuant — Session 7: Walk-forward ML backtest
#
# Trains elastic net, random forest, and XGBoost in an expanding window
# with a 12-month embargo, then evaluates with rank IC and decile spreads.
#
# ⚠️ THE EMBARGO IS THE WHOLE POINT. Your target is a 12-month forward
# return, so a stock formed in Jan 2010 has an outcome realized in Jan
# 2011. Training on Jan 2010 and testing on 2011 leaks. Every default CV
# scheme in caret and tidymodels gets this wrong.
# =====================================================================

library(tidyverse)
library(lubridate)
library(glmnet)
library(ranger)
library(xgboost)

# ---- SWITCH BETWEEN SYNTHETIC AND REAL HERE ------------------------
#PANEL_PATH <- "data/panel_ranked_SYNTH.rds"
PANEL_PATH <- "data/panel_ranked.rds"
# --------------------------------------------------------------------

panel    <- read_rds(PANEL_PATH)
FEATURES <- read_rds("data/feature_list.rds")
RK       <- paste0("rk_", FEATURES)
IS_SYNTH <- str_detect(PANEL_PATH, "SYNTH")

TRAIN_START  <- as.Date("1995-01-31")
FIRST_TEST   <- 2010
LAST_TEST    <- 2024
EMBARGO_MO   <- 12
VALID_YEARS  <- 3     # tail of training window, for hyperparameter choice

# ---- XGBoost hyperparameters ---------------------------------------
# Returns have a signal-to-noise ratio near 1:6, so trees need enough
# capacity to find structure but not so much that they fit noise.
# min_child_weight controls this most directly: too large and every
# split averages the signal away along with the noise.
XGB_ETA       <- 0.05
XGB_DEPTH     <- 6
XGB_MINCHILD  <- 50
XGB_MAXROUNDS <- 600

# ---- IMPUTATION ----------------------------------------------------
# Must match 05_factor_analysis.R exactly, or the linear and ML results
# are computed on different samples and cannot be compared.
#
# Median is taken WITHIN form_date. Using a global median would leak
# information across time.
miss_rate  <- panel |> summarise(across(all_of(RK), ~ mean(is.na(.x)))) |>
  pivot_longer(everything(), names_to = "term", values_to = "rate")
needs_flag <- miss_rate |> filter(rate > 0.05) |> pull(term)

panel <- panel |>
  mutate(across(all_of(needs_flag),
                ~ as.integer(is.na(.x)), .names = "miss_{.col}")) |>
  group_by(form_date) |>
  mutate(across(all_of(RK), ~ {
    m <- is.na(.x)
    if (any(m) && !all(m)) .x[m] <- median(.x, na.rm = TRUE)
    .x
  })) |>
  ungroup()

RK <- c(RK, paste0("miss_", needs_flag))
cat("Imputed.", length(needs_flag), "missingness flags added.\n")
# --------------------------------------------------------------------
# ---- DROP DEGENERATE PERIODS ---------------------------------------
# The first ~14 months have features that are 100% missing: lagged
# fundamentals and 12-month momentum don't exist yet. Median imputation
# can't fill a column with no observations, so models see zero complete
# cases. Drop these periods entirely.
bad_periods <- panel |>
  group_by(form_date) |>
  summarise(across(all_of(RK), ~ mean(is.na(.x))), .groups = "drop") |>
  pivot_longer(-form_date) |>
  filter(value == 1) |>
  distinct(form_date) |>
  pull(form_date)

cat("Dropping", length(bad_periods), "periods with fully-missing features\n")
if (length(bad_periods)) cat("  range:", format(range(bad_periods)), "\n")

panel <- panel |> filter(!form_date %in% bad_periods)

cat("Remaining:", n_distinct(panel$form_date), "periods,",
    nrow(panel), "rows\n")
# --------------------------------------------------------------------
model_data <- panel |>
  filter(!is.na(fwd_12m), form_date >= TRAIN_START) |>
  select(permno, ticker, form_date, gsector, mktcap, in_sp500,
         fwd_12m, all_of(RK)) |>
  drop_na(all_of(RK))   # now drops almost nothing

stopifnot(nrow(model_data) > 0.9 * sum(!is.na(panel$fwd_12m)))

cat("Rows:", nrow(model_data),
    "| Periods:", n_distinct(model_data$form_date), "\n")


# =====================================================================
# 1. The split function — read this carefully
# =====================================================================
make_split <- function(test_year) {
  test_start <- as.Date(paste0(test_year, "-01-01"))
  
  # Last usable training formation date: its 12-month outcome must have
  # been fully realized BEFORE the test period begins.
  train_end <- test_start %m-% months(EMBARGO_MO)
  
  train <- model_data |> filter(form_date < train_end)
  test  <- model_data |> filter(year(form_date) == test_year)
  
  # Inner validation split, also embargoed
  valid_start <- train_end %m-% years(VALID_YEARS)
  inner_end   <- valid_start %m-% months(EMBARGO_MO)
  
  list(
    train      = train,
    test       = test,
    inner_fit  = train |> filter(form_date < inner_end),
    inner_val  = train |> filter(form_date >= valid_start),
    train_end  = train_end,
    test_year  = test_year
  )
}

# ---- VERIFY THE EMBARGO before running anything else
s <- make_split(2015)
cat("Test year 2015\n",
    " last train form_date:", format(max(s$train$form_date)), "\n",
    " its outcome realized:", format(max(s$train$form_date) %m+% months(12)), "\n",
    " first test form_date:", format(min(s$test$form_date)), "\n")
# The realized date must be <= the first test date. If not, STOP.


# =====================================================================
# 2. Model fitters — each returns predictions on `test`
# =====================================================================

fit_enet <- function(sp) {
  x  <- as.matrix(sp$train[, RK]);  y  <- sp$train$fwd_12m
  xv <- as.matrix(sp$inner_val[, RK]); yv <- sp$inner_val$fwd_12m
  
  # Choose alpha and lambda on the embargoed inner validation set only
  grid <- expand_grid(alpha = c(0, 0.25, 0.5, 0.75, 1))
  best <- map_dfr(grid$alpha, function(a) {
    m <- glmnet(as.matrix(sp$inner_fit[, RK]), sp$inner_fit$fwd_12m, alpha = a)
    p <- predict(m, xv)
    tibble(alpha = a,
           lambda = m$lambda,
           mse = colMeans((p - yv)^2))
  }) |> slice_min(mse, n = 1, with_ties = FALSE)   # ties -> vector -> glmnet errors
  
  stopifnot(length(best$alpha) == 1, length(best$lambda) == 1)
  
  m <- glmnet(x, y, alpha = best$alpha, lambda = best$lambda)
  tibble(pred = as.numeric(predict(m, as.matrix(sp$test[, RK]))),
         model = "elastic_net",
         param = paste0("a=", best$alpha, " l=", signif(best$lambda, 3)))
}

fit_rf <- function(sp) {
  m <- ranger(
    x = sp$train[, RK], y = sp$train$fwd_12m,
    num.trees = 500, mtry = floor(sqrt(length(RK))),
    min.node.size = 500,          # large: returns are extremely noisy
    max.depth = 8,
    sample.fraction = 0.6,
    num.threads = parallel::detectCores() - 1,
    seed = 42
  )
  tibble(pred = predict(m, sp$test[, RK])$predictions,
         model = "random_forest",
         param = "ntree=500 depth=8 minnode=500")
}

fit_xgb <- function(sp) {
  dtrain <- xgb.DMatrix(as.matrix(sp$inner_fit[, RK]), label = sp$inner_fit$fwd_12m)
  dvalid <- xgb.DMatrix(as.matrix(sp$inner_val[, RK]), label = sp$inner_val$fwd_12m)
  
  p <- list(objective = "reg:squarederror",
            eta = XGB_ETA, max_depth = XGB_DEPTH,
            subsample = 0.7, colsample_bytree = 0.7,
            min_child_weight = XGB_MINCHILD, lambda = 5, nthread = 4)
  
  # xgboost 3.x returns an external pointer; best_iteration / niter /
  # evaluation_log are no longer list elements. Capture the printed
  # evaluation log instead — version-agnostic and always available.
  log_txt <- capture.output(
    m0 <- xgb.train(p, dtrain, nrounds = XGB_MAXROUNDS,
                    evals = list(val = dvalid),
                    early_stopping_rounds = 50, verbose = 1)
  )
  
  rmse <- as.numeric(str_extract(log_txt, "(?<=val-rmse:)[0-9.]+"))
  rmse <- rmse[!is.na(rmse)]
  best_n <- if (length(rmse) > 0) which.min(rmse) else 100L
  best_n <- max(as.integer(best_n), 10L)
  
  dfull <- xgb.DMatrix(as.matrix(sp$train[, RK]), label = sp$train$fwd_12m)
  m <- xgb.train(p, dfull, nrounds = best_n, verbose = 0)
  
  tibble(pred = predict(m, as.matrix(sp$test[, RK])),
         model = "xgboost",
         param = paste0("eta=", XGB_ETA, " depth=", XGB_DEPTH,
                        " minchild=", XGB_MINCHILD, " nrounds=", best_n))
}


# =====================================================================
# 3. The walk-forward loop
# =====================================================================
run_walkforward <- function(fitter, label) {
  map_dfr(FIRST_TEST:LAST_TEST, function(yr) {
    sp <- make_split(yr)
    if (nrow(sp$test) < 100 || nrow(sp$inner_val) < 1000) return(NULL)
    cat(label, yr, "| train n =", nrow(sp$train),
        "| test n =", nrow(sp$test), "\n")
    out <- fitter(sp)
    bind_cols(sp$test |> select(permno, ticker, form_date, gsector,
                                mktcap, in_sp500, fwd_12m), out)
  })
}

preds <- bind_rows(
  run_walkforward(fit_enet, "ENET"),
  run_walkforward(fit_rf,   "RF  "),
  run_walkforward(fit_xgb,  "XGB ")
)

write_rds(preds, "output/predictions.rds")


# =====================================================================
# 4. Rank IC — the primary metric
# =====================================================================
# Spearman correlation between predicted and actual, WITHIN each period,
# then averaged. This measures the thing that matters: did you order the
# stocks correctly? RMSE does not, and is dominated by a few stocks that
# tripled.

ic_by_period <- preds |>
  group_by(model, form_date) |>
  filter(n() >= 50) |>
  summarise(ic = cor(pred, fwd_12m, method = "spearman"), .groups = "drop")

ic_summary <- ic_by_period |>
  group_by(model) |>
  summarise(
    mean_ic  = mean(ic, na.rm = TRUE),
    sd_ic    = sd(ic, na.rm = TRUE),
    ic_tstat = mean(ic, na.rm = TRUE) / (sd(ic, na.rm = TRUE) / sqrt(n())),
    hit_rate = mean(ic > 0, na.rm = TRUE),
    n_periods = n()
  ) |>
  arrange(desc(mean_ic))

print(ic_summary)

# ⚠️ SANITY GATE
# mean_ic of 0.02-0.05  -> plausible, this is what real signals look like
# mean_ic > 0.15        -> you have a leak. Stop and find it.
if (max(ic_summary$mean_ic) > 0.15) {
  warning("Rank IC implausibly high. Check for look-ahead bias before ",
          "interpreting anything below.")
}


# =====================================================================
# 5. Decile portfolios, gross and net of costs
# =====================================================================
COST_BPS <- 20   # per side, one-way

deciles <- preds |>
  group_by(model, form_date) |>
  mutate(dec = ntile(pred, 10)) |>
  group_by(model, form_date, dec) |>
  summarise(ret = mean(fwd_12m), n = n(), .groups = "drop")

spread_by_period <- deciles |>
  filter(dec %in% c(1, 10)) |>
  pivot_wider(names_from = dec, values_from = c(ret, n)) |>
  mutate(spread = ret_10 - ret_1)

# Turnover: how much of the top decile changes each rebalance
turnover <- preds |>
  group_by(model, form_date) |>
  mutate(dec = ntile(pred, 10)) |>
  filter(dec == 10) |>
  select(model, form_date, permno) |>
  group_by(model) |>
  arrange(form_date) |>
  group_modify(function(d, k) {
    dates <- sort(unique(d$form_date))
    map_dfr(seq_along(dates)[-1], function(i) {
      cur  <- d$permno[d$form_date == dates[i]]
      prev <- d$permno[d$form_date == dates[i-1]]
      tibble(form_date = dates[i],
             turnover = 1 - length(intersect(cur, prev)) / length(cur))
    })
  }) |> ungroup()

avg_turnover <- turnover |> group_by(model) |>
  summarise(turnover = mean(turnover, na.rm = TRUE))

backtest_summary <- spread_by_period |>
  group_by(model) |>
  summarise(
    gross_spread = mean(spread, na.rm = TRUE),
    spread_t     = mean(spread, na.rm = TRUE) / (sd(spread, na.rm = TRUE)/sqrt(n())),
    sharpe       = mean(spread, na.rm = TRUE) / sd(spread, na.rm = TRUE),
    hit_rate     = mean(spread > 0, na.rm = TRUE),
    worst_year   = min(spread, na.rm = TRUE),
    n_periods    = n()
  ) |>
  left_join(avg_turnover, by = "model") |>
  # both legs, both sides, monthly rebalance
  mutate(cost = turnover * (COST_BPS/1e4) * 2 * 2 * 12,
         net_spread = gross_spread - cost) |>
  arrange(desc(net_spread))

print(backtest_summary)


# =====================================================================
# 6. Alpha vs Fama-French 5 + momentum
# =====================================================================
# The question this answers: is your model finding NEW information, or
# repackaging factors that were documented decades ago? Either answer is
# publishable. Not checking is not defensible.

alpha_tests <- NULL

if (file.exists("data/ff_factors.rds")) {
  ff <- read_rds("data/ff_factors.rds") |>
    mutate(form_date = ceiling_date(date, "month") - 1)
  
  alpha_tests <- spread_by_period |>
    left_join(ff, by = "form_date") |>
    drop_na(mktrf) |>
    group_by(model) |>
    group_modify(~ broom::tidy(
      lm(spread ~ mktrf + smb + hml + rmw + cma + umd, data = .x))) |>
    ungroup() |>
    filter(term == "(Intercept)") |>
    rename(alpha = estimate, alpha_t = statistic)
  
  print(alpha_tests)
} else {
  cat("\nSkipping Fama-French alpha test: data/ff_factors.rds not found.\n",
      "This is expected on synthetic data. It becomes available after the\n",
      "WRDS pull, and it is not optional on real results — it answers\n",
      "whether your model found new information or repackaged known factors.\n")
}


# =====================================================================
# 7. Does nonlinearity actually buy anything?
# =====================================================================
# Compare XGBoost against elastic net on IDENTICAL periods. If the answer
# is "barely," say so in the paper. That is a real, well-documented
# finding, and reporting it honestly reads as competence.

ic_by_period |>
  pivot_wider(names_from = model, values_from = ic) |>
  drop_na() |>
  summarise(
    xgb_minus_enet = mean(xgboost - elastic_net),
    t_stat = mean(xgboost - elastic_net) /
      (sd(xgboost - elastic_net) / sqrt(n())),
    xgb_wins = mean(xgboost > elastic_net)
  ) |> print()


# =====================================================================
# 8. Figures
# =====================================================================
theme_set(theme_minimal(base_size = 12))

fig_ic <- ic_by_period |>
  ggplot(aes(form_date, ic, colour = model)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_line(alpha = .4) +
  geom_smooth(se = FALSE, span = .3) +
  labs(title = "Rank information coefficient over time",
       subtitle = "Spearman correlation between predicted and realized returns",
       x = NULL, y = "Rank IC", colour = NULL)
ggsave("figures/ic_timeseries.png", fig_ic, width = 10, height = 5, dpi = 300)

fig_dec <- deciles |>
  group_by(model, dec) |>
  summarise(ret = mean(ret), se = sd(ret)/sqrt(n()), .groups = "drop") |>
  ggplot(aes(factor(dec), ret, fill = model)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = ret - 1.96*se, ymax = ret + 1.96*se),
                position = position_dodge(.9), width = .2) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Out-of-sample forward return by predicted decile",
       x = "Predicted decile (10 = highest)", y = "Mean 12-month return",
       fill = NULL)
ggsave("figures/deciles.png", fig_dec, width = 10, height = 5, dpi = 300)

fig_cum <- spread_by_period |>
  group_by(model) |> arrange(form_date) |>
  mutate(cum = cumprod(1 + spread/12)) |>   # monthly overlapping, scaled
  ggplot(aes(form_date, cum, colour = model)) +
  geom_hline(yintercept = 1, linetype = 2, colour = "grey50") +
  geom_line(linewidth = .9) +
  labs(title = "Cumulative long-short performance, out of sample",
       subtitle = "Long top decile, short bottom decile, gross of costs",
       x = NULL, y = "Growth of $1", colour = NULL)
ggsave("figures/cumulative.png", fig_cum, width = 10, height = 5, dpi = 300)


# =====================================================================
# 9. Export
# =====================================================================
write_csv(ic_summary,        "output/ic_summary.csv")
write_csv(backtest_summary,  "output/backtest_summary.csv")
write_csv(ic_by_period,      "output/ic_by_period.csv")
if (!is.null(alpha_tests)) write_csv(alpha_tests, "output/ff_alpha.csv")


# =====================================================================
# WHAT GOOD LOOKS LIKE
# =====================================================================
# mean_ic        0.02 - 0.05        > 0.15 means a leak
# ic_tstat       2 - 5              > 10 means a leak
# hit_rate       0.55 - 0.68
# gross_spread   0.03 - 0.08 /yr
# net_spread     often about half of gross
# ff_alpha_t     if < 2, your signal is explained by known factors —
#                report that plainly, it is a legitimate finding
#
# If your numbers are far above these ranges, do not celebrate. Go back
# to Session 2 and re-audit the joins. Results that look too good are
# almost always a leak, and finding it yourself is much better than
# having an interviewer find it.