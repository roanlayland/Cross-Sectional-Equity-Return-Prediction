# =====================================================================
# Cross-Sectional-Equity-Return-Prediction — Conditional & interaction models
#
# Question: does explicitly modelling the conditional structure found
# in 07 improve out-of-sample performance over the pooled models in 06?
#
# Three specifications, all inside the same walk-forward + embargo:
#   A. Elastic net + explicit interaction terms
#   B. Separate XGBoost per volatility tercile
#   C. Pooled XGBoost (baseline from 06)
#
# THE DATA-MINING TRAP
# Do not search across factor combinations and report the best backtest.
# With 26 factors the best-of-millions combination looks spectacular by
# chance alone. Everything here is SPECIFIED IN ADVANCE from theory
# (limits to arbitrage) and estimated only on training data.
#=====================================================================

library(tidyverse)
library(lubridate)
library(glmnet)
library(xgboost)

PANEL_PATH <- "data/panel_ranked.rds"
panel      <- read_rds(PANEL_PATH)
FEATURES   <- read_rds("data/feature_list.rds")
RK         <- paste0("rk_", FEATURES)

TRAIN_START  <- as.Date("1994-03-31")
FIRST_TEST   <- 2010
LAST_TEST    <- 2023
EMBARGO_MO   <- 12
VALID_YEARS  <- 3

XGB_ETA <- 0.05; XGB_DEPTH <- 6; XGB_MINCHILD <- 50; XGB_MAXROUNDS <- 600


# =====================================================================
# 1. Imputation + drop degenerate periods  (same as 05 and 06)
# =====================================================================
miss_rate  <- panel |> summarise(across(all_of(RK), ~ mean(is.na(.x)))) |>
  pivot_longer(everything(), names_to = "term", values_to = "rate")
needs_flag <- miss_rate |> filter(rate > 0.05) |> pull(term)

panel <- panel |>
  mutate(across(all_of(needs_flag), ~ as.integer(is.na(.x)),
                .names = "miss_{.col}")) |>
  group_by(form_date) |>
  mutate(across(all_of(RK), ~ {
    m <- is.na(.x); if (any(m) && !all(m)) .x[m] <- median(.x, na.rm = TRUE); .x
  })) |>
  ungroup()

RK <- c(RK, paste0("miss_", needs_flag))

bad_periods <- panel |>
  group_by(form_date) |>
  summarise(across(all_of(RK), ~ mean(is.na(.x))), .groups = "drop") |>
  pivot_longer(-form_date) |> filter(value == 1) |>
  distinct(form_date) |> pull(form_date)

panel <- panel |> filter(!form_date %in% bad_periods)


# =====================================================================
# 2. Interaction features
# =====================================================================
# Specified from the 07 finding: factor spreads scale with idiosyncratic
# volatility, consistent with limits to arbitrage. Centering at 0.5 (the
# rank midpoint) keeps the main effects interpretable.

panel <- panel |>
  mutate(
    c_vol  = rk_vol_12m - 0.5,
    c_size = rk_log_mktcap - 0.5,
    # volatility interactions — the limits-to-arbitrage prediction
    x_vol_assetgrowth = c_vol * (rk_asset_growth - 0.5),
    x_vol_sp          = c_vol * (rk_sp - 0.5),
    x_vol_grossmargin = c_vol * (rk_gross_margin - 0.5),
    x_vol_fcfp        = c_vol * (rk_fcfp - 0.5),
    # size interactions — the classic small-cap concentration
    x_size_assetgrowth = c_size * (rk_asset_growth - 0.5),
    x_size_sp          = c_size * (rk_sp - 0.5),
    x_size_bp          = c_size * (rk_bp - 0.5)
  )

INTERACTIONS <- c("x_vol_assetgrowth","x_vol_sp","x_vol_grossmargin",
                  "x_vol_fcfp","x_size_assetgrowth","x_size_sp","x_size_bp")

RK_X <- c(RK, INTERACTIONS)

model_data <- panel |>
  filter(!is.na(fwd_12m), form_date >= TRAIN_START) |>
  select(permno, ticker, form_date, gsector, mktcap, in_sp500,
         fwd_12m, rk_vol_12m, all_of(RK_X)) |>
  drop_na(all_of(RK_X))

cat("Rows:", nrow(model_data),
    "| Periods:", n_distinct(model_data$form_date), "\n")


# =====================================================================
# 3. Walk-forward split (identical to 06)
# =====================================================================
make_split <- function(test_year) {
  test_start <- as.Date(paste0(test_year, "-01-01"))
  train_end  <- test_start %m-% months(EMBARGO_MO)
  train      <- model_data |> filter(form_date < train_end)
  valid_start <- train_end %m-% years(VALID_YEARS)
  inner_end   <- valid_start %m-% months(EMBARGO_MO)
  list(train = train,
       test  = model_data |> filter(year(form_date) == test_year),
       inner_fit = train |> filter(form_date < inner_end),
       inner_val = train |> filter(form_date >= valid_start))
}


# =====================================================================
# 4. Model fitters
# =====================================================================

fit_enet_generic <- function(sp, cols, label) {
  xv <- as.matrix(sp$inner_val[, cols]); yv <- sp$inner_val$fwd_12m
  best <- map_dfr(c(0, 0.25, 0.5, 0.75, 1), function(a) {
    m <- glmnet(as.matrix(sp$inner_fit[, cols]), sp$inner_fit$fwd_12m, alpha = a)
    tibble(alpha = a, lambda = m$lambda,
           mse = colMeans((predict(m, xv) - yv)^2))
  }) |> slice_min(mse, n = 1, with_ties = FALSE)
  
  m <- glmnet(as.matrix(sp$train[, cols]), sp$train$fwd_12m,
              alpha = best$alpha, lambda = best$lambda)
  tibble(pred = as.numeric(predict(m, as.matrix(sp$test[, cols]))),
         model = label)
}

xgb_params <- list(objective = "reg:squarederror",
                   eta = XGB_ETA, max_depth = XGB_DEPTH,
                   subsample = 0.7, colsample_bytree = 0.7,
                   min_child_weight = XGB_MINCHILD, lambda = 5, nthread = 4)

xgb_best_rounds <- function(fit_df, val_df, cols) {
  d1 <- xgb.DMatrix(as.matrix(fit_df[, cols]), label = fit_df$fwd_12m)
  d2 <- xgb.DMatrix(as.matrix(val_df[, cols]), label = val_df$fwd_12m)
  lg <- capture.output(
    xgb.train(xgb_params, d1, nrounds = XGB_MAXROUNDS,
              evals = list(val = d2), early_stopping_rounds = 50, verbose = 1)
  )
  r <- as.numeric(str_extract(lg, "(?<=val-rmse:)[0-9.]+"))
  r <- r[!is.na(r)]
  max(if (length(r)) which.min(r) else 100L, 10L)
}

fit_xgb_generic <- function(sp, cols, label) {
  n <- xgb_best_rounds(sp$inner_fit, sp$inner_val, cols)
  m <- xgb.train(xgb_params,
                 xgb.DMatrix(as.matrix(sp$train[, cols]), label = sp$train$fwd_12m),
                 nrounds = n, verbose = 0)
  tibble(pred = predict(m, as.matrix(sp$test[, cols])), model = label)
}

# SPEC B: a separate model per volatility tercile.
# Terciles are assigned WITHIN each month, so no future information is
# used to define the regimes.
fit_xgb_by_vol <- function(sp) {
  assign_terc <- function(d) d |> group_by(form_date) |>
    mutate(vterc = ntile(rk_vol_12m, 3)) |> ungroup()
  
  tr <- assign_terc(sp$train); fi <- assign_terc(sp$inner_fit)
  va <- assign_terc(sp$inner_val); te <- assign_terc(sp$test)
  
  out <- map_dfr(1:3, function(k) {
    tr_k <- filter(tr, vterc == k); te_k <- filter(te, vterc == k)
    fi_k <- filter(fi, vterc == k); va_k <- filter(va, vterc == k)
    if (nrow(tr_k) < 5000 || nrow(te_k) == 0) return(NULL)
    
    n <- xgb_best_rounds(fi_k, va_k, RK)
    m <- xgb.train(xgb_params,
                   xgb.DMatrix(as.matrix(tr_k[, RK]), label = tr_k$fwd_12m),
                   nrounds = n, verbose = 0)
    te_k |> select(permno, form_date, fwd_12m) |>
      mutate(pred = predict(m, as.matrix(te_k[, RK])), vterc = k)
  })
  
  # Predictions from three separate models aren't on a common scale.
  # Re-rank within each month so they can be pooled into deciles.
  out |> group_by(form_date) |>
    mutate(pred = percent_rank(pred)) |> ungroup() |>
    mutate(model = "xgb_by_vol")
}


# =====================================================================
# 5. Verify the conditioning on training data only
# =====================================================================
# The interactions were chosen after seeing full-sample results. This
# checks whether the same pattern is visible using pre-2010 data alone —
# i.e. whether a researcher standing in 2009 would have specified them.

cat("\n===== VOLATILITY GRADIENT, PRE-2010 TRAINING DATA ONLY =====\n")
panel |>
  filter(form_date < as.Date("2009-01-01"), !is.na(fwd_12m)) |>
  group_by(form_date) |>
  mutate(vterc = ntile(rk_vol_12m, 3)) |>
  group_by(form_date, vterc) |>
  mutate(d = ntile(rk_asset_growth, 10)) |>
  filter(d %in% c(1, 10)) |>
  group_by(form_date, vterc, d) |>
  summarise(ret = mean(fwd_12m), .groups = "drop") |>
  pivot_wider(names_from = d, values_from = ret, names_prefix = "d") |>
  mutate(spread = d10 - d1) |>
  group_by(vterc) |>
  summarise(avg_spread = mean(spread, na.rm = TRUE),
            t_stat = mean(spread, na.rm = TRUE) /
              (sd(spread, na.rm = TRUE) / sqrt(n()))) |>
  print()
cat("If the gradient is visible here, the specification is defensible.\n")
cat("If it only appears in the full sample, disclose it as ex-post.\n\n")


# =====================================================================
# 6. Run all specifications
# =====================================================================
run_wf <- function(fitter, label) {
  map_dfr(FIRST_TEST:LAST_TEST, function(yr) {
    sp <- make_split(yr)
    if (nrow(sp$test) < 100 || nrow(sp$inner_val) < 1000) return(NULL)
    cat(label, yr, "| train", nrow(sp$train), "| test", nrow(sp$test), "\n")
    out <- fitter(sp)
    if ("fwd_12m" %in% names(out)) return(out)
    bind_cols(sp$test |> select(permno, form_date, fwd_12m), out)
  })
}

preds2 <- bind_rows(
  run_wf(\(s) fit_enet_generic(s, RK,   "enet_base"),  "ENET-base"),
  run_wf(\(s) fit_enet_generic(s, RK_X, "enet_inter"), "ENET-int "),
  run_wf(\(s) fit_xgb_generic(s,  RK,   "xgb_base"),   "XGB-base "),
  run_wf(\(s) fit_xgb_by_vol(s),                       "XGB-vol  ")
)

write_rds(preds2, "output/predictions_conditional.rds")


# =====================================================================
# 7. Compare
# =====================================================================
ic2 <- preds2 |>
  group_by(model, form_date) |>
  filter(n() >= 50) |>
  summarise(ic = cor(pred, fwd_12m, method = "spearman"), .groups = "drop")

ic_sum2 <- ic2 |> group_by(model) |>
  summarise(mean_ic = mean(ic, na.rm = TRUE),
            ic_tstat = mean(ic, na.rm = TRUE) / (sd(ic, na.rm = TRUE)/sqrt(n())),
            hit_rate = mean(ic > 0, na.rm = TRUE), n = n()) |>
  arrange(desc(mean_ic))

cat("\n===== RANK IC BY SPECIFICATION =====\n"); print(ic_sum2)

spreads2 <- preds2 |>
  group_by(model, form_date) |>
  mutate(d = ntile(pred, 10)) |>
  filter(d %in% c(1, 10)) |>
  group_by(model, form_date, d) |>
  summarise(ret = mean(fwd_12m), .groups = "drop") |>
  pivot_wider(names_from = d, values_from = ret, names_prefix = "d") |>
  mutate(spread = d10 - d1)

bt2 <- spreads2 |> group_by(model) |>
  summarise(gross = mean(spread, na.rm = TRUE),
            t = mean(spread, na.rm = TRUE)/(sd(spread, na.rm = TRUE)/sqrt(n())),
            sharpe = mean(spread, na.rm = TRUE)/sd(spread, na.rm = TRUE),
            worst = min(spread, na.rm = TRUE)) |>
  arrange(desc(gross))

cat("\n===== DECILE SPREADS =====\n"); print(bt2)

# Fama-French alpha — the real test
if (file.exists("data/ff_factors.rds")) {
  ff <- read_rds("data/ff_factors.rds") |>
    mutate(form_date = ceiling_date(date, "month") - 1)
  alpha2 <- spreads2 |> left_join(ff, by = "form_date") |> drop_na(mktrf) |>
    group_by(model) |>
    group_modify(~ broom::tidy(
      lm(spread ~ mktrf + smb + hml + rmw + cma + umd, data = .x))) |>
    filter(term == "(Intercept)") |> ungroup() |>
    select(model, alpha = estimate, alpha_t = statistic, p.value) |>
    arrange(desc(alpha_t))
  cat("\n===== FAMA-FRENCH 6-FACTOR ALPHA =====\n"); print(alpha2)
}

# Paired comparisons on identical periods
cat("\n===== DID CONDITIONING HELP? =====\n")
ic2 |> pivot_wider(names_from = model, values_from = ic) |> drop_na() |>
  summarise(
    inter_minus_base = mean(enet_inter - enet_base),
    inter_t = mean(enet_inter - enet_base)/(sd(enet_inter - enet_base)/sqrt(n())),
    volmodel_minus_base = mean(xgb_by_vol - xgb_base),
    volmodel_t = mean(xgb_by_vol - xgb_base)/(sd(xgb_by_vol - xgb_base)/sqrt(n()))
  ) |> print(width = Inf)

write_csv(ic_sum2, "output/conditional_ic.csv")
write_csv(bt2,     "output/conditional_spreads.csv")
