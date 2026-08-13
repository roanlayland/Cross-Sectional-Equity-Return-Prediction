# =====================================================================
# Cross-Sectional-Equity-Return-Prediction— ML over an extended period, with optional screen
#
# TWO CHANGES vs 06_ml_backtest.R:
#   1. Test period starts 2000 instead of 2010, so the sample spans two
#      regimes rather than one.
#   2. Optional accruals screen applied before portfolio formation.
#
# WHY THE PERIOD MATTERS MORE THAN THE SCREEN
# The rules-based composite produced CAPM alpha of +7.2% (t = 2.49) over
# 1994-2009 and -6.9% (t = -2.28) over 2010-2024. Every prior ML test
# ran only on the second window. If the ML model shows the same
# reversal, the negative result is a statement about the decade, not
# about the model.
#
# The screen improved composite Sharpe from 0.55 to 0.60 on matched
# months, with alpha t = 1.21 -- not significant. Expect little.
# =====================================================================

library(tidyverse)
library(lubridate)
library(xgboost)
library(glmnet)
library(scales)

PANEL      <- "data/panel_ranked_plus.rds"   # has accruals
FIRST_TEST <- 2000
LAST_TEST  <- 2023
EMBARGO_MO <- 12
VALID_YRS  <- 3
HOLD_MO    <- 6
N_HOLD     <- 100
COST_BPS   <- 20
MIN_PRICE  <- 5
MIN_MKTCAP <- 100
SPLIT      <- as.Date("2010-01-01")

XGB <- list(objective="reg:squarederror", eta=0.05, max_depth=6,
            subsample=0.7, colsample_bytree=0.7,
            min_child_weight=50, lambda=5, nthread=4)

stopifnot(file.exists(PANEL))
panel <- read_rds(PANEL)
msf   <- read_rds("data/msf.rds")
dl    <- read_rds("data/delist.rds")
ff    <- read_rds("data/ff_factors.rds") |>
  mutate(month_date = ceiling_date(date, "month") - 1)

FEATURES <- read_rds("data/feature_list.rds")
RK <- paste0("rk_", FEATURES)
if ("rk_accruals" %in% names(panel)) RK <- unique(c(RK, "rk_accruals"))


# =====================================================================
# 1. Returns
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


# =====================================================================
# 2. Imputation + drop degenerate periods
# =====================================================================
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

dat <- panel |>
  filter(!form_date %in% bad) |>
  left_join(rets |> select(permno, month_date, price),
            by = c("permno","form_date"="month_date")) |>
  filter(price >= MIN_PRICE, mktcap >= MIN_MKTCAP)

cat("Panel ready:", nrow(dat), "rows |",
    n_distinct(dat$form_date), "periods |",
    length(RK), "features\n")


# =====================================================================
# 3. Train one model per year, score everything
# =====================================================================
train_year <- function(yr) {
  train_end <- as.Date(paste0(yr, "-01-01")) %m-% months(EMBARGO_MO)
  tr <- dat |> filter(!is.na(fwd_12m), form_date < train_end) |>
    drop_na(all_of(RK))
  if (nrow(tr) < 15000) return(NULL)

  fit <- tr |> filter(form_date <  max(form_date) %m-% years(VALID_YRS + 1))
  val <- tr |> filter(form_date >= max(form_date) %m-% years(VALID_YRS))
  if (nrow(fit) < 5000 || nrow(val) < 1500) return(NULL)

  lg <- capture.output(
    xgb.train(XGB, xgb.DMatrix(as.matrix(fit[,RK]), label=fit$fwd_12m),
              nrounds = 600,
              evals = list(v = xgb.DMatrix(as.matrix(val[,RK]),
                                           label = val$fwd_12m)),
              early_stopping_rounds = 50, verbose = 1))
  r <- as.numeric(str_extract(lg, "(?<=val-rmse:)[0-9.]+")); r <- r[!is.na(r)]
  n <- max(if (length(r)) which.min(r) else 150L, 10L)

  xgb.train(XGB, xgb.DMatrix(as.matrix(tr[,RK]), label=tr$fwd_12m),
            nrounds = n, verbose = 0)
}

scored <- map_dfr(FIRST_TEST:LAST_TEST, function(yr) {
  m <- train_year(yr)
  if (is.null(m)) { cat("  skip", yr, "(insufficient training data)\n"); return(NULL) }
  cat("  trained", yr, "\n")
  cs <- dat |> filter(year(form_date) == yr) |> drop_na(all_of(RK))
  if (nrow(cs) == 0) return(NULL)
  cs$score <- predict(m, as.matrix(cs[, RK]))
  cs |> select(permno, ticker, form_date, gsector, mktcap, score,
               any_of("rk_accruals"))
})

write_rds(scored, "output/ml_scores_extended.rds")
cat("\nScored", nrow(scored), "stock-months |",
    n_distinct(scored$form_date), "periods |",
    format(min(scored$form_date)), "to", format(max(scored$form_date)), "\n")


# =====================================================================
# 4. Portfolio engine with optional screen
# =====================================================================
# MATCHED MONTHS
# Screening shrinks the pool, which can silently drop months where the
# minimum-size constraint fails. 

has_acc <- "rk_accruals" %in% names(scored)

pool_dates <- function(keep) {
  d <- scored
  if (keep < 1 && has_acc) d <- d |> group_by(form_date) |>
    filter(is.na(rk_accruals) |
             rk_accruals <= quantile(rk_accruals, keep, na.rm=TRUE)) |> ungroup()
  d |> count(form_date) |> filter(n >= N_HOLD * 2) |> pull(form_date)
}

KEEPS <- if (has_acc) c(1.00, 0.75, 0.50, 0.30) else 1.00
common <- reduce(map(KEEPS, pool_dates), intersect)
cat("Common months across all screens:", length(common), "\n")

run_ml <- function(keep, label, date_from = NULL, date_to = NULL) {
  d <- scored |> filter(form_date %in% common)
  if (!is.null(date_from)) d <- d |> filter(form_date >= date_from)
  if (!is.null(date_to))   d <- d |> filter(form_date <  date_to)

  # accruals is raw-ranked: LOW is good (Sloan). Keep the best `keep`.
  if (keep < 1 && has_acc) d <- d |> group_by(form_date) |>
    filter(is.na(rk_accruals) |
             rk_accruals <= quantile(rk_accruals, keep, na.rm=TRUE)) |> ungroup()

  picks <- d |> group_by(form_date) |>
    filter(n() >= N_HOLD * 2) |>
    slice_max(score, n = N_HOLD) |> ungroup()
  if (nrow(picks) < 100) return(NULL)

  held <- picks |> select(cohort = form_date, permno) |>
    crossing(k = 1:HOLD_MO) |>
    mutate(month_date = ceiling_date(cohort %m+% months(k), "month") - 1) |>
    left_join(rets |> select(permno, month_date, ret_adj),
              by = c("permno","month_date")) |>
    mutate(ret_adj = replace_na(ret_adj, 0)) |>
    group_by(cohort, month_date) |>
    summarise(ret = mean(ret_adj), .groups="drop")

  p <- held |> group_by(month_date) |>
    summarise(ret = mean(ret), nc = n_distinct(cohort), .groups="drop") |>
    filter(nc == HOLD_MO) |>
    left_join(ff |> select(month_date, mktrf, smb, hml, rmw, cma, umd, rf),
              by="month_date") |> drop_na(mktrf)
  if (nrow(p) < 30) return(NULL)

  rn <- p$ret - (1/HOLD_MO)*2*COST_BPS/1e4
  ex <- rn - p$rf
  capm <- lm(ex ~ p$mktrf)
  ff6  <- lm(ex ~ p$mktrf+p$smb+p$hml+p$rmw+p$cma+p$umd)

  tibble(strategy = label, n_mo = nrow(p),
         ann_ret = prod(1+rn)^(12/length(rn)) - 1,
         mkt_ret = prod(1+p$mktrf+p$rf)^(12/nrow(p)) - 1,
         vol = sd(rn)*sqrt(12),
         sharpe = mean(ex)*12/(sd(rn)*sqrt(12)),
         mkt_sharpe = mean(p$mktrf)*12/(sd(p$mktrf)*sqrt(12)),
         beta = coef(capm)[2],
         capm_a = coef(capm)[1]*12, capm_t = summary(capm)$coef[1,3],
         ff6_a = coef(ff6)[1]*12, ff6_t = summary(ff6)$coef[1,3])
}

fmt <- function(x) x |>
  mutate(across(c(ann_ret, mkt_ret, vol, capm_a, ff6_a),
                ~ percent(.x, accuracy=0.1)),
         across(c(sharpe, mkt_sharpe, beta, capm_t, ff6_t), ~ round(.x,2)))


# =====================================================================
# 5. THE MAIN RESULT — full period vs each era
# =====================================================================
cat("\n========== ML MODEL BY ERA (no screen) ==========\n")
bind_rows(
  run_ml(1.00, "Full 2000-2023"),
  run_ml(1.00, "2000-2009", NULL, SPLIT),
  run_ml(1.00, "2010-2023", SPLIT, NULL)
) |> fmt() |> print(width = Inf)

cat("\nCompare against the rules-based composite:\n")
cat("  1994-2009: alpha +7.2% (t = 2.49), beta 0.85\n")
cat("  2010-2024: alpha -6.9% (t = -2.28), beta 1.25\n")
cat("If the ML model shows the same reversal, the earlier negative\n")
cat("result was a statement about the decade, not the model.\n")


# =====================================================================
# 6. Does the screen help the ML model?
# =====================================================================
if (has_acc) {
  cat("\n========== ACCRUALS SCREEN ON ML PICKS ==========\n")
  cat("All rows use the same", length(common), "common months.\n\n")
  bind_rows(
    run_ml(1.00, "No screen"),
    run_ml(0.75, "Drop worst 25%"),
    run_ml(0.50, "Best half"),
    run_ml(0.30, "Best 30%")
  ) |> fmt() |> print(width = Inf)

  cat("\n========== SCREEN, BY ERA ==========\n")
  bind_rows(
    run_ml(1.00, "2000-2009 none",   NULL, SPLIT),
    run_ml(0.50, "2000-2009 best50", NULL, SPLIT),
    run_ml(1.00, "2010-2023 none",   SPLIT, NULL),
    run_ml(0.50, "2010-2023 best50", SPLIT, NULL)
  ) |> fmt() |> print(width = Inf)

  cat("\nThe screen is real only if it helps in BOTH eras.\n")
} else {
  cat("\nrk_accruals not found; screen skipped.\n")
}


# =====================================================================
# 7. Rank IC by era
# =====================================================================
fwd1 <- rets |> arrange(permno, month_date) |> group_by(permno) |>
  mutate(fwd_1m = lead(ret_adj, 1)) |> ungroup() |>
  select(permno, month_date, fwd_1m)

ic_era <- scored |>
  left_join(fwd1, by = c("permno","form_date"="month_date")) |>
  group_by(form_date) |>
  filter(sum(!is.na(score) & !is.na(fwd_1m)) >= 50) |>
  summarise(ic = suppressWarnings(cor(score, fwd_1m, method="spearman",
                                      use="complete.obs")), .groups="drop") |>
  filter(!is.na(ic)) |>
  mutate(era = if_else(form_date < SPLIT, "2000-2009", "2010-2023"))

cat("\n========== RANK IC BY ERA ==========\n")
bind_rows(
  ic_era |> summarise(era = "Full", mean_ic = mean(ic),
                      t = mean(ic)/(sd(ic)/sqrt(n())), n_mo = n()),
  ic_era |> group_by(era) |>
    summarise(mean_ic = mean(ic), t = mean(ic)/(sd(ic)/sqrt(n())),
              n_mo = n())
) |> mutate(mean_ic = round(mean_ic,4), t = round(t,2)) |> print()


# =====================================================================
# 8. Export
# =====================================================================
write_csv(ic_era, "output/ml_ic_by_era.csv")

