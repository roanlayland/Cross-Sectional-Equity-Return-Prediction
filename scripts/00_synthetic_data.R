# =====================================================================
# AlphaQuant — Synthetic panel generator
#
# Produces data/panel_ranked_SYNTH.rds matching the column contract.
# Signals of KNOWN strength are planted, so you can verify your pipeline
# recovers what was put in.
#
# ⚠️ FOR PIPELINE TESTING ONLY. Never put a synthetic result in the paper.
#
# Why this works: real data cannot tell you whether your code is correct,
# because you don't know the true answer. Here you do. If your backtest
# recovers ~0.03 IC, the pipeline is sound. If it reports 0.35, you have
# a leak — and you found it on fake data in an afternoon.
# =====================================================================

library(tidyverse)
library(lubridate)
set.seed(42)

# ---------------------------------------------------------------------
# GROUND TRUTH — write these down, then check whether you recover them
# ---------------------------------------------------------------------
TRUE_BETAS <- c(
  rk_mom_12_1 = 0.045,   # strongest linear signal
  rk_roe      = 0.030,
  rk_ep       = 0.022,
  rk_fcfp     = 0.018,
  rk_leverage = -0.015,  # negative: leverage hurts
  rk_asset_growth = -0.020
)
# Every other feature has a TRUE coefficient of exactly zero.
# Your model should find nothing in them. If it does, it is overfitting.

TRUE_INTERACTION <- 0.020   # high mom AND high roe together, nonlinear
IDIO_VOL   <- 0.42          # annual idiosyncratic vol
MARKET_VOL <- 0.16          # common component — this is what makes
# pooled t-stats wrong, and it's here on purpose

N_STOCKS <- 1500
# Seq from the FIRST of the month, then snap to month-end. Starting from
# the 31st overflows ("Feb 31" -> Mar 3) and produces duplicate dates.
DATES <- seq(as.Date("1995-01-01"), as.Date("2025-12-01"), by = "month") |>
  (\(d) ceiling_date(d, "month") - 1)()

stopifnot(!any(duplicated(DATES)))

FEATURES <- c("ep","bp","sp","fcfp","divy","ebitda_ev",
              "roe","roa","roic","gross_margin","op_margin","net_margin",
              "rev_growth","eps_growth","asset_growth","fcf_growth",
              "leverage","current_ratio","cash_ratio","int_coverage",
              "log_mktcap","vol_12m","turnover",
              "mom_12_1","mom_6_1","mom_1m")


# ---------------------------------------------------------------------
# 1. Stock universe with realistic entry and exit
# ---------------------------------------------------------------------
stocks <- tibble(
  permno = 10001:(10000 + N_STOCKS),
  ticker = paste0("SYN", str_pad(1:N_STOCKS, 4, pad = "0")),
  gsector = sample(c("10","15","20","25","30","35","40","45","50","55","60"),
                   N_STOCKS, replace = TRUE),
  entry = sample(seq_along(DATES)[1:200], N_STOCKS, replace = TRUE),
  life  = round(rlnorm(N_STOCKS, log(180), 0.6))
) |>
  mutate(exit = pmin(entry + life, length(DATES)))

panel_skeleton <- stocks |>
  rowwise() |>
  mutate(idx = list(entry:exit)) |>
  unnest(idx) |>
  ungroup() |>
  mutate(form_date = DATES[idx]) |>
  select(permno, ticker, gsector, form_date)

cat("Skeleton rows:", nrow(panel_skeleton), "\n")


# ---------------------------------------------------------------------
# 2. Features with persistence and realistic correlation
# ---------------------------------------------------------------------
# Value factors need to correlate with each other (they do in reality,
# 0.5-0.8), and characteristics need to persist over time rather than
# being redrawn each month.

n_f <- length(FEATURES)
Sigma <- diag(n_f)
dimnames(Sigma) <- list(FEATURES, FEATURES)

value_block <- c("ep","bp","sp","fcfp","ebitda_ev")
prof_block  <- c("roe","roa","roic","gross_margin","op_margin","net_margin")
mom_block   <- c("mom_12_1","mom_6_1")

for (b in list(value_block, prof_block, mom_block)) {
  Sigma[b, b] <- 0.65
}
diag(Sigma) <- 1

L <- chol(Sigma)
PERSIST <- 0.92   # monthly AR(1) on characteristics

panel <- panel_skeleton |>
  arrange(permno, form_date) |>
  group_by(permno) |>
  group_modify(function(d, k) {
    n <- nrow(d)
    innov <- matrix(rnorm(n * n_f), n, n_f) %*% L
    z <- matrix(NA_real_, n, n_f)
    z[1, ] <- innov[1, ]
    if (n > 1) for (i in 2:n) {
      z[i, ] <- PERSIST * z[i-1, ] + sqrt(1 - PERSIST^2) * innov[i, ]
    }
    colnames(z) <- FEATURES
    bind_cols(d, as_tibble(z))
  }) |>
  ungroup()


# ---------------------------------------------------------------------
# 3. Cross-sectional percentile ranks — same step as the real pipeline
# ---------------------------------------------------------------------
panel <- panel |>
  group_by(form_date) |>
  mutate(across(all_of(FEATURES), ~ percent_rank(.x), .names = "rk_{.col}"),
         mktcap = exp(8 + 2 * rk_log_mktcap)) |>
  ungroup()


# ---------------------------------------------------------------------
# 4. Plant the signal
# ---------------------------------------------------------------------
# Structure: common market component + linear signal + one nonlinear
# interaction + heavy idiosyncratic noise.
#
# The market component matters. It makes returns correlated WITHIN each
# month, which is precisely why you must compute t-stats on the time
# series of period-level statistics rather than on pooled stocks. If your
# pipeline reports t = 15 on this data, that error is why.

market <- tibble(
  form_date = DATES,
  mkt_ret = rnorm(length(DATES), 0.08, MARKET_VOL)
)

n_before <- nrow(panel)

# Join first, THEN compute the signal against the joined frame. This is
# the ordering that prevents the recycling bug.
panel <- panel |>
  left_join(market, by = "form_date", relationship = "many-to-one")

stopifnot(nrow(panel) == n_before)

# sum_f beta_f * (x_f - 0.5)  ==  X %*% beta - 0.5 * sum(beta)
fnames   <- names(TRUE_BETAS)
lin_part <- as.numeric(as.matrix(panel[, fnames]) %*% TRUE_BETAS[fnames]) -
  0.5 * sum(TRUE_BETAS)

stopifnot(length(lin_part) == nrow(panel), !any(is.na(lin_part)))

panel <- panel |>
  mutate(
    interaction = TRUE_INTERACTION *
      pmax(0, rk_mom_12_1 - 0.7) * pmax(0, rk_roe - 0.7) * 10,,
    fwd_12m = mkt_ret + lin_part + interaction + rnorm(n(), 0, IDIO_VOL)
  ) |>
  select(-mkt_ret, -interaction)


# ---------------------------------------------------------------------
# 5. Add realistic mess — your cleaning code needs something to clean
# ---------------------------------------------------------------------
panel <- panel |>
  mutate(
    # missingness, heavier in the features that are missing in reality
    across(c(rk_fcfp, rk_ebitda_ev, rk_int_coverage),
           ~ if_else(runif(n()) < 0.18, NA_real_, .x)),
    across(c(rk_eps_growth, rk_fcf_growth),
           ~ if_else(runif(n()) < 0.25, NA_real_, .x)),
    # a few extreme return outliers
    fwd_12m = if_else(runif(n()) < 0.002, fwd_12m * 4, fwd_12m),
    # last 12 months have no realized outcome, same as real data
    fwd_12m = if_else(form_date > max(DATES) %m-% months(12),
                      NA_real_, fwd_12m),
    in_sp500 = rk_log_mktcap > 0.70
  )

dir.create("data", showWarnings = FALSE)
write_rds(panel, "data/panel_ranked_SYNTH.rds")
saveRDS(FEATURES, "data/feature_list.rds")
write_rds(TRUE_BETAS, "data/TRUE_BETAS.rds")

cat("Wrote", nrow(panel), "rows,", n_distinct(panel$form_date), "periods\n")


# =====================================================================
# 6. WHAT YOUR PIPELINE SHOULD RECOVER
# =====================================================================
# Run 02_analysis.R and 06_ml_backtest.R against panel_ranked_SYNTH.rds,
# then compare against these. This is the actual test.

cat("
================== GROUND TRUTH ==================

Single-factor sorts should find:
  mom_12_1        strong positive spread
  roe             moderate positive
  ep, fcfp        weaker positive
  leverage        negative
  asset_growth    negative
  everything else FLAT (true beta = 0)

Fama-MacBeth coefficients should land near:
")
print(TRUE_BETAS)

cat("
Walk-forward backtest should produce:
  Rank IC          0.03 - 0.06
  IC t-stat        3 - 8
  Decile spread    4% - 8% per year
  XGBoost > enet   by a small margin (the planted interaction)

================== FAILURE MODES ==================

Rank IC > 0.15
  -> LOOK-AHEAD LEAK. Most likely: embargo not applied in make_split(),
     ranking done across the whole sample instead of within form_date,
     or a feature built using future data.

t-stats > 15 on single-factor sorts
  -> You pooled stocks instead of using the time series of spreads.
     The market component in this data is specifically there to expose
     that mistake.

Coefficients far off TRUE_BETAS, or significant on zero-beta features
  -> Overfitting, or the rank transform is being applied incorrectly.

Rank IC near 0.00
  -> Features and target misaligned in a join, or the target got shuffled
     relative to the features.

==================================================
")
panel |> count(permno, form_date) |> filter(n > 1)   # empty

grep("source\\(", readLines("scripts/00_synthetic_data.R"), value = TRUE)
