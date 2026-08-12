# =====================================================================
# AlphaQuant — Accruals as a quality screen
#
# Instead of weighting accruals inside the composite, use it as a GATE:
# exclude stocks with the worst accruals, then rank what remains by the
# composite score.
#
# Run AFTER 19_composite_score.R, which leaves `signed`, `rets`, `ff`
# in the environment.
#
# ⚠️ THE TRADE-OFF
# Screening shrinks the candidate pool, so the 100 picks come from
# fewer names and their composite scores are weaker on average. A screen
# only helps if what it removes is worse than what it forces you to
# accept instead. This is not obvious in advance.
#
# ⚠️ FIVE CUTOFFS TESTED
# The best of five looks good by construction. Read the PATTERN across
# cutoffs, not the maximum. A monotonic improvement is believable; a
# single spike is not.
# =====================================================================

library(tidyverse)
library(lubridate)
library(scales)

stopifnot(exists("signed"), exists("rets"), exists("ff"))
stopifnot("s_accruals" %in% names(signed))

HOLD_MO  <- 6
COST_BPS <- 20


# =====================================================================
# 1. Portfolio engine
# =====================================================================
run_screened <- function(d, n_side, hold_mo, cost_bps, label) {
  picks <- d |> group_by(form_date) |>
    filter(n() >= n_side * 2) |>
    slice_max(composite, n = n_side) |> ungroup()
  if (nrow(picks) < 100) return(NULL)

  held <- picks |> select(cohort = form_date, permno) |>
    crossing(k = 1:hold_mo) |>
    mutate(month_date = ceiling_date(cohort %m+% months(k), "month") - 1) |>
    left_join(rets |> select(permno, month_date, ret_adj),
              by = c("permno","month_date")) |>
    mutate(ret_adj = replace_na(ret_adj, 0)) |>
    group_by(cohort, month_date) |>
    summarise(ret = mean(ret_adj), .groups = "drop")

  p <- held |> group_by(month_date) |>
    summarise(ret = mean(ret), nc = n_distinct(cohort), .groups = "drop") |>
    filter(nc == hold_mo) |>
    left_join(ff |> select(month_date, mktrf, smb, hml, rmw, cma, umd, rf),
              by = "month_date") |> drop_na(mktrf)
  if (nrow(p) < 36) return(NULL)

  rn   <- p$ret - (1/hold_mo) * 2 * cost_bps / 1e4
  ex   <- rn - p$rf
  capm <- lm(ex ~ p$mktrf)
  ff6  <- lm(ex ~ p$mktrf + p$smb + p$hml + p$rmw + p$cma + p$umd)

  tibble(strategy = label, n_mo = nrow(p),
         ann_ret = prod(1 + rn)^(12/length(rn)) - 1,
         mkt_ret = prod(1 + p$mktrf + p$rf)^(12/nrow(p)) - 1,
         vol     = sd(rn) * sqrt(12),
         sharpe  = mean(ex) * 12 / (sd(rn) * sqrt(12)),
         mkt_sharpe = mean(p$mktrf) * 12 / (sd(p$mktrf) * sqrt(12)),
         beta    = coef(capm)[2],
         capm_a  = coef(capm)[1] * 12, capm_t = summary(capm)$coef[1,3],
         ff6_a   = coef(ff6)[1] * 12,  ff6_t  = summary(ff6)$coef[1,3])
}


# =====================================================================
# 2. The screen
# =====================================================================
# s_accruals is already SIGNED, so HIGH is good. Keeping the best `keep`
# fraction means keeping values above the (1 - keep) quantile.
#
# Stocks with missing accruals are RETAINED. Dropping them would turn a
# quality screen into a data-availability screen, which is a different
# and less defensible thing.

screen_test <- function(keep, n_side = 100, lo = NULL, hi = NULL, label) {
  d <- signed

  if (!is.null(lo)) {
    d <- d |> group_by(form_date) |>
      mutate(sz = percent_rank(mktcap)) |> ungroup() |>
      filter(sz >= lo, sz <= hi)
  }

  if (keep < 1) {
    d <- d |> group_by(form_date) |>
      filter(is.na(s_accruals) |
               s_accruals >= quantile(s_accruals, 1 - keep, na.rm = TRUE)) |>
      ungroup()
  }

  run_screened(d, n_side, HOLD_MO, COST_BPS, label)
}

fmt <- function(x) x |>
  mutate(across(c(ann_ret, mkt_ret, vol, capm_a, ff6_a),
                ~ percent(.x, accuracy = 0.1)),
         across(c(sharpe, mkt_sharpe, beta, capm_t, ff6_t), ~ round(.x, 2)))


# =====================================================================
# 3. How much does each screen remove?
# =====================================================================
cat("===== POOL SIZE AFTER SCREENING =====\n")
map_dfr(c(1.00, 0.90, 0.75, 0.50, 0.30), function(k) {
  d <- signed
  if (k < 1) d <- d |> group_by(form_date) |>
    filter(is.na(s_accruals) |
             s_accruals >= quantile(s_accruals, 1-k, na.rm=TRUE)) |> ungroup()
  tibble(keep = percent(k, accuracy = 1),
         median_pool = d |> count(form_date) |> pull(n) |> median(),
         min_pool    = d |> count(form_date) |> pull(n) |> min())
}) |> print()

cat("\nIf median_pool falls near 200, a 100-name portfolio is taking\n")
cat("half the available universe and the screen dominates the score.\n")


# =====================================================================
# 4. Screen strength, full universe
# =====================================================================
cat("\n===== ACCRUALS SCREEN, ALL SIZES, 100 NAMES =====\n")
bind_rows(
  screen_test(1.00, label = "No screen"),
  screen_test(0.50, label = "Drop worst 10%"),
  screen_test(0.30, label = "Drop worst 25%"),
  screen_test(0.20, label = "Best half only"),
  screen_test(0.10, label = "Best 30% only")
) |> fmt() |> print(width = Inf)


# =====================================================================
# 5. Screen combined with the best size band
# =====================================================================
cat("\n===== SCREEN + P80-100 =====\n")
bind_rows(
  screen_test(1.00, lo = 0.80, hi = 1.00, label = "P80-100, no screen"),
  screen_test(0.90, lo = 0.80, hi = 1.00, label = "P80-100, drop worst 10%"),
  screen_test(0.75, lo = 0.80, hi = 1.00, label = "P80-100, drop worst 25%"),
  screen_test(0.50, lo = 0.80, hi = 1.00, label = "P80-100, best half")
) |> fmt() |> print(width = Inf)

cat("\n===== SCREEN + P60-95 =====\n")
bind_rows(
  screen_test(1.00, lo = 0.60, hi = 0.95, label = "P60-95, no screen"),
  screen_test(0.75, lo = 0.60, hi = 0.95, label = "P60-95, drop worst 25%"),
  screen_test(0.50, lo = 0.60, hi = 0.95, label = "P60-95, best half")
) |> fmt() |> print(width = Inf)


# =====================================================================
# 6. Wider portfolio, where screening costs less
# =====================================================================
# With 200 names the screen bites harder on pool size, so this shows
# whether the effect survives when breadth is held high.

cat("\n===== SCREEN AT 200 NAMES =====\n")
bind_rows(
  screen_test(1.00, n_side = 200, label = "200 names, no screen"),
  screen_test(0.75, n_side = 200, label = "200 names, drop worst 25%"),
  screen_test(0.50, n_side = 200, label = "200 names, best half")
) |> fmt() |> print(width = Inf)


# =====================================================================
# 7. Does the screen hold in both halves?
# =====================================================================
# Any improvement that only appears in one period is not a screen,
# it is a coincidence.

cat("\n===== SPLIT SAMPLE =====\n")
SPLIT <- as.Date("2010-01-01")

half_test <- function(keep, from, to, label) {
  d <- signed |> filter(form_date >= from, form_date < to)
  if (keep < 1) d <- d |> group_by(form_date) |>
    filter(is.na(s_accruals) |
             s_accruals >= quantile(s_accruals, 1-keep, na.rm=TRUE)) |> ungroup()
  run_screened(d, 100, HOLD_MO, COST_BPS, label)
}

bind_rows(
  half_test(1.00, as.Date("1994-01-01"), SPLIT, "1994-2009 no screen"),
  half_test(0.3, as.Date("1994-01-01"), SPLIT, "1994-2009 screened"),
  half_test(1.00, SPLIT, as.Date("2026-01-01"), "2010-2024 no screen"),
  half_test(0.3, SPLIT, as.Date("2026-01-01"), "2010-2024 screened")
) |> fmt() |> print(width = Inf)

cat("\nThe screen is real only if it helps in BOTH halves.\n")


# =====================================================================
# HOW TO READ THIS
# =====================================================================
# Compare each screened row against the "no screen" row IN THE SAME
# BLOCK -- market returns differ across blocks because the usable date
# ranges differ.
#
# BELIEVABLE: Sharpe improves monotonically as the screen tightens from
#   none -> 10% -> 25% -> 50%, and the improvement appears in both
#   sample halves.
#
# NOT BELIEVABLE: one cutoff spikes while its neighbours are flat. Five
#   cutoffs were tested; the maximum of five noisy draws looks good by
#   construction.
#
# ALSO WATCH: if tightening the screen raises returns but the pool falls
#   below ~400 names, you are no longer running a composite strategy —
#   you are running an accruals strategy with a composite tiebreak.
