# =====================================================================
# AlphaQuant — Build the panel  (self-contained version)
#
# WRDS raw tables  ->  data/panel_ranked.rds
#
# Handles the rdq join internally, so it works whether or not
# data/funda.rds already has report dates attached.
#
# ⚠️ Section 4 is where look-ahead bias lives. Read it.
# =====================================================================

library(tidyverse)
library(lubridate)
library(slider)
library(data.table)

msf     <- read_rds("data/msf.rds")
dl      <- read_rds("data/delist.rds")
funda   <- read_rds("data/funda.rds")
link    <- read_rds("data/link.rds")
comp    <- read_rds("data/company.rds")
idx     <- read_rds("data/sp500_members.rds")
bp_nyse <- read_rds("data/nyse_breakpoints.rds")


# =====================================================================
# 0. Attach report dates (rdq) if not already present
# =====================================================================
# rdq lives in comp.fundq, not comp.funda. A firm's fiscal-year-end
# datadate equals its Q4 datadate, so joining on gvkey + datadate
# attaches the announcement date of the filing that reported the
# annual figures.

if (!"rdq" %in% names(funda)) {
  stopifnot(file.exists("data/rdq.rds"))
  
  rdq_tbl <- read_rds("data/rdq.rds") |>
    mutate(gvkey = as.character(gvkey), datadate = as.Date(datadate)) |>
    filter(!is.na(rdq)) |>
    distinct(gvkey, datadate, .keep_all = TRUE)
  
  funda <- funda |>
    mutate(gvkey = as.character(gvkey), datadate = as.Date(datadate)) |>
    left_join(rdq_tbl, by = c("gvkey", "datadate"))
}

cat("rdq present for", round(100 * mean(!is.na(funda$rdq)), 1),
    "% of firm-years\n")
# Expect 70-90%. Pre-1996 and some filers lack rdq; those fall back to
# the datadate + 6 months convention in Section 4.

if (mean(!is.na(funda$rdq)) < 0.30) {
  warning("rdq match rate is very low. The join keys may not align. ",
          "Check class(funda$gvkey) vs class(rdq_tbl$gvkey).")
}


# =====================================================================
# 1. CRSP monthly: delisting-adjusted returns, momentum, forward return
# =====================================================================
# Returns are COMPOUNDED from monthly returns, never computed from price
# ratios. Price ratios miss dividends and silently ignore delistings.

crsp <- msf |>
  mutate(ym = floor_date(date, "month")) |>
  left_join(dl |> mutate(ym = floor_date(dlstdt, "month")) |>
              select(permno, ym, dlret) |>
              distinct(permno, ym, .keep_all = TRUE),
            by = c("permno", "ym")) |>
  mutate(ret_adj = case_when(
    !is.na(ret) & !is.na(dlret) ~ (1 + ret) * (1 + dlret) - 1,
    is.na(ret) & !is.na(dlret) ~ dlret,
    TRUE ~ ret)) |>
  arrange(permno, ym)

cum_ret <- function(x, n) {
  slide_dbl(x, ~ if (sum(!is.na(.x)) < n) NA_real_ else prod(1 + .x) - 1,
            .before = n - 1, .complete = TRUE)
}

crsp <- crsp |>
  group_by(permno) |>
  mutate(
    mom_12_1   = lag(cum_ret(ret_adj, 11), 1),   # t-12 to t-2
    mom_6_1    = lag(cum_ret(ret_adj, 5),  1),
    mom_1m     = ret_adj,
    vol_12m    = slide_dbl(ret_adj, sd, .before = 11, .complete = TRUE),
    turnover   = vol / shrout,
    mktcap     = abs(prc) * shrout / 1000,
    log_mktcap = log(pmax(mktcap, 1)),
    fwd_12m    = lead(cum_ret(ret_adj, 12), 12)  # TARGET: t+1 .. t+12
  ) |>
  ungroup()

cat("CRSP processed:", nrow(crsp), "rows\n")


# =====================================================================
# 2. Universe: size floor at the NYSE 20th percentile
# =====================================================================
universe <- crsp |>
  left_join(bp_nyse, by = "ym") |>
  filter(shrcd %in% c(10, 11),
         exchcd %in% c(1, 2, 3),
         !is.na(mktcap), mktcap > 0,
         mktcap >= nyse_p20) |>
  mutate(form_date = ceiling_date(ym, "month") - 1)

cat("Universe:", nrow(universe), "stock-months\n")


# =====================================================================
# 3. Compustat ratios
# =====================================================================
fa <- funda |>
  arrange(gvkey, datadate) |>
  group_by(gvkey, fyear) |>
  slice_max(datadate, n = 1, with_ties = FALSE) |>   # dedupe restatements
  group_by(gvkey) |>
  mutate(across(c(at, ceq, revt, epspx, oancf, capx, seq),
                lag, .names = "{.col}_lag")) |>
  ungroup() |>
  mutate(
    pref        = coalesce(pstkrv, pstkl, pstk, 0),
    book_equity = coalesce(ceq, seq - pref, at - lt) + coalesce(txditc, 0) - pref,
    fcf         = oancf - capx,
    total_debt  = coalesce(dltt, 0) + coalesce(dlc, 0),
    ev_ex_mkt   = total_debt - coalesce(che, 0),
    roe   = ib / na_if((ceq + ceq_lag) / 2, 0),
    roa   = ib / na_if((at + at_lag) / 2, 0),
    roic  = oiadp / na_if(total_debt + ceq, 0),
    gross_margin = (revt - cogs) / na_if(revt, 0),
    op_margin    = oiadp / na_if(revt, 0),
    net_margin   = ni / na_if(revt, 0),
    leverage      = total_debt / na_if(at, 0),
    current_ratio = act / na_if(lct, 0),
    cash_ratio    = che / na_if(lct, 0),
    int_coverage  = oiadp / na_if(xint, 0),
    rev_growth   = if_else(revt_lag  > 0, revt / revt_lag - 1, NA_real_),
    eps_growth   = if_else(epspx_lag > 0, epspx / epspx_lag - 1, NA_real_),
    asset_growth = if_else(at_lag    > 0, at / at_lag - 1, NA_real_),
    fcf_growth   = if_else((oancf_lag - capx_lag) > 0,
                           fcf / (oancf_lag - capx_lag) - 1, NA_real_)
  )


# =====================================================================
# 4. ⚠️ THE TIMING RULE — the most important block in the project
# =====================================================================
# A fundamental becomes usable the month AFTER its earnings announcement.
# Where rdq is missing, fall back to datadate + 6 months (the Fama-French
# convention, deliberately conservative).

fa_timed <- fa |>
  mutate(
    avail_rdq  = ceiling_date(rdq %m+% months(1), "month") - 1,
    avail_ff   = ceiling_date(datadate %m+% months(6), "month") - 1,
    avail_date = coalesce(avail_rdq, avail_ff)
  ) |>
  filter(!is.na(avail_date)) |>
  arrange(gvkey, avail_date)

cat("\n--- LOOK-AHEAD AUDIT (pre-join) ---\n")
fa_timed |> filter(!is.na(rdq)) |>
  mutate(gap = as.numeric(avail_date - rdq)) |>
  summarise(min_gap = min(gap), pct_negative = mean(gap < 0),
            median_gap = median(gap)) |> print()
# pct_negative MUST be 0.


# =====================================================================
# 5. Rolling join: most recent already-available fundamental
# =====================================================================
link_dt <- as.data.table(link)

uni_dt <- as.data.table(
  universe |> select(permno, form_date, ticker, mktcap, log_mktcap,
                     vol_12m, turnover, mom_12_1, mom_6_1, mom_1m,
                     fwd_12m, exchcd)
)

uni_dt <- merge(uni_dt, link_dt, by = "permno", allow.cartesian = TRUE)
uni_dt <- uni_dt[form_date >= linkdt & form_date <= linkenddt]
uni_dt[, c("linktype","linkprim","linkdt","linkenddt","permco") := NULL]
uni_dt[, gvkey := as.character(gvkey)]

fa_dt <- as.data.table(
  fa_timed |>
    mutate(gvkey = as.character(gvkey)) |>
    select(gvkey, avail_date, datadate, rdq, book_equity, ib, ni,
           revt, fcf, ebitda, ev_ex_mkt, dvc, total_debt, at,
           roe, roa, roic, gross_margin, op_margin, net_margin,
           leverage, current_ratio, cash_ratio, int_coverage,
           rev_growth, eps_growth, asset_growth, fcf_growth)
)

setkey(fa_dt, gvkey, avail_date)
setkey(uni_dt, gvkey, form_date)

# In a data.table rolling join X[i], the output's join column keeps X's
# name and i's VALUES. So `avail_date` will hold form_date values after
# the join, and the fundamental's own availability date would be lost.
# Copy it to a separate column first.
fa_dt[, fund_avail := avail_date]

# roll = 730 : a fundamental stays current up to 2 years, then goes stale
panel <- fa_dt[uni_dt, roll = 730, rollends = c(FALSE, TRUE)] |>
  as_tibble() |>
  rename(form_date = avail_date)

stopifnot("form_date" %in% names(panel), "rdq" %in% names(panel))

cat("\n--- LOOK-AHEAD AUDIT (post-join) ---\n")
panel |> filter(!is.na(rdq)) |>
  mutate(gap = as.numeric(form_date - rdq)) |>
  summarise(min_gap = min(gap, na.rm = TRUE),
            pct_negative = mean(gap < 0, na.rm = TRUE),
            median_gap = median(gap, na.rm = TRUE)) |> print()
# STILL must be 0.


# =====================================================================
# 6. Price-scaled features — market cap AT FORMATION
# =====================================================================
# Using the fiscal-year-end price would embed a stale valuation and
# reintroduce the circularity that makes naive P/E studies meaningless.

panel <- panel |>
  mutate(
    ep        = ib / na_if(mktcap, 0),
    bp        = book_equity / na_if(mktcap, 0),
    sp        = revt / na_if(mktcap, 0),
    fcfp      = fcf / na_if(mktcap, 0),
    divy      = dvc / na_if(mktcap, 0),
    ebitda_ev = ebitda / na_if(ev_ex_mkt + mktcap, 0)
  )


# =====================================================================
# 7. Sector and index membership
# =====================================================================
panel <- panel |>
  left_join(comp |> mutate(gvkey = as.character(gvkey)) |>
              select(gvkey, gsector) |> distinct(gvkey, .keep_all = TRUE),
            by = "gvkey")

idx_long <- idx |>
  mutate(gvkey = as.character(gvkey),
         thru_dt = coalesce(thru_dt, Sys.Date())) |>
  select(gvkey, from_dt, thru_dt)

panel <- panel |>
  left_join(idx_long, by = "gvkey", relationship = "many-to-many") |>
  mutate(in_sp500 = !is.na(from_dt) &
           form_date >= from_dt & form_date <= thru_dt) |>
  group_by(permno, form_date) |>
  slice_max(in_sp500, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(-from_dt, -thru_dt)


# =====================================================================
# 8. Save
# =====================================================================
panel <- panel |>
  distinct(permno, form_date, .keep_all = TRUE) |>
  arrange(form_date, permno)

write_rds(panel, "data/panel_raw.rds")


# =====================================================================
# 9. CHECKS — record these in research_log.md
# =====================================================================
cat("\n=================== PANEL SUMMARY ===================\n")
cat("Rows:            ", nrow(panel), "\n")
cat("Distinct stocks: ", n_distinct(panel$permno), "\n")
cat("Date range:      ", format(min(panel$form_date)), "to",
    format(max(panel$form_date)), "\n")

panel |> count(form_date) |>
  summarise(min_n = min(n), median_n = median(n), max_n = max(n)) |> print()

cat("\n--- Missingness by feature ---\n")
panel |> summarise(across(c(ep, bp, sp, fcfp, divy, ebitda_ev,
                            roe, roa, roic, leverage, rev_growth,
                            mom_12_1, fwd_12m), ~ mean(is.na(.x)))) |>
  pivot_longer(everything(), names_to = "feature", values_to = "pct_na") |>
  arrange(desc(pct_na)) |> print(n = 30)

cat("\n--- Smell test: Apple 2015 ---\n")
cat("Expect roe ~0.35-0.45, ep ~0.07-0.09, leverage ~0.20-0.30\n")
panel |> filter(ticker == "AAPL", year(form_date) == 2015) |>
  select(form_date, ep, bp, roe, leverage, mom_12_1, fwd_12m) |>
  print(n = 12)


# =====================================================================
# 10. Cross-sectional ranks — every model uses these
# =====================================================================
FEATURES <- c("ep","bp","sp","fcfp","divy","ebitda_ev",
              "roe","roa","roic","gross_margin","op_margin","net_margin",
              "rev_growth","eps_growth","asset_growth","fcf_growth",
              "leverage","current_ratio","cash_ratio","int_coverage",
              "log_mktcap","vol_12m","turnover",
              "mom_12_1","mom_6_1","mom_1m")

panel_ranked <- panel |>
  group_by(form_date) |>
  mutate(across(all_of(FEATURES), ~ percent_rank(.x), .names = "rk_{.col}")) |>
  ungroup()

write_rds(panel_ranked, "data/panel_ranked.rds")
saveRDS(FEATURES, "data/feature_list.rds")

cat("\n--- Rank check: every mean should be ~0.5 ---\n")
panel_ranked |> select(starts_with("rk_")) |>
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) |>
  pivot_longer(everything()) |> print(n = 30)

cat("\nDone. Wrote data/panel_ranked.rds\n")