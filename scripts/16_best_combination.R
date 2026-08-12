# =====================================================================
# AlphaQuant — Testing the "best combination"
#
# Runs the configuration assembled from the winning grid cells, AND the
# honest version: choose sectors using only the first half of the sample,
# then test on the second half.
#
# ⚠️ WHY THE NAIVE VERSION IS NOT EVIDENCE
# Energy and Industrials were the top 2 of 10 sectors by t-statistic.
# The maximum of 10 noisy draws looks good by construction. The $2B size
# floor was likewise picked by outcome. Stacking selections made after
# seeing results produces a number that will not repeat.
#
# The split-sample test in Section 3 is the one that can actually
# distinguish signal from selection.
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
# 1. Backtest engine
# =====================================================================
backtest <- function(dat, n_side, min_cap, hold_mo, cost_bps,
                     sectors = NULL, label = "", date_from = NULL,
                     date_to = NULL) {

  d <- dat |> filter(mktcap >= min_cap)
  if (!is.null(sectors))   d <- d |> filter(sector %in% sectors)
  if (!is.null(date_from)) d <- d |> filter(form_date >= date_from)
  if (!is.null(date_to))   d <- d |> filter(form_date <= date_to)
  if (nrow(d) == 0) return(NULL)

  picks <- d |> group_by(form_date) |>
    filter(n() >= n_side * 2) |>            # need a real cross-section
    slice_max(score, n = n_side) |> ungroup()
  if (nrow(picks) == 0) return(NULL)

  held <- picks |> select(cohort = form_date, permno) |>
    crossing(k = 1:hold_mo) |>
    mutate(month_date = ceiling_date(cohort %m+% months(k), "month") - 1) |>
    left_join(rets, by = c("permno","month_date")) |>
    mutate(ret_adj = replace_na(ret_adj, 0)) |>
    group_by(cohort, month_date) |>
    summarise(ret = mean(ret_adj), .groups = "drop")

  p <- held |> group_by(month_date) |>
    summarise(ret = mean(ret), n_coh = n_distinct(cohort), .groups="drop") |>
    filter(n_coh == hold_mo) |>
    left_join(ff |> select(month_date, mktrf, smb, hml, rmw, cma, umd, rf),
              by = "month_date") |> drop_na(mktrf)
  if (nrow(p) < 24) return(NULL)

  cost <- (1/hold_mo) * 2 * cost_bps / 1e4
  rn <- p$ret - cost; ex <- rn - p$rf
  capm <- lm(ex ~ p$mktrf)
  ff6  <- lm(ex ~ p$mktrf + p$smb + p$hml + p$rmw + p$cma + p$umd)
  mkt  <- prod(1 + p$mktrf + p$rf)^(12/nrow(p)) - 1

  tibble(
    config = label, n_months = nrow(p),
    ann_ret = prod(1+rn)^(12/length(rn)) - 1,
    mkt_ret = mkt,
    ann_vol = sd(rn)*sqrt(12),
    sharpe  = mean(ex)*12 / (sd(rn)*sqrt(12)),
    mkt_sharpe = mean(p$mktrf)*12 / (sd(p$mktrf)*sqrt(12)),
    beta    = coef(capm)[2],
    capm_a  = coef(capm)[1]*12, capm_t = summary(capm)$coef[1,3],
    ff6_a   = coef(ff6)[1]*12,  ff6_t  = summary(ff6)$coef[1,3]
  )
}

show <- function(x) x |>
  mutate(across(c(ann_ret, mkt_ret, ann_vol, capm_a, ff6_a),
                ~ percent(.x, accuracy = 0.1)),
         across(c(sharpe, mkt_sharpe, beta, capm_t, ff6_t), ~ round(.x, 2))) |>
  print(width = Inf)


# =====================================================================
# 2. The requested configuration
# =====================================================================
cat("\n============ 'BEST' COMBINATION (post-hoc) ============\n")
cat("Energy + Industrials | $2B floor | 6mo hold | 20bp\n")
cat("⚠️ Three of these four choices were made AFTER seeing results.\n\n")

best <- bind_rows(
  backtest(scored, 25,  2000, 6, 20, c("Energy","Industrials"), "Best-cfg, 25 names"),
  backtest(scored, 50,  2000, 6, 20, c("Energy","Industrials"), "Best-cfg, 50 names"),
  backtest(scored, 100, 2000, 6, 20, c("Energy","Industrials"), "Best-cfg, 100 names"),
  backtest(scored, 50,  2000, 6, 50, c("Energy","Industrials"), "Best-cfg, 50 @ 50bp"),
  backtest(scored, 50,  40000, 5, 20, NULL, "All sectors, 50 5 200k"),
  backtest(scored, 50,  40000, 6, 20, NULL, "All sectors, 50 6 200k"),
  backtest(scored, 50,  40000, 7, 20, NULL, "All sectors, 50 7 200k"),
  backtest(scored, 50,  40000, 8, 20, NULL, "All sectors, 50 8 200k"),
  backtest(scored, 50,  40000, 9, 20, NULL, "All sectors, 50 9 200k"),
  backtest(scored, 50,  1000, 5, 20, NULL, "All sectors, 50 5 1000"),
  backtest(scored, 50,  1000, 6, 20, NULL, "All sectors, 50 6 1000"),
  backtest(scored, 50,  1000, 7, 20, NULL, "All sectors, 50 7 1000"),
  backtest(scored, 50,  1000, 8, 20, NULL, "All sectors, 50 8 1000"),
  backtest(scored, 25,  1000, 9, 20, NULL, "All sectors, 50 9 1000"),
  backtest(scored, 25,  2000, 5, 20, NULL, "All sectors, 25 5 2000"),
  backtest(scored, 25,  2000, 6, 20, NULL, "All sectors, 25 6 2000"),
  backtest(scored, 25,  2000, 7, 20, NULL, "All sectors, 25 7 2000"),
  backtest(scored, 25,  2000, 8, 20, NULL, "All sectors, 25 8 2000"),
  backtest(scored, 25,  2000, 9, 20, NULL, "All sectors, 25 9 2000"),
  backtest(scored, 25,  1000, 5, 20, NULL, "All sectors, 25 5 1000"),
  backtest(scored, 25,  1000, 6, 20, NULL, "All sectors, 25 6 1000"),
  backtest(scored, 25,  1000, 7, 20, NULL, "All sectors, 25 7 1000"),
  backtest(scored, 25,  1000, 8, 20, NULL, "All sectors, 25 8 1000"),
  backtest(scored, 25,  1000, 9, 20, NULL, "All sectors, 25 9 1000")
)
show(best)

cat("\nHow many stocks are even available in these two sectors?\n")
scored |> filter(sector %in% c("Energy","Industrials"), mktcap >= 2000) |>
  count(form_date) |>
  summarise(median_available = median(n), min = min(n), max = max(n)) |> print()
cat("If the median is near your portfolio size, you are holding nearly\n")
cat("the whole sector and this is a sector bet, not stock selection.\n")


# =====================================================================
# 3. ⚠️ THE HONEST TEST — split-sample sector selection
# =====================================================================
# Rank sectors using 2010-2017 ONLY, take the top 2, then trade them in
# 2018-2024. This is what a researcher standing in 2018 could have done.
# If the winners repeat, the effect is real. If not, it was noise.

SPLIT <- as.Date("2018-01-01")

fwd1 <- msf |>
  mutate(month_date = ceiling_date(floor_date(date,"month"),"month")-1) |>
  arrange(permno, month_date) |> group_by(permno) |>
  mutate(fwd_1m = lead(ret, 1)) |> ungroup() |>
  select(permno, month_date, fwd_1m)

rank_sectors <- function(from, to) {
  scored |>
    filter(mktcap >= 2000, !is.na(sector),
           form_date >= from, form_date < to) |>
    left_join(fwd1, by = c("permno","form_date"="month_date")) |>
    group_by(form_date, sector) |>
    filter(sum(!is.na(fwd_1m)) >= 30) |>
    mutate(d = ntile(score, 5)) |> filter(d %in% c(1,5)) |>
    group_by(form_date, sector, d) |>
    summarise(r = mean(fwd_1m, na.rm=TRUE), .groups="drop") |>
    pivot_wider(names_from=d, values_from=r, names_prefix="q") |>
    mutate(spread = q5 - q1) |>
    group_by(sector) |>
    summarise(ann = (1+mean(spread,na.rm=TRUE))^12 - 1,
              t = mean(spread,na.rm=TRUE)/(sd(spread,na.rm=TRUE)/sqrt(n()))) |>
    arrange(desc(t))
}

first_half  <- rank_sectors(as.Date("2010-01-01"), SPLIT)
second_half <- rank_sectors(SPLIT, as.Date("2025-12-31"))

cat("\n============ SECTOR RANKS, FIRST HALF (2010-2017) ============\n")
print(first_half |> mutate(ann = percent(ann, accuracy=0.1),
                           t = round(t,2)), n = 12)

cat("\n============ SECTOR RANKS, SECOND HALF (2018-2024) ============\n")
print(second_half |> mutate(ann = percent(ann, accuracy=0.1),
                            t = round(t,2)), n = 12)

cat("\n============ DO THE RANKS PERSIST? ============\n")
persist <- first_half |> select(sector, t1 = t, ann1 = ann) |>
  left_join(second_half |> select(sector, t2 = t, ann2 = ann), by="sector") |>
  drop_na()

cat("Rank correlation between halves:",
    round(cor(rank(-persist$t1), rank(-persist$t2), method="spearman"), 3), "\n")
cat("(Near 0 means sector rankings are noise. Near 1 means they persist.)\n\n")
print(persist |> mutate(across(c(ann1, ann2), ~ percent(.x, accuracy=0.1)),
                        across(c(t1, t2), ~ round(.x,2))), n = 12)

top2_first <- head(first_half$sector, 2)
cat("\nTop 2 sectors chosen using 2010-2017 data only:",
    paste(top2_first, collapse = ", "), "\n")
cat("Their rank in 2018-2024: ",
    paste(match(top2_first, second_half$sector), collapse = ", "),
    "out of", nrow(second_half), "\n")


# =====================================================================
# 4. The out-of-sample verdict
# =====================================================================
cat("\n============ OUT-OF-SAMPLE TEST ============\n")
cat("Sectors chosen on 2010-2017, traded 2018-2024:\n\n")

oos <- bind_rows(
  backtest(scored, 50, 2000, 6, 20, top2_first,
           paste0("OOS: ", paste(top2_first, collapse="+")),
           date_from = SPLIT),
  backtest(scored, 50, 2000, 6, 20, c("Energy","Industrials"),
           "OOS: Energy+Industrials (hindsight)", date_from = SPLIT),
  backtest(scored, 50, 2000, 6, 20, NULL,
           "OOS: all sectors", date_from = SPLIT)
)
show(oos)

cat("\n=================== HOW TO READ THIS ===================\n")
cat("Compare row 1 (sectors picked WITHOUT hindsight) against row 3\n")
cat("(no sector selection at all).\n\n")
cat("If row 1 beats row 3 -> sector selection adds value, and it is\n")
cat("   defensible because the choice used only prior data.\n")
cat("If row 1 is similar or worse -> the sector effect was noise, and\n")
cat("   the in-sample 'best combination' in Section 2 is an artifact.\n\n")
cat("Row 2 is the hindsight version. It will likely look better than\n")
cat("row 1. That gap IS the selection bias, measured directly -- which\n")
cat("makes it a genuinely useful number to report in the paper.\n")

write_csv(best,   "output/best_config.csv")
write_csv(persist,"output/sector_persistence.csv")
write_csv(oos,    "output/oos_sector_test.csv")
