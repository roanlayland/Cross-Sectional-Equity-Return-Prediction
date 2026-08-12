# =====================================================================
# Cross-Sectional-Equity-Return-Prediction — Live scoring
#
# Trains on all available history, then ranks the most recent
# cross-section. 
#
# WHAT THIS IS
# Out-of-sample rank IC was 0.06-0.08. That means the ranking is
# directionally right roughly 55-60% of the time on any individual
# stock, with enormous variance. The worst backtest year saw a -62%
# decile spread. This is a weak statistical signal designed for
# diversified portfolios, not a list of stock picks.
#
# NOT INVESTMENT ADVICE. Research output only.
# =====================================================================

library(tidyverse)
library(lubridate)
library(xgboost)
library(RPostgres)

USER <- "roanlayland"


# =====================================================================
# 1. Refresh recent data
# =====================================================================
# Only pulls the last few years; your existing .rds files cover history.

wrds <- dbConnect(Postgres(), host = "wrds-pgdata.wharton.upenn.edu",
                  port = 9737, dbname = "wrds",
                  sslmode = "require", user = USER)

cat("Latest available data:\n")
print(dbGetQuery(wrds, "select max(date) as crsp_max from crsp.msf"))
print(dbGetQuery(wrds, "select max(datadate) as comp_max, max(rdq) as rdq_max
                        from comp.fundq"))

REFRESH_FROM <- "2022-01-01"

msf_new <- dbGetQuery(wrds, paste0("
  select a.permno, a.date, a.ret, a.retx, a.prc, a.shrout, a.vol,
         b.shrcd, b.exchcd, b.ticker, b.comnam, b.siccd
  from crsp.msf as a
  left join crsp.msenames as b
    on a.permno = b.permno
   and b.namedt <= a.date and a.date <= b.nameendt
  where a.date >= '", REFRESH_FROM, "'
    and b.shrcd in (10,11) and b.exchcd in (1,2,3)
")) |> as_tibble()

funda_new <- dbGetQuery(wrds, paste0("
  select gvkey, datadate, fyear, fyr,
         at, lt, ceq, seq, pstk, pstkl, pstkrv, txditc,
         ni, ib, oiadp, oibdp, ebit, ebitda, revt, sale, cogs,
         oancf, capx, dvc, dltt, dlc, che, act, lct, xint, csho, epspx
  from comp.funda
  where indfmt='INDL' and datafmt='STD' and popsrc='D' and consol='C'
    and datadate >= '2019-01-01'
")) |> as_tibble()

rdq_new <- dbGetQuery(wrds, "
  select gvkey, datadate, rdq
  from comp.fundq
  where indfmt='INDL' and datafmt='STD' and popsrc='D' and consol='C'
    and rdq is not null and datadate >= '2019-01-01'
") |> as_tibble()

dbDisconnect(wrds)

# Merge with existing history
msf   <- bind_rows(read_rds("data/msf.rds") |> filter(date < REFRESH_FROM),
                   msf_new) |> distinct(permno, date, .keep_all = TRUE)
funda <- bind_rows(read_rds("data/funda.rds") |> filter(datadate < "2019-01-01"),
                   funda_new) |> distinct(gvkey, datadate, .keep_all = TRUE)

write_rds(msf,   "data/msf.rds")
write_rds(funda, "data/funda.rds")
write_rds(bind_rows(read_rds("data/rdq.rds"), rdq_new) |>
            distinct(gvkey, datadate, .keep_all = TRUE), "data/rdq.rds")

cat("\nData refreshed. CRSP now runs to", format(max(msf$date)), "\n")
cat("Now re-run scripts/03_build_panel.R, then continue below.\n")
cat("STOP HERE, run 03, then source this script again from Section 2.\n")


# =====================================================================
# 2. Train on everything, score the newest cross-section
# =====================================================================
panel    <- read_rds("data/panel_ranked.rds")
FEATURES <- read_rds("data/feature_list.rds")
RK       <- paste0("rk_", FEATURES)

miss_rate  <- panel |> summarise(across(all_of(RK), ~ mean(is.na(.x)))) |>
  pivot_longer(everything(), names_to = "term", values_to = "rate")
needs_flag <- miss_rate |> filter(rate > 0.05) |> pull(term)

panel <- panel |>
  mutate(across(all_of(needs_flag), ~ as.integer(is.na(.x)),
                .names = "miss_{.col}")) |>
  group_by(form_date) |>
  mutate(across(all_of(RK), ~ {
    m <- is.na(.x); if (any(m) && !all(m)) .x[m] <- median(.x, na.rm=TRUE); .x
  })) |>
  ungroup()

RK <- c(RK, paste0("miss_", needs_flag))

bad <- panel |> group_by(form_date) |>
  summarise(across(all_of(RK), ~ mean(is.na(.x))), .groups="drop") |>
  pivot_longer(-form_date) |> filter(value == 1) |>
  distinct(form_date) |> pull(form_date)
panel <- panel |> filter(!form_date %in% bad)

# Training set: everything with a realised 12-month outcome
train <- panel |> filter(!is.na(fwd_12m)) |> drop_na(all_of(RK))

# Scoring set: the newest cross-section (no outcome yet, by definition)
CURRENT <- max(panel$form_date)
current <- panel |> filter(form_date == CURRENT) |> drop_na(all_of(RK))

cat("\nTraining through:", format(max(train$form_date)), "|", nrow(train), "rows\n")
cat("Scoring date:    ", format(CURRENT), "|", nrow(current), "stocks\n")

if (as.numeric(Sys.Date() - CURRENT) > 120) {
  warning("Scoring cross-section is more than 4 months stale. ",
          "Momentum and valuation inputs are out of date.")
}

xgb_params <- list(objective="reg:squarederror", eta=0.05, max_depth=6,
                   subsample=0.7, colsample_bytree=0.7,
                   min_child_weight=50, lambda=5, nthread=4)

fit <- train |> filter(form_date <  max(form_date) %m-% years(4))
val <- train |> filter(form_date >= max(form_date) %m-% years(3))

lg <- capture.output(
  xgb.train(xgb_params, xgb.DMatrix(as.matrix(fit[,RK]), label=fit$fwd_12m),
            nrounds=600,
            evals=list(v=xgb.DMatrix(as.matrix(val[,RK]), label=val$fwd_12m)),
            early_stopping_rounds=50, verbose=1))
r <- as.numeric(str_extract(lg, "(?<=val-rmse:)[0-9.]+")); r <- r[!is.na(r)]
BEST_N <- max(if (length(r)) which.min(r) else 150L, 10L)

model <- xgb.train(xgb_params,
                   xgb.DMatrix(as.matrix(train[,RK]), label=train$fwd_12m),
                   nrounds = BEST_N, verbose = 0)

current$score_raw <- predict(model, as.matrix(current[, RK]))


# =====================================================================
# 3. Build the score table
# =====================================================================
sector_names <- c("10"="Energy","15"="Materials","20"="Industrials",
                  "25"="Cons Disc","30"="Cons Staples","35"="Health Care",
                  "40"="Financials","45"="Tech","50"="Telecom",
                  "55"="Utilities","60"="Real Estate")

scores <- current |>
  mutate(
    percentile = percent_rank(score_raw),
    decile     = ntile(score_raw, 10),
    # 0-100 for readability; it is a RANK, not a return forecast
    alphaquant_score = round(100 * percentile),
    sector = recode(as.character(gsector), !!!sector_names)
  ) |>
  select(ticker, permno, sector, mktcap, alphaquant_score, decile,
         score_raw, everything()) |>
  arrange(desc(score_raw))

cat("\n===== TOP 25 =====\n")
scores |> slice_head(n = 25) |>
  select(ticker, sector, mktcap, alphaquant_score,
         rk_fcfp, rk_sp, rk_asset_growth, rk_divy) |>
  print(n = 25)

cat("\n===== BOTTOM 25 =====\n")
scores |> slice_tail(n = 25) |>
  select(ticker, sector, mktcap, alphaquant_score,
         rk_fcfp, rk_sp, rk_asset_growth, rk_divy) |>
  print(n = 25)

cat("\n===== TOP 25 AMONG LARGE CAPS (top size quintile) =====\n")
scores |> filter(mktcap >= quantile(mktcap, 0.80)) |>
  slice_head(n = 25) |>
  select(ticker, sector, mktcap, alphaquant_score) |> print(n = 25)

cat("\n===== SECTOR TILT OF THE TOP DECILE =====\n")
bind_rows(
  scores |> filter(decile == 10) |> count(sector) |>
    mutate(pct = n/sum(n), grp = "Top decile"),
  scores |> count(sector) |> mutate(pct = n/sum(n), grp = "Universe")
) |> select(sector, grp, pct) |>
  pivot_wider(names_from = grp, values_from = pct) |>
  mutate(tilt = `Top decile` - Universe) |>
  arrange(desc(tilt)) |> print(n = 12)


# =====================================================================
# 4. Export
# =====================================================================
write_csv(scores |> select(ticker, sector, mktcap, alphaquant_score,
                           decile, score_raw),
          paste0("output/alphaquant_scores_", format(CURRENT, "%Y%m"), ".csv"))

cat("\nWrote output/alphaquant_scores_", format(CURRENT, "%Y%m"), ".csv\n", sep="")

