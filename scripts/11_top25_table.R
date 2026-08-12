# =====================================================================
# Cross-Sectional-Equity-Return-Prediction — Top 25 scorecard
#
# Scores one cross-section, attaches share price, and (if the forward
# year has already resolved) shows what each stock actually did.
#
# Set SCORE_DATE to 2023-12-31 to get a scorecard you can grade.
# Set it to the last available month for a "current" ranking with no
# outcome yet.
# =====================================================================

library(tidyverse)
library(lubridate)
library(xgboost)
library(scales)
library(knitr)

# ---- CHOOSE THE DATE ------------------------------------------------
SCORE_DATE <- as.Date("2023-12-31")   # has realised 2024 returns
# SCORE_DATE <- NULL                  # NULL = most recent available
# ---------------------------------------------------------------------

panel    <- read_rds("data/panel_ranked.rds")
FEATURES <- read_rds("data/feature_list.rds")
RK       <- paste0("rk_", FEATURES)
msf      <- read_rds("data/msf.rds")


# =====================================================================
# 1. Standard prep
# =====================================================================
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

if (is.null(SCORE_DATE)) SCORE_DATE <- max(panel$form_date)


# =====================================================================
# 2. Train with a 12-month embargo before the scoring date
# =====================================================================

train_end <- SCORE_DATE %m-% months(12)

train <- panel |>
  filter(!is.na(fwd_12m), form_date < train_end) |>
  drop_na(all_of(RK))

score_set <- panel |> filter(form_date == SCORE_DATE) |> drop_na(all_of(RK))

cat("Scoring date:  ", format(SCORE_DATE), "\n")
cat("Training up to:", format(max(train$form_date)),
    "|", nrow(train), "rows\n")
cat("Stocks scored: ", nrow(score_set), "\n")
stopifnot(nrow(score_set) > 0)

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

score_set$score_raw <- predict(model, as.matrix(score_set[, RK]))


# =====================================================================
# 3. Attach share price and company name
# =====================================================================
prices <- msf |>
  mutate(form_date = ceiling_date(floor_date(date, "month"), "month") - 1) |>
  filter(form_date == SCORE_DATE) |>
  transmute(permno, price = abs(prc), company = comnam) |>
  distinct(permno, .keep_all = TRUE)

sector_names <- c("10"="Energy","15"="Materials","20"="Industrials",
                  "25"="Cons Disc","30"="Cons Staples","35"="Health Care",
                  "40"="Financials","45"="Tech","50"="Telecom",
                  "55"="Utilities","60"="Real Estate")

scores <- score_set |>
  mutate(score = round(100 * percent_rank(score_raw)),
         decile = ntile(score_raw, 10),
         sector = recode(as.character(gsector), !!!sector_names)) |>
  left_join(prices, by = "permno") |>
  arrange(desc(score_raw))


# =====================================================================
# 4. The table
# =====================================================================
has_outcome <- !all(is.na(scores$fwd_12m))

make_tbl <- function(d) {
  out <- d |>
    transmute(
      Rank    = row_number(),
      Ticker  = ticker,
      Company = str_trunc(coalesce(company, ""), 24),
      Sector  = sector,
      Date    = format(SCORE_DATE, "%Y-%m-%d"),
      Price   = dollar(price, accuracy = 0.01),
      `Mkt Cap ($M)` = comma(round(mktcap)),
      Score   = score
    )
  if (has_outcome) out$`Actual Ret` <- percent(d$fwd_12m, accuracy = 0.1)
  out
}

top25    <- scores |> slice_head(n = 25) |> make_tbl()
bottom25 <- scores |> slice_tail(n = 25) |>
  arrange(score_raw) |> make_tbl()

cat("\n\n================ TOP 25 ================\n")
print(kable(top25, align = "r"))

cat("\n\n============== BOTTOM 25 ==============\n")
print(kable(bottom25, align = "r"))


# =====================================================================
# 5. Did it work?
# =====================================================================
if (has_outcome) {
  cat("\n\n============== SCORECARD ==============\n")
  summ <- scores |>
    summarise(
      top25_avg     = mean(head(fwd_12m, 25), na.rm = TRUE),
      bottom25_avg  = mean(tail(fwd_12m, 25), na.rm = TRUE),
      top_decile    = mean(fwd_12m[decile == 10], na.rm = TRUE),
      bottom_decile = mean(fwd_12m[decile == 1],  na.rm = TRUE),
      universe_avg  = mean(fwd_12m, na.rm = TRUE),
      rank_ic       = cor(score_raw, fwd_12m, method = "spearman",
                          use = "complete.obs")
    )
  print(summ |> mutate(across(-rank_ic, ~ percent(.x, accuracy = 0.1)),
                       rank_ic = round(rank_ic, 3)) |> t())

  cat("\nOne cross-section is ONE observation. A single good or bad\n")
  cat("year says almost nothing. The 14-year backtest is the evidence.\n")
}


# =====================================================================
# 6. Chart
# =====================================================================
plot_dat <- scores |> slice_head(n = 25) |>
  mutate(lab = paste0(ticker, "  ($", round(price), ")"))

if (has_outcome) {
  fig <- plot_dat |>
    ggplot(aes(fwd_12m, fct_reorder(lab, score_raw), fill = fwd_12m > 0)) +
    geom_col() +
    geom_vline(xintercept = 0, colour = "grey40") +
    scale_x_continuous(labels = percent) +
    scale_fill_manual(values = c("TRUE"="steelblue4","FALSE"="firebrick"),
                      guide = "none") +
    labs(title = paste("Top 25 ranked stocks —", format(SCORE_DATE, "%B %Y")),
         subtitle = "Bars show the return actually realised over the following 12 months",
         x = "Realised 12-month return", y = NULL)
} else {
  fig <- plot_dat |>
    ggplot(aes(score, fct_reorder(lab, score_raw))) +
    geom_col(fill = "steelblue4") +
    labs(title = paste("Top 25 ranked stocks —", format(SCORE_DATE, "%B %Y")),
         subtitle = "Score is a percentile rank within the month, not a return forecast",
         x = "AlphaQuant score (0-100)", y = NULL)
}

ggsave(paste0("figures/top25_", format(SCORE_DATE, "%Y%m"), ".png"),
       fig, width = 9, height = 7, dpi = 300)

write_csv(scores |> select(ticker, company, sector, price, mktcap,
                           score, decile, fwd_12m),
          paste0("output/scores_", format(SCORE_DATE, "%Y%m"), ".csv"))

cat("\nSaved figure and CSV.\n")
