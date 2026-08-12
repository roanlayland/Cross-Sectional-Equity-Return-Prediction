# =====================================================================
# AlphaQuant — Do the spending factors work in any size segment?
#
# Self-contained. Rebuilds everything it needs from data/, so it does
# not depend on objects left over from earlier scripts.
#
# ⚠️ 6 factors x 3 size bands = 18 tests. Roughly one will look
# significant by chance. A single isolated hit means little. What would
# be meaningful is a MONOTONIC gradient — a factor strengthening
# steadily from large to small caps — because that has the
# limits-to-arbitrage mechanism behind it.
# =====================================================================

library(tidyverse)
library(lubridate)
library(sandwich)
library(lmtest)
library(broom)
library(scales)

PANEL <- "data/panel_ranked_plus.rds"
stopifnot(file.exists(PANEL))

panel2 <- read_rds(PANEL)
msf    <- read_rds("data/msf.rds")
dl     <- read_rds("data/delist.rds")

cat("Panel:", nrow(panel2), "rows,", n_distinct(panel2$form_date), "periods\n")


# =====================================================================
# 1. Forward returns
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

fwd1 <- rets |> arrange(permno, month_date) |> group_by(permno) |>
  mutate(fwd_1m = lead(ret_adj, 1)) |> ungroup() |>
  select(permno, month_date, fwd_1m)


# =====================================================================
# 2. Variables
# =====================================================================
BASE <- c("rk_asset_growth","rk_fcfp","rk_sp","rk_gross_margin",
          "rk_roe","rk_bp","rk_rev_growth","rk_log_mktcap","rk_mom_12_1")
ADD  <- c("rk_accruals","rk_capx_ocf","rk_capx_at",
          "rk_capx_sales","rk_opex_sales","rk_cash_conv")
ALL  <- c(BASE, ADD)

miss_cols <- setdiff(ALL, names(panel2))
if (length(miss_cols)) stop("Missing columns: ", paste(miss_cols, collapse=", "))


# =====================================================================
# 3. Build the estimation sample
# =====================================================================
base_dat <- panel2 |>
  left_join(rets |> select(permno, month_date, price),
            by = c("permno", "form_date" = "month_date")) |>
  filter(price >= 5, mktcap >= 100) |>
  left_join(fwd1, by = c("permno", "form_date" = "month_date")) |>
  filter(!is.na(fwd_1m)) |>
  group_by(form_date) |>
  mutate(sz = percent_rank(mktcap),
         across(all_of(ALL), ~ {
           m <- is.na(.x)
           if (any(m) && !all(m)) .x[m] <- median(.x, na.rm = TRUE)
           .x
         })) |>
  ungroup()

cat("After filters:", nrow(base_dat), "rows,",
    n_distinct(base_dat$form_date), "periods\n")


# =====================================================================
# 4. Drop periods that cannot support a regression
# =====================================================================
# Two separate problems, both fatal to lm():
#   (a) a feature 100% missing in that month -- imputation cannot fill it
#   (b) a feature with zero variance -- perfectly collinear with intercept

diag <- base_dat |>
  group_by(form_date) |>
  summarise(n_rows = n(),
            n_complete = sum(complete.cases(pick(all_of(ALL)))),
            n_allna = sum(map_lgl(pick(all_of(ALL)), ~ all(is.na(.x)))),
            n_novar = sum(map_lgl(pick(all_of(ALL)),
                                  ~ length(unique(.x[!is.na(.x)])) < 2)),
            .groups = "drop")

cat("\n--- Period diagnostics ---\n")
diag |> summarise(periods = n(),
                  median_rows = median(n_rows),
                  median_complete = median(n_complete),
                  periods_with_allna = sum(n_allna > 0),
                  periods_with_novar = sum(n_novar > 0),
                  periods_under_100 = sum(n_complete < 100)) |>
  print(width = Inf)

good <- diag |> filter(n_complete >= 150, n_allna == 0, n_novar == 0) |>
  pull(form_date)

cat("\nUsable periods:", length(good), "of", nrow(diag), "\n")
if (length(good) < 60) stop("Too few usable periods to proceed.")
cat("Range:", format(range(good)), "\n")

base_dat <- base_dat |> filter(form_date %in% good)


# =====================================================================
# 5. Size-band Fama-MacBeth
# =====================================================================
fml <- as.formula(paste("fwd_1m ~", paste(ALL, collapse = " + ")))

by_size <- function(lo, hi, label) {
  d <- base_dat |> filter(sz >= lo, sz <= hi)
  
  # Re-check within the band: a slice can lose variance the full
  # cross-section had.
  ok <- d |> group_by(form_date) |>
    summarise(n_c = sum(complete.cases(pick(all_of(ALL)))),
              n_nv = sum(map_lgl(pick(all_of(ALL)),
                                 ~ length(unique(.x[!is.na(.x)])) < 2)),
              .groups = "drop") |>
    filter(n_c >= 100, n_nv == 0) |> pull(form_date)
  
  d <- d |> filter(form_date %in% ok)
  cat(label, ":", length(ok), "periods,", nrow(d), "rows\n")
  if (length(ok) < 36) return(NULL)
  
  s1 <- d |> group_by(form_date) |>
    group_modify(~ {
      fit <- try(lm(fml, data = .x), silent = TRUE)
      if (inherits(fit, "try-error")) return(tibble())
      tidy(fit)
    }) |> ungroup()
  
  if (nrow(s1) == 0) return(NULL)
  
  s1 |> filter(term %in% ADD, !is.na(estimate)) |>
    group_by(term) |> filter(n() >= 24) |>
    group_modify(function(dd, k) {
      m  <- lm(estimate ~ 1, data = dd)
      L  <- min(12, max(1, floor(nrow(dd)/4)))
      nw <- coeftest(m, vcov = NeweyWest(m, lag = L, prewhite = FALSE))
      tibble(coef = nw[1,1], t = nw[1,3], n_mo = nrow(dd))
    }) |> ungroup() |> mutate(band = label)
}

cat("\n--- Running bands ---\n")
res_size <- bind_rows(
  by_size(0.00, 0.33, "Small"),
  by_size(0.33, 0.67, "Mid"),
  by_size(0.67, 1.00, "Large"),
  by_size(0.00, 1.00, "All")
)


# =====================================================================
# 6. Results
# =====================================================================
if (nrow(res_size) == 0) {
  cat("\nNo band produced results. Check the diagnostics above.\n")
} else {
  cat("\n========== T-STATISTICS BY SIZE BAND ==========\n")
  cat("Multivariate, controlling for the 9 baseline factors.\n\n")
  
  tbl <- res_size |> select(term, band, t) |>
    mutate(t = round(t, 2)) |>
    pivot_wider(names_from = band, values_from = t)
  print(tbl, width = Inf)
  
  cat("\n========== SUMMARY ==========\n")
  n_sig <- res_size |> filter(band != "All", abs(t) > 2) |> nrow()
  n_tests <- res_size |> filter(band != "All") |> nrow()
  cat("Significant at |t| > 2:", n_sig, "of", n_tests, "band tests\n")
  cat("Expected by chance at 5%:", round(n_tests * 0.05, 1), "\n\n")
  
  if (n_sig > 0) {
    cat("Hits:\n")
    res_size |> filter(band != "All", abs(t) > 2) |>
      select(term, band, coef, t) |>
      mutate(coef = round(coef, 4), t = round(t, 2)) |> print()
  } else {
    cat("No factor reaches significance in any size band.\n")
  }
  
  cat("\nCheck for a MONOTONIC gradient (Large -> Mid -> Small).\n")
  cat("An isolated hit in one band with the others flat is noise.\n")
  
  write_csv(res_size, "output/spending_by_size.csv")
}