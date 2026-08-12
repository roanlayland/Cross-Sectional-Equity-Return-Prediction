# =====================================================================
# AlphaQuant — Stage 2: WRDS data extraction

library(RPostgres)
library(tidyverse)
library(lubridate)

wrds <- dbConnect(Postgres(),
                  host = "wrds-pgdata.wharton.upenn.edu",
                  port = 9737, dbname = "wrds",
                  sslmode = "require", user = "roanlayland")

dir.create("data", showWarnings = FALSE)
START <- "1993-01-01"   # 2 yrs before 1995 so lagged features exist


# =====================================================================
# 1. CRSP MONTHLY  (~3 min)
# =====================================================================
msf <- dbGetQuery(wrds, paste0("
  select a.permno, a.date, a.ret, a.retx, a.prc, a.shrout, a.vol,
         b.shrcd, b.exchcd, b.ticker, b.comnam, b.siccd
  from crsp.msf as a
  left join crsp.msenames as b
    on a.permno = b.permno
   and b.namedt <= a.date and a.date <= b.nameendt
  where a.date >= '", START, "'
    and b.shrcd in (10, 11)
    and b.exchcd in (1, 2, 3)
")) |> as_tibble()

write_rds(msf, "data/msf.rds")
# CHECK:
nrow(msf); range(msf$date); n_distinct(msf$permno)


# =====================================================================
# 2. DELISTING RETURNS  (~10 sec) — do not skip this
# =====================================================================
# Without these, bankrupt firms exit your sample at their last traded
# price instead of at -100%. This inflates every return you compute.

dl <- dbGetQuery(wrds, "
  select permno, dlstdt, dlret, dlstcd
  from crsp.msedelist
") |> as_tibble()

write_rds(dl, "data/delist.rds")


# =====================================================================
# 3. CRSP DAILY  (~30-45 min, several GB) — needed for beta & volatility
# =====================================================================

dsf <- dbGetQuery(wrds, paste0("
  select permno, date, ret
  from crsp.dsf
  where date >= '", START, "'
")) |> as_tibble()

write_rds(dsf, "data/dsf.rds")

# Market return for beta estimation
mkt_d <- dbGetQuery(wrds, paste0("
  select date, vwretd
  from crsp.dsi
  where date >= '", START, "'
")) |> as_tibble()

write_rds(mkt_d, "data/mkt_daily.rds")


# =====================================================================
# 4. COMPUSTAT ANNUAL  (~2 min)
# =====================================================================
# Without the four filters you get duplicate rows
# per firm-year from restatements and alternate reporting formats.

funda <- dbGetQuery(wrds, paste0("
  select gvkey, datadate, fyear, fyr,
         at, lt, ceq, seq, pstk, pstkl, pstkrv, txditc, txdb, itcb,
         ni, ib, oiadp, oibdp, ebit, ebitda,
         revt, sale, cogs, xsga, xrd,
         oancf, capx, dv, dvc, dvp,
         dltt, dlc, dd1, che, ivst,
         act, lct, invt, rect, ap,
         xint, csho, ajex, prcc_f, epspx, epsfx,
         mib, ppent, gdwl, intan
  from comp.funda
  where indfmt = 'INDL' and datafmt = 'STD'
    and popsrc = 'D'   and consol  = 'C'
    and datadate >= '", START, "'
")) |> as_tibble()

write_rds(funda, "data/funda.rds")
# CHECK: should be ~250k-350k rows
nrow(funda); n_distinct(funda$gvkey)


# =====================================================================
# 5. COMPUSTAT QUARTERLY  (~3 min) — optional, gives fresher data
# =====================================================================
# Annual data is up to 18 months stale at formation. Quarterly cuts that
# to ~4 months.

fundq <- dbGetQuery(wrds, paste0("
  select gvkey, datadate, fyearq, fqtr, rdq,
         atq, ltq, ceqq, seqq, pstkq, txditcq,
         niq, ibq, oiadpq, revtq, saleq, cogsq, xsgaq,
         oancfy, capxy, dlttq, dlcq, cheq, actq, lctq,
         cshoq, prccq, epspxq
  from comp.fundq
  where indfmt = 'INDL' and datafmt = 'STD'
    and popsrc = 'D'   and consol  = 'C'
    and datadate >= '", START, "'
")) |> as_tibble()

write_rds(fundq, "data/fundq.rds")


# =====================================================================
# 6. CCM LINK TABLE  (~5 sec) — joins Compustat gvkey to CRSP permno
# =====================================================================
link <- dbGetQuery(wrds, "
  select gvkey, lpermno as permno, lpermco as permco,
         linktype, linkprim, linkdt, linkenddt
  from crsp.ccmxpf_lnkhist
  where linktype in ('LU','LC') and linkprim in ('P','C')
") |> as_tibble() |>
  mutate(linkenddt = if_else(is.na(linkenddt), Sys.Date(), linkenddt))

write_rds(link, "data/link.rds")


# =====================================================================
# 7. SECTOR / COMPANY INFO
# =====================================================================
company <- dbGetQuery(wrds, "
  select gvkey, conm, gsector, ggroup, gind, gsubind, sic, naics
  from comp.company
") |> as_tibble()

write_rds(company, "data/company.rds")


# =====================================================================
# 8. S&P 500 MEMBERSHIP HISTORY
# =====================================================================

idx <- dbGetQuery(wrds, "
  select gvkey, \"from\" as from_dt, thru as thru_dt
  from comp.idxcst_his
  where gvkeyx = '000003'
") |> as_tibble()

write_rds(idx, "data/sp500_members.rds")


# =====================================================================
# 9. FAMA-FRENCH FACTORS - for alpha calculations
# =====================================================================
ff <- dbGetQuery(wrds, paste0("
  select date, mktrf, smb, hml, rmw, cma, umd, rf
  from ff.fivefactors_monthly
  where date >= '", START, "'
")) |> as_tibble()

write_rds(ff, "data/ff_factors.rds")

dbDisconnect(wrds)


# =====================================================================
# 10. NYSE SIZE BREAKPOINTS — defines the universe floor
# =====================================================================
# excludes stocks below the 20th percentile of NYSE
# market cap. Removes microcap noise without gutting the sample.

msf <- read_rds("data/msf.rds")

breakpoints <- msf |>
  mutate(ym = floor_date(date, "month"),
         mktcap = abs(prc) * shrout / 1000) |>
  filter(exchcd == 1, !is.na(mktcap), mktcap > 0) |>
  group_by(ym) |>
  summarise(nyse_p20 = quantile(mktcap, 0.20, na.rm = TRUE),
            nyse_p50 = quantile(mktcap, 0.50, na.rm = TRUE),
            .groups = "drop")

write_rds(breakpoints, "data/nyse_breakpoints.rds")

# CHECK: breakpoint should rise over time (inflation + market growth)
breakpoints |> filter(month(ym) == 12) |> print(n = 40)

