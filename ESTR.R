setwd("~/R")
rm(list = ls(all = TRUE))
gc()

install_and_load <- function(...) {
  pkgs <- c(...)
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
  }
}

install_and_load("dplyr", "ecb", "lubridate", "openxlsx", "RQuantLib", "tidyr")

df <- full_join(
  get_data("EON.D.EONIA_TO.RATE") |>
    select(obstime, obsvalue) |>
    rename(eonia = obsvalue),
  get_data("EST.B.EU000A2X2A25.WT") |>
    select(obstime, obsvalue) |>
    rename(estr = obsvalue),
  by = "obstime"
) |> rename(date = obstime) |>
  mutate(date = as.Date(date))

last_date <- max(df$date, na.rm = TRUE)
next_target_bd <- businessDayList("TARGET", as.Date(last_date), as.Date(last_date + 10))[2]
df <- bind_rows(
  tibble(
    date  = as.Date(next_target_bd),
    eonia = NA_real_,
    estr  = NA_real_
  ),
  df
) |>
  arrange(date)

first_eonia <- df$eonia[which.min(df$date)]

df <- bind_rows(
  tibble(
    date  = as.Date("1998-12-31"),
    eonia = first_eonia,
    estr  = NA_real_
  ),
  df
) |>
  arrange(date)

df <- df |>
  mutate(spread = eonia - estr)
n_dev <- sum(abs(df$spread - 0.085) > 1e-9, na.rm = TRUE) #1e-12
if (n_dev > 0) {
  stop(paste("Deviation(s) detected:", n_dev))
}

df <- df |>
  mutate(
    bfestr = if_else(
      !is.na(estr),
      estr,
      eonia - 0.085
    )
  )

df <- df |>
  arrange(date) |>
  mutate(
    days = as.numeric(date - lag(date)),
    indexur = if_else(date == as.Date("2019-10-01"), 100, NA_real_)
  )

anchor_row <- which(df$date == as.Date("2019-10-01"))
if (length(anchor_row) == 0) {
  stop("Anchor date 2019-10-01 not found in dataset.")
}
if (anchor_row < nrow(df)) {
  for (i in (anchor_row + 1):nrow(df)) {
    df$indexur[i] <- df$indexur[i - 1] * (
      1 + (df$bfestr[i - 1] / 100) * (df$days[i] / 360)
    )
  }
}
if (anchor_row > 1) {
  for (i in (anchor_row - 1):1) {
    df$indexur[i] <- df$indexur[i + 1] / (
      1 + (df$bfestr[i] / 100) * (df$days[i + 1] / 360)
    )
  }
}

df <- df |>
  complete(date = seq(min(date), max(date), by = "day")) |>
  arrange(date)

df <- df |>
  mutate(
    last_bd_index = indexur,
    last_bd_rate  = bfestr,
    last_bd_date  = if_else(!is.na(indexur), date, NA_Date_)
  ) |>
  fill(last_bd_index, last_bd_rate, last_bd_date, .direction = "down") |>
  mutate(
    indexwa = if_else(
      !is.na(indexur),
      indexur,
      last_bd_index * (1 + (last_bd_rate / 100) *
        (as.numeric(date - last_bd_date) / 360))
    )
  ) |>
  select(-last_bd_index, -last_bd_rate, -last_bd_date)

df <- df |> select(date, eonia, estr, bfestr, indexur, indexwa)

wb <- createWorkbook()
addWorksheet(wb, "Sheet1")
writeData(wb, "Sheet1", df, keepNA = TRUE)
saveWorkbook(wb, "estr_index.xlsx", overwrite = TRUE)
df_export <- df
df_export[is.na(df_export)] <- NA
writeData(wb, "Sheet1", df_export, keepNA = TRUE)
saveWorkbook(wb, "estr_index.xlsx", overwrite = TRUE)
