library(tidyverse)
library(readxl)
library(zoo)
library(stringr)

traffic2 <- read_excel("15 - traffic2/traffic2.xlsx") |>
  select(-fatacc) |>
  rename(total = totacc, weekends = wkends, speedlaw = spdlaw)

month_ids <- str_c("M", str_pad(1:12, width = 2, pad = 0))
st <- zooreg(traffic2, start = c(1981, 1), frequency = 12)
months <- outer(cycle(st), 1:12, \(x, y) as.integer(x == y))
colnames(months) <-  month_ids

st_months <- zooreg(months, start = start(st), frequency = frequency(st))
t <- zooreg(1:NROW(st), start = start(st), frequency = frequency(st))
st <- merge(st, t, st_months)

st_df <- as_tibble(st) |>
    mutate(Index = str_c(year(st), "M", str_pad(cycle(st), width = 2, pad = 0))) |>
    relocate(Index)



write.table(st_df, "15 - traffic2/traffic2.csv", quote = FALSE, row.names = FALSE,
          sep = ";", dec = ",")
