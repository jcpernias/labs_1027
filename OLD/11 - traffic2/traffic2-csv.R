library(tidyverse)
library(readxl)
library(zoo)

traffic2 <- read_excel("traffic2.xlsx") |>
  select(-c(spdlaw, beltlaw)) |>
  rename(total = totacc, fatal = fatacc, weekends = wkends)

st <- zooreg(traffic2, start = c(1981, 1), frequency = 12)
st_month <- cycle(st)
st_year <- format(time(st), "%Y") |> as.integer() |>
  zooreg(start = start(st), frequency = frequency(st))

st <- merge(year = st_year, month = st_month, st)

write.csv(st, "traffic2.csv", quote = FALSE, row.names = FALSE)


