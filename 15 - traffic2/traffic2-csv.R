library(tidyverse)
library(readxl)
library(zoo)

traffic2 <- read_excel("15 - traffic2/traffic2.xlsx") |>
  select(-fatacc) |>
  rename(total = totacc, weekends = wkends, speedlaw = spdlaw)

st <- zooreg(traffic2, start = c(1981, 1), frequency = 12)
months <- outer(cycle(st), 1:12, \(x, y) as.integer(x == y))
colnames(months) <- month.abb

st_months <- zooreg(months, start = start(st), frequency = frequency(st))
t <- zooreg(1:NROW(st), start = start(st), frequency = frequency(st))
st <- merge(st, t, st_months)


write.zoo(st, "15 - traffic2/traffic2.csv", quote = FALSE, row.names = FALSE,
          sep = ";", dec = ",")


