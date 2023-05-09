library(readxl)
library(zoo)
library(mFilter)

bdrems_y <- read_excel("BDREMS.xlsx",
                       sheet = "BDREMS",
                       range = "C4:C170",
                       col_names = "Y")

bdrems_u <- read_excel("BDREMS.xlsx",
                       sheet = "BDREMS",
                       range = "AL4:AL170",
                       col_names = "U")
y_ts <- zooreg(bdrems_y$Y, frequency = 4, start = c(1980, 1))
u_ts <- zooreg(bdrems_u$U, frequency = 4, start = c(1980, 1))
ts <- merge(y = y_ts, ly = log(y_ts), u = u_ts) |>
  window(end = "2019-4")

hp_u <- hpfilter(ts$u, freq = 1600)
hp_ly <- hpfilter(ts$ly, freq = 1600)
ts$ly_hp <- zooreg(hp_ly$trend, start = start(ts), frequency = 4)
ts$u_hp <- zooreg(hp_u$trend, start = start(ts), frequency = 4)
ts$gy <- 100 * diff(ts$ly)
ts_year <- time(ts) |> format("%Y") |> as.integer() |>
  zooreg(start = start(ts), frequency = 4)
ts_qtr <- cycle(ts)

ts_all <- merge(year = ts_year, qtr = ts_qtr, ts) |>
  window(start = "1980-2")
write.csv(ts_all,
          file = "okun.csv", quote = FALSE,
          row.names = FALSE)


library(urca)

st <- window(ts, end = "2019-4")
hp_ly <- hpfilter(st$ly, freq = 1600)
ur.df(hp_ly$cycle, type = "drift", lags = 6, selectlags = "AIC") |>
  summary()
plot(1:length(hp_ly$cycle), hp_ly$cycle, typ = "l")
