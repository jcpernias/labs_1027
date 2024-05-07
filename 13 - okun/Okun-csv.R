library(readxl)
library(zoo)
# library(mFilter)

bdrems_y <- read_excel("13 - okun/BDREMS.xlsx",
                       sheet = "BDREMS",
                       range = "C4:C170",
                       col_names = "Y")

bdrems_u <- read_excel("13 - okun/BDREMS.xlsx",
                       sheet = "BDREMS",
                       range = "AL4:AL170",
                       col_names = "U")
y_ts <- zooreg(bdrems_y$Y, frequency = 4, start = c(1980, 1))
u_ts <- zooreg(bdrems_u$U, frequency = 4, start = c(1980, 1))
ts <- merge(y = y_ts, u = u_ts) |>
  window(end = "2019-4")

# hp_u <- hpfilter(ts$u, freq = 1600)
# ts$un <- zooreg(hp_u$trend, start = start(ts), frequency = 4)

write.zoo(ts, file = "13 - okun/okun.csv", index.name = "trimestre",
          row.names = FALSE, sep = ";", dec = ",", quote = FALSE)
