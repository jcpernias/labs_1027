library(ggplot2)
library(readxl)
library(forecast)
library(zoo)

## PIB
## =================================================================

pib_db <- read_xlsx("BDMACRO.xlsx",
                      sheet = "SERIES INDIVIDUALES",
                      range = "b3:b70")

pib <- zooreg(pib_db$PIBpm, start = 1954, frequency = 1)

major <- seq(1960, 2020, 10)
minor <- seq(1960, 2020, 5)

autoplot(pib) +
  scale_x_continuous(breaks = major, minor_breaks = minor)

autoplot(pib) +
  scale_x_continuous(breaks = major, minor_breaks = minor) +
  scale_y_log10()

## Deflactor del PIB
## =================================================================

dpib_db <- read_xlsx("BDMACRO.xlsx",
                     sheet = "SERIES INDIVIDUALES",
                     range = "n3:n70")

defl_pib <- zooreg(dpib_db$dPIB, start = 1954, frequency = 1)

autoplot(defl_pib) +
  scale_x_continuous(breaks = major, minor_breaks = minor)


## PIB real
## =================================================================

db <- merge(pib, defl_pib, y = 100 * pib / defl_pib)

autoplot(subset(db, select = c(pib, y)), facets = NULL) +
  scale_x_continuous(breaks = major, minor_breaks = minor)


# Tasa de crecimiento PIB real

db$ly <- log(db$y)
db$gy <- 100 * diff(db$ly)

autoplot(db$gy)  +
  scale_x_continuous(breaks = major, minor_breaks = minor) +
  geom_hline(yintercept = mean(db$gy, na.rm = TRUE), color = I("DarkGray"))

# Inflación

db$infl <- 100 * diff(log(db$defl_pib))

autoplot(db$infl)  +
  scale_x_continuous(breaks = major, minor_breaks = minor) +
  geom_hline(yintercept = mean(db$infl, na.rm = TRUE), color = I("DarkGray"))

# Desempleo

unem_db <- read_xlsx("BDMACRO.xlsx",
                  sheet = "SERIES INDIVIDUALES",
                  range = "hk3:hk70")
db$unem <- zooreg(unem_db$U, start = 1954, frequency = 1)

autoplot(db$unem)  +
  scale_x_continuous(breaks = major, minor_breaks = minor) +
  geom_hline(yintercept = mean(db$unem, na.rm = TRUE), color = I("DarkGray"))



# Autocorrelación

autoplot(db$gy) +
  scale_x_continuous(breaks = major, minor_breaks = minor)

lags <- cbind(gy = db$gy, gy_1 = lag(db$gy, -1))

head(lags)

head(cbind(ly = db$ly, lags))

lags <- na.trim(lags)
qplot(gy_1, gy, data = lags)
cor(lags)


Acf(db$gy, lag.max = 10, ylim = c(-1, 1))

ggtsdisplay(db$gy, plot.type = "scatter")



