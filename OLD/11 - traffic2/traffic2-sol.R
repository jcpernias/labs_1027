library(readxl)
library(zoo)
library(dynlm)
library(ggplot2)
library(car)
library(sandwich)
library(lmtest)

#' # Apartado 1
#'

db <- read_xlsx("traffic2.xlsx")

ts_db <- zooreg(db, start = c(1981, 1), frequency = 12)

#' Entrada en vigor de la ley que eleva el límite de velocidad
ts_db$spdlaw[which(ts_db$spdlaw == 1)[1]]

#' Entrada en vigor de la ley que obliga a usar el cinturón de seguridad
ts_db$beltlaw[which(ts_db$beltlaw == 1)[1]]

#' # Apartado 2
#'
ts_db$ltotacc <- log(ts_db$totacc)
autoplot(ts_db$ltotacc)

mod0 <- dynlm(ltotacc ~ trend(ts_db) + season(ts_db), data = ts_db)
summary(mod0)
lht(mod0, matchCoefs(mod0, "season"))

#' # Apartado 3
#'
mod1 <- update(mod0, . ~ . + wkends + unem + spdlaw + beltlaw)
summary(mod1)



#' # Apartado 5
#'
ts_db$prcfat <- ts_db$fatacc / ts_db$totacc * 100

mod2 <- update(mod1, prcfat ~ .)
round(coeftest(mod2), 8)

