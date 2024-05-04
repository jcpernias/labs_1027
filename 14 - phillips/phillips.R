library(zoo)
library(sandwich)
library(lmtest)
library(urca)
library(dynlm)



db_all <- read.csv2.zoo("phillips.csv", regular = TRUE, colClasses = "numeric")

infl <- 100 * diff(log(db_all$defl))
dinfl <- diff(infl)
paro_c <- db_all$paro - db_all$paro_n

db <- window(merge(infl, dinfl, paro = db_all$paro, paro_c), start = 1964)
rm(infl, dinfl, paro_c, db_all)

autoplot.zoo(db$infl)

db$infl |>
  ur.df(type = "drift", lags = 3, selectlags = "AIC") |>
  summary()

autoplot.zoo(db$dinfl)

db$dinfl |>
  ur.df(type = "drift", lags = 3, selectlags = "AIC") |>
  summary()

autoplot.zoo(db$paro)

db$paro |>
  ur.df(type = "drift", lags = 3, selectlags = "AIC") |>
  summary()

autoplot.zoo(db$paro_c)

db$paro_c |>
  ur.df(type = "drift", lags = 3, selectlags = "AIC") |>
  summary()



mod1 <- dynlm(infl ~ paro, data = db, start = 1967)
coeftest(mod1, vcov. = vcovHAC)

mod2 <- dynlm(dinfl ~ paro, data = db, start = 1967)
coeftest(mod2, vcov. = vcovHAC)


for (i in 0:3) {
  dynlm(dinfl ~ L(paro_c, 0:i) , data = db, start = 1967) |>
    BIC() |>
    print()
}

mod20 <- dynlm(dinfl ~ L(paro_c, 0:1), data = db, start = 1967)
coeftest(mod20, vcov. = vcovHAC)


mod35 <- dynlm(dinfl ~ L(paro_c, 0:1) + L(dinfl), data = db, start = 1967)
coeftest(mod35, vcov. = vcovHAC)


mod25 <- dynlm(dinfl ~ d(paro_c) + L(paro_c), data = db, start = 1967)
coeftest(mod25, vcov. = vcovHAC)

mod40 <- dynlm(dinfl ~ d(paro_c), data = db, start = 1967)
coeftest(mod40, vcov. = vcovHAC)

