library(readxl)
library(mosaic, warn.conflicts = FALSE)
library(zoo)
library(urca)
library(dynlm)
library(sandwich)
library(lmtest)

traffic2 <- read_excel("traffic2.xlsx")
st <- zooreg(traffic2, start = c(1981, 1), frequency = 12)

st$ltotacc <- log(st$totacc)
st$dltotacc <- diff(st$ltotacc)
autoplot(st$ltotacc)
autoplot(st$dltotacc)

ur.df(st$ltotacc, type = "trend", lags = 6, selectlags = "AIC") |>
  summary()

autoplot(st$wkends)
ur.df(st$wkends, type = "drift", lags = 6, selectlags = "AIC") |>
  summary()

autoplot(st$unem)
ur.df(st$unem, type = "trend", lags = 6, selectlags = "AIC") |>
  summary()


mod1 <- dynlm(ltotacc ~ trend(st) +  + season(st)
                + unem + wkends, data = st)
summary(mod1)
coeftest(mod1, vcov. = vcovHAC)

mod2 <- update(mod1, . ~ .  + spdlaw + beltlaw)
summary(mod2)
coeftest(mod2, vcov. = vcovHAC)

st$fr_fatal <- st$fatacc / st$totacc * 1000
autoplot(st$fr_fatal)
ur.df(st$fr_fatal, type = "trend", lags = 6, selectlags = "AIC") |>
  summary()
mod3 <- dynlm(fr_fatal ~ trend(st) + season(st) +
                + unem + spdlaw + beltlaw + wkends, data = st)
summary(mod3)
coeftest(mod3, vcov. = vcovHAC)
