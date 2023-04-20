library(mosaic, warn.conflicts = FALSE)
library(zoo)
library(dynlm)
library(sandwich)
library(lmtest)

db <- read.csv("phillips.csv")

st <- zooreg(db, start = 1963, frequency = 1)

st$infl <- 100 * diff(log(st$defl))

autoplot(st$infl)
autoplot(st$paro)

mod1 <- dynlm(infl ~ paro, data = st, start = 1967)
summary(mod1)
uhat1 <- resid(mod1)
autoplot(uhat1)
coeftest(mod1, vcov. = vcovHAC)

acf(uhat1, plot = FALSE)
acf(uhat1)


mod2 <- dynlm(d(infl) ~ paro, data = st, start = 1967)
summary(mod2)
uhat2 <- resid(mod2)
autoplot(uhat2)
coeftest(mod2, vcov. = vcovHAC)

acf(uhat2, plot = FALSE)
acf(uhat2)



