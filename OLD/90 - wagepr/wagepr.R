library(readxl)
library(zoo)
library(dynlm)
library(ggplot2)
library(car)
library(sandwich)
library(lmtest)

library(forecast)

db_orig <- read_xlsx("wagepr.xlsx")
db_all <- zooreg(db_orig[, c("hwage", "dcons", "prod")],
                start = c(1980, 1),
                frequency = 4)
db <- window(db_all, start = "1995-1", end = "2019-4")

db$lw <- log(db$hwage)
db$lp <- log(db$dcons)
db$lprod <- log(db$prod)
db$lwp <- log(db$hwage / db$dcons)

autoplot(db$lw)
autoplot(db$lp)
autoplot(db$lwp)
autoplot(db$lprod)

ur <- dynlm(lwp ~ L(lwp) + trend(db), data = db)
summary(ur)

ur <- dynlm(lprod ~ L(lprod) + trend(db), data = db)
summary(ur)

db$gwp <- 100 * diff(db$lwp, 1)
db$gprod <- 100 * diff(db$lprod, 1)

autoplot(db$gwp)
autoplot(db$gprod)

qplot(data = db, x = gwp, y = gprod)


mod1 <- dynlm(gwp ~ gprod, data = db, start = "1996-1")
summary(mod1)


uhat1 <- resid(mod1)
autoplot(uhat1)
bgaux <- update(mod1, uhat1 ~ . + L(uhat1))
summary(bgaux)

bgaux <- update(mod1, uhat1 ~ . + L(uhat1, 1:2))
summary(bgaux)

mod2 <- update(mod1, . ~ . + L(gwp) + L(gwp, 2) + L(gprod, 1) + L(gprod, 2))
summary(mod2)

uhat2 <- resid(mod2)
autoplot(uhat2)
bgaux <- update(mod2, uhat2 ~ . + L(uhat2))
summary(bgaux)

bgaux <- update(mod2, uhat2 ~ . + L(uhat2, 1:2))
summary(bgaux)
lht(bgaux, matchCoefs(bgaux, "uhat"))

## bug lht: H0 printed incorrectly but the test is correct
lht(mod2, c("L(gp, 2)", "L(gw, 1)", "gw"))

mod3 <- dynlm(gp ~  L(gp) + L(gw, 2),
              data = db, start = "1982-1")
summary(mod3)


uhat3 <- resid(mod3)
autoplot(uhat3)
ggtsdisplay(uhat3, plot.type = "scatter")


bgaux <- update(mod3, uhat3 ~ . + L(uhat3))
summary(bgaux)

bgaux <- update(mod3, uhat3 ~ . + L(uhat3, 1:2))
summary(bgaux)



