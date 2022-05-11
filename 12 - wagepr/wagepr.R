library(readxl)
library(zoo)
library(dynlm)
library(ggplot2)
library(car)
library(sandwich)
library(lmtest)

db <- read_xlsx("wagepr.xlsx")

db_ts <- zooreg(db[, c("hwage", "dcons")], start = c(1980, 1), frequency = 4)

db_ts$lw <- log(db_ts$hwage)
db_ts$lp <- log(db_ts$dcons)
db_ts$gw <- diff(db_ts$lw, 4)
db_ts$gp <- diff(db_ts$lp, 4)

autoplot(db_ts$gw)
autoplot(db_ts$gp)

ggplot(data = db_ts, aes(x = gw, y = gp)) +
  geom_point()

mod1 <- dynlm(gp ~ gw, data = db_ts, start = "1982-1", end = "2019-4")
summary(mod1)
uhat1 <- resid(mod1)
bgaux <- update(mod1, uhat1 ~ . + L(uhat1))
summary(bgaux)

bgaux <- update(mod1, uhat1 ~ . + L(uhat1, 1:2))
summary(bgaux)

mod2 <- update(mod1, . ~ . + L(gp) + L(gp, 2) + L(gw, 1) + L(gw, 2))
summary(mod2)

mod3 <- dynlm(gp ~  L(gp) + L(gw, 2),
              data = db_ts, start = "1982-1", end = "2019-4")
summary(mod3)

autoplot(uhat3)

uhat3 <- resid(mod3)
bgaux <- update(mod3, uhat3 ~ . + L(uhat3))
summary(bgaux)

bgaux <- update(mod3, uhat3 ~ . + L(uhat3, 1:2))
summary(bgaux)

