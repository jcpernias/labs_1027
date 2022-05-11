library(readxl)
library(zoo)
library(dynlm)
library(ggplot2)
library(car)
library(sandwich)
library(lmtest)

db <- read_xlsx("phillips.xlsx")

db_ts <- zooreg(db[, c("U", "dPIB")], start = 1954, frequency = 1)

db_ts$unem <- db_ts$U
db_ts$lp <- log(db_ts$dPIB)
db_ts$infl <- diff(db_ts$lp)

autoplot(db_ts$unem)
autoplot(db_ts$infl)

ggplot(data = db_ts, aes(x = unem, y = infl)) +
  geom_point()

mod1 <- dynlm(infl ~ L(infl), data = db_ts, start = 1965)
summary(mod1)

mod2 <- dynlm(unem ~ L(unem), data = db_ts, start = 1965)
summary(mod2)



mod3 <- dynlm(d(infl) ~ d(unem), data = db_ts, start = 1965)
summary(mod3)

uhat3 <- resid(mod3)
bgaux <- update(mod3, uhat3 ~ . + L(uhat3))
summary(bgaux)

db_ts$dunem <- diff(db_ts$unem)
db_ts$dinfl <- diff(db_ts$infl)

mod4 <- dynlm(dinfl ~ L(dinfl) + dunem + L(dunem, 1), data = db_ts, start = 1965)
summary(mod4)


