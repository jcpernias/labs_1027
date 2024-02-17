library(readxl)
library(zoo)
library(dynlm)
library(ggplot2)
library(car)
library(sandwich)
library(lmtest)

xls <- read_xlsx("phillips.xlsx")

db <- zooreg(xls[, c("U", "dPIB")], start = 1954, frequency = 1)

db$unem <- db$U
db$lp <- log(db$dPIB)
db$infl <- 100 * diff(db$lp)

autoplot(db$unem)
autoplot(db$infl)

qplot(data = db, x = unem, y = infl)


mod1 <- dynlm(infl ~ unem, data = db, start = 1965)
summary(mod1)

uhat1 <- resid(mod1)
autoplot(uhat1)
bgaux <- update(mod1, uhat1 ~ . + L(uhat1))
summary(bgaux)

mod2 <- dynlm(infl ~ L(infl) + trend(db), data = db, start = 1965)
summary(mod2)

mod3 <- dynlm(unem ~ L(unem) + trend(db), data = db, start = 1965)
summary(mod3)

db$d_infl <- diff(db$infl)
db$d_unem <- diff(db$unem)

mod3 <- dynlm(d_infl ~ L(d_infl) + d_unem + L(d_unem, 1),
              data = db, start = 1965)
summary(mod3)
uhat3 <- resid(mod3)
autoplot(uhat3)
bgaux <- update(mod3, uhat3 ~ . + L(uhat3))
summary(bgaux)
lht(mod3, c("L(d_infl)", "L(d_unem, 1)"))

mod4 <- dynlm(d_infl ~ d_unem, data = db, start = 1965)
summary(mod4)

uhat3 <- resid(mod3)
bgaux <- update(mod3, uhat3 ~ . + L(uhat3))
summary(bgaux)




