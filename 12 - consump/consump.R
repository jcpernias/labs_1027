library(readxl)
library(zoo)
library(dynlm)
library(ggplot2)
library(car)
library(sandwich)
library(lmtest)

## library(forecast)
## library(wooldridge)
## library(openxlsx)

## data("consump")

xlsx <- read_xlsx("consump.xlsx")
db <- zooreg(xlsx, start = 1959, frequency = 1)
db$lc <- log(db$cons)
db$ly <- log(db$ydisp)

autoplot(db$lc)
autoplot(db$ly)

qplot(data = db, y = lc, x = ly)

mod1 <- dynlm(lc ~ ly, data = db)
summary(mod1)

uhat1 <- resid(mod1)
autoplot(uhat1)

bgaux <- update(mod1, uhat1 ~ . + L(uhat1))
summary(bgaux)

ur <- dynlm(lc ~ L(lc) + trend(db), data = db)
summary(ur)

ur <- dynlm(ly ~ L(ly) + trend(db), data = db)
summary(ur)


db$dlc <- diff(db$lc)
db$dly <- diff(db$ly)


qplot(data = db, y = dlc, x = dly)

mod2 <- dynlm(dlc ~ dly, data = db)
summary(mod2)

uhat2 <- resid(mod2)
autoplot(uhat2)

bgaux <- update(mod2, uhat2 ~ . + L(uhat2))
summary(bgaux)

mod3 <- dynlm(dlc ~ L(dlc) + dly + L(dly), data = db)
summary(mod3)


mod4 <- dynlm(dlc ~ dly + L(uhat1), data = db)
summary(mod4)

## write.xlsx(subset(db, select = c(cons = c, ydisp = y)), "consump.xlsx")


