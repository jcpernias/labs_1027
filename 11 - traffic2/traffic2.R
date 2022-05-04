library(readxl)
library(zoo)
library(dynlm)
library(ggplot2)
library(car)

db <- read_xlsx("traffic2.xlsx")

ts_db <- zooreg(db, start = c(1989, 1), frequency = 12)

ts_db$ltotacc <- log(ts_db$totacc)

autoplot(ts_db$ltotacc)

mod0 <- dynlm(ltotacc ~ trend(ts_db) + season(ts_db), data = ts_db)
summary(mod0)
lht(mod0, matchCoefs(mod0, "season"))

mod1 <- update(mod0, . ~ . + wkends + unem + spdlaw + beltlaw)
summary(mod1)

ts_db$prcfat <- ts_db$fatacc / ts_db$totacc * 100

mod2 <- update(mod1, prcfat ~ .)
summary(mod1)
