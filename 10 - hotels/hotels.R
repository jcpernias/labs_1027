library(readxl)
library(zoo)
library(ggplot2)
library(dynlm)

db_hotels <- read_xlsx("2011.xlsx",
                       range = "B9:D287",
                       col_names =  c("total_beds", "occup", "empl"),
                       na = ".")

ts_hotels <- zooreg(db_hotels, start = c(1999, 1), frequency = 12)


db_index <- read_excel("25980.xlsx",
                       range = "B9:B334",
                       col_names = "index")

ts_index <- zooreg(db_index, start = c(1995, 1), frequency = 12)


db_all <- merge(ts_hotels, ts_index)

db_all$beds <- db_all$total_beds * db_all$occup / 100

db <- window(db_all, start = "1999-01", end = "2019-12")
db$lempl  <- log(db$empl)
db$lbeds  <- log(db$beds)
db$lindex <- log(db$index)

autoplot(db$lbeds)
autoplot(db$lempl)
autoplot(db$lindex)

mod1 <- dynlm(lempl ~ lbeds + lindex, data = db, start = "2001-01")
summary(mod1)

uhat1 <- resid(mod1)
autoplot(uhat1)

mod2 <- update(mod1, . ~ . + trend(db) + season(db))
summary(mod2)

X <- model.matrix(mod3)

uhat3 <- resid(mod3)
autoplot(uhat3)


mod4 <- dynlm(lempl ~ trend(db2) + season(db2),
              data = db2,
              start = "2001-01")
lempl_nots <- resid(mod4)

mod5 <- update(mod4, lbeds ~ .)
lbeds_nots <- resid(mod5)

mod6 <- update(mod4, lindex ~ .)
lindex_nots <- resid(mod6)

nots <- merge(lempl = lempl_nots, lbeds = lbeds_nots, lindex = lindex_nots)

autoplot(nots$lbeds)
autoplot(nots$lempl)
autoplot(nots$lindex)


mod7 <- dynlm(lempl ~ lbeds + lindex - 1,
              data = nots,
              start = "ene 2001")
summary(mod7)

uhat7 <- resid(mod7)
autoplot(uhat7)


d12 <- diff(db2, 12)

mod8 <- dynlm(lempl ~ L(lempl, c(1:2, 12)) + L(lbeds, c(0:2)) + L(lindex, c(0:2)),
              data = db2,
              start = "ene 2001")
summary(mod8)
uhat8 <- resid(mod8)
autoplot(uhat8)

