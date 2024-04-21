# library(wooldridge)

# data(hseinv)
# db <- subset(hseinv, select = c(year, inv, pop, price))
# write.csv2(db, file = "hseinv.csv", row.names = FALSE, quote = FALSE)

library(zoo)
db <- read.csv2.zoo("hseinv.csv", regular = TRUE)

invpc <- db$inv / db$pop
linvpc <- log(invpc)
lprice <- log(db$price)
t <- zooreg(1:nrow(db), start = start(db))

mod0 <- lm(linvpc ~ lprice)
summary(mod0)

autoplot.zoo(linvpc)
linvpc_t <- lm(linvpc ~ t)
summary(linvpc_t)

autoplot.zoo(lprice)
lprice_t <- lm(lprice ~ t)
summary(lprice_t)

mod1 <- lm(linvpc ~ lprice + t)
summary(mod1)


