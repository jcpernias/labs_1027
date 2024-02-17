library(readxl)
library(zoo)
library(dynlm)
library(ggplot2)
library(car)
library(sandwich)
library(lmtest)

#' # Apartado 1
#'

db <- read_xlsx("traffic2.xlsx")

ts_db <- zooreg(db, start = c(1989, 1), frequency = 12)

#' Entrada en vigor de la ley que eleva el límite de velocidad
ts_db$spdlaw[which(ts_db$spdlaw == 1)[1]]

#' Entrada en vigor de la ley que obliga a usar el cinturón de seguridad
ts_db$spdlaw[which(ts_db$spdlaw == 1)[1]]

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

# Test Breusch-Godfrey
#
uhat <- resid(mod1)
autoplot(uhat)

bgaux <- update(mod1, uhat ~ . + L(uhat))
summary(bgaux)

coeftest(mod1, vcov. = vcovHAC)


X <- coredata(model.matrix(mod1))
y <- model.response(model.frame(mod1))
rho <- cor(merge(uhat, lag(uhat, -1)), use = "complete.obs")[1, 2]
qdX <- X[-1, ] -  rho * X[-NROW(X), ]
qdY <- y - rho * lag(y, -1)
ar1 <- lm(qdY ~ qdX - 1)
summary(ar1)


library(orcutt)
library(glue)

seas <- cycle(ts_db)
the_months <- setNames(1:12, month.abb)
sdummies <- outer(seas, the_months, "==") + 0
colnames(sdummies) <- names(the_months)
Z <- cbind(trend = 1:NROW(sdummies), sdummies)
ts_db2 <- merge(ts_db,
                zooreg(Z, start = start(ts_db), frequency = frequency(ts_db)))
yname <- "ltotacc"
xnames <- c("trend", month.abb[-1], "wkends", "unem", "spdlaw", "beltlaw")
frml_str <-  as.formula(glue("{yname} ~ {paste(xnames, collapse = ' + ')}"))
co0 <- lm(frml_str, ts_db2)
co1 <- cochrane.orcutt(co0)
summary(co1)
summary(mod1)

library(forecast)
uhat <- zooreg(resid(co1)[, 1], start = c(1989, 2), frequency = 12)

Acf(uhat)
ggtsdisplay(uhat)

