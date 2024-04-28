library(zoo)
library(sandwich)
library(lmtest)
library(dynlm)
library(urca)

db <- read.csv2.zoo("okun.csv", regular = TRUE, FUN = as.yearqtr)

# Diferencia del desempleo
du <- diff(db$u)

# Logaritmo de la producción
ly <- log(db$y) 

# Tasa de crecimiento trimestral de la producción
gy <- 100 * diff(ly)

# Contrastes de raíces unitarias
# Desempleo
autoplot.zoo(db$u)
adf <- ur.df(db$u, type = "drift", lags = 6, selectlags = "AIC")
summary(adf)

# Diferencia del desempleo
autoplot.zoo(du)
adf <- ur.df(du, type = "drift", lags = 6, selectlags = "AIC")
summary(adf)

# Logaritmo de la producción
autoplot.zoo(ly)
adf <- ur.df(ly, type = "trend", lags = 6, selectlags = "AIC")
summary(adf)


# Tasa de crecimiento trimestral de la producción
autoplot.zoo(gy)
adf <- ur.df(gy, type = "drift", lags = 6, selectlags = "AIC")
summary(adf)

# Modelos de retardos distribuidos
dl0 <- dynlm(du ~ gy, start = "1982 Q1")
dl1 <- dynlm(du ~ L(gy, 0:1), start = "1982 Q1")
dl2 <- dynlm(du ~ L(gy, 0:2), start = "1982 Q1")
dl3 <- dynlm(du ~ L(gy, 0:3), start = "1982 Q1")
dl4 <- dynlm(du ~ L(gy, 0:4), start = "1982 Q1")
dl5 <- dynlm(du ~ L(gy, 0:5), start = "1982 Q1")
dl6 <- dynlm(du ~ L(gy, 0:6), start = "1982 Q1")

# Criterios de información
BIC(dl0)
BIC(dl1)
BIC(dl2)
BIC(dl3)
BIC(dl4)
BIC(dl5)
BIC(dl6)

summary(dl1)

# Errores típicos robustos a autocorrelación y heteroscedasticidad
coeftest(dl1, vcov. = vcovHAC)

# Multiplicadores acumulados y de largo plazo
dl1_lr <- dynlm(du ~ d(gy) + L(gy), start = "1982 Q1")
coeftest(dl1_lr, vcov. = vcovHAC)

