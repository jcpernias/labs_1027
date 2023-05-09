#' ---
#' author: "=============== NOMBRE Y APELLIDO(S) ==============="
#' title: "La ley de Okun en España: 1980--2019"
#' date: "`r format(Sys.Date(), '%d-%m-%Y')`"
#' output:
#'   html_document:
#'     number_sections: true
#'     toc: yes
#'     toc_depth: 2
#'     toc_float: yes
#'     highlight: pygments
#'     theme: cerulean
#' ---

#+ include=FALSE
knitr::opts_chunk$set(echo = TRUE, comment = "")


#'
#' # Preliminares
#'
#' - Cargue los paquetes  `mosaic`, `zoo`,  `urca`, `dynlm`, `sandwich`
#' y `lmtest`.

library(mosaic, warn.conflicts = FALSE)
library(zoo)
library(urca)
library(dynlm)
library(sandwich)
library(lmtest)

#'
#' # Datos
#'
#' - Lea los datos del fichero "okun.csv" y guárdelos en la
#' variable `okun`.

okun <- read.csv("okun.csv")

#' - Con los datos guardados en `okun`, cree una base de datos de series
#' temporales y guárdela en la variable `st`. Los datos son trimestrales y
#' comienzan en el segundo trimestre de 1980

st <- zooreg(okun, frequency = 4, start = c(1980, 2))

#'
#' # Transformación de variables
#'

st$uc <- st$u - st$un
st$yb <- 100 * (st$ly - st$lyp)


#'
#' # Contrastes de raíces unitarias
#'

#'
#' ## Logaritmo de la producción agregada
#'

autoplot(st$ly)
df <- ur.df(st$ly, type = "trend", lags = 6, selectlags = "AIC")
summary(df)

#'
#' ## Tasa de crecimiento de la producción agregada
#'

autoplot(st$gy)
df <- ur.df(st$gy, type = "drift", lags = 6, selectlags = "AIC")
summary(df)

#'
#' ## Tasa de desempleo
#'

autoplot(st$u)
df <- ur.df(st$u, type = "drift", lags = 6, selectlags = "AIC")
summary(df)


#'
#' ## Desempleo cíclico
#'

autoplot(st$uc)
df <- ur.df(st$uc, type = "drift", lags = 6, selectlags = "AIC")
summary(df)

#'
#' ## *Output gap*
#'

autoplot(st$yb)
df <- ur.df(st$yb, type = "drift", lags = 6, selectlags = "AIC")
summary(df)



#'
#' # Estimación de la ecuación en niveles
#'


#'
#' ## Selección del número de retardos
#'

mod1_l0 <- dynlm(uc ~ yb, data = st, start = "1982-1")
mod1_l1 <- update(mod1_l0, . ~ . + L(yb, 1))
mod1_l2 <- update(mod1_l1, . ~ . + L(yb, 2))
mod1_l3 <- update(mod1_l2, . ~ . + L(yb, 3))
mod1_l4 <- update(mod1_l3, . ~ . + L(yb, 4))
mod1_l5 <- update(mod1_l4, . ~ . + L(yb, 5))
mod1_l6 <- update(mod1_l5, . ~ . + L(yb, 6))

BIC(mod1_l0)
BIC(mod1_l1)
BIC(mod1_l2)
BIC(mod1_l3)
BIC(mod1_l4)
BIC(mod1_l5)
BIC(mod1_l6)

#'
#' ## Modelo de retardos distribuidos
#'

coeftest(mod1_l1, vcov. = vcovHAC)


#'
#' ## Estimación del multiplicador de largo plazo
#'

mod1_lp <- dynlm(uc ~ d(yb) + L(yb), data = st, start = "1982-1")
coeftest(mod1_lp, vcov. = vcovHAC)

#'
#' # Estimación de la ecuación en diferencias
#'

#'
#' ## Selección del número de retardos
#'
mod2_l0 <- dynlm(d(u) ~ gy, data = st, start = "1982-1")
mod2_l1 <- update(mod2_l0, . ~ . + L(gy, 1))
mod2_l2 <- update(mod2_l1, . ~ . + L(gy, 2))
mod2_l3 <- update(mod2_l2, . ~ . + L(gy, 3))
mod2_l4 <- update(mod2_l3, . ~ . + L(gy, 4))
mod2_l5 <- update(mod2_l4, . ~ . + L(gy, 5))
mod2_l6 <- update(mod2_l5, . ~ . + L(gy, 6))

BIC(mod2_l0)
BIC(mod2_l1)
BIC(mod2_l2)
BIC(mod2_l3)
BIC(mod2_l4)
BIC(mod2_l5)
BIC(mod2_l6)

#'
#' ## Modelo de retardos distribuidos
#'

coeftest(mod2_l1, vcov. = vcovHAC)

#'
#' ## Estimación del multiplicador de largo plazo
#'

mod2_lp <- dynlm(d(u) ~ d(gy) + L(gy), data = st, start = "1982-1")
coeftest(mod2_lp, vcov. = vcovHAC)

