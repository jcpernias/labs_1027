#' ---
#'  title: "Curva de Phillips (I)"
#'  author: "EC1027 --- Econometría I"
#'  date: "Curso 2022-2023"
#' ---

#+ setup, include=FALSE
knitr::opts_chunk$set(echo = TRUE, comment = "")

#' Cargamos los paquetes que se usarán en esta sesión.
#+ packages, message=FALSE, warning=FALSE
library(mosaic, warn.conflicts = FALSE)
library(zoo)
library(dynlm)
library(sandwich)
library(lmtest)

#+ load-urca
library(urca)

#' Leemos los datos del fichero csv.
db <- read.csv("phillips.csv")

#' Establecemos el periodo inicial y la frecuencia de las series.
st <- zooreg(db, start = 1963, frequency = 1)

#' Calulamos la tasa de inflación y la guardamos en la base de datos.
st$infl <- 100 * diff(log(st$defl))


#' Eliminamos la primera observación de la base de datos
st <- window(st, start = 1964)

#' Gráficos de series temporales: inflación.
#+ plot-infl, warning=FALSE
autoplot(st$infl)

#' Contraste de raíz unitaria: inflación.
#'
df_infl <- ur.df(st$infl, type = "drift",
                 lags = 3, selectlags = "AIC")
summary(df_infl)

#' Contraste de raíz unitaria: diferencia de la inflación.
df_dinfl <- ur.df(diff(st$infl), type = "drift",
                  lags = 3, selectlags = "AIC")
summary(df_dinfl)

#' Gráficos de series temporales: tasa de paro.
#+ plot-unem, warning=FALSE
autoplot(st$paro)

#' Contraste de raíz unitaria: tasa de paro.
#'
df_paro <- ur.df(st$paro, type = "drift",
                 lags = 3, selectlags = "AIC")
summary(df_paro)

#' Contraste de raíz unitaria: diferencia tasa de paro.
#'
df_dparo <- ur.df(diff(st$paro), type = "drift",
                  lags = 3, selectlags = "AIC")
summary(df_dparo)

#' Desempleo cíclico
#'
st$paro_c <- st$paro - st$paro_n

#' Gráfico de series temporales: desempleo cíclico
autoplot(st$paro_c)

#' Contraste de raíz unitaria: desempleo cíclico
#' (por construcción no presenta raíces unitarias).
df_paro_c <- ur.df(st$paro_c, type = "drift",
                   lags = 3, selectlags = "AIC")
summary(df_paro_c)

#' Selección del número de retardos. Estimamos modelos de retardos
#' distribuidos para 0, 1, 2 y 3 retardos. Todas las regresiones usan la misma
#' muestra, 1967-2022.
#'
#' Modelo estático
mod3_l0 <- dynlm(d(infl) ~ paro_c , data = st, start = 1967)

#' Añadimos un retardo de `paro_c`:
mod3_l1 <- dynlm(d(infl) ~ paro_c + L(paro_c) , data = st, start = 1967)

#' Usamos `update` para añadir el segundo retardo al modelo anterior:
mod3_l2 <- update(mod3_l1, . ~ . + L(paro_c, 2))

#' Añadimos el último retardo:
mod3_l3 <- update(mod3_l2, . ~ . + L(paro_c, 3))


#' Calculamos los BIC de todos los modelos:
BIC(mod3_l0)
BIC(mod3_l1)
BIC(mod3_l2)
BIC(mod3_l3)

#' Seleccionamos el modelo con 1 retardo, `mod3_l1` por ser el que
#' tiene menor BIC. Tabla de las regresiones
summary(mod3_l1)

#' Tabla de la regresión usando errores robustos a heteroscedasticidad y
#' autocorrelación:
coeftest(mod3_l1, vcov. = vcovHAC)

#' Coeficientes de autocorrelación de los residuos:
acf(resid(mod3_l1))


#' Estimación de un modelo ARDL(1, 1)
mod4 <- update(mod3_l1, . ~ . + L(d(infl)))
summary(mod4)
coeftest(mod4)

#' Reparametrización del modelo `mod3_l1` para obtener el multiplicador de
#' largo plazo:
mod3_lp <- update(mod3_l1, . ~ d(paro_c) + L(paro_c))
coeftest(mod3_lp)

#' Estimamos el modelo imponiendo un multiplicador de largo plazo igual a 0:
mod3_lp2 <- update(mod3_l1, . ~ d(paro_c))
coeftest(mod3_lp2)

