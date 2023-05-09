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
#' - Lea los datos del fichero `okun.csv` y guárdelos en la
#' variable `okun`.
okun <- read.csv("okun.csv")

#' - Con los datos guardados en `okun`, cree una base de datos de series
#' temporales y guárdela en la variable `st`. Los datos son trimestrales y
#' comienzan en el segundo trimestre de 1980
st <- zooreg(okun, frequency = 4, start = c(1980, 2))

#'
#' # Transformación de variables
#'
#' - Calcule el desempleo cíclico como la diferencia entre el desempleo, `u`, y
#' el desempleo natural, `un`, y guárdelo en la variable `uc` en la base de
#' datos `st`.
st$uc <- st$u - st$un

#' - Calcule la brecha de la producción y guárdela en la variable `yb` en la
#' base de datos `st`. La brecha de la producción, medida en puntos porcentuales
#' de la producción potencial, es igual a 100 por la diferencia entre el
#' logaritmo de la producción, `ly`, y el logaritmo de la producción potencial,
#' `lyp`.
st$yb <- 100 * (st$ly - st$lyp)

#'
#' # Contrastes de raíces unitarias
#'
#' Determine para las variables que se enumeran a continuación la presencia de
#' raíces unitarias. En todos los casos:
#'
#' 1. Determine mediante la inspección de un gráfico de series temporales si es
#' necesario incluir una tendencia lineal en la regresión de Dickey-Fuller.
#'
#' 2. Seleccione el número de retardos a incluir en la regresión de
#' Dickey-Fuller usando el AIC usando como máximo 6 retardos.
#'
#' ## Tasa de desempleo
#'
#' - Gráfico de series temporales de `u`.
autoplot(st$u)

#' - Contraste aumentado de Dickey-Fuller.
df <- ur.df(st$u, type = "drift", lags = 6, selectlags = "AIC")
summary(df)

#' - Resultado del contraste para un nivel de significación del $5\%$.
#'
#' El valor del contraste de Dickey-Fuller es $-2.65$ que es mayor que $-2.88$,
#' el valor crítico al $5\%$ de significación, por lo que no se rechaza
#' la hipótesis nula de una raíz unitaria en `u`.

#'
#' ## Desempleo cíclico
#'
#' - Gráfico de series temporales de `uc`.
autoplot(st$uc)

#' - Contraste aumentado de Dickey-Fuller.
df <- ur.df(st$uc, type = "drift", lags = 6, selectlags = "AIC")
summary(df)

#' - Resultado del contraste para un nivel de significación del $5\%$.
#'
#' El valor del contraste de Dickey-Fuller es $-4.11$ que es menor que $-2.88$,
#' el valor crítico al $5\%$ de significación, por lo que se rechaza
#' la hipótesis nula de una raíz unitaria en `uc`.

#'
#' ## Logaritmo de la producción agregada
#'
#' - Gráfico de series temporales de `ly`.
autoplot(st$ly)

#' - Contraste aumentado de Dickey-Fuller.
df <- ur.df(st$ly, type = "trend", lags = 6, selectlags = "AIC")
summary(df)

#' - Resultado del contraste para un nivel de significación del $5\%$.
#'
#' El valor del contraste de Dickey-Fuller es $-1.90$ que es mayor que $-3.43$,
#' el valor crítico al $5\%$ de significación, por lo que no se puede rechazar
#' la hipótesis nula de una raíz unitaria en `ly`.

#'
#' ## Brecha de la producción
#'

#' - Gráfico de series temporales de `yb`.
autoplot(st$yb)

#' - Contraste aumentado de Dickey-Fuller.
df <- ur.df(st$yb, type = "drift", lags = 6, selectlags = "AIC")
summary(df)

#' - Resultado del contraste para un nivel de significación del $5\%$.
#'
#' El valor del contraste de Dickey-Fuller es $-4.16$ que es menor que $-2.88$,
#' el valor crítico al $5\%$ de significación, por lo que se rechaza
#' la hipótesis nula de una raíz unitaria en `yb`.

#'
#' ## Tasa de crecimiento de la producción agregada
#'
#' - Gráfico de series temporales de `gy`.
autoplot(st$gy)

#' - Contraste aumentado de Dickey-Fuller.
df <- ur.df(st$gy, type = "drift", lags = 6, selectlags = "AIC")
summary(df)

#' - Resultado del contraste para un nivel de significación del $5\%$.
#'
#' El valor del contraste de Dickey-Fuller es $-2.93$ que es menor que $-2.88$,
#' el valor crítico al $5\%$ de significación, por lo que se rechaza
#' la hipótesis nula de una raíz unitaria en `gy`.

#'
#' # Estimación de la ecuación en niveles
#'
#' Para garantizar que todas las regresiones de esta sección utilizan la misma
#' muestra, **no utilice en las estimaciones los datos anteriores al primer
#' trimestre de 1982.**
#'
#' ## Selección del número de retardos
#'
#' - Estime un modelo estático con el desempleo cíclico como variable dependiente
#' y la brecha de la producción como explicativa. Guarde los resultados en la
#' variable `mod1_l0`.
mod1_l0 <- dynlm(uc ~ yb, data = st, start = "1982-1")

#' - Amplíe el modelo estático añadiendo como regresores adicionales retardos
#' de la brecha de la producción. Guarde en las variables `mod1_l1`,  `mod1_l2`,
#' ..., `mod1_l6` las estimaciones con 1, 2, ..., 6 retardos de `yb`.
mod1_l1 <- update(mod1_l0, . ~ . + L(yb, 1))
mod1_l2 <- update(mod1_l1, . ~ . + L(yb, 2))
mod1_l3 <- update(mod1_l2, . ~ . + L(yb, 3))
mod1_l4 <- update(mod1_l3, . ~ . + L(yb, 4))
mod1_l5 <- update(mod1_l4, . ~ . + L(yb, 5))
mod1_l6 <- update(mod1_l5, . ~ . + L(yb, 6))

#' - Utilice el BIC de los modelos `mod1_l0`, `mod1_l1`, ..., `mod1_l6` para
#' seleccionar el número de retardos.
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
#' - Obtenga los errores típicos robustos a heteroscedasticidad y
#' autocorrelación del modelo seleccionado con el BIC.
coeftest(mod1_l1, vcov. = vcovHAC)

#'
#' ## Estimación del multiplicador de largo plazo
#'
#' - Reescriba el modelo de forma que se estimen directamente los
#' multiplicadores acumulativos y el multiplicador de largo plazo. Obtenga los
#' errores típicos robustos a heteroscedasticidad y autocorrelación.
mod1_lp <- dynlm(uc ~ d(yb) + L(yb), data = st, start = "1982-1")
coeftest(mod1_lp, vcov. = vcovHAC)

#'
#' # Estimación de la ecuación en tasas de crecimiento
#'
#'
#' Para garantizar que todas las regresiones de esta sección utilizan la misma
#' muestra, **no utilice en las estimaciones los datos anteriores al primer
#' trimestre de 1982.**
#'
#'
#' ## Selección del número de retardos
#'
#' - Estime un modelo estático con la diferencia del desempleo como variable
#' dependiente y la tasa de crecimiento de la producción como explicativa. Guarde los resultados en la
#' variable `mod2_l0`.
mod2_l0 <- dynlm(d(u) ~ gy, data = st, start = "1982-1")

#' - Amplíe el modelo estático añadiendo como regresores adicionales retardos
#' de la tasa de crecimiento de la producción. Guarde en las variables
#' `mod2_l1`,  `mod2_l2`, ..., `mod2_l6` las estimaciones con 1, 2, ...,
#' 6 retardos de `gy`.
mod2_l1 <- update(mod2_l0, . ~ . + L(gy, 1))
mod2_l2 <- update(mod2_l1, . ~ . + L(gy, 2))
mod2_l3 <- update(mod2_l2, . ~ . + L(gy, 3))
mod2_l4 <- update(mod2_l3, . ~ . + L(gy, 4))
mod2_l5 <- update(mod2_l4, . ~ . + L(gy, 5))
mod2_l6 <- update(mod2_l5, . ~ . + L(gy, 6))

#' - Utilice el BIC de los modelos `mod2_l0`, `mod2_l1`, ..., `mod2_l6` para
#' seleccionar el número de retardos.
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
#' - Obtenga los errores típicos robustos a heteroscedasticidad y
#' autocorrelación del modelo seleccionado con el BIC.
coeftest(mod2_l1, vcov. = vcovHAC)

#'
#' ## Estimación del multiplicador de largo plazo
#'
#' - Reescriba el modelo de forma que se estimen directamente los
#' multiplicadores acumulativos y el multiplicador de largo plazo. Obtenga los
#' errores típicos robustos a heteroscedasticidad y autocorrelación.
mod2_lp <- dynlm(d(u) ~ d(gy) + L(gy), data = st, start = "1982-1")
coeftest(mod2_lp, vcov. = vcovHAC)

