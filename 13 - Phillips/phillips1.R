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

#' Leemos los datos del fichero csv.
db <- read.csv("phillips.csv")

#' Establecemos el periodo inicial y la frecuencia de las series.
st <- zooreg(db, start = 1963, frequency = 1)

#' Calulamos la tasa de inflación y la guardamos en la base de datos.
st$infl <- 100 * diff(log(st$defl))

#' Gráficos de series temporales: inflación.
#+ plot-infl, warning=FALSE
autoplot(st$infl)

#' Gráficos de series temporales: tasa de paro.
#+ plot-unem, warning=FALSE
autoplot(st$paro)

#' # Curva de Phillips estática
#'
#' Estimación de la curva de Phillips estática.
#' La función `dynlm` permite indicar el primer periodo
#' que se utilizará en la estimación.
mod1 <- dynlm(infl ~ paro, data = st, start = 1967)

#' Tabla de la regresión.
summary(mod1)

#' Con series temporales y, especialmente con modelos estáticos,
#' es muy frecuente que exista autocorrelación del término de
#' error. Podemos obtener errores típicos validos con
#' autocorrelación y heteroscedasticidad con la siguiente
#' instrucción:
coeftest(mod1, vcov. = vcovHAC)

#' De acuerdo con la tabla anterior, el parámetro $\beta$ es
#' significativamente distinto de 0. Sin embargo, los gráficos
#' de las tasas de inflación y paro mostraban que esas variables
#' presentan tendencias estocásticas. En estas condiciones, existe
#' la posibilidad de que los resultados que hemos obtenido sean
#' espurios.
#'
#' Gráfico de los residuos de la curva de Phillips estática:
uhat1 <- resid(mod1)
autoplot(uhat1)

#' Coeficientes de autocorrelación de los residuos:
acf(uhat1, plot = FALSE)
acf(uhat1)

#' Tanto el gráfico de los residuos como los coeficientes de
#' autocorrelación señalan que los residuos no se comportan
#' como un proceso I(0). La regresión que hemos analizado es
#' espuria.

#'
#'
#' # Curva de Phillips con expectativas
#'
#' Estimamos la curva de Phillips con expectativas usando la
#' misma muestra que en la regresión anterior:
mod2 <- dynlm(d(infl) ~ paro, data = st, start = 1967)

#' Tabla de la regresión:
summary(mod2)

#' Tabla usando errores típicos robustos a heteroscedasticidad y
#' autocorrelación:
coeftest(mod2, vcov. = vcovHAC)

#' Gráfico de los residuos
uhat2 <- resid(mod2)
autoplot(uhat2)

#' Coeficientes de autocorrelación de los residuos:
acf(uhat2, plot = FALSE)
acf(uhat2)

#' En este caso los residuos se comportan como un proceso I(0).
#' Sin embargo la estimación del parámetro $\beta$ no es
#' estadísticamente distinta de 0 para un nivel de significación
#' del 5%. Con esta especificación de
#' la curva de Phillips no encontramos ninguna relación de
#' intercambio entre inflación y desempleo.



