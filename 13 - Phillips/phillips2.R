#' ---
#'  title: "Curva de Phillips (II): solución"
#'  author: "EC1027 --- Econometría I"
#'  date: "Curso 2022-2023"
#' ---

#+ setup, include=FALSE
knitr::opts_chunk$set(echo = TRUE, comment = "")

#' # Paquetes R
#' Cargamos los paquetes que se usarán en esta sesión.
#+ packages, message=FALSE, warning=FALSE
library(mosaic, warn.conflicts = FALSE)
library(zoo)
library(dynlm)
library(sandwich)
library(lmtest)
library(urca)

#' # Datos
#'
#' Leemos los datos del fichero csv:
db <- read.csv("phillips.csv")

#' Establecemos el periodo inicial y la frecuencia de las series:
st <- zooreg(db, start = 1963, frequency = 1)

#' Calulamos la tasa de inflación y la guardamos en la base de datos:
st$infl <- 100 * diff(log(st$defl))

#' El desempleo cíclico es la diferencia del desempleo observado
#' y del desempleo natural:
st$paro_c <- st$paro - st$paro_n

#' Eliminamos la primera observación de la base de datos:
st <- window(st, start = 1964)


#' # Contrastes de raíz unitaria
#'
#' Se usan los contrastes aumentados de Dickey-Fuller para
#' determinar la presencia de raíces unitarias en las series
#' de inflación y desempleo. Se seleccionará el número de
#' retardos de acuerdo con el AIC considerando 3 retardos como
#' máximo.
#'
#' ## Inflación.
#'
#' Gráfico de series temporales: inflación.
autoplot(st$infl)

#' Contraste de raíz unitaria: inflación.
df_infl <- ur.df(st$infl, type = "drift",
                 lags = 3, selectlags = "AIC")
summary(df_infl)

#' Contraste de raíz unitaria: diferencia de la inflación.
df_dinfl <- ur.df(diff(st$infl), type = "drift",
                  lags = 3, selectlags = "AIC")
summary(df_dinfl)

#' Los contrastes muestran que la inflación tiene una raíz
#' unitaria y que la diferencia de la inflación no tiene
#' tendencias estocásticas.
#'
#' ## Desempleo cíclico
#'
#' Gráfico de series temporales: desempleo cíclico.
autoplot(st$paro_c)

#' Contraste de raíz unitaria: desempleo cíclico
df_paro_c <- ur.df(st$paro_c, type = "drift",
                   lags = 3, selectlags = "AIC")
summary(df_paro_c)

#' El desempleo cíclico no tiene una raíz unitaria.
#'
#' # Selección del número de retardos
#'
#' Estimamos modelos de retardos distribuidos para 0, 1, 2 y
#' 3 retardos. Todas las regresiones usan la misma muestra,
#' 1967-2022.
#'
#' Modelo estático:
mod3_l0 <- dynlm(d(infl) ~ paro_c , data = st, start = 1967)

#' Añadimos un retardo de `paro_c`:
mod3_l1 <- dynlm(d(infl) ~ paro_c + L(paro_c) , data = st, start = 1967)

#' Usamos `update` para añadir el segundo retardo al modelo anterior:
mod3_l2 <- update(mod3_l1, . ~ . + L(paro_c, 2))

#' Añadimos el tercer retardo de `paro_c`:
mod3_l3 <- update(mod3_l2, . ~ . + L(paro_c, 3))


#' Calculamos los BIC de todos los modelos:
BIC(mod3_l0)
BIC(mod3_l1)
BIC(mod3_l2)
BIC(mod3_l3)

#' Seleccionamos el modelo con 1 retardo, `mod3_l1` por ser el que
#' tiene menor BIC.
#'
#' # Modelo dinámico
#'
#' Tabla de la regresión de3l modelo con 1 retardo:
summary(mod3_l1)

#' Tabla de la regresión usando errores típicos robustos a
#' heteroscedasticidad y autocorrelación:
coeftest(mod3_l1, vcov. = vcovHAC)

#' Coeficientes de autocorrelación de los residuos:
acf(resid(mod3_l1))
#' Los coeficientes de autocorrelación de los residuos son muy
#' pequeños y no muestran evidencias de dependencia fuerte.
#'
#' Investigamos la posibilidad de que sea necesario incluir un
#' retardo de la variable dependiente. Estimación de un modelo
#' ARDL(1, 1):
ardl <- update(mod3_l1, . ~ . + L(d(infl)))
coeftest(ardl, vcov. = vcovHAC)
#' El retardo de la variable dependiente no es significativo.

#'
#' # Efectos a largo plazo
#'
#' Reparametrización del modelo `mod3_l1` para obtener los
#' multiplicadores acumulativos y de largo plazo:
mod3_lp <- update(mod3_l1, . ~ d(paro_c) + L(paro_c))
coeftest(mod3_lp, vcov. = vcovHAC)

#' Las estimaciones anteriores indican que a corto plazo hay un
#' efecto negativo y significativo del paro cíclico sobre la
#' variación de la  inflación. Sin embargo, a largo plazo el
#' desempleo no influiría en la inflación.
#'
#' Estimamos el modelo imponiendo un multiplicador de largo
#' plazo igual a 0:
mod3_lp0 <- update(mod3_lp, . ~ . - L(paro_c))
coeftest(mod3_lp0, vcov. = vcovHAC)

