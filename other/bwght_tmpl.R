#' ---
#' author: "=============== NOMBRE Y APELLIDO(S) ==============="
#' title: "El efecto del tabaco sobre el peso de los recién nacidos (y II)"
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

#' # Preliminares
#'
#' Recuperamos las variables que creamos en la sesión anterior y
#' estimamos el modelo de regresión simple inicial.
#'
#' ## Datos y transformación de variables
#'
#' Cargamos el paquete wooldridge para acceder a los datos.
library(wooldridge)

#' Los datos se encuentran en la base de datos bwght.
data(bwght)

#' Creamos una nueva variable que contenga el peso de los recién
#' nacidos en kilogramos:
bwght$bwght_kg <- bwght$bwght * 29 / 1000

#' Creamos otra variable que contenga el logaritmo de bwght_kg
bwght$lbwght_kg <- log(bwght$bwght_kg)

#' Creamos la variable lógica smoker que toma el valor TRUE para
#' las observaciones en que la madre fumó durante el embarazo:
bwght$smoker <- bwght$cigs > 0

#' ## Modelo de regresión inicial
#'
#' Estimamos los parámetros de un modelo de regresión simple de
#' lbwght_kg sobre smoker:
mod1 <- lm(lbwght_kg ~ smoker, data = bwght)
summary(mod1)

#' Calculamos intervalos de confianza para los parámetros del
#' modelo de regresión simple:
confint(mod1)

#' # Regresión múltiple
#'
#' Ampliaremos el modelo de regresión de la sección anterior para
#' incluir más variables explicativas que pueden estar relacionadas
#' con el peso de los recién nacidos. Trataremos de determinar si el
#' efecto del consumo de tabaco es genuino o se debe a factores que
#' se omitieron en el modelo de regresión simple.
#'
#' ## Más transformaciones de variables
#'
#' Cree en la base de datos `bwght` las siguientes variables:
#'
#' - `lfaminc`: el logaritmo de la renta familiar.
#'
## >>>>>>>>

#' - `smoker_hi`: variable ficticia que indica si la madre fumó más
#' de 15 cigarrillos  diarios. Usamos la función `as.integer` para
#' convertir los valores `TRUE` y `FALSE` en 0 y 1, respectivamente:
#'
bwght$smoker_hi <- as.integer(bwght$cigs > 15)

#' - `smoker_med`: variable ficticia que indica si la madre fumó
#' entre 6 y 15 cigarrillos diarios.
#'
## >>>>>>>>


#' - `smoker_low`: variable ficticia que indica si la madre fumó
#' entre 1 y 5 cigarrillos diarios.
#'
## >>>>>>>>


#' - `first`: variable ficticia que indica si la observación
#' corresponde al primer hijo de la madre (la variable `parity`
#' toma el valor 1).
#'
## >>>>>>>>


#' ## Regresión múltiple
#'
#' El modelo de regresión múltiple que utilizaremos a partir de
#' ahora es:
#'   $$\begin{align*}
#' \log(\mathit{bwght\_kg}) = \beta_0
#' & + \beta_1 \, \mathit{first}
#' + \beta_2  \, \mathit{white}
#' + \beta_3 \, \mathit{male}
#' + \beta_4 \, \log(\mathit{faminc}) \\
#' &+ \beta_5 \, \mathit{smoker\_low}
#' + \beta_6 \, \mathit{smoker\_med}
#' + \beta_7 \, \mathit{smoker\_hi}
#' + u
#' \end{align*}
#' $$
#'
#' Estime por MCO un modelo de regresión de `lbwght_kg` sobre las
#' explicativas `first`, `male`, `white`, `lfaminc`, `smoker_low`,
#' `smoker_med` y `smoker_hi`. Guarde los resultados de la estimación
#' en la variable `mod2` e imprima la tabla con los resultados de la
#' estimación con la función `summary`.
#'
## >>>>>>>>

#' ## Contrastes de hipótesis
#'
#' Usaremos la función `lht` del paquete `car` para llevar a cabo
#' contrastes de hipótesis lineales sobre los parámetros del modelo
#' de regresión. Cargue el paquete `car` con la función `library` (es
#' posible que tenga que instalar el paquete previamente).
#'
## >>>>>>>>

#' Para contrastar la hipótesis nula de que el parámetro de `male`
#' es igual a 0, primero creamos una variable con el nombre de la
#' explicativa entre comillas (dobles o simples):
#'
h0_1 <- "male"

#' Ahora usamos la función `lht` e indicamos, en primer lugar, el
#' modelo de regresión, `mod2`, y, en segundo lugar, la hipótesis que
#' queremos contrastar, `h0_1`:
#'
## lht(mod2, h0_1)


#' Contraste ahora la significación de la variable `lfaminc`.
#' Cree la variable `h0_2` para especificar la nueva hipótesis nula.
#'
## >>>>>>>>

#' Realice el contraste de la hipótesis guardada en `h0_2` usando
#' la función `lht`.
## >>>>>>>>

#' Para contrastar hipótesis complejas necesitamos crear vectores que
#' indiquen qué parámetros o combinaciones de parámetros son iguales
#' a 0 bajo la hipótesis nula. Por ejemplo, consideremos la hipótesis:
#' $$ H_0: \beta_5 = \beta_6 = \beta_7. $$
#' Podemos reescribirla como
#' $$ H_0: \begin{cases}
#' \beta_5 - \beta_6 = 0, \\
#' \beta_5 - \beta_7 = 0.
#' \end{cases}
#' $$
#' Para especificar esta hipótesis nula, creamos un vector con la
#' función `c`, con los elementos separados por comas:
#'
h0_3 <- c("smoker_low - smoker_med", "smoker_low - smoker_hi")

#' Contraste la hipótesis `h0_3`.
#'
## >>>>>>>>


#' La última hipótesis que contrastaremos es la unión de `h0_2` y
#' `h0_3`:
#' $$ H_0: \begin{cases}
#' \beta_4 = 0, \\
#' \beta_5 = \beta_6 = \beta_7.
#' \end{cases}
#' $$
#' Podemos combinar vectores usando la función `c` para concatenarlos.
#' Obtenga la hipótesis `h0_4` combinando los vectores
#' `h0_2` y `h0_3` con la función `c`:
#'
## h0_4 <- c(h0_2, h0_3)

#' Contraste la hipótesis `h0_4`.
#'
## >>>>>>>>

#' ## Modelo final
#'
#' Estime y presente los resultados del modelo de regresión después
#' de imponer las hipótesis contenidas en `h0_4`.
#'
## >>>>>>>>

#' Comente los resultados obtenidos. ¿Qué efecto tiene el consumo de
#' tabaco durante el embarazo sobre el peso de los recién nacidos?
#'
## >>>>>>>>
