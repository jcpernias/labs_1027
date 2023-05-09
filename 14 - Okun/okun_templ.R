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
## >>>>>>>>>>>>>>>>>>>>

#'
#' # Datos
#'
#' - Lea los datos del fichero `okun.csv` y guárdelos en la
#' variable `okun`.
## >>>>>>>>>>>>>>>>>>>>

#' - Con los datos guardados en `okun`, cree una base de datos de series
#' temporales y guárdela en la variable `st`. Los datos son trimestrales y
#' comienzan en el segundo trimestre de 1980
## >>>>>>>>>>>>>>>>>>>>

#'
#' # Transformación de variables
#'
#' - Calcule el desempleo cíclico como la diferencia entre el desempleo, `u`, y
#' el desempleo natural, `un`, y guárdelo en la variable `uc` en la base de
#' datos `st`.
## >>>>>>>>>>>>>>>>>>>>

#' - Calcule la brecha de la producción y guárdela en la variable `yb` en la
#' base de datos `st`. La brecha de la producción, medida en puntos porcentuales
#' de la producción potencial, es igual a 100 por la diferencia entre el
#' logaritmo de la producción, `ly`, y el logaritmo de la producción potencial,
#' `lyp`.
## >>>>>>>>>>>>>>>>>>>>

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
## >>>>>>>>>>>>>>>>>>>>

#' - Contraste aumentado de Dickey-Fuller.
## >>>>>>>>>>>>>>>>>>>>

#' - Resultado del contraste para un nivel de significación del $5\%$.
## >>>>>>>>>>>>>>>>>>>>

#'
#' ## Desempleo cíclico
#'
#' - Gráfico de series temporales de `uc`.
## >>>>>>>>>>>>>>>>>>>>

#' - Contraste aumentado de Dickey-Fuller.
## >>>>>>>>>>>>>>>>>>>>

#' - Resultado del contraste para un nivel de significación del $5\%$.
## >>>>>>>>>>>>>>>>>>>>

#'
#' ## Logaritmo de la producción agregada
#'
#' - Gráfico de series temporales de `ly`.
## >>>>>>>>>>>>>>>>>>>>

#' - Contraste aumentado de Dickey-Fuller.
## >>>>>>>>>>>>>>>>>>>>

#' - Resultado del contraste para un nivel de significación del $5\%$.
## >>>>>>>>>>>>>>>>>>>>

#'
#' ## Brecha de la producción
#'
#' - Gráfico de series temporales de `yb`.
## >>>>>>>>>>>>>>>>>>>>

#' - Contraste aumentado de Dickey-Fuller.
## >>>>>>>>>>>>>>>>>>>>

#' - Resultado del contraste para un nivel de significación del $5\%$.
## >>>>>>>>>>>>>>>>>>>>

#'
#' ## Tasa de crecimiento de la producción agregada
#'
#' - Gráfico de series temporales de `gy`.
## >>>>>>>>>>>>>>>>>>>>

#' - Contraste aumentado de Dickey-Fuller.
## >>>>>>>>>>>>>>>>>>>>

#' - Resultado del contraste para un nivel de significación del $5\%$.
## >>>>>>>>>>>>>>>>>>>>

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
## >>>>>>>>>>>>>>>>>>>>

#' - Amplíe el modelo estático añadiendo como regresores adicionales retardos
#' de la brecha de la producción. Guarde en las variables `mod1_l1`,  `mod1_l2`,
#' ..., `mod1_l6` las estimaciones con 1, 2, ..., 6 retardos de `yb`.
## >>>>>>>>>>>>>>>>>>>>

#' - Utilice el BIC de los modelos `mod1_l0`, `mod1_l1`, ..., `mod1_l6` para
#' seleccionar el número de retardos.
## >>>>>>>>>>>>>>>>>>>>


#'
#' ## Modelo de retardos distribuidos
#'
#' - Obtenga los errores típicos robustos a heteroscedasticidad y
#' autocorrelación del modelo seleccionado con el BIC.
## >>>>>>>>>>>>>>>>>>>>

#'
#' ## Estimación del multiplicador de largo plazo
#'
#' - Reescriba el modelo de forma que se estimen directamente los
#' multiplicadores acumulativos y el multiplicador de largo plazo. Obtenga los
#' errores típicos robustos a heteroscedasticidad y autocorrelación.
## >>>>>>>>>>>>>>>>>>>>

#'
#' # Estimación de la ecuación en diferencias
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
## >>>>>>>>>>>>>>>>>>>>

#' - Amplíe el modelo estático añadiendo como regresores adicionales retardos
#' de la tasa de crecimiento de la producción. Guarde en las variables
#' `mod2_l1`,  `mod2_l2`, ..., `mod2_l6` las estimaciones con 1, 2, ...,
#' 6 retardos de `gy`.
## >>>>>>>>>>>>>>>>>>>>

#' - Utilice el BIC de los modelos `mod2_l0`, `mod2_l1`, ..., `mod2_l6` para
#' seleccionar el número de retardos.
## >>>>>>>>>>>>>>>>>>>>

#'
#' ## Modelo de retardos distribuidos
#'
#' - Obtenga los errores típicos robustos a heteroscedasticidad y
#' autocorrelación del modelo seleccionado con el BIC.
## >>>>>>>>>>>>>>>>>>>>

#'
#' ## Estimación del multiplicador de largo plazo
#'
#' - Reescriba el modelo de forma que se estimen directamente los
#' multiplicadores acumulativos y el multiplicador de largo plazo. Obtenga los
#' errores típicos robustos a heteroscedasticidad y autocorrelación.
## >>>>>>>>>>>>>>>>>>>>

