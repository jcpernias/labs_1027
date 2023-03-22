#' ---
#' author: "=============== NOMBRE Y APELLIDO(S) ==============="
#' title: "La mujer en el mercado laboral español"
#' date: "`r format(Sys.Date(), '%d-%m-%Y')`"
#' output:
#'   html_document:
#'     number_sections: false
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
#' - Cargue los paquetes R necesarios para usar matrices
#' de covarianzas robustas a heteroscedasticidad y realizar
#' contrastes de hipótesis lineales sobre los parámetros de los
#' modelos de regresión.
## >>>>>>>>>>>>>>>>>>>>>



#' # Datos
#'
#' - Lea los datos del fichero "esp.csv" y guárdelos en la
#' variable `esp`.
## >>>>>>>>>>>>>>>>>>>>>



#' # Transformación de variables
#'
#' En esta sección se crean nuevas variables variables a
#' partir de las originales del PIAAC y se guardan en la
#' base de datos `esp`.
#'
#' - Cree la variable `lsalario`.
## >>>>>>>>>>>>>>>>>>>>>



#' - Cree la variable `asalariado`.
## >>>>>>>>>>>>>>>>>>>>>



#' - Cree la variable `mujer`.
## >>>>>>>>>>>>>>>>>>>>>



#' - Cree la variable `educ`.
## >>>>>>>>>>>>>>>>>>>>>



#' - Cree la variable `activ`.
## >>>>>>>>>>>>>>>>>>>>>



#' - Cree la variable `exper`.
## >>>>>>>>>>>>>>>>>>>>>



#' - Cree la variable `antig`.
## >>>>>>>>>>>>>>>>>>>>>



#' - Cree la variable `pub`.
## >>>>>>>>>>>>>>>>>>>>>



#' - Cree la variable `ong`.
## >>>>>>>>>>>>>>>>>>>>>



#' - Cree la variable `temp`.
## >>>>>>>>>>>>>>>>>>>>>



#' - Cree la variable `apr`.
## >>>>>>>>>>>>>>>>>>>>>



#' - Cree la variable `otro`.
## >>>>>>>>>>>>>>>>>>>>>



#' - Cree la variable `edad`.
## >>>>>>>>>>>>>>>>>>>>>



#' - Cree la variable `hijos`.
## >>>>>>>>>>>>>>>>>>>>>



#' - Cree la variable `inmigr`.
## >>>>>>>>>>>>>>>>>>>>>



#' - Cree la variable `lsalario`.
## >>>>>>>>>>>>>>>>>>>>>



#' # Ecuaciones de salarios
#'
#' - Cree una nueva base de datos que sólo contenga las
#' observaciones que corresponden a asalariados.
## >>>>>>>>>>>>>>>>>>>>>



#' ## Brecha salarial no ajustada
#'
#' - Estime el modelo $\mathrm{(Mod1)}$ por MCO y guarde
#' los resultados de la estimación en la variable `mod1`.
## >>>>>>>>>>>>>>>>>>>>>



#' - Presente los resultados de la estimación. Utilice estadísticos
#' robustos a heteroscedasticidad.
## >>>>>>>>>>>>>>>>>>>>>



#' - Interprete el parámetro $\beta_1$. Si existe discriminación
#' en contra de las mujeres, ¿qué signo tendrá $\beta_1$?
## >>>>>>>>>>>>>>>>>>>>>



#' - Contraste la existencia de una brecha salarial en
#' contra de la mujer.
## >>>>>>>>>>>>>>>>>>>>>



#' ## Brecha salarial ajustada
#'
#' - Estime el modelo $\mathrm{(Mod2)}$ por MCO y guarde
#' los resultados de la estimación en la variable `mod2`.
## >>>>>>>>>>>>>>>>>>>>>




#' - Presente los resultados de la estimación. Utilice estadísticos
#' robustos a heteroscedasticidad.
## >>>>>>>>>>>>>>>>>>>>>




#' - Contraste la significación conjunta de la regresión.
## >>>>>>>>>>>>>>>>>>>>>




#' - Discuta brevemente el signo y la magnitud de las estimaciones
#' de los parámetros y la significación de las variables explicativas.
## >>>>>>>>>>>>>>>>>>>>>


#' - ¿Hay diferencias entre la brecha salarial no ajustada
#' y la ajustada?
## >>>>>>>>>>>>>>>>>>>>>


#' - Obtenga un intervalo de confianza para la brecha salarial
#' ajustada.
## >>>>>>>>>>>>>>>>>>>>>




#' # Participación en el mercado laboral
#'
#' ## Modelo de probabilidad lineal simple
#'

#' - Estime el modelo $\mathrm{(Mod3)}$ por MCO y guarde
#' los resultados de la estimación en la variable `mod3`.
## >>>>>>>>>>>>>>>>>>>>>




#' - Presente los resultados de la estimación. Utilice estadísticos
#' robustos a heteroscedasticidad.
## >>>>>>>>>>>>>>>>>>>>>




#' - ¿Cuál es la interpretación del parámetro $\alpha_1$?
## >>>>>>>>>>>>>>>>>>>>>



#' - ¿Hay diferencias entre las tasas de actividad de hombres y
#' mujeres? ¿Son significativas esas diferencias?
## >>>>>>>>>>>>>>>>>>>>>



#' ## Modelo de probabilidad lineal múltiple
#'

#' - Estime el modelo $\mathrm{(Mod4)}$ por MCO y guarde
#' los resultados de la estimación en la variable `mod4`.
## >>>>>>>>>>>>>>>>>>>>>




#' - Presente los resultados de la estimación. Utilice estadísticos
#' robustos a heteroscedasticidad.
## >>>>>>>>>>>>>>>>>>>>>




#' - ¿Hay diferencias entre las estimaciones del  parámetro $\alpha_1$
#' en el modelo $\mathrm{(Mod3)}$ y en el modelo $\mathrm{(Mod4)}$?
## >>>>>>>>>>>>>>>>>>>>>



#' - Contraste la significación conjunta de la regresión.
## >>>>>>>>>>>>>>>>>>>>>



#' - Discuta brevemente el signo y la magnitud de las estimaciones
#' de los parámetros y la significación de las variables explicativas.
## >>>>>>>>>>>>>>>>>>>>>

