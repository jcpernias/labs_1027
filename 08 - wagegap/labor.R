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


library(lmtest)
library(sandwich)
library(car)

#' # Datos
#'
#' - Lea los datos del fichero "esp.csv" y guárdelos en la
#' variable `esp`.
## >>>>>>>>>>>>>>>>>>>>>

esp <- read.csv("esp.csv")

#' # Transformación de variables
#'
#' En esta sección se crean nuevas variables variables a
#' partir de las originales del PIAAC y se guardan en la
#' base de datos `esp`.
#'
#' - Cree la variable `lsalario`.
## >>>>>>>>>>>>>>>>>>>>>

esp$lsalario <- log(esp$EARN)

#' - Cree la variable `asalariado`.
## >>>>>>>>>>>>>>>>>>>>>

esp$asalariado <- !is.na(esp$EARNHR)

#' - Cree la variable `mujer`.
## >>>>>>>>>>>>>>>>>>>>>

esp$mujer <- as.integer(esp$GENDER_R == 2)

#' - Cree la variable `educ`.
## >>>>>>>>>>>>>>>>>>>>>

esp$educ <- esp$YRSQUAL

#' - Cree la variable `activ`.
## >>>>>>>>>>>>>>>>>>>>>

esp$activ <- as.integer(esp$C_D05 != 3)

#' - Cree la variable `exper`.
## >>>>>>>>>>>>>>>>>>>>>

esp$exper <- esp$C_Q09

#' - Cree la variable `antig`.
## >>>>>>>>>>>>>>>>>>>>>

esp$antig <- 2012 - esp$D_Q05a2

#' - Cree la variable `pub`.
## >>>>>>>>>>>>>>>>>>>>>

esp$pub <- as.integer(esp$D_Q03 == 2)

#' - Cree la variable `ong`.
## >>>>>>>>>>>>>>>>>>>>>

esp$ong <- as.integer(esp$D_Q03 == 3)

#' - Cree la variable `temp`.
## >>>>>>>>>>>>>>>>>>>>>

esp$temp <- as.integer(esp$D_Q09 %in% c(2, 3))

#' - Cree la variable `apr`.
## >>>>>>>>>>>>>>>>>>>>>

esp$apr <- as.integer(esp$D_Q09 == 4)

#' - Cree la variable `otro`.
## >>>>>>>>>>>>>>>>>>>>>

esp$otro <- as.integer(esp$D_Q09 %in% c(5, 6))

#' - Cree la variable `edad`.
## >>>>>>>>>>>>>>>>>>>>>

esp$edad <- esp$AGE_R

#' - Cree la variable `hijos`.
## >>>>>>>>>>>>>>>>>>>>>

esp$hijos <- as.integer(esp$J_Q03b > 0)

#' - Cree la variable `inmigr`.
## >>>>>>>>>>>>>>>>>>>>>

esp$inmigr <- as.integer(esp$J_Q04a == 2)

#' - Cree la variable `pareja`.
## >>>>>>>>>>>>>>>>>>>>>

esp$pareja <- as.integer(esp$J_Q02a == 1)

#' # Ecuaciones de salarios
#'
#' - Cree una nueva base de datos que sólo contenga las
#' observaciones que corresponden a asalariados.
## >>>>>>>>>>>>>>>>>>>>>

esp_asal <- subset(esp, asalariado)

#' ## Brecha salarial no ajustada
#'
#' - Estime el modelo $\mathrm{(Mod1)}$ por MCO y guarde
#' los resultados de la estimación en la variable `mod1`.
## >>>>>>>>>>>>>>>>>>>>>

mod1 <- lm(lsalario ~ mujer, data = esp_asal)

#' - Presente los resultados de la estimación. Utilice estadísticos
#' robustos a heteroscedasticidad.
## >>>>>>>>>>>>>>>>>>>>>

coeftest(mod1, vcov. = vcovHC)

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

mod2 <- update(mod1, . ~ . + educ + exper + antig +
                 pub + ong  +
                 temp + apr + otro + edad + hijos +
                 inmigr + pareja)

#' - Presente los resultados de la estimación. Utilice estadísticos
#' robustos a heteroscedasticidad.
## >>>>>>>>>>>>>>>>>>>>>

coeftest(mod2, vcov. = vcovHC)

#' - Contraste la significación conjunta de la regresión.
## >>>>>>>>>>>>>>>>>>>>>

bhat2 <- coef(mod2)
joint_h0 <- names(bhat2[-1])
lht(mod2, joint_h0, vcov. = vcovHC)

#' - Discuta brevemente el signo y la magnitud de las estimaciones
#' de los parámetros y la significación de las variables explicativas.
## >>>>>>>>>>>>>>>>>>>>>


#' - ¿Hay diferencias entre la brecha salarial no ajustada
#' y la ajustada?
## >>>>>>>>>>>>>>>>>>>>>


#' - Obtenga un intervalo de confianza para la brecha salarial
#' ajustada.
## >>>>>>>>>>>>>>>>>>>>>

coefci(mod2, parm = "mujer", vcov. = vcovHC)


#' # Participación en el mercado laboral
#'
#' ## Modelo de probabilidad lineal simple
#'

#' - Estime el modelo $\mathrm{(Mod3)}$ por MCO y guarde
#' los resultados de la estimación en la variable `mod3`.
## >>>>>>>>>>>>>>>>>>>>>

mod3 <- lm(activ ~ mujer, data = esp)

#' - Presente los resultados de la estimación. Utilice estadísticos
#' robustos a heteroscedasticidad.
## >>>>>>>>>>>>>>>>>>>>>

coeftest(mod3, vcov. = vcovHC)

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

mod4 <- lm(activ ~ mujer + educ + exper +
					 	edad + hijos + inmigr + pareja,
					 data = esp)

#' - Presente los resultados de la estimación. Utilice estadísticos
#' robustos a heteroscedasticidad.
## >>>>>>>>>>>>>>>>>>>>>

coeftest(mod4, vcov. = vcovHC)


#' - ¿Hay diferencias entre las estimaciones del  parámetro $\alpha_1$
#' en el modelo $\mathrm{(Mod3)}$ y en el modelo $\mathrm{(Mod4)}$?
## >>>>>>>>>>>>>>>>>>>>>

#' - Contraste la significación conjunta de la regresión.
## >>>>>>>>>>>>>>>>>>>>>

bhat4 <- coef(mod4)
joint_h0 <- names(bhat4[-1])
lht(mod4, joint_h0, vcov. = vcovHC)

#' - Discuta brevemente el signo y la magnitud de las estimaciones
#' de los parámetros y la significación de las variables explicativas.
## >>>>>>>>>>>>>>>>>>>>>
