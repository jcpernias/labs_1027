#' ---
#' author: "José Pernías Cerrillo"
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
library(lmtest)
library(sandwich)
library(car)

#' # Datos
#'
#' - Lea los datos del fichero "esp.csv" y guárdelos en la
#' variable `esp`.
esp <- read.csv("esp.csv")

#' # Transformación de variables
#'
#' En esta sección se crean nuevas variables variables a
#' partir de las originales del PIAAC y se guardan en la
#' base de datos `esp`.
#'
#' - Cree la variable `lsalario`.
esp$lsalario <- log(esp$EARN)

#' - Cree la variable `asalariado`.
esp$asalariado <- !is.na(esp$EARNHR)

#' - Cree la variable `mujer`.
esp$mujer <- as.integer(esp$GENDER_R == 2)

#' - Cree la variable `educ`.
esp$educ <- esp$YRSQUAL

#' - Cree la variable `activ`.
esp$activ <- as.integer(esp$C_D05 != 3)

#' - Cree la variable `exper`.
esp$exper <- esp$C_Q09

#' - Cree la variable `antig`.
esp$antig <- 2012 - esp$D_Q05a2

#' - Cree la variable `pub`.
esp$pub <- as.integer(esp$D_Q03 == 2)

#' - Cree la variable `ong`.
esp$ong <- as.integer(esp$D_Q03 == 3)

#' - Cree la variable `temp`.
esp$temp <- as.integer(esp$D_Q09 %in% c(2, 3))

#' - Cree la variable `apr`.
esp$apr <- as.integer(esp$D_Q09 == 4)

#' - Cree la variable `otro`.
esp$otro <- as.integer(esp$D_Q09 %in% c(5, 6))

#' - Cree la variable `edad`.
esp$edad <- esp$AGE_R

#' - Cree la variable `hijos`.
esp$hijos <- as.integer(esp$J_Q03b > 0)

#' - Cree la variable `inmigr`.
esp$inmigr <- as.integer(esp$J_Q04a == 2)

#' - Cree la variable `pareja`.
esp$pareja <- as.integer(esp$J_Q02a == 1)

#' # Ecuaciones de salarios
#'
#' - Cree una nueva base de datos que sólo contenga las
#' observaciones que corresponden a asalariados.
esp_asal <- subset(esp, asalariado)

#' ## Brecha salarial no ajustada
#'
#' - Estime el modelo $\mathrm{(Mod1)}$ por MCO y guarde
#' los resultados de la estimación en la variable `mod1`.
mod1 <- lm(lsalario ~ mujer, data = esp_asal)

#' - Presente los resultados de la estimación. Utilice estadísticos
#' robustos a heteroscedasticidad.
coeftest(mod1, vcov. = vcovHC)

#' - Interprete el parámetro $\beta_1$. Si existe discriminación
#' en contra de las mujeres, ¿qué signo tendrá $\beta_1$?
#'
#'   De acuerdo con el Modelo 1, la diferencia entre el salario
#'   de las mujeres y de los hombres es igual al $100 \beta_1$% del
#'   salario de los hombres. Hay discriminación salarial contra las
#'   mujeres si el signo de $\beta_1$ es negativo.
#'
#'
#' - Contraste la existencia de una brecha salarial en
#' contra de la mujer.
#'
#'   Las estimaciones del Modelo 1 muestra que el salario medio de
#'   las mujeres era un 13% inferior al salario de los hombres. La
#'   hipótesis de que no existe brecha salarial se puede formular como
#'   $H_0: \beta_1 = 0$. La hipótesis alternativa de que hay
#'   discriminación salarial en contra de las mujeres sería
#'   $H_1: \beta_1 < 0$. El estadístico $t$ para contrastar esta
#'   hipótesis es  $-6.14$, valor que cae dentro de la zona de
#'   rechazo para $\alpha = 5\%$.

#'
#' ## Brecha salarial ajustada
#'
#' - Estime el modelo $\mathrm{(Mod2)}$ por MCO y guarde
#' los resultados de la estimación en la variable `mod2`.
mod2 <- update(mod1, . ~ . + educ + exper + antig +
                 pub + ong  + temp + apr + otro +
                 edad + hijos + inmigr + pareja)

#' - Presente los resultados de la estimación. Utilice estadísticos
#' robustos a heteroscedasticidad.
coeftest(mod2, vcov. = vcovHC)

#' - Contraste la significación conjunta de la regresión.
bhat2 <- coef(mod2)
joint_h0 <- names(bhat2[-1])
lht(mod2, joint_h0, vcov. = vcovHC)
#'
#'   El estadístico $F$ robusto a heteroscedasticidad toma un valor
#'   muy elevado, $F = 107$, y el valor de probabilidad es prácticamente
#'   $0$, por lo que rechazamos la hipótesis de que todas las
#'   pendientes del Modelo 2 son iguales a $0$.
#'

#' - Discuta brevemente el signo y la magnitud de las estimaciones
#' de los parámetros y la significación de las variables explicativas.
#'
#'   El principal parámetro de la ecuación de salarios es $\beta_1$,
#'   la pendiente de la variable `mujer`. La estimación negativa y
#'   significativa muestra que, a igualdad de condiciones, el salario
#'   de las mujeres es un $16\%$ inferior al de los hombres.
#'
#'   La educación de los trabajadores también es un determinante
#'   significativo de los salario de los trabajadores. Las estimaciones
#'   del Modelo 2 muestran que cada año adicional de educación se
#'   traduce en un aumento del $6\%$ del salario medio.
#'
#'   Los años de experiencia laboral parecen tener un efecto pequeño
#'   sobre el salario medio (aumento de $0.2\%$ al año) y no significativo.
#'   Por otro lado, cada año de antigüedad en la empresa eleva el salario
#'   en casi un $1\%$ siendo significativo este efecto.
#'
#'   El tipo de empresa en la que se desarrolla la actividad laboral
#'   influye mucho sobre el salario. Nuestras estimaciones muestran
#'   que el salario de de las empresas públicas
#'   es significativamente mayor que el que pagan las empresas privadas
#'   (un $16.7\%$ mayor). Por el contrario, el salario en las empresas
#'   sin ánimo de lucro el salario es un $10.8\%$ inferior al de las
#'   empresas privadas. La precisión de la estimación del parámetro de
#'   `ong` no es grande y el efecto no es significativo para
#'   $\alpha = 5\%$.
#'
#'
#'


#' - ¿Hay diferencias entre la brecha salarial no ajustada
#' y la ajustada?
#'
#'   No hay grandes diferencias. La estimación de la  brecha
#'   salarial del Modelo 2 es un poco mayor que la que se obtuvo
#'   con el Modelo 1 ($3$ puntos porcentuales). En ambos casos,
#'   los resultados indican la existencia de una importante brecha
#'   salarial y las estimaciones son significativamente distintas de 0.
#'
#' - Obtenga un intervalo de confianza para la brecha salarial
#' ajustada.
coefci(mod2, parm = "mujer", vcov. = vcovHC)


#' # Participación en el mercado laboral
#'
#' ## Modelo de probabilidad lineal simple
#'

#' - Estime el modelo $\mathrm{(Mod3)}$ por MCO y guarde
#' los resultados de la estimación en la variable `mod3`.
mod3 <- lm(activ ~ mujer, data = esp)

#' - Presente los resultados de la estimación. Utilice estadísticos
#' robustos a heteroscedasticidad.
coeftest(mod3, vcov. = vcovHC)

#' - ¿Cuál es la interpretación del parámetro $\alpha_1$?
#'
#'   El parámetro $\alpha_1$ mide la diferencia de la probabilidades
#'   de estar activo en el mercado laboral de mujeres y hombres.
#'   Alternativamente, $100 \alpha_1$ es la diferencia en las tasas de
#'   actividad de mujeres y de hombres.


#' - ¿Hay diferencias entre las tasas de actividad de hombres y
#' mujeres? ¿Son significativas esas diferencias?
#'
#'   De acuerdo con las estimaciones del Modelo 3, la tasa de actividad
#'   de las mujeres es 11 puntos porcentuales menor que la de los hombres.
#'   y esta diferencia es significativa distinta de 0 a un nivel de
#'   significación del $5\%$.
#'
#' ## Modelo de probabilidad lineal múltiple
#'
#' - Estime el modelo $\mathrm{(Mod4)}$ por MCO y guarde
#' los resultados de la estimación en la variable `mod4`.
mod4 <- lm(activ ~ mujer + educ + exper +
					 	edad + hijos + inmigr + pareja,
					 data = esp)

#' - Presente los resultados de la estimación. Utilice estadísticos
#' robustos a heteroscedasticidad.
coeftest(mod4, vcov. = vcovHC)

#' - ¿Hay diferencias entre las estimaciones del  parámetro $\alpha_1$
#' en el modelo $\mathrm{(Mod3)}$ y en el modelo $\mathrm{(Mod4)}$?
#'
#'   Sí: una vez se toman en cuenta otras características de los
#'   trabajadores la diferencia en las probabilidades de hombres y
#'   mujeres de participar en el mercado de trabajo se reduce a
#'   6.6 puntos porcentuales. Aún así la diferencia sigue siendo
#'   significativa y se corresponde a una menor participaciópn de las
#'   mujeres en el mercado laboral, a igualdad de las características
#'   que se han incluido en la regresión.

#' - Contraste la significación conjunta de la regresión.
#'
bhat4 <- coef(mod4)
joint_h0 <- names(bhat4[-1])
lht(mod4, joint_h0, vcov. = vcovHC)
#'
#'   El estadístico $F$ robusto a heteroscedasticidad toma un valor
#'   muy elevado, $F = 109$, y el valor de probabilidad es prácticamente
#'   $0$, por lo que rechazamos la hipótesis de que todas las
#'   pendientes del Modelo 4 son iguales a $0$.
#'
#' - Discuta brevemente el signo y la magnitud de las estimaciones
#' de los parámetros y la significación de las variables explicativas.
## >>>>>>>>>>>>>>>>>>>>>
