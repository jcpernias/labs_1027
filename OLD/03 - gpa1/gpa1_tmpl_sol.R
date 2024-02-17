#' ---
#' author: "José Pernías Cerrillo"
#' title: "El rendimiento académico de los universitarios"
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
#' Nuestro objetivo principal será cuantificar el efecto que la no
#' asistencia a las clases tiene sobre las notas medias de los
#' estudiantes universitarios. Estimaremos los parámetros del modelo:
#' $$ \mathit{colGPA} = \beta_0
#'     + \beta_1 \mathit{skipped}
#'     + \beta_2  \mathit{ACT}
#'     + \beta_3 \mathit{hsGPA}
#'     + \beta_4 \mathit{PC}
#'     + u
#' $$
#' donde:
#'
#' - $\mathit{colGPA}$: la nota media obtenida en la universidad.
#'
#' - $\mathit{skipped}$: número medio de clases a la semana a las que
#' el alumno no asiste.
#'
#' - $\mathit{ACT}$: puntuación obtenida en la prueba de acceso a la
#' universidad.
#'
#' - $\mathit{hsGPA}$: nota media en secundaria.
#'
#' - $\mathit{PC}$: variable ficticia que toma el valor 1 si el estudiante
#' disponía de un ordenador propio.
#'
#' ## Datos
#'
#' Utilizaremos la base de datos `gpa1` que se encuentra en el paquete
#' `wooldridge`. Los datos se recogieron en 1994 y proporcionan
#' información sobre el rendimiento académico y las condiciones
#' personales y familiares de 141 estudiantes de la Universidad
#' Estatal de Michigan.
#'
#' Cargue el paquete `wooldridge` y la base de datos `gpa1`.
library(wooldridge)
data(gpa1)

#' Calcule la media y la desviación típica de la variable `colGPA`.
#' Calcule también los valores mínimo y máximo que esta variable toma
#' en la muestra.
# Media
mean(gpa1$colGPA)

# Desviación típica
sd(gpa1$colGPA)

# Mínimo
min(gpa1$colGPA)

# Máximo
max(gpa1$colGPA)

# La función `range` devuelve un vector con el mínimo y el máximo
range(gpa1$colGPA)


#' Calcule la media y la desviación típica de la variable `skipped`.
#' Use la función `table` para examinar los valores que toma esta variable
#' y con qué frecuencia se observan.
# Media
mean(gpa1$skipped)

# Desviación típica
sd(gpa1$skipped)

# Tabla de valores
table(gpa1$skipped)

#' ## Otros paquetes
#'
#' Utilizaremos tres paquetes más:
#'
#' - `sandwich`: proporciona funciones para obtener errores típicos
#' robustos a diversos problemas de especificación. Utilizaremos la
#' función `vcovHC` para calcular errotes típicos robustos a
#' heteroscedasticidad.
#'
#' - `lmtest`: contiene las funciones `coeftest` y `coefci` con las que
#' podemos obtener los resultados de la regresión e intervalos de confianza
#' válidos en presencia heteroscedasticidad.
#'
#' - `car`: la función `lht` calcula contrastes de hipótesis
#' lineales sobre los parámetros y admite matrices de covarianzas
#' robustas a heteroscedasticidad.
#'
#' - `mosaic`: la función `gf_point` realiza gráficos de dispersión.
#'
#' Cargue los paquetes `sandwich`, `lmtest`, `car` y `mosaic`
#' (instálelos previamente si es necesario).
library(sandwich)
library(lmtest)
library(car)
# El argumento `warn.conflicts = FALSE` evita que aparezcan muchos
# mensajes de advertencia cuando se carga el paquete `mosaic`.
library(mosaic, warn.conflicts = FALSE)


#' # Resultados
#'
#' ## Estimación por MCO
#'
#' Estime por MCO los parámetros del modelo de regresión.
mod1 <- lm(colGPA ~ skipped + ACT + hsGPA + PC, data = gpa1)

#' Presente los resultados de la estimación con la función `summary`.
summary(mod1)

#' La función `summary` no permite usar errores típicos robustos a
#' la heteroscedasticidad. ¿Cuáles de los estadísticos que presenta
#' `summary` siguen siendo válidos en presencia de heteroscedasticidad?
## >>>>>>>>>>>>>>>>>>>>>

#' ## Detección de la heteroscedasticidad
#'
#' Una forma simple de detectar la presencia de heteroscedasticidad es
#' examinar un **gráfico de dispersión** de los residuos al cuadrado
#' frente a las predicciones.
#'
#' Use la función `fitted` para obtener las predicciones de la variable
#' dependiente. Guarde las predicciones en la variable `yhat1`.
yhat <- fitted(mod1)

#' Guarde en la variable `uhat1` los residuos de MCO usando la
#' función `resid`.
uhat1 <- resid(mod1)

#' Guarde en `uhat1_sq` el cuadrado de los residuos de MCO
uhat1_sq <- uhat1 ^ 2

#' Use `gf_point` para obtener el gráfico de los residuos al cuadrado y
#' las predicciones.
gf_point(uhat1_sq ~ yhat)

#' ¿Muestra el gráfico alguna evidencia de heteroscedasticidad?
## >>>>>>>>>>>>>>>>>>>>>

#' Usaremos también el **contraste de Breusch-Pagan** para determinar
#' la existencia de heteroscedasticidad.
#'
#' Guarde en `aux_bp` la regresión auxiliar de `uhat1_sq` sobre las
#' variables explicativas
aux_bp <- update(mod1, uhat1_sq ~ .)

# Otra forma equivalente:
#   aux_bp <- lm(uhat1_sq ~ skipped + ACT + hsGPA + PC, data = gpa1)

#' Use `summary` para presentar las estimaciones de la regresión auxiliar.
summary(aux_bp)

#' ¿Cuál es el resultado del contraste de Breusch-Pagan? ¿Se rechaza la
#' hipótesis nula al 5%? ¿Hay heteroscedasticidad?
## >>>>>>>>>>>>>>>>>>>>>

#' ## Errores típicos robustos
#'
#' La función `vcovHC` calcula matrices de covarianzas de los estimadores
#' robustas a heteroscedasticidad. Para obtener la tabla de la regresión
#' usando errores típicos robustos usamos la función `coeftest`. En el
#' primer argumento especificamos el modelo estimado con MCO y en el
#' segundo argumento especificamos la función para obtener las varianzas
#' robustas de los estimadores.
coeftest(mod1, vcov. = vcovHC)

#' La función `coefci` calcula intervalos de confianza con errores
#' típicos robustos a heteroscedasticidad. Se usa con los mismos
#' argumentos que `coeftest`.
coefci(mod1, vcov. = vcovHC)

#' Utilizando errores típicos robustos a heteroscedasticidad,
#' determine la significación de las variables explicativas para
#' $\alpha = 5\%$. ¿Tienen las estimaciones de los parámetros los
#' signos esperados?
#'
#' ## Contrastes de hipótesis e intervalos de confianza
#'
#' La función `lht` se puede usar para contrastar hipótesis lineales
#' sobre los parámetros del modelo aun en presencia de heteroscedasticidad.
#' Para utilizar contrastes de Wald con una matriz de covarianzas robusta
#' utilizamos el argumento `vcov.` de la misma forma que en las funciones
#' `coeftest` y `coefci`. En esta sesión calcularemos un contraste $F$ de
#' significación conjunta robusto a hetroscedasticidad.

#' Cree un vector con los nombres (entre comillas) de todas las variables
#' explicativas y guárdelo en la variable `h0_F`.
h0_F <- c("skipped", "ACT", "hsGPA", "PC")

#' Calcule el contraste con la función `lht`. El primer argumento contiene
#' las estimaciones de MCO, el segundo la hipótesis nula y el tercero la
#' matriz de covarianzas robusta (`vcov. = vcovHC`).
lht(mod1, h0_F, vcov. = vcovHC)

#' De acuerdo con este contraste y para $\alpha = 5\%$, ¿son conjuntamente
#' significativas los regresores?
## >>>>>>>>>>>>>>>>>>>>>

#' # Conclusión
#'
#' De acuerdo con los resultados anteriores, discuta cuál es el
#' efecto que la no asistencia a las clases tiene sobre las notas
#' medias de los estudiantes universitarios.
## >>>>>>>>>>>>>>>>>>>>>

