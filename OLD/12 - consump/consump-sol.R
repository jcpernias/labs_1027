#' ---
#' title: "Función de consumo agregado"
#' author: "EC1027 --- Econometría I"
#' date: "Curso 2021--2022"
#' ---
#'
#+ setup, include=FALSE
knitr::opts_chunk$set(collapse = FALSE, comment = "")
#'
#+ packages, message=FALSE, warning=FALSE
library(readxl)
library(zoo)
library(dynlm)
library(ggplot2)
library(car)
library(sandwich)
library(lmtest)

#' # Apartado 1
xlsx <- read_xlsx("consump.xlsx")
db <- zooreg(xlsx, start = 1959, frequency = 1)
db$lc <- log(db$cons)
db$ly <- log(db$ydisp)

autoplot(db$lc)
autoplot(db$ly)

mod1 <- dynlm(lc ~ ly, data = db)
summary(mod1)


#' # Apartado 2
#'
#' Raíces unitarias: log del consumo
ur <- dynlm(lc ~ L(lc) + trend(db), data = db)
summary(ur)

#' Raíces unitarias: log de la renta disponible
ur <- dynlm(ly ~ L(ly) + trend(db), data = db)
summary(ur)

#' Contraste de Breusch-Godfrey de autocorrelación de orden 1
uhat1 <- resid(mod1)
bgaux <- update(mod1, uhat1 ~ . + L(uhat1))
summary(bgaux)

#' # Apartado 3
#'
db$dlc <- diff(db$lc)
db$dly <- diff(db$ly)

autoplot(db$dlc)
autoplot(db$dly)

mod2 <- dynlm(dlc ~ dly, data = db)
summary(mod2)

#' # Apartado 4
#'

#' Contraste de Breusch-Godfrey de autocorrelación de orden 1
uhat2 <- resid(mod2)
bgaux <- update(mod2, uhat2 ~ . + L(uhat2))
summary(bgaux)

mod3 <- dynlm(dlc ~ L(dlc) + dly + L(dly), data = db)
summary(mod3)

lht(mod3, matchCoefs(mod3, "L"))


