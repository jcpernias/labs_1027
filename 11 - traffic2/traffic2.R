#' ---
#'  title: "Accidentes de tráfico y leyes de circulación (I)"
#'  author: "EC1027 --- Econometría I"
#'  date: "Curso 2022-2023"
#' ---

#+ setup, include=FALSE
knitr::opts_chunk$set(echo = TRUE, comment = "")

#+ packages, warning=FALSE, message=FALSE
library(mosaic, warn.conflicts = FALSE)
library(zoo)
library(dynlm)
library(urca)
library(sandwich)
library(lmtest)
library(car)


traffic2 <- read.csv("traffic2.csv")
st <- zooreg(traffic2, start = c(1981, 1), frequency = 12)

st$t <- 1:nrow(st)

## May 87 -> spdlaw
## Jan 86 -> beltlaw
st$belt <- as.integer(time(st) >= "1986-01")
st$speed <- as.integer(time(st) >= "1987-05")

st$ltotal <- log(st$total)
autoplot(st$ltotal)

ur.df(st$ltotal, type = "trend", lags = 6, selectlags = "AIC") |>
  summary()

autoplot(st$weekends)
ur.df(st$weekends, type = "drift", lags = 6, selectlags = "AIC") |>
  summary()

autoplot(st$unem)
ur.df(st$unem, type = "trend", lags = 6, selectlags = "AIC") |>
  summary()

autoplot(st$unem)
ur.df(st$unem, type = "drift", lags = 6, selectlags = "AIC") |>
  summary()


mod1 <- dynlm(ltotal ~ t + season(st) + weekends + unem +
                belt + speed, data = st)
summary(mod1)
coeftest(mod1, vcov. = vcovHAC)
acf(resid(mod1))

h0 <- matchCoefs(mod1, "season")
lht(mod1, h0, vcov. = vcovHAC)

st$fr_fatal <- st$fatal / st$total * 1000
autoplot(st$fr_fatal)
ur.df(st$fr_fatal, type = "trend", lags = 6, selectlags = "AIC") |>
  summary()

mod2 <- dynlm(fr_fatal ~ t + season(st) + weekends +
                + unem + speed + belt, data = st)
summary(mod2)
coeftest(mod2, vcov. = vcovHAC)

h0 <- matchCoefs(mod2, "season")
lht(mod2, h0, vcov. = vcovHAC)


