#' ---
#'  title: "Accidentes de tráfico y leyes de circulación (I)"
#'  author: "EC1027 --- Econometría I"
#'  date: "Curso 2022-2023"
#' ---

#+ setup, include=FALSE
knitr::opts_chunk$set(echo = TRUE, comment = "")

#+ packages, warning=FALSE, message=FALSE
library(readxl)
library(mosaic, warn.conflicts = FALSE)
library(zoo)
library(urca)
library(dynlm)
library(sandwich)
library(lmtest)

traffic2 <- read_excel("traffic2.xlsx") |>
  select(-c(spdlaw, beltlaw)) |>
  rename(total = totacc, fatal = fatacc, weekends = wkends)

st <- zooreg(traffic2, start = c(1981, 1), frequency = 12)
st_month <- cycle(st)
st_year <- format(time(st), "%Y") |> as.integer() |>
  zooreg(start = start(st), frequency = frequency(st))

st <- merge(year = st_year, month = st_month, st)

write.csv(st, "traffic2.csv", quote = FALSE, row.names = FALSE)


traffic2 <- read.csv("traffic2.csv")
st <- zooreg(traffic2, start = c(1981, 1), frequency = 12)

## May 87 -> spdlaw
## Jan 86 -> beltlaw
st$belt <- as.integer(time(st) >= "Jan 1986")
st$speed <- as.integer(time(st) >= "May 1987")

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

ur.df(diff(st$unem), type = "drift", lags = 6, selectlags = "AIC") |>
  summary()


mod1 <- dynlm(ltotal ~ trend(st) + season(st) + weekends + unem +
                belt + speed, data = st)
summary(mod1)
coeftest(mod1, vcov. = vcovHAC)

acf(resid(mod1))

mod2 <- dynlm(ltotal ~ trend(st) + season(st) + weekends + d(unem) +
                belt + speed, data = st)
summary(mod2)
coeftest(mod2, vcov. = vcovHAC)
acf(resid(mod2))


st$fr_fatal <- st$fatal / st$total * 1000
autoplot(st$fr_fatal)
ur.df(st$fr_fatal, type = "trend", lags = 6, selectlags = "AIC") |>
  summary()

mod3 <- dynlm(fr_fatal ~ trend(st) + season(st) + weekends +
                + unem + speed + belt, data = st)
summary(mod3)
coeftest(mod3, vcov. = vcovHAC)


