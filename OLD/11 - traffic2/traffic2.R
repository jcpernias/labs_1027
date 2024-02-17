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

st$ltotal <- log(st$total)
st$pfatal <- st$fatal / st$total * 1000

st$t <- 1:nrow(st)

## May 87 -> spdlaw
## Jan 86 -> beltlaw
st$belt <- as.integer(time(st) >= "1986-01")
st$speed <- as.integer(time(st) >= "1987-05")

autoplot(st$ltotal)

ur.df(st$ltotal, type = "trend", lags = 6, selectlags = "AIC") |>
  summary()

autoplot(st$pfatal)
ur.df(st$pfatal, type = "trend", lags = 6, selectlags = "AIC") |>
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


h0 <- matchCoefs(mod1, "season")
lht(mod1, h0, vcov. = vcovHAC)

mod2 <- dynlm(pfatal ~ t + season(st) + weekends +
                + unem + speed + belt, data = st)
summary(mod2)
coeftest(mod2, vcov. = vcovHAC)

h0 <- matchCoefs(mod2, "season")
lht(mod2, h0, vcov. = vcovHAC)



X <- coredata(model.matrix(mod1))
Y <- model.response(model.frame(mod1)) |> coredata()
uhat <- coredata(resid(mod1))
Nobs <- nrow(X)
Yname <- mod1$terms[[2]] |> as.character()
Xnames <- variable.names(mod1)
frml <- paste(Yname,
              paste0(paste0("`", Xnames, "`", collapse = " + "), " + 0"),
              sep = " ~ ") |>
  as.formula()

rho_reg <- lm(uhat[-1] ~ uhat[-Nobs])
rho <- coef(rho_reg)[2]
Xd <- X[-1, ] - rho * X[-Nobs, ]
Yd <-  Y[-1] - rho * Y[-Nobs]
YXd <- cbind(Yd, Xd)
colnames(YXd) <- c(Yname, Xnames)
co_reg <- dynlm(frml, data = as.data.frame(YXd))

## co_reg <- lm(Yd ~ Xd + 0)
summary(co_reg)
uhat <- Y - X %*% coef(co_reg)


X <- coredata(model.matrix(mod1))
Y <- model.response(model.frame(mod1)) |> coredata()
uhat <- coredata(resid(mod1))
Nobs <- nrow(X)

rho_reg <- lm(uhat[-1] ~ uhat[-Nobs])
rho <- coef(rho_reg)[2]
Xd <- X[-1, ] - rho * X[-Nobs, ]
Yd <-  Y[-1] - rho * Y[-Nobs]
X1 <- sqrt(1 - rho^2) * X[1, ]
Y1 <- sqrt(1 - rho^2) * Y[1]

Xpw <- rbind(X1, Xd)
Ypw <- c(Y1, Yd)
pw_reg <- lm(Ypw ~ Xpw + 0)
summary(pw_reg)
uhat <- Y - X %*% coef(pw_reg)
