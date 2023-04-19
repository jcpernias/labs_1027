library(mosaic, warn.conflicts = FALSE)
library(zoo)
library(mFilter)
library(urca)
library(dynlm)
library(sandwich)
library(lmtest)


## BASE DE DATOS MACROECONÓMICOS DE ESPAÑA 1954 - 2022
## Fecha de actualización: 31 de Marzo de 2023
## Ministerio de Hacienda y Función Pública
## DIRECCIÓN GENERAL DE PRESPUESTOS
## Subdirección General de Análisis y Programación Económica

## https://www.sepg.pap.hacienda.gob.es/sitios/sepg/es-ES/Presupuestos/DocumentacionEstadisticas/D

## Hoja: SERIES INDIVIDUALES
## U: HL5:HL72 (1955-2022)
## u: JZ14:JZ72 (1964-2022)
## dPIB: N4:N72 (1954-2022)
## dCpr: O4:O72 (1954-2022)

library(readxl)
bdm_unem <- read_excel("BDMACRO.xlsx",
                   sheet = "SERIES INDIVIDUALES",
                   range = "JZ14:JZ72",
                   col_names = "unem")


bdm_p <- read_excel("BDMACRO.xlsx",
                    sheet = "SERIES INDIVIDUALES",
                    range = "O4:O72",
                    col_names = "p")

paro <- zooreg(bdm_unem$unem, start = 1964, frequency = 1)
defl <- zooreg(bdm_p$p, start = 1954, frequency = 1)

hp_paro <- hpfilter(paro, freq = 6.25)
paro_n <- zooreg(as.vector(hp_paro$trend), start = start(paro), frequency = 1)
year <- zooreg(1954:2022, start = 1954, frequency = 1)

st <- merge(periodo = year, defl, paro, paro_n) |>
  window(start = 1963)
rm(bdm_unem, bdm_p, hp_paro, defl, paro, paro_n)

write.csv(st, "phillips.csv", quote = FALSE, row.names = FALSE, na = "")




db <- read.csv("phillips.csv")

st <- zooreg(db, start = 1963, frequency = 1)

st$infl <- 100 * diff(log(st$defl))
smpl <- window(st, start = 1964, end = 2022)

autoplot(smpl$infl)
autoplot(smpl$paro)

mod1 <- dynlm(infl ~ paro, data = smpl, start = 1967)
summary(mod1)
uhat1 <- resid(mod1)
autoplot(uhat1)
coeftest(mod1, vcov. = vcovHAC)

acf(uhat1, plot = FALSE)
acf(uhat1)


mod2 <- dynlm(d(infl) ~ paro, data = smpl, start = 1967)
summary(mod2)
uhat2 <- resid(mod2)
autoplot(uhat2)
coeftest(mod2, vcov. = vcovHAC)

acf(uhat2, plot = FALSE)
acf(uhat2)



ur.df(smpl$paro, type = "drift", lags = 3, selectlags = "AIC") |>
  summary()

ur.df(smpl$infl, type = "drift", lags = 3, selectlags = "AIC") |>
  summary()

ur.df(diff(smpl$infl), type = "drift", lags = 3, selectlags = "AIC") |>
  summary()

smpl$paro_c <- smpl$paro - smpl$paro_n

ur.df(smpl$paro_c, type = "drift", lags = 3, selectlags = "AIC") |>
  summary()


for (i in 0:3) {
  dynlm(d(infl) ~ L(paro_c, 0:i) , data = smpl, start = 1967) |>
    BIC() |>
    print()
}

mod20 <- dynlm(d(infl) ~ L(paro_c, 0:1), data = smpl, start = 1967)
summary(mod20)
uhat20 <- resid(mod20)
autoplot(uhat20)
coeftest(mod20, vcov. = vcovHAC)


mod25 <- dynlm(d(infl) ~ L(d(paro_c), 0:0) + L(paro_c, 1), data = smpl, start = 1967)
summary(mod25)
uhat25 <- resid(mod25)
autoplot(uhat25)

coeftest(mod25, vcov. = vcovHAC)
acf(uhat25, plot=FALSE)
acf(uhat25)

mod35 <- dynlm(d(infl) ~ L(paro_c, 0:1) + L(d(infl)), data = smpl, start = 1967)
summary(mod35)
coeftest(mod35, vcov. = vcovHAC)


mod40 <- dynlm(d(infl) ~ d(paro_c), data = smpl, start = 1967)
summary(mod40)
uhat40 <- resid(mod40)
autoplot(uhat40)
coeftest(mod40, vcov. = vcovHAC)

mod40 <- dynlm(d(infl) ~ d(paro_c), data = smpl, start = 1967)
summary(mod40)
uhat40 <- resid(mod40)
autoplot(uhat40)
coeftest(mod40, vcov. = vcovHAC)
