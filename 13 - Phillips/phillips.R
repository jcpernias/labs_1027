library(readxl)
library(mosaic, quietly = TRUE, warn.conflicts = FALSE)
library(zoo)
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


bdmacro <- read_excel("BDMACRO.xlsx",
                      sheet = "SERIES INDIVIDUALES",
                      range = "JZ14:JZ72",
                      col_names = "unem")


bdmacro2 <- read_excel("BDMACRO.xlsx",
                        sheet = "SERIES INDIVIDUALES",
                        range = "O4:O72",
                        col_names = "p")

unem <- zooreg(bdmacro$unem, start = 1964, frequency = 1)
p <- zooreg(bdmacro2$p, start = 1954, frequency = 1)

st <- merge(unem, p)
rm(bdmacro, bdmacro2, unem, p)

autoplot(st$unem)
autoplot(st$p)
st$lp <- log(st$p)
st$infl <- 100 * diff(st$lp)

smpl <- window(st, start = 1964, end = 2019)

autoplot(smpl$infl)
autoplot(smpl$unem)


gf_point(infl ~ unem, data = smpl) +
  geom_segment(aes(xend = c(tail(unem, n = -1), NA),
                   yend = c(tail(infl, n = -1), NA),
                   color = Index)) +
  scale_color_gradientn(colours = rainbow(5))

mod1 <- dynlm(infl ~ unem, data = smpl)
summary(mod1)
uhat1 <- resid(mod1)
autoplot(uhat1)



coeftest(mod1, vcov. = vcovHAC)

mod2 <- dynlm(d(infl) ~ L(unem, 0:2), data = smpl)
summary(mod2)
uhat2 <- resid(mod2)
autoplot(uhat2)

coeftest(mod2, vcov. = vcovHAC)

mod3 <- dynlm(d(infl) ~ L(unem, 0:1), data = smpl)
summary(mod3)
uhat3 <- resid(mod3)
autoplot(uhat3)

coeftest(mod3, vcov. = vcovHAC)

mod4 <- dynlm(d(infl) ~ L(d(unem), 0:1) + L(unem, 2), data = smpl)
summary(mod4)
uhat4 <- resid(mod4)
autoplot(uhat4)

coeftest(mod4, vcov. = vcovHAC)


mod5 <- dynlm(d(infl) ~ L(d(unem), 0) + L(unem, 1), data = smpl)
summary(mod5)
uhat5 <- resid(mod5)
autoplot(uhat5)

coeftest(mod5, vcov. = vcovHAC)

