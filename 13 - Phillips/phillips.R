library(readxl)
library(mosaic, warn.conflicts = FALSE)
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
                      skip = 2)

bdm_unem <- read_excel("BDMACRO.xlsx",
                   sheet = "SERIES INDIVIDUALES",
                   range = "JZ14:JZ72",
                   col_names = "unem")


bdm_p <- read_excel("BDMACRO.xlsx",
                    sheet = "SERIES INDIVIDUALES",
                    range = "O4:O72",
                    col_names = "p")

unem <- zooreg(bdm_unem$unem, start = 1964, frequency = 1)
p <- zooreg(bdm_p$p, start = 1954, frequency = 1)

st <- merge(unem, p)
rm(bdm_unem, bdm_p, unem, p)

autoplot(st$unem)
autoplot(st$p)
st$lp <- log(st$p)
st$infl <- 100 * diff(st$lp)

smpl <- window(st, start = 1964, end = 2022)

autoplot(smpl$infl)
autoplot(smpl$unem)

smpl2 <- merge(smpl,
               infl_end = stats::lag(smpl$infl),
               unem_end = stats::lag(smpl$unem))

gf_segment(infl +  infl_end ~ unem + unem_end,
           data = window(smpl2, start = 1967, end = 2021),
           color = ~ Index) |>
  gf_refine(scale_color_gradientn(colours = hcl.colors(5, "Dark 3"))) |>
  gf_point(infl ~ unem) +
  gf_theme(theme_bw())



mod1 <- dynlm(infl ~ unem, data = smpl, start = 1967)
summary(mod1)
uhat1 <- resid(mod1)
autoplot(uhat1)
coeftest(mod1, vcov. = vcovHAC)


library(urca)
unem_df <- ur.df(smpl$unem, type = "drift", lags = 3, selectlags = "AIC")
summary(unem_df)

infl_df <- ur.df(smpl$infl, type = "drift", lags = 3, selectlags = "AIC")
summary(infl_df)

infl_df <- ur.df(diff(smpl$infl), type = "drift", lags = 3, selectlags = "AIC")
summary(infl_df)

library(mFilter)
unem_tc <- hpfilter(smpl$unem, freq = 10)
smpl$unem_gap <- zooreg(unem_tc$cycle, start = start(smpl), frequency = 1)

autoplot(smpl$unem_gap)
autoplot(smpl$unem - smpl$unem_gap)

unem_df <- ur.df(smpl$unem_gap, type = "drift", lags = 3, selectlags = "AIC")
summary(unem_df)


for (i in 0:3) {
  print(BIC(dynlm(d(infl) ~ L(unem_gap, 0:i) , data = smpl, start = 1967)))
}

mod20 <- dynlm(d(infl) ~ L(unem_gap, 0:1), data = smpl)
summary(mod20)
uhat20 <- resid(mod20)
autoplot(uhat20)
coeftest(mod20, vcov. = vcovHAC)


mod25 <- dynlm(d(infl) ~ L(d(unem_gap), 0:0) + L(unem_gap, 1), data = smpl, start = 1967)
summary(mod25)
uhat25 <- resid(mod25)
autoplot(uhat25)

coeftest(mod25, vcov. = vcovHAC)

