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
library(zoo)
library(mFilter)

bdm_unem <- read_excel("14 - phillips/BDMACRO.xlsx",
                       sheet = "SERIES INDIVIDUALES",
                       range = "JZ14:JZ72",
                       col_names = "unem")


bdm_p <- read_excel("14 - phillips/BDMACRO.xlsx",
                    sheet = "SERIES INDIVIDUALES",
                    range = "O4:O72",
                    col_names = "p")

paro <- zooreg(bdm_unem$unem, start = 1964, frequency = 1)
defl <- zooreg(bdm_p$p, start = 1954, frequency = 1)

hp_paro <- hpfilter(paro, freq = 6.25)
paro_n <- zooreg(as.vector(hp_paro$trend), start = start(paro), frequency = 1)

st <- merge(defl, paro, paro_n) |>
  window(start = 1962)

write.zoo(st, file = "14 - phillips/phillips.csv", index.name = "periodo", row.names = FALSE,
          sep = ";", dec = ",", quote = FALSE, na = "")

