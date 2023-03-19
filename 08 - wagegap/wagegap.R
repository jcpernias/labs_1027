library(lmtest)
library(sandwich)
library(car)

esp <- read.csv("esp.csv")

esp$lsalario <- log(esp$EARN)

esp$asalariado <- as.integer(!is.na(esp$lsalario))

esp$desempl <- as.integer(esp$C_D05 == 2)

esp$activ <- as.integer(esp$C_D05 != 3)


esp$mujer <- as.integer(esp$GENDER_R == 2)

esp$educ <- esp$YRSQUAL

esp$exper <- esp$C_Q09

esp$antig <- 2012 - esp$D_Q05a2

esp$pub <- as.integer(esp$D_Q03 == 2)

esp$ong <- as.integer(esp$D_Q03 == 3)

esp$temp <- as.integer(esp$D_Q09 %in% c(2, 3))

esp$apr <- as.integer(esp$D_Q09 == 4)

esp$otro <- as.integer(esp$D_Q09 %in% c(5, 6))

esp$edad <- esp$AGE_R

esp$hijos <- as.integer(esp$J_Q03b > 0)

esp$inmigr <- as.integer(esp$J_Q04a == 2)

esp$pareja <- as.integer(esp$J_Q02a == 1)


mod1 <- lm(lsalario ~ mujer, data = esp, subset = asalariado == 1)
summary(mod1)

mod2 <- update(mod1, . ~ . + educ + exper + antig +
                 pub + ong  +
                 temp + apr + otro + edad + hijos +
                 inmigr + pareja)
coeftest(mod2, vcov. = vcovHC)
coefci(mod2, vcov. = vcovHC)


mod3 <- lm(activ ~ mujer, data = esp)
coeftest(mod3)


mod4 <- lm(activ ~ mujer + educ + exper +
					 	edad + hijos + inmigr + pareja,
					 data = esp)
coeftest(mod4)
coefci(mod4, vcov. = vcovHC)



