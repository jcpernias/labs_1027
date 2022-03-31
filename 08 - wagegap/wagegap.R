library(lmtest)
library(sandwich)
library(car)


esp <- read.csv("esp.csv.gz")

db <- data.frame(lwage = log(esp$EARNHR))

db$educ <- esp$YRSQUAL
db$exper <- esp$C_Q09
db$age <- esp$AGE_R
db$female <- as.integer(esp$GENDER_R == 2)
db$public <- as.integer(esp$D_Q03 == 2)
db$ngo <- as.integer(esp$D_Q03 == 3)
db$temp <- as.integer(esp$D_Q09 %in% c(2, 3))
db$appr <- as.integer(esp$D_Q09 == 4)
db$other <- as.integer(esp$D_Q09 %in% c(5, 6))
db$children <- as.integer(esp$J_Q03a == 1)
db$native <- as.integer(esp$J_Q04a == 1)
db$partner <- as.integer(esp$J_Q02a == 1)

db_emp <- na.omit(db)
mod1 <- lm(lwage ~ female, data = db_emp)
summary(mod1)

mod2 <- update(mod1, . ~ . + educ + exper + public + ngo  +
							 	temp + appr + other + age + children + native + partner)
summary(mod2)
coeftest(mod2, vcov. = vcovHC)


uhat <- resid(mod2)
uhat_sq <- uhat^2
yhat <- fitted(mod2)
yhat_sq <- yhat^2
yhat_cb <- yhat^3

het_aux <- lm(uhat_sq ~ yhat + yhat_sq)
summary(het_aux)

reset <- update(mod2, . ~ . + yhat_sq + yhat_cb)
summary(reset)
lht(reset, matchCoefs(reset, "yhat_"), vcov. = vcovHC)

confint(coeftest(mod2, vcov. = vcovHC, "constant"))

mod3 <- update(mod2, . ~ . + female * partner)
coeftest(mod3, vcov. = vcovHC)
lht(mod3, "partner + female:partner", vcov. = vcovHC)
