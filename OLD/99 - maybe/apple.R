library(wooldridge)
library(sandwich)
library(lmtest)

data(apple)

table(apple$ecolbs > 0)

buys <- as.integer(apple$ecolbs > 0)

mod1 <- lm(buys ~ ecoprc, data = apple)
coeftest(mod1)

mod2 <- lm(buys ~ ecoprc + regprc, data = apple)
coeftest(mod2)

mod3 <- lm(buys ~ I(ecoprc - regprc), data = apple)
coeftest(mod3)

mod4 <- lm(buys ~ log(ecoprc/regprc) + log(faminc) + inseason + male, data = apple)
coeftest(mod4, vcovHC)
phat <- fitted(mod4)
range(phat)

h <- phat*(1 - phat)
mod5 <- update(mod4, weights = 1 / h)
coeftest(mod5, vcovHC)

apple_sim <- apple

apple_sim$regprc <- 0.89
apple_sim$ecoprc <- apple_sim$regprc * 1.0
p_sim0 <- predict(mod5, newdata = apple_sim)

apple_sim$ecoprc <- apple_sim$regprc * 1.10
p_sim1 <- predict(mod5, newdata = apple_sim)

mean(100 * (p_sim1 - p_sim0))
