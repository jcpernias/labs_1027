library(mosaic, warn.conflicts = FALSE)
library(zoo)
library(urca)
library(dynlm)
library(sandwich)
library(lmtest)

okun <- read.csv("okun.csv")

st <- zooreg(okun, frequency = 4, start = c(1980, 2))

st$u_gap <- st$u - st$u_hp
st$ly_gap <- 100 * (st$ly - st$ly_hp)



autoplot(st$ly)
df <- ur.df(st$ly, type = "trend", lags = 6, selectlags = "AIC")
summary(df)

autoplot(st$gy)
df <- ur.df(diff(st$ly), type = "drift", lags = 6, selectlags = "AIC")
summary(df)

autoplot(st$u)
df <- ur.df(st$u, type = "drift", lags = 6, selectlags = "AIC")
summary(df)

okun1 <- dynlm(u ~ ly, data = st, start = "1982-1")
coeftest(okun1, vcov. = vcovHAC)

okun2 <- dynlm(d(u) ~ gy, data = st, start = "1982-1")
coeftest(okun2, vcov. = vcovHAC)


autoplot(st$u_gap)
df <- ur.df(st$u_gap, type = "drift", lags = 6, selectlags = "AIC")
summary(df)

autoplot(st$ly_gap)
df <- ur.df(st$ly_gap, type = "drift", lags = 6, selectlags = "AIC")
summary(df)


okun2 <- dynlm(u_gap ~ ly_gap, data = st, start = "1982-1")
summary(okun2)

for (i in 0:6) {
  okuni <- dynlm(u_gap ~ L(ly_gap, 0:i), data = st, start = "1982-1")
  print(BIC(okuni))
}

okun3 <- dynlm(u_gap ~ L(ly_gap, 0:1), data = st, start = "1982-1")
summary(okun3)
coeftest(okun3, vcov. = vcovHAC)

okun4 <- dynlm(u_gap ~ d(ly_gap) + L(ly_gap), data = st, start = "1982-1")
summary(okun4)
coeftest(okun4, vcov. = vcovHAC)


for (i in 0:6) {
  okuni <- dynlm(d(u) ~ L(gy, 0:i), data = st, start = "1982-1")
  print(BIC(okuni))
}


okun5 <- dynlm(d(u) ~ L(gy, 0:1), data = st, start = "1982-1")
summary(okun5)
coeftest(okun5, vcov. = vcovHAC)

okun6 <- dynlm(d(u) ~ L(d(gy), 0:1) + L(gy, 2), data = st, start = "1982-1")
summary(okun6)
coeftest(okun6, vcov. = vcovHAC)

