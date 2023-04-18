library(readxl)
library(mosaic, warn.conflicts = FALSE)
library(zoo)
library(dynlm)
library(sandwich)
library(lmtest)

bdrems_y <- read_excel("BDREMS.xlsx",
                       sheet = "BDREMS",
                       range = "C4:C170",
                       col_names = "Y")

bdrems_u <- read_excel("BDREMS.xlsx",
                       sheet = "BDREMS",
                       range = "AL4:AL170",
                       col_names = "U")
y_ts <- zooreg(bdrems_y$Y, frequency = 4, start = c(1980, 1))
u_ts <- zooreg(bdrems_u$U, frequency = 4, start = c(1980, 1))
ts <- merge(y = y_ts, u = u_ts)

smpl <- window(ts, end = "2021Q4")

smpl$ly <- log(smpl$y)

autoplot(smpl$ly)

okun1 <- dynlm(u ~ ly, data = smpl, start = c(1982, 1))
summary(okun1)
autoplot(resid(okun1))

library(mFilter)
hp_u <- hpfilter(smpl$u, freq = 1600)
hp_ly <- hpfilter(smpl$ly, freq = 1600)
smpl$u_gap <- zooreg(hp_u$cycle, start = start(smpl), frequency = 4)
smpl$ly_gap <- zooreg(100 * hp_ly$cycle, start = start(smpl), frequency = 4)

library(urca)
unem_df <- ur.df(smpl$u_gap, type = "drift", lags = 6, selectlags = "AIC")
summary(unem_df)

ly_df <- ur.df(smpl$ly_gap, type = "drift", lags = 6, selectlags = "AIC")
summary(ly_df)

smpl$covid <- as.integer(time(ts) == "2020 Q2")

autoplot(smpl$ly_gap)
autoplot(smpl$u_gap)

okun2 <- dynlm(u_gap ~ ly_gap + covid, data = smpl, start = c(1982, 1))
summary(okun2)

for (i in 0:6) {
  okuni <- dynlm(u_gap ~ L(ly_gap, 0:i), data = smpl, start = c(1982, 1))
  print(BIC(okuni))
}

okun3 <- dynlm(u_gap ~ L(ly_gap, 0:2), data = smpl, start = c(1982, 1))
summary(okun3)
coeftest(okun3, vcov. = vcovHAC)

okun4 <- dynlm(u_gap ~ L(d(ly_gap), 0:1) + L(ly_gap, 2), data = smpl, start = c(1982, 1))
summary(okun4)
coeftest(okun4, vcov. = vcovHAC)

okun3b <- dynlm(u_gap ~ L(ly_gap, 0:3), data = smpl, start = c(1982, 1))
summary(okun3b)
coeftest(okun3b, vcov. = vcovHAC)

okun4b <- dynlm(u_gap ~ L(d(ly_gap), 0:2) + L(ly_gap, 3), data = smpl, start = c(1982, 1))
summary(okun4b)
coeftest(okun4b, vcov. = vcovHAC)


for (i in 1:6) {
  okuni <- dynlm(u_gap ~ L(u_gap, 1:i) + ly_gap + L(ly_gap, 1:i), data = smpl, start = c(1982, 1))
  print(BIC(okuni))
}

okun5 <- dynlm(u_gap ~ L(u_gap, 1:2) + ly_gap + L(ly_gap, 1:2) + covid, data = smpl, start = c(1982, 1))
summary(okun5)
coeftest(okun5, vcov. = vcovHAC)


