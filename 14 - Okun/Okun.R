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

smpl1 <- window(ts, end = "2022Q4")

smpl1$ly <- log(smpl1$y)

autoplot(smpl1$ly)

autoplot(smpl1$u)
unem_df <- ur.df(smpl1$u, type = "drift", lags = 6, selectlags = "AIC")
summary(unem_df)

autoplot(smpl1$ly)
unem_df <- ur.df(smpl1$ly, type = "trend", lags = 6, selectlags = "AIC")
summary(unem_df)


okun1 <- dynlm(u ~ ly, data = smpl1, start = "1982-1")
summary(okun1)
autoplot(resid(okun1))

library(mFilter)
hp_u <- hpfilter(smpl1$u, freq = 1600)
hp_ly <- hpfilter(smpl1$ly, freq = 1600)
smpl1$u_gap <- zooreg(hp_u$cycle, start = start(smpl1), frequency = 4)
smpl1$ly_gap <- zooreg(100 * hp_ly$cycle, start = start(smpl1), frequency = 4)

library(urca)

smpl1$covid <- as.integer(time(smpl1) == "2020-2")

smpl <- window(smpl1, end = "2021Q4")

autoplot(smpl$u_gap)
unem_df <- ur.df(smpl$u_gap, type = "drift", lags = 6, selectlags = "AIC")
summary(unem_df)

autoplot(smpl$ly_gap)
ly_df <- ur.df(smpl$ly_gap, type = "drift", lags = 6, selectlags = "AIC")
summary(ly_df)


autoplot(smpl$ly_gap)
autoplot(smpl$u_gap)

okun2 <- dynlm(u_gap ~ ly_gap, data = smpl, start = "1982-1")
summary(okun2)

for (i in 0:6) {
  okuni <- dynlm(u_gap ~ L(ly_gap, 0:i), data = smpl, start = "1982-1")
  print(BIC(okuni))
}

okun3 <- dynlm(u_gap ~ L(ly_gap, 0:2), data = smpl, start = "1982-1")
summary(okun3)
coeftest(okun3, vcov. = vcovHAC)

okun4 <- dynlm(u_gap ~ L(d(ly_gap), 0:1) + L(ly_gap, 2), data = smpl, start = "1982-1")
summary(okun4)
coeftest(okun4, vcov. = vcovHAC)

okun5 <- dynlm(u_gap ~ L(d(ly_gap), 0:1) + L(ly_gap, 2) + covid, data = smpl, start = "1982-1")
summary(okun5)
coeftest(okun5, vcov. = vcovHAC)

acf(resid(okun5))
