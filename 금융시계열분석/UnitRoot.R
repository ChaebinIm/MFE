# 7주차 강의 뒷부분..!

## Unit root test
# Augmented Dickey Fuller test
# UK 1966:Q4 ~ 1991:Q2
# seasonally adjusted real consumption expenditure in 1985 prices

rm(list = ls())
library(urca)
data(Raotbl3) # 영국의 소비자료!
attach(Raotbl3)
lc <- ts(lc, start = c(1966, 4), end = c(1991, 2), frequency = 4)
plot.ts(lc, lty = 1, ylab = '', main = 'UK real consumption expenditure')


# Regression with a constant + a trend
lc.ct <- ur.df(lc, lags = 3, type = 'trend') # trend included
# ur.df :: unit root test할때 쓰는 함수!
# lag : augmented dickey fuller test with trend term
summary(lc.ct)
# tau3 : z.lag.1의 estimator가 0인지 아닌지
plot(lc.ct)


# Regression with a constant
lc.co <- ur.df(lc, lags = 3, type = 'drift') # only constant included
# 1-(b) or 2-(b) form
summary(lc.co)
plot(lc.co)


# differencing series => stationary?? OOOO
lc2 <- diff(lc)
lc2.ct <- ur.df(lc2, type = 'trend', lags = 3)
summary(lc2.ct)


# KPSS test
library(urca)
ir <- na.omit(nporg[, 'bnd']) # 이자율
wg <- log(na.omit(nporg[, 'wg.n'])) # 로그임금

plot.ts(ir, lty=1, main='interest rate', ylab='', xlab='')
plot.ts(wg, lty=1, main='log of wage', ylab='', xlab='')

# intercept only
ir.kpss <- ur.kpss(ir, type = 'mu', use.lag = 8)

# linear trend
wg.kpss <- ur.kpss(wg, type = 'tau', use.lag = 8) #trend-stationary

summary(ir.kpss)
summary(wg.kpss)



# 두 개의 서로 독립인 random walk processes

y1 <- cumsum(rnorm(500))
y2 <- cumsum(rnorm(500))

c1 <- max(c(y1, y2))
c2 <- min(c(y1, y2))
plot.ts(y1, lty = 1, main = 'independent RWs', ylab = '', xlab = '', ylim = c(c1, c2))
lines(y2, lty = 2, col = 'red')

y3 <- 0.9*y1 + rnorm(500)
y4 <- 0.8*y1 + rnorm(500)
plot.ts(y3[1:100], lty = 1, main = 'cointegrated two RWs', ylab = '', xlab = '')
lines(y4[1:100], lty = 2, col = 'red')













