rm(list = ls())

install.packages('urca')
library(urca)

data(npext)
head(npext)

y <- ts(na.omit(npext$unemploy), start = 1890, end = 1988, frequency = 1)

op <- par(no.readonly = TRUE)
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
par(mar = c(1, 1, 1, 1))
plot(y, ylab = 'unemployment rate (logarithm)')
acf(y, main = 'Autocorrelations', ylab = '', ylim = c(-1, 1))
pacf(y, main = 'Partial Autocorrelations', ylab = '', ylim = c(-1, 1))
par(op)


## tentative ARMA(2, 0)
arma20 <- arima(y, order = c(2, 0, 0))
ll20 <- logLik(arma20) # log likelihood
aic20 <- arma20$aic
res20 <- residuals(arma20)
Box.test(res20, lag = 20, type = 'Ljung-Box') # auto correlation 1~20까지 모두가 동시에 0이라는 것을 test
# 귀무가설 기각 불가. -> AR(2)에서 나온 잔차가 lag 20까지 주더라도 거의 안나타난다.
# AR(2)가 굉장히 실업률 데이터를 잘 키핑하고있다.
shapiro.test(res20) # normality test -> 귀무가설 기각 불가 -> 정규성 만족.

## ARMA(1, 1)
arma11 <- arima(y, order = c(1, 0, 1))
ll11 <- logLik(arma11)
aic11 <- arma11$aic
tsdiag(arma11)
res11 <- residuals(arma11)
Box.test(res11, lag = 20, type = 'Ljung-Box')
shapiro.test(res11)
tsdiag(arma11)



## using auto.arima()
install.packages('forecast')
library(forecast)
auto.arima(y, max.p = 3, max.q = 3, start.p = 1, start.q = 1, ic = 'aic')
# p, q를 1~3까지 aic를 체크하여 가장 적합한 모형을 찾아내는 것.
# 여기서는 arma(1,1)를 찾아낸다.


# forecasts
arma11.pred <- predict(arma11, n.ahead = 10)
arma11.pred$pred # value of prediction (예측치) -> point forecast
arma11.pred$se # error of the value of prediction (예측 오차)
predict <- ts(c(rep(NA, length(y) - 1), y[length(y)], 
                arma11.pred$pred), start = 1890, frequency = 1)
upper <- ts(c(rep(NA, length(y) - 1), y[length(y)],
              arma11.pred$pred + 2 * arma11.pred$se), # 2는 95% 신뢰구간(1.96) 대신함.
            start = 1890, frequency = 1)
lower <- ts(c(rep(NA, length(y) - 1), y[length(y)],
              arma11.pred$pred - 2 * arma11.pred$se),
            start = 1890, frequency = 1)
observed <- ts(c(y, rep(NA, 10)), start = 1890, frequency = 1)


plot(observed, type = 'l', ylab = '',
    
     'Actual and predicted values', xlab = '')
lines(predict, col = 'blue', lty = 2)
lines(lower, col = 'red', lty = 5)
lines(upper, col = 'red', lty = 5)
abline(v = 1988, col = 'gray', lty = 3)


