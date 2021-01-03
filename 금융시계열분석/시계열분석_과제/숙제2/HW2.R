install.packages("car", dependencies = T)
install.packages('lmtest')
library(lmtest)
library(car)
# ---------------------------- Problem 4 ----------------------------------
# (1)
# data generation
y1 <- rnorm(500)
y2 <- arima.sim(n = 500, model = list(ma = c(-0.6, 0.4, -0.5, 0.3)))
y3 <- arima.sim(n = 500, model = list(ar = c(0.3, 0.4)))
# draw ACF, PACF
# y1
op <- par(no.readonly = TRUE)
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE)) # 2 by 2 graph. 1행에 1개 2행에 3개
par("mar")
par(mar=c(1,1,1,1))
plot.ts(y1, ylab='simulated y series')
acf(y1, main = 'Autocorrelations', ylab = '', ylim = c(-1,1), ci.col = 'red') # ci = maybe confidence interval.
pacf(y1, main = 'Partial Autocorrelations', ylab = '', ylim = c(-1, 1), ci.col = 'black')
# y2
plot.ts(y2, ylab='simulated y series')
acf(y2, main = 'Autocorrelations', ylab = '', ylim = c(-1,1), ci.col = 'red') # ci = maybe confidence interval.
pacf(y2, main = 'Partial Autocorrelations', ylab = '', ylim = c(-1, 1), ci.col = 'black')
# y3
plot.ts(y3, ylab='simulated y series')
acf(y3, main = 'Autocorrelations', ylab = '', ylim = c(-1,1), ci.col = 'red') # ci = maybe confidence interval.
pacf(y3, main = 'Partial Autocorrelations', ylab = '', ylim = c(-1, 1), ci.col = 'black')



# (2) : estimate MA(4) with y2

arma04 <- arima(y2, order = c(0, 0, 4))
ll04 <- logLik(arma04) # log likelihood
aic04 <- arma04$aic
res04 <- residuals(arma04)
Box.test(res04, lag = 4, type = 'Ljung-Box') # auto correlation 1~4까지 모두가 동시에 0이라는 것을 test
shapiro.test(res04) # normality test -> 귀무가설 기각 불가 -> 정규성 만족.
tsdiag(arma04)



# invertible(stationary) test
ma4.st <- arima(y2, c(0, 0, 4), include.mean = FALSE, transform.pars = FALSE, method = 'ML') 
# MA(4), 추정방법 : MLE(Maximum Likelihood)
ma4.st$coef
polyroot(c(1, ma4.st$coef))
Mod(polyroot(c(1, ma4.st$coef))) # 이를 복소수 평면에 표시했을 때, 1보다 크면, stability condition 만족!
root.comp <- Im(polyroot(c(1, ma4.st$coef))) 
root.real <- Re(polyroot(c(1, ma4.st$coef)))
# plotting the roots in a unit circle
x <- seq(-1, 1, length = 1000)
y1 <- sqrt(1-x^2)
y2 <- -sqrt(1-x^2)
plot(c(x, x), c(y1, y2), xlab = 'Real part', ylab = 'Complex part', type = 'l',
     main = 'Unit Circle', ylim = c(-2, 2), xlim = c(-2, 2))
abline(h = 0)
abline(v = 0)
points(root.real, root.comp, pch = 19)
legend(-1.5, -1.5, legend = 'Roots of MA(4)', pch = 19)





# -------------------------------- Problem 5 --------------------------------------

setwd('/Users/imchaebin/Desktop/시계열분석/숙제2/')
data2 <- read.table("data.txt", head = T)
data2
data2$GDP <- as.numeric(data2$GDP)
data2$M1 <- as.numeric(data2$M1)

data2$gr_gdp <- (data2$GDP - lag(data2$GDP, 1)) / data2$GDP
data2$gr_gdp_lag1 <- lag(data2$gr_gdp, 1)
data2$gr_gdp_lag2 <- lag(data2$gr_gdp, 2)
data2$gr_gdp_lag3 <- lag(data2$gr_gdp, 3)
data2$gr_gdp_lag4 <- lag(data2$gr_gdp, 4)

data2$gr_m1 <- (data2$M1 - lag(data2$M1, 1)) / data2$M1
data2$gr_m1_lag1 <- lag(data2$gr_m1, 1)
data2$gr_m1_lag2 <- lag(data2$gr_m1, 2)
data2$gr_m1_lag3 <- lag(data2$gr_m1, 3)
data2$gr_m1_lag4 <- lag(data2$gr_m1, 4)

data2 <- data2[which(data2$OBS == '1986:1') : which(data2$OBS == '2004:4'), ]
data2 <- na.omit(data2)


# 1st estimation
lm1 <- lm(gr_gdp ~ gr_gdp_lag1 + gr_gdp_lag2 + gr_gdp_lag3 + gr_gdp_lag4 + gr_m1_lag1 + gr_m1_lag2 + gr_m1_lag3 + gr_m1_lag4, 
          data = data2)
summary(lm1)$coefficients['gr_m1_lag1',] # T-test
linearHypothesis(lm1, 'gr_m1_lag1=0') # F-test


# 2nd estimation
granger_data <- data2[c('gr_gdp', 'gr_m1')]
grangertest(gr_gdp ~ gr_m1, granger_data, order=4)
lm2 <- lm(gr_gdp ~ gr_gdp_lag1 + gr_gdp_lag2 + gr_gdp_lag3 + gr_gdp_lag4 + I(gr_m1_lag1 + gr_m1_lag2 + gr_m1_lag3 + gr_m1_lag4), 
          data = data2)
summary(lm2) # T-test
anova(lm1, lm2) # F-test

















