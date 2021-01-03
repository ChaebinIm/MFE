# simulation of AR(1) process with phi = 0.9
# 2020-09-18
# Financial Time series analysis

set.seed(12345)

#y <- arima.sim(n = 100, list(ar = 0.9), innov = rnorm(100))
y <- arima.sim(n = 100, list(ar = c(0.4, -0.28, 0.5)), innov = rnorm(100))
# y <- arima.sim(n = 100, list(ma = 0.7), innov = rnorm(100)) # coefficient

op <- par(no.readonly = TRUE)
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE)) # 2 by 2 graph. 1행에 1개 2행에 3개
par("mar")
par(mar=c(1,1,1,1))
plot.ts(y, ylab='simulated y series')
acf(y, main = 'Autocorrelations', ylab = '', ylim = c(-1,1), ci.col = 'red') # ci = maybe confidence interval.
pacf(y, main = 'Partial Autocorrelations', ylab = '', ylim = c(-1, 1), ci.col = 'black')



# Estimation of AR(2) process with phi1 = 0.6, phi2 = -0.28

series <- rnorm(1000000)

y.st <- filter(series, filter = c(0, 0, 0, 0.3), method = 'recursive')

ar2.st <- arima(y.st, c(0, 0, 4), include.mean = FALSE, transform.pars = FALSE, method = 'ML') # AR(2), 추정방법 : MLE(Maximum Likelihood)
ar2.st$coef
polyroot(c(1, -ar2.st$coef)) # 1 - 0.6279L + 0.2741L^2 -> 허근이 나옴.
Mod(polyroot(c(1, -ar2.st$coef))) # 이를 복소수 평면에 표시했을 때, 1보다 크니까, stability condition 만족!
root.comp <- Im(polyroot(c(1, -ar2.st$coef))) 
root.real <- Re(polyroot(c(1, -ar2.st$coef)))

# plotting the roots in a unit circle
x <- seq(-1, 1, length = 1000)
y1 <- sqrt(1-x^2)
y2 <- -sqrt(1-x^2)
plot(c(x, x), c(y1, y2), xlab = 'Real part', ylab = 'Complex part', type = 'l',
     main = 'Unit Circle', ylim = c(-2, 2), xlim = c(-2, 2))
abline(h = 0)
abline(v = 0)
points(root.comp, root.real, pch = 19)
legend(-1.5, -1.5, legend = 'Roots of AR(2)', pch = 19)







