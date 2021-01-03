# Trend-stationary vs Difference-stationary

# Caution: this clears the Environment
rm(list=ls()) 
T <- 1500

set.seed(123456)
e <- rnorm(T)

## pure random walk
rw.nd <- cumsum(e)

## trend
trd <- 1:T

## random walk with drift : y_t = 0.5 + y_(t-1) + u_t
rw.wd <- 0.5*trd + cumsum(e)

## deterministic trend and noise : y_t = a0(0) + a1(0.5) * trend + u_t
trend.stationary <- e + 0.5*trd

## plotting
par(mar=rep(5,4))
plot.ts(trend.stationary, lty=1, ylab='', xlab='')
lines(rw.wd, lty=2)
par(new=T)
plot.ts(rw.nd, lty=3, axes=FALSE)
axis(4, pretty(range(rw.nd)))
lines(rw.nd, lty=3)

#legend(1, 1.8, legend=c('det. trend + noise (ls)','rw drift (ls)', 'rw (rs)'), lty=c(1, 2, 3)) 


y1 <- ts(rw.nd, start=1, end=T,frequency=1)

op <- par(no.readonly=TRUE)

layout(matrix(c(1, 1, 2, 3), 2, 2, byrow=TRUE))
par(mar=c(1,1,1,1))
plot(y1, ylab="Random walk")
acf(y1, main='Autocorrelations', ylab='', ylim=c(-1, 1))
pacf(y1, main='Partial Autocorrelations', ylab='', ylim=c(-1, 1)) # AR(1)
par(op)


# differncing TS -> MA(1) error
y2 <- ts(trend.stationary, start=1, end=T,frequency=1)
dy2 <- y2 - c(NA,y2[-T]) # 차분한 것. (원래 detrending해야되는데 차분한 것.)

layout(matrix(c(1, 1, 2, 3), 2, 2, byrow=TRUE))
par(mar=c(1,1,1,1))
plot(dy2, ylab="Differncing trend-stationary process")
acf(na.omit(dy2), main='Autocorrelations', ylab='', ylim=c(-1, 1))
pacf(na.omit(dy2), main='Partial Autocorrelations', ylab='', ylim=c(-1, 1))
# MA(1)인데 MA(2)인것 처럼 이상하게 나타나긴 한다. 그래도 MA(1)임을 알아놓자.
par(op)

arima(dy2, order = c(1, 0, 1))

# detrending DS -> create Spurious cycles
# 반대의 상황(에러) -> 차분해야 되는데(ds 데이터인데), detrending한 경우.
# 
y3 <- ts(rw.wd)
mod3 <- lm(y3~trd) # y_t = a0 + a1 * trend + u_t
e3 <-ts(mod3$residuals) # 잔차를 뽑아주었음.

layout(matrix(c(1, 1, 2, 2, 3, 4), 3, 2, byrow=TRUE))
par(mar=c(1,1,1,1))
plot(y3, ylab="")
plot(e3, ylab="Detrending difference-stationary process")
# differencing해야 되는데 detrending하면서, 없는 사이클이 생기는 등 문제가 생김.
acf(na.omit(e3), main='Autocorrelations', ylab='', ylim=c(-1, 1))
pacf(na.omit(e3), main='Partial Autocorrelations', ylab='', ylim=c(-1, 1))
par(op)
