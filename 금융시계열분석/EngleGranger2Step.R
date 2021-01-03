# Engle-Granger ECM for consumption and income of the UK
rm(list = ls())
library(tseries)
library(urca)
data(Raotbl3)
attach(Raotbl3)

lc <- ts(lc, start=c(1966, 4), end = c(1991, 2), frequency = 4)
li <- ts(li, start=c(1966, 4), end = c(1991, 2), frequency = 4)
lw <- ts(lw, start=c(1966, 4), end = c(1991, 2), frequency = 4)

ukcons <- window(cbind(lc, li, lw), start = c(1967, 2), end = c(1991, 2))

lc.eq <- summary(lm(lc ~ li + lw, data = ukcons))
li.eq <- summary(lm(li ~ lc + lw, data = ukcons))
lw.eq <- summary(lm(lw ~ lc + li, data = ukcons))

error.lc <- ts(resid(lc.eq), start = c(1967,2), end = c(1991,2), frequency = 4)
error.li <- ts(resid(li.eq), start = c(1967,2), end = c(1991,2), frequency = 4)
error.lw <- ts(resid(lw.eq), start = c(1967,2), end = c(1991,2), frequency = 4)

ukcons2 <- ts(embed(diff(ukcons), dim = 2), start = c(1967, 4), freq = 4)
colnames(ukcons2) <- c('lc.d', 'li.d', 'lw.d', 'lc.d1', 'li.d1', 'lw.d1')

err.ecm1 <- window(lag(error.lc, k = -1), start = c(1967,4), end = c(1991,2))
err.ecm2 <- window(lag(error.li, k = -1), start = c(1967,4), end = c(1991,2))

ecm.eq1 <- lm(lc.d ~ err.ecm1 + lc.d1 + lc.d1 + lw.d1, data = ukcons2)
ecm.eq2 <- lm(lc.d ~ err.ecm2 + lc.d1 + lc.d1 + lw.d1, data = ukcons2)

summary(ecm.eq1)
summary(ecm.eq2)
