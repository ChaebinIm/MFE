# Engle-Granger two-step procedure
rm(list = ls())

set.seed(123456)
e1 <- rnorm(100) # 독립적인 정규분포 샘플
e2 <- rnorm(100)

# generate two I(1) with 1 common stochastic trend
y1 <- cumsum(e1)
y2 <- 0.6*y1 + e2

# estimate cointegration vector and get EC term
lr.reg <- lm(y2 ~ y1) # y1 coefficients is about 0.6
error <- residuals(lr.reg)
# residual -> cointegration vector = error correction = Z_t
error.lagged <- error[-c(1, 100)] # Z_(t-1)


# construct ECM
dy1 <- diff(y1)
dy2 <- diff(y2)

# embed (x, 2) -> 2개의 벡터 생성 x, lagged x (dim = (T-1) by 2)
diff.dat <- data.frame(embed(cbind(dy1, dy2), 2))
colnames(diff.dat) <- c('dy1', 'dy2', 'dy1.1', 'dy2.1')
ecm.reg1 <- lm(dy1 ~ error.lagged + dy1.1 + dy2.1,
               data = diff.dat)
summary(ecm.reg1)
ecm.reg2 <- lm(dy2 ~ error.lagged + dy1.1 + dy2.1,
               data = diff.dat)
summary(ecm.reg2)

