

################# Chapter 3 ###########################

# data import & preprocessing
install.packages('readxl')
library(readxl)
setwd('/Users/imchaebin/Desktop/시계열분석/숙제1/data/')
data <- read_excel('macro.xls')
colnames(data)
data$dspread <- c(NA, diff(data$`BAA-AAA SPREAD`))
data$dcredit <- c(NA, diff(data$`CONSUMER CREDIT`))
data$dprod <- c(NA, diff(data$`Industrial production`))
data$rmsoft <- c(NA, 100*diff(log(data$`Microsoft`)))
data$rsandp <- c(NA, 100*diff(log(data$`SANDP`)))
data$dmoney <- c(NA, diff(data$`M1MONEY SUPPLY`))
data$inflation <- c(NA, 100*diff(log(data$`CPI`)))
data$term <- data$USTB10Y - data$USTB3M
data$dinflation <- c(NA, diff(data$`inflation`))
data$mustb3m <- data$USTB3M/12
data$rterm <- c(NA, diff(data$`term`))
data$ermsoft <- data$rmsoft - data$mustb3m
data$ersandp <- data$rsandp - data$mustb3m

# multiple linear regression
df <- data[,c('ermsoft', 'ersandp', 'dprod', 'dcredit', 'dinflation', 'dmoney', 'dspread', 'rterm')]
fitAll <- lm(ermsoft ~ ., data = df)
formula(fitAll)
par(mfrow = c(2, 4))
plot(formula(fitAll), data = df)
plot(fitAll)

summary(fitAll)

# multiple linear regression with making DPROD, DCREDIT, DSPREAD zero
df2 <- data[,c('ermsoft', 'ersandp', 'dinflation', 'dmoney', 'rterm')]
fit2 <- lm(ermsoft ~ ., data = df2)
anova(fitAll, fit2)


# stepwise linear regression
fit <- lm(ermsoft ~ 1, data = df)
head(df)
summary(fit)
step(fit, direction = 'forward', scope = formula(fitAll))

