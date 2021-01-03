################# Chapter 2 ###########################


# data import
install.packages('readxl')
install.packages("lmtest")
install.packages('fBasics')
install.packages("car")
library(readxl)
library(lmtest)
library(fBasics)
library(car)

setwd('/Users/imchaebin/Desktop/시계열분석/숙제1/data/') # need to be customized
data <- read_excel('capm.xls')


# add log return
# SAP
rsandp <- 100*diff(log(data$SANDP))
data$RSANDP <- c(NA, rsandp)

# Ford stock
ford <- 100*diff(log(data$FORD))
data$RFORD <- c(NA, ford)
data

# US - TBond : annually to monthly
data$USTB3M <- data$USTB3M/12
# Excess return of S&P500
data$ERSANDP <- data$RSANDP - data$USTB3M
data$ERFORD <- data$RFORD - data$USTB3M


# draw plot (ERSANDP, ERFORD)
plot(data$Date, data$ERSANDP, type = 'l', ylim=range(-80, 80), col = 'red') # line
# plot(data$Date, data$ERSANDP, ylim=range(-80, 80), col = 'red') # scatter
par(new=TRUE)
plot(data$Date, data$ERFORD, type = 'l', ylim=range(-80, 80), col = 'blue') # line
# plot(data$Date, data$ERFORD, ylim=range(-80, 80), col = 'blue') # scatter


# simple linear regression
m <- lm(ERFORD ~ ERSANDP, data = data)
coef(m)
summary(m)
plot(ERFORD ~ ERSANDP, data = data)
abline(m, col = 'red')


#Testing normal distribution and independence assumptions
jarqueberaTest(m$resid) #Test residuals for normality
#Null Hypothesis: Skewness and Kurtosis are equal to zero
dwtest(m) #Test for independence of residuals
#Null Hypothesis: Errors are serially UNcorrelated
par(mfrow = c(2, 2))
plot(m)





# linear regression with constraints
linearHypothesis(m, "ERSANDP = 1")
linearHypothesis(m, c("(Intercept) = 1", "ERSANDP = 1"))



