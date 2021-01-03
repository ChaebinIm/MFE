# least squares estimation example:
# http://www.learnbymarketing.com/tutorials/linear-regression-in-r
# install.packages('fBasics')

rm(list = ls())
setwd('/Users/imchaebin/Desktop/R/금융시계열분석/')

dataset = read.csv('data-marketing-budget-12mo.csv', header=T,
                   colClasses = c('numeric', 'numeric', 'numeric'))

head(dataset, 5)

simple.fit = lm(Sales ~ Spend, data = dataset)
summary(simple.fit)


#Loading the necessary libraries
library(lmtest)
library(fBasics)

#Testing normal distribution and independence assumptions
jarqueberaTest(simple.fit$resid)
#Null Hypothetis ; Skewness and Kurtosis are equal to zero
dwtest(simple.fit) # Test for independence of residuals
#Null Hypothesis:Errors are serially Uncorrelated


# Simple Regression Residual Plots
layout(matrix(c(1,1,2,3), 2, 2, byrow = T))
# Spend x Residuals Plot
plot(simple.fit$resid~dataset$Spend[order(dataset$Spend)],
     main = 'Spend X Residuals\nfor Simple Regression',
     xlab = 'Marketing Spend', ylab = 'Residuals')
abline(h = 0, lty=2)

# Histogram of Residuals
hist(simple.fit$resid, main = "Histogram of Residuals",
     ylab = 'Residuals')

#Q-Q Plot
qqnorm(simple.fit$resid)
qqline(simple.fit$resid)


#/////////////Multiple Regression Exampel //////////

multi.fit = lm(Sales ~ Spend + Month, data = dataset)
summary(multi.fit)


install.packages('car')
library(car)
linearHypothesis(multi.fit, c('Spend=0', 'Month=0'))
