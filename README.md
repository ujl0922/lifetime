# Customer Lifetime Value

##Reading data
file <- read.csv("In sample.csv")
test <- read.csv("Out sample.csv")


##installing packages
install.packages("BTYD")
install.packages("bbmle")

##loading packages
library("BTYD")
library("bbmle")

startingparams <- c(1.0, 3, 1.0, 3)


##trim data
file <- file[!(file$x>50),]
file <- file %>% filter(x!=0)
test <- test[!(file$x>50),]

##data is big, randomly sample 10%

library("dplyr")
set.seed(10)
train <- sample_frac(file, size=0.1, replace=FALSE)

testt <- test[test$CONSUMER_ID %in% train$CONSUMER_ID, ]

##explore data
hist(train$x)
summary(train$x)

# estimated parameters & log-likelihood of estimated parameters
est.params <- bgnbd.EstimateParameters(train, startingparams)
bgnbd.cbs.LL(est.params, train)

params <- bgnbd.EstimateParameters(train)

# To visualize the distribution of P(Alive) across customers
p.alives <- bgnbd.PAlive(params, train[,"x"], train[,"t.x"], train[,"T.cal"])
plot(density(p.alives))

##fitting example
bgnbd.ConditionalExpectedTransactions(params, T.star=22, x=18, t.x=36.28571, T.cal=14.142857)

#transaction in calibration period - cumulative
a <- bgnbd.ExpectedCumulativeTransactions(params, T.cal=train[,"T.cal"], T.tot=22, n.periods.final=154)

##histogram
bgnbd.PlotFrequencyInCalibration(params, train, censor=22)
bgnbd.PlotFrequencyInCalibration(params, train, censor=50)

##we need to add a spike here


##fitting it on test model
bgnbd.PlotFrequencyInCalibration(params, testt, censor=22)

library(dplyr)




##Plots the actual and expected cumulative total repeat transactions by all customers for the calibration and holdout periods, and returns this comparison in a matrix..

