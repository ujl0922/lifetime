# Customer Lifetime Value

##Start
file <- read.csv("RF data.csv")
tx <- read.csv("tx.csv")
head(file)

##installing packages
install.packages("BTYD")
install.packages("bbmle")

##loading packages
library("BTYD")
library("bblmle")


txx <- read.csv("txx.csv")

startingparams <- c(1.0, 3, 1.0, 3)
# estimated parameters
est.params <- bgnbd.EstimateParameters(tx, startingparams)
# log-likelihood of estimated parameters
bgnbd.cbs.LL(est.params, tx)

params <- bgnbd.EstimateParameters(tx)

bgnbd.PAlive(params, x=23, t.x=39, T.cal=39)
# P(Alive) of a customer who has the same recency and total
# time observed.
bgnbd.PAlive(params, x=2, t.x=30.43, T.cal=38.86)
bgnbd.PAlive(params, x=2, t.x=3, T.cal=38.86)


# To visualize the distribution of P(Alive) across customers
p.alives <- bgnbd.PAlive(params, tx[,"x"], tx[,"t.x"], tx[,"T.cal"])
plot(density(p.alives))

#transaction in calibration period
bgnbd.ExpectedCumulativeTransactions(params, T.cal=txx[,"T.cal"], T.tot=39, n.periods.final=273)

##histogram - Calibration period repeat transaction frequency comparison matrix (actual vs. expected)
bgnbd.PlotFrequencyInCalibration(est.params, txx, censor=7)

# Calculating conditional expected transaciton per individual
bgnbd.ConditionalExpectedTransactions(params, T.star=39, x=2, t.x=30.43, T.cal=38.86)
bgnbd.Expectation(params, t=2)


library(dplyr)

## assess model accuracy
txx <- 
  txx %>% 
  mutate(x.star = bgnbd.ConditionalExpectedTransactions(params,T.star,x,t.x,T.cal))


bgnbd.PlotFreqVsConditionalExpectedFrequency(est.params, T.star=39, txx, x.star, censor=7)


# actual vs conditional expected transactions by recency
bgnbd.PlotRecVsConditionalExpectedFrequency(est.params, txx, T.star=39, x.star)

bgnbd.PlotTrackingCum(est.params, T.cal=txx[,"T.cal"], T.tot=78, cu.tracking)



##Plots the actual and expected cumulative total repeat transactions by all customers for the calibration and holdout periods, and returns this comparison in a matrix..

