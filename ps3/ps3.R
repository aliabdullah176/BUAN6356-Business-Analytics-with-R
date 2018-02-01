#################
# Ali Abdullah
# axa171831
# ps3
################

  #workspace prep
rm(list = ls(all=TRUE))
t0 <- Sys.time()
setwd("D:/Study/OneDrive - The University of Texas at Dallas/Semester 1/BUAN 6356 BA with R/ps3")
library(data.table); library(sandwich); library(lmtest); library(plm); library(tseries);

  #problem1

context1 <- fread('hprice1.csv')
attach(context1)
model1 <- lm(price~bdrms+lotsize+sqrft)
model1res <- residuals(model1)
model1ressq <- residuals(model1)^2
summary(model1)
bptest(model1)
plot(model1res~sqrft)
#plot(model1)
coeftest(model1, vcov. = vcovHC)

model2 <- lm(log(price)~bdrms+log(lotsize)+log(sqrft))
summary(model2)

bptest(model2)
#plot(model2)
coeftest(model2, vcov. = vcovHC)

detach(context1)

  ##interpretations
# a   sqrft and lotsize are significant
# b   only sqrft is now significant
# c   log(lotsize) and log(sqrft) are significant
# d   log(lotsize) and log(sqrft) are significant
# e   Log reduces hetereskedasticity

  #problem2
context2 <- fread('beveridge.csv')
head(context2)
attach(context2)
model3 <- lm(urate~vrate)
summary(model3)
coeftest(model3, vcov. = NeweyWest(model3, lag = 5))
model3res <- residuals(model3)
plot(model3res~t)
# ts.plot(urate)
# ts.plot(vrate)
# ts.plot(diff(urate))
# ts.plot(diff(vrate))
# ts.plot(diff(diff(urate)))
# ts.plot(diff(diff(vrate)))

kpss.test(urate, null="Level")
kpss.test(urate, null="Trend")
kpss.test(vrate, null="Level")
kpss.test(vrate, null="Trend")
#none of the things is stationary

kpss.test(diff(urate), null="Level") #good
kpss.test(diff(urate), null="Trend")
kpss.test(diff(vrate), null="Level") #good
kpss.test(diff(vrate), null="Trend")

kpss.test(diff(diff(urate)), null="Level") #good
kpss.test(diff(diff(urate)), null="Trend") #good
kpss.test(diff(diff(vrate)), null="Level") #good
kpss.test(diff(diff(vrate)), null="Trend") #good

model4 <- lm(diff(urate)~diff(vrate))
summary(model4)
coeftest(model4, vcov. = NeweyWest(model4, lag = 5))
detach(context2)

    ##interpretations
# f   yes, the vrate is significant
# g   first difference is level stationary
# h   first difference is level stationary
# i   yes, vrate is no longer significant at 5% or even 10% significance level. it is highly insignificant
# j   model4, as it corrects for autocorrelation in the data

  #problem3
context3 <- fread('JTRAIN.csv')
context3copy <- fread('JTRAIN.csv')
attach(context3)
context3$d88 <- as.numeric(year==1988)
context3$d89 <- as.numeric(year==1989)
head(context3)
#?shift
x <- data.frame((shift(grant, type = "lag")))
x[is.na(x)] <- 0
context3$grantlastyear <- x
context3$grantlastyear[year==1987] <- 0
context3 <- plm.data(context3, index=c("fcode","year"))
rm(x)

model5 <- plm(log(scrap)~d88+d89+grant+grantlastyear, model="pooling", data=context3)
summary(model5)
model6 <- plm(log(scrap)~d88+d89+grant+grantlastyear, model="within", data=context3)
summary(model6)
coeftest(model5, vcov=vcovHC(model5, method = "arellano"))
coeftest(model6, vcov=vcovHC(model6, method = "arellano"))
detach(context3)

    ##interpretations
# k   firms which received grant in current time had on average an increase of 20% in 
#     scrap while keeping rest of the variables constant
# l   firms which received grant in previous time period had on average an increase of 4% in 
#     scrap while keeping rest of the variables constant
# m   grant leads to higher scrap production
# n   firms which received grant in current time had on average a decrease of 25% in 
#     scrap while keeping rest of the variables constant
# o   firms which received grant in previous time had on average a decrease of 42% in 
#     scrap while keeping rest of the variables constant
# p   grants are related to reduced scrap production
# q   In model5, HAC estimator makes d89 significant at 5% significance level
#     In model6, HAC estimator makes grant(t-1) insignficant at 5% significance level

Sys.time() - t0
