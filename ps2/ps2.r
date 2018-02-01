#################
# Ali Abdullah
# axa171831
# ps2
################

#workspace prep
rm(list = ls(all=TRUE))
t0 <- Sys.time()
setwd("D:/Study/OneDrive - The University of Texas at Dallas/Semester 1/BUAN 6356 BA with R/ps2")
library(data.table)

####  Problem1
context1 <- fread('attend.csv')
summary(context1)
context1$hwrt <- context1$hw/8
context1$attendrt <- context1$attend/32
model1 <- lm(termGPA~priGPA+ACT+attendrt+hwrt, data = context1)
summary(model1)
coefficients(model1)
# Coefficients:   
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.286983   0.164169  -7.839 1.77e-14 ***
#   priGPA       0.548962   0.042418  12.942  < 2e-16 ***
#   ACT          0.036099   0.006051   5.966 3.92e-09 ***
#   attendrt     1.052246   0.155436   6.770 2.81e-11 ***
#   hwrt         0.913031   0.116932   7.808 2.22e-14 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.4788 on 675 degrees of freedom
# Multiple R-squared:   0.58,	Adjusted R-squared:  0.5775 
# F-statistic:   233 on 4 and 675 DF,  p-value: < 2.2e-16

q1pred <- data.frame(priGPA=c(2.2,3.9,3,3),ACT=c(32,20,25,25),attendrt=c(28/32,28/32,1,0.5), hwrt=c(1,1,0.5,1))
predict.lm(model1, q1pred)
q1pred

## B1 * stddev(x1) is roughly compareable to B2 * stddev(x2)

####  Interpretations for Problem 1
# a.  Each one unit increase in attendrt is with an increase of 
#     1.052246 in term GPA, while controlling for hwrt, priGPA 
#     and ACT. attendrt is a percentage expressed as a decimal 
#     between 0 and 1 so a better way to interpret would be to 
#     say that a 0.01 (ie 1%) increase in attndrt is with an 
#     increase of 0.01052246 in termGPA while controlling for
#     all other independent variables.
# b.  Each one unit increase in hwrt is with an increase of 
#     0.913031 in termGPA while controlling for attndrt, priGPA
#     and ACT. Alternatively, each one percent increase in hwrt
#     is with an increase of 0.00913031 in termGPA, controlling
#     for attndrt, priGPA and ACT
# c.  2.909664
# d.  3.409706
# e.  priGPA seems to be more important as the student with average
#     GPA and good ACT (problem c) had a worse termGPA as compared to 
#     a student with average ACT and good priGPA in part d.
# f.  2.771152
# g.  2.701545
# h.  attendrt has a higher slope coefficient so it is more important
#     towards termGPA
# i.  attendrt and hwrt are on the same scale ie decimal from 0 to 1
#     giving the percentage. ACT and priGPA are not on the same scale
#     resulting in problems in comparing. We cannot, for example, say
#     that an increase of 1 in priGPA is comparable to xyz increase
#     in ACT scores.
#####################################################################

####Problem 2
context2 <- fread('CEOSAL2.csv')
summary(context2)
#hist(context2$profits, breaks = 20)
#hist(log(context2$profits), breaks = 20)
#log(-1)

# model2 <- lm(log(salary)~log(mktval)+profits+ceoten, data= context2)
# summary(model2)
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.63382 -0.34660  0.00627  0.35059  1.96220 
# 
# Coefficients:
#             Estimate    Std. Error t value Pr(>|t|)    
# (Intercept) 4.7095052   0.3954502  11.909  < 2e-16 ***
# log(mktval) 0.2386220   0.0559166   4.267 3.25e-05 ***
# profits     0.0000793   0.0001566   0.506   0.6132    
# ceoten      0.0114646   0.0055816   2.054   0.0415 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.5289 on 173 degrees of freedom
# Multiple R-squared:  0.2514,	Adjusted R-squared:  0.2384 
# F-statistic: 19.36 on 3 and 173 DF,  p-value: 7.141e-11

model3 <- lm(log(salary)~log(mktval)+profits+ceoten+log(sales), data= context2)
summary(model3)
library(car)
vif(model3) #log(mktval) has a somewhat high multicollinearity value

# Coefficients:
#               Estimate    Std. Error t value Pr(>|t|)    
# (Intercept)   4.558e+00  3.803e-01  11.986  < 2e-16 ***
# log(mktval)   1.018e-01  6.303e-02   1.614   0.1083    
# profits       2.905e-05  1.503e-04   0.193   0.8470    
# ceoten        1.168e-02  5.342e-03   2.187   0.0301 *  
# log(sales)    1.622e-01  3.948e-02   4.109 6.14e-05 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.5062 on 172 degrees of freedom
# Multiple R-squared:  0.3183,	Adjusted R-squared:  0.3024 
# F-statistic: 20.08 on 4 and 172 DF,  p-value: 1.387e-13

####  Interpretations for Problem 3
# j.  Profits include negative values. It is not possible to take log of
#     negative values.
# k.  A one percent incrase in mktval is with an increase of 0.2386% in
#     CEO salary provided that profits, and ceoten remain constant.
# l.  A one percent increase in mktval is with an increase of 0.1018%
#     increase in CEO salary while keeping profits, ceoten and sales
#     constant
# m.  In model2, the slope coefficient of mktval is significant, even at
#     0.1% significance level. In model3, the coefficient for mktval is
#     not significant, even at 10% significance level. Adding the sales
#     variable has led to this change in significance. This possibly means
#     that model 2 had an omitted variable bias because of leaving out
#     sales. It may be the case that mktval and salary are both dependent
#     on the sales. Model3 also has a better adjusted R square value.
# n.  The coefficients on profit on model 3 are not significant as the p-value
#     is too high.
# o.  Each one percent increase in sales is with a 0.1622% increase in CEO
#     salary provided that mktval, profits and ceoten remain constant.
#####################################################################

#### Problem 3
context3 <- fread('hprice1.csv')
summary(context3)
#pricei = b0 + b1bdrmsi + b2 ln [lotsizei] + b3 ln [sqrfti] + b4coloniali
model4 <- lm(price~bdrms+log(lotsize)+log(sqrft)+colonial, data = context3)
summary(model4)
# Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       -2030.455    210.967  -9.625 3.68e-15 ***
#   bdrms           18.572      9.308   1.995   0.0493 *  
#   log(lotsize)    61.446     12.372   4.966 3.60e-06 ***
#   log(sqrft)      225.508     30.072   7.499 6.41e-11 ***
#   colonial        4.134     14.509   0.285   0.7764    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 59.66 on 83 degrees of freedom
# Multiple R-squared:  0.6781,	Adjusted R-squared:  0.6626 
# F-statistic: 43.71 on 4 and 83 DF,  p-value: < 2.2e-16

model5 <- lm(log(price)~bdrms+log(lotsize)+log(sqrft)+colonial, data = context3)
summary(model5)

# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   -1.34959    0.65104  -2.073   0.0413 *  
#   bdrms       0.02683    0.02872   0.934   0.3530    
# log(lotsize)  0.16782    0.03818   4.395 3.25e-05 ***
# log(sqrft)    0.70719    0.09280   7.620 3.69e-11 ***
#   colonial    0.05380    0.04477   1.202   0.2330    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.1841 on 83 degrees of freedom
# Multiple R-squared:  0.6491,	Adjusted R-squared:  0.6322 
# F-statistic: 38.38 on 4 and 83 DF,  p-value: < 2.2e-16


library(car)
vif(model4)
vif(model5) #checking for multicollinearity

####  Interpretations for Problem 3
# p.  Each percent change in lotsize is with an increase of $614.46
#     in house value while keeping bdrms, sqrft and colonial constant.
# q.  Each percent change in lotsize is with an increase of 0.16782% 
#     in house value while keeping bdrms, sqrft and colonial constant.
# r.  Houses that are colonial are valued, on average, $4134 higher
#     than houses that are not constructed in colonial style while
#     keeping bdrms, sqrft and lotsize equal.
# s.  Model 4 fits this data set better in terms of the business
#     information required. As someone who wants to predict how much
#     a house will sell for, getting the dollar value of, say one extra
#     bedroom, is much more useful than knowing the percentage change
#     in price due to one extra bedroom.
# t.  The expansion will add a total of $41.12k to the sale value of the
#     house. This does not cover the cost of expansion which is $50k. 
#     However, the expansion provides $20k value in terms of enjoying the
#     expansion. If this $20k valpue is not lost when selling the house, then
#     the total value of the exapansion is $61.12k whille the cost is $50k, in
#     which case it is logical to go ahead with the expansion.
#####################################################################

#### Problem 4
context4 <- fread('JTRAIN2.csv')
summary(context4)
model6 <- lm(re78~re75+train+educ+black, data = context4)
summary(model6)

# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)   
# (Intercept)   1.97686    1.89028   1.046   0.2962   
# re75          0.14697    0.09811   1.498   0.1349   
# train         1.68422    0.62700   2.686   0.0075 **
# educ          0.41026    0.17267   2.376   0.0179 * 
# black         -2.11277    0.82941  -2.547   0.0112 * 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 6.496 on 440 degrees of freedom
# Multiple R-squared:  0.04917,	Adjusted R-squared:  0.04053 
# F-statistic: 5.688 on 4 and 440 DF,  p-value: 0.00018

####  Interpretations for problem 4
# u.  Each one unit (ie $1000) increase in real earnings in 1975 
#     is with an increase of $146.97 in real earnings in 1975 for low
#     income workers while keeping training, education and ethnicity/race
#     constant. However, this coefficient is not significant.
# v.  Training of low income workers is assosciated with an increase of
#     ~$1684 in their 1978 real earnings while keeping educ, re75 and black
#     constant. The coefficient is significant at significance level of 1%.
# w.  Low income men who were black earned, on average, ~$2112 less than
#     low income men who were not black keeping re75, train and educ constant.
#####################################################################

Sys.time() - t0
