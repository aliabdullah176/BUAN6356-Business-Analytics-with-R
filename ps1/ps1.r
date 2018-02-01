# File Name:    ps1.r
# Author:       Ali Abdullah
# NetID:        axa171831
# Date:         2017-09-08
# Description:  Solution for problem set #1

#workspace prep
rm(list=ls(all=TRUE))
t0 <- Sys.time()
setwd("D:/Study/OneDrive - The University of Texas at Dallas/Semester 1/BUAN 6356 BA with R/ps1")
#install.packages('data.table') 
library(data.table)

#Read and browse through data
wageDat <- fread('WAGE1.csv')
#wageDat
#View(wageDat)

#Summary of data
summary(wageDat)
 
#natural log of wage generated and appended to the datatable
lwage <- log(wageDat$wage)
wageDat <- cbind(wageDat, lwage)
#wageDat

#model1 >> wagei = b0 + b1educi + ei
model1 <- lm(wage~educ, data = wageDat)
summary(model1)

##################################################################
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -5.3707 -2.1578 -0.9854  1.1864 16.3975 
# 
# Coefficients:
#             Estimate    Std. Error  t value   Pr(>|t|)    
# (Intercept) -0.93389    0.68769     -1.358    0.175    
# educ         0.54470    0.05346     10.189    <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 3.392 on 524 degrees of freedom
# Multiple R-squared:  0.1654,	Adjusted R-squared:  0.1638 
# F-statistic: 103.8 on 1 and 524 DF,  p-value: < 2.2e-16
##################################################################

#model 2 >> wagei = b0 + b1educi + b2experi + b3tenurei + ei
model2 <- lm(wage~educ+exper+tenure, data = wageDat)

summary(model2)

##################################################################
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -7.6498 -1.7708 -0.6407  1.2051 14.7201 
# 
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -2.91354    0.73172  -3.982 7.81e-05 ***
#   educ      0.60268     0.05148  11.708  < 2e-16 ***
#   exper     0.02252     0.01210   1.861   0.0633 .  
# tenure      0.17002     0.02173   7.825 2.83e-14 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 3.096 on 522 degrees of freedom
# Multiple R-squared:  0.3072,	Adjusted R-squared:  0.3032 
# F-statistic: 77.15 on 3 and 522 DF,  p-value: < 2.2e-16
##################################################################

#model 3 >> lwagei = b0 + b1educi + b2experi + b3tenurei + ei
model3 <- lm(lwage~educ+exper+tenure, data = wageDat)

summary(model3)

##################################################################
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.05911 -0.29563 -0.03302  0.28590  1.42657 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.282635   0.104331   2.709  0.00697 ** 
#   educ        0.092256   0.007340  12.569  < 2e-16 ***
#   exper       0.004137   0.001726   2.397  0.01687 *  
#   tenure      0.022112   0.003098   7.138 3.19e-12 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.4415 on 522 degrees of freedom
# Multiple R-squared:  0.3165,	Adjusted R-squared:  0.3125 
# F-statistic: 80.56 on 3 and 522 DF,  p-value: < 2.2e-16
##################################################################


##################################################################
# Interpretations
##################################################################

# a. Interpret the estimated coefficient on educ from model1 (eq 1).
#     Every increase of one year in education of a person is with an
#     increase of hourly wage by 0.54470 dollars for workers at
#     employee level

# b. Interpret the estimated coefiicient on educ from model2 (eq 2).
#     Every increase of one year in education of a person is with an
#     increase of hourly wage by 0.60268 dollars for workers at
#     employee level, controlling for the values for number of years
#     of experience and tenure.

# c. Interpret the estimated coefficient on exper from model2 (eq 2).
#     Every increase of one year in experience of a person is with an
#     increase of hourly wage by 0.02252 dollars for workers at
#     employee level, controlling for the values for number of years
#     of education and tenure.    

# d. Interpret the estimated coefficient on tenure from model2 (eq 2).
#     Every increase of one year in tenure of a person at a firm is with an
#     increase of hourly wage by 0.17002 dollars for workers at
#     employee level, controlling for the values for number of years
#     of education and experience.

# e. Interpret the estimated intercept from model2 (eq 2).
#     A person with no formal education, no experience and no tenure at
#     their current workplace is estimated to have an hourly wage of 
#     -2.91354 dollars which does not make sense as hourly wage cannot
#     be negative. This estimate is because our data, as shown in the
#     summary, does not have a single data point where all 3 variables
#     are zero. We are trying to make an estimate for the point where 
#     all x variables are zero but that is not within the scope of our
#     data. We can only make meaningful interpretations within the ranges
#     of our inpdependent variable data. So the intercept is meaningless.

# f. Interpret the estimated coefficient on educ from model3 (eq 3).
#     Every increase of one year in education of a person is with an
#     increase of hourly wage by 9.2256203 percent for
#     workers at employee level, controlling for the values for number
#     of years of experience and tenure.

coef(model3)*100          

# g. Interpret the estimated coefficient on exper from model3 (eq 3).
#     Every increase of one year in experience of a person is with an
#     increase of hourly wage by 0.4136804 percent for
#     workers at employee level, controlling for the values for number
#     of years of education and tenure. However, at 5% significance level,
#     we fail to reject the hypothesis that this coefficient is not zero.

# h. Interpret the estimated coefficient on tenure from model3 (eq 3)
#     Every increase of one year in experience of a person is with an
#     increase of hourly wage by 2.2111673 percent for workers at
#     employee level, controlling for the values for number of years
#     of education and experience.
##################################################################

t1 <- Sys.time()
t1-t0