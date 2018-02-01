#################
# Ali Abdullah
# axa171831
# ps4
################

  #workspace prep
rm(list = ls(all=TRUE))
t0 <- Sys.time()
setwd("D:/Study/OneDrive - The University of Texas at Dallas/Semester 1/BUAN 6356 BA with R/ps4")
library(data.table); library(sandwich); library(lmtest); library(plm);
library(tseries); library(RcmdrMisc); library(glmx)
library(evtree); library(tree); library(party); library(rpart)

  #problem1
#import and summary stats
context1 <- fread("htv.csv")
head(context1)
summary(context1)
#variable generation
context1$abil.sq <- context1$abil^2
context1$educ.sq <- context1$educ^2
context1$exper.sq <- context1$exper^2
context1$abil.educ <- context1$abil*context1$educ
context1$abil.exper <- context1$abil*context1$exper
context1$educ.exper <- context1$educ*context1$exper
attach(context1)
  #model1
model1 <- lm(log(wage)~abil+educ+exper)
summary(model1)
c(AIC(model1),BIC(model1))

  #model2
model2 <- lm(log(wage)~abil+educ+exper+abil.sq+
               educ.sq+exper.sq+abil.exper+abil.educ+educ.exper)
summary(model2)
c(AIC(model2),BIC(model2))

#backwards stepwise regression
mod2.bck <- step(model2, scope=list(lower=model1, upper=model2),
                 direction="backward", k=log(1230))
c(AIC(mod2.bck),BIC(mod2.bck))
summary(mod2.bck)

#forward stepwise regression
mod2.fwd <- step(model1,
                 scope= ~abil+educ+exper+abil.sq+educ.sq+exper.sq+abil.exper+abil.educ+educ.exper, 
                 direction="forward", k=log(1230))
summary(mod2.fwd)
c(AIC(mod2.fwd),BIC(mod2.fwd))

#both
mod2.both <- step(model2, 
                  scope=list(lower=model1, upper=model2), 
                  direction="both", k=log(1230))

#backwards stepwise using BIC
stepwise(model2, direction="backward",criterion = "BIC")
mod2.0 <- stepwise(model2, direction="backward",criterion = "BIC")
BIC(mod2.0)
summary(mod2.0)

#final model
model2 <- lm(log(wage) ~ exper + abil.educ + educ.exper)
c(AIC(model2),BIC(model2))
summary(model2)

detach(context1)

#####   Interpretations              ####################################
#########################################################################
## a. The new model consists of exper, ebil*educ and educ*exper. There are two interaction terms
## b. educ*exper is giving the combined effect of the two variables. The two variables are no
##    longer independently impacting the dependent variable. Instead the effect of change in educ
##    depends on the value of exper
########################################################################

  #problem2
context2 <- fread('loanapp.csv')
head(context2)
attach(context2)

#model3 with only 1 variable
model3 <- glm(approve~white,family=binomial(link="logit"))
summary(model3)
coeftest(model3, vcoc.=vcovHC)

#model4 with 15 variables
model4 <- glm(approve~white+hrat+obrat+loanprc+unem+male+married
              +dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr,
              family=binomial(link="logit"))
summary(model4)
coeftest(model4, vcoc.=vcovHC)

#model5 with interaction variable white*obrat
model5 <- glm(approve~white+hrat+obrat+loanprc+unem+male+married+dep+sch
              +cosign+chist+pubrec+mortlat1+mortlat2+vr+I(white*obrat),
              family=binomial(link="logit"))
summary(model5)
coeftest(model5, vcoc.=vcovHC)
vif(model5)
detach(context2)

#####   Interpretations              ####################################
#########################################################################
##  a.  The coefficient roughly tells that being white increases the chance
##      of approval of loan application.
##  b.  The coefficient changes but it is still significant at 5% level.
##  c.  white is no longer significant.
##  d.  It is the interaction term between white and obrat. This means that
##      the two variables have a combined effect on loan application. The
##      effects of white and obrat on loan application are no longer
##      independent. Rather, the effect of one will depend on the value of
##      the other. 
##      The reason it has such a big effect on the model is that it is not
##      a very logical interaction term. The beta coefficient is insignificant
##      and the inclusion of the term is introdcing multicillinearity in the
##      model which is causing white variable to go insiginificant.
#########################################################################

  #problem3
#impprt and attach data
context3 <- fread('smoke.csv')
attach(context3)
#running the model
model6 <- glm(cigs~educ+age+I(age^2)+log(income)+restaurn,family=poisson(link="log"))
summary(model6)
#heteroskedasticity corrected coefficients
coeftest(model6,vcov. = vcovHC)
detach(context3)

#taking derivative to find rate of change
0.11398+(2*-0.0013679*20) #0.059264
0.11398+(2*-0.0013679*60) #-0.050168

#####   Interpretations              ####################################
#########################################################################
##  a.  One unit increase in educ is with a decrease of 0.0595 units in
##      the log of cigs count, while controlling for all other variables.
##  b.  At age 20 >>> 0.059264
##      At age 60 >>> -0.050168
#########################################################################

  #problem4
#reaading data, generating variables and attaching the data table
context4 <- fread('hdisease.csv')
context4 <- data.table(context4)
##converting to factor to force classification instead of regression
context4$hdisease <- as.factor(context4$hdisease)
##coverting to factor to remove coercion
context4$exang <- as.factor(context4$exang)
attach(context4)

#model7 using evtree
model7 <- evtree(hdisease~age+cp+trestbps+thalach+exang)
plot(model7)
model7
table(predict(model7), context4$hdisease)
1 - mean(predict(model7) == context4$hdisease)  #classification error

#model8 using ctree
model8 <- ctree(hdisease~age+cp+trestbps+thalach+exang)
model8
plot(model8)
summary(model8)
1 - mean(predict(model8) == context4$hdisease)  #classification error

#unseen data
context5 <- fread('hdisease-new.csv')
context5$exang <- as.factor(context5$exang)
context5$dset <- NULL                           #removing column not used in training set
context5$hdisease_pred <- predict(model8, context5)
detach(context4)

#####   Interpretations              ####################################
#########################################################################
##  a.  Model8 may be overfitting the data. Model7 seems to not overfit or
##      underfit the data
##  b.  Adding dset will specify our model to only the 4 hospitals chosen
##      and will not let us generalize our findings to all heart patients.
##      In a way, that will overfit the data to only those 4 hospitals.
##      In the unseen data, we have data from a fifth hospital, which
##      will not be predictable by a model including the dset variable.
#########################################################################

Sys.time() - t0