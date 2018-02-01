#################
# Ali Abdullah
# axa171831
# ps5
################

  #workspace prep
rm(list = ls(all=TRUE))
t0 <- Sys.time()
setwd("D:/Study/OneDrive - The University of Texas at Dallas/Semester 1/BUAN 6356 BA with R/ps5")
library(data.table); library(tseries);

  #problem1
context1 <- read.csv("WAGE1.csv")

# K-means Estimation
seed        <-	2	
maxClusters	<-	10

## Use within-group variation to choose k
wss	<- rep(-1,maxClusters)
for (i in 1:maxClusters) { # i represents the k value
  set.seed(seed)
  model <- kmeans(context1,centers=i,nstart=10)
  wss[i] <- model$tot.withinss
}
plot(1:maxClusters,	wss, type="b", 
     xlab="Number of Clusters",
     ylab="Aggregate Within Group SS")

## Run the model
set.seed(seed)
model1 <- kmeans(context1,centers=3,nstart=10)
model1$centers
context1$cluster <- model1$cluster

# Run linear models separately for each cluster
model2 <- lm(wage ~ educ + exper + tenure, data=subset(context1, cluster==1))
model3 <- lm(wage ~ educ + exper + tenure, data=subset(context1, cluster==2))
model4 <- lm(wage ~ educ + exper + tenure, data=subset(context1, cluster==3))
summary(model2)
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  3.78799    2.83502   1.336 0.184419    
# educ         0.40735    0.10276   3.964 0.000135 ***
# exper       -0.10172    0.05851  -1.739 0.085077 .  
# tenure       0.13653    0.03098   4.407 2.55e-05 ***
summary(model3)
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -3.21053    0.81295  -3.949 0.000101 ***
# educ         0.52524    0.06025   8.717 3.49e-16 ***
# exper        0.17286    0.03806   4.542 8.55e-06 ***
# tenure       0.29156    0.06613   4.409 1.52e-05 ***
summary(model4)
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -4.85740    2.03152  -2.391    0.018 *  
# educ         0.73631    0.11888   6.194 5.30e-09 ***
# exper        0.05905    0.05871   1.006    0.316    
# tenure       0.21796    0.04655   4.683 6.26e-06 ***

#####   Interpretations              ####################################
#########################################################################
##  a.  From the Elbow test, the optimal number of clusters seems to be
##      four.
##  b.  Group1 is people with high tenure, high experience but lower
##      education. Their wage is intermediate.      
##      Group 2 is people with low tenure and experience, but high educ
##      and their wage is lowest among the 3 groups
##      Group3 is people with intermediate educ, exper an d tenure and
##      their wage is highest in the 3 clusters.
##  c.  Education has a bigger positive effect in cluster3. Tenure has
##      a bigger effect in cluster2 and experience has a negative impact      
##      on wage in model2. So there are differences in the slopes of      
##      the variables across models.
#########################################################################

  #problem2
context2 <- read.csv("ffportfolios.csv")
Xdata <- context2[,2:33] #exclude the year
head(Xdata)

#loop to kpss-test all 32 portfolios
i <- 1
for (i in 1:32){
    kpss.test(Xdata[,i], null="Level")
}
rm(i)
warnings() #to display all kpss-test results

model5 <- prcomp(Xdata) #PCA model
summary(model5) #first PC captures 73% of variation, second captures only 6%
screeplot(model5,type="lines")

model5$rotation[,1:1]

# get the pca loadings
context2$factor <- model5$x[,1:1]
summary(context2$factor)

# standardize the factors using scale function
context2$factor <- scale(context2$factor)
var(context2$factor)

# finding the rows with principal component less than -2.58
context3 <- subset(context2, factor < -2.58)
summary(context3)

#####   Interpretations              ####################################
#########################################################################
##  a.  We should use 1 principal component.
##  b.  All of the portfolios in these years are giving negative returns.
##      So maybe this principal component is the general index of the      
##      market. It gives the general trend of the entire market and in      
##      years of downfall, the standardized component is below -2.58.
#########################################################################

Sys.time() - t0
