#
#      	    
#			Multiple Linear Regression
#	 Transformations, Qualitative Variables & ANCOVA
#
#******************************************************

traindir <- "~/Google Drive/UVA/Courses/LSM/Fall2021/Data/TrainData/"
sourcedir <-"~/Google Drive/UVA/Courses/LSM/Fall2021/Source/"

setwd(sourcedir)
source("AccidentInput.R")

setwd(traindir)

acts <- file.inputl(traindir)

totacts <- combine.data(acts)

# Build a data frame xdmg with only extreme accidents for ACCDMG
dmgbox <-boxplot(totacts$ACCDMG)
xdmg <- totacts[totacts$ACCDMG > dmgbox$stats[5],]

## Remove 9/11
xdmg <- xdmg[-185,]

# Remove duplicates from xdmg and call new data frame xdmgnd
xdmgnd <- xdmg[!(duplicated(xdmg[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])),]


##******************************************* 
## Build linear regression models in R: lm ##
##*******************************************

# Build 2 multiple linear regression models with the data fram xdmgnd: 
# xdmgnd.lm1  ACCDMG ~ TEMP + TRNSPD + TONS + CARS + HEADEND1
# xdmgnd.lm2  ACCDMG ~ TEMP + TONS + CARS 

xdmgnd.lm1<-lm(ACCDMG ~ TEMP + TRNSPD  + CARS + HEADEND1,data=xdmgnd)
xdmgnd.lm2<-lm(ACCDMG ~ TEMP + CARS ,data=xdmgnd)

# Display regression results for each model
summary(xdmgnd.lm1)
summary(xdmgnd.lm2)

#  create models with interactions between all of the quantitative predictors ----
xdmgnd.lm3<-lm(ACCDMG~(TEMP+TRNSPD+CARS+HEADEND1)^2,data=xdmgnd)

summary(xdmgnd.lm3) 

library(car)
vif(xdmgnd.lm3)

xdmgnd.lm.test<-lm(ACCDMG~(TEMP+TRNSPD+CARS+HEADEND1+HEADEND1*TRNSPD),data=xdmgnd)
vif(xdmgnd.lm.test)

# Is this the complete second order model?

#  I() allows your model to contain normal mathematical sysmbols 
#  Create complete second order model                            
xdmgnd.lm4<-lm(ACCDMG~(TEMP+TRNSPD+CARS+HEADEND1)^2+I(TEMP^2)+I(TRNSPD^2)+I(CARS^2)+I(HEADEND1^2),data=xdmgnd)
summary(xdmgnd.lm4)
vif(xdmgnd.lm4)

# How many parameters and coefficients are in each of the models?


# Create a stepwise regression model on xdmgnd.lm4
xdmgnd.lm4.step <- step(xdmgnd.lm4)
summary(xdmgnd.lm4.step)
vif(xdmgnd.lm4.step)

# partial F test ----
anova(xdmgnd.lm3,xdmgnd.lm4)

# Which model is better based on the partial F test, the larger or smaller?


# Interaction Plot Example
trnspdbox<-boxplot(xdmgnd$TRNSPD)
TRNSPD.factor<-xdmgnd$TRNSPD
TRNSPD.factor[which(xdmgnd$TRNSPD<50)]<-'low train speed'
TRNSPD.factor[which(xdmgnd$TRNSPD>=50)]<-'high train speed'
TRNSPD.factor <- factor(TRNSPD.factor)


# Create derailment variable
Derail <- rep(0, nrow(xdmgnd))
Derail[which(xdmgnd$TYPE == 1)] <- 1 
Derail <- as.factor(Derail)
contrasts(Derail)

# create an interaction plot for TRNSPD and Derailments
interaction.plot(TRNSPD.factor,Derail, xdmgnd$ACCDMG)



##************************* 
## Qualitative Variables ##
##************************* 

# Create a qualitative variable for Cause 
Cause <- rep(NA, nrow(xdmgnd))
Cause[which(substr(xdmgnd$CAUSE, 1, 1) == "M")] <- "M"  ##Miscellaneous
Cause[which(substr(xdmgnd$CAUSE, 1, 1) == "T")] <- "T"  ##Track
Cause[which(substr(xdmgnd$CAUSE, 1, 1) == "S")] <- "S"  ##Signal or communication
Cause[which(substr(xdmgnd$CAUSE, 1, 1) == "H")] <- "H"  ##Human
Cause[which(substr(xdmgnd$CAUSE, 1, 1) == "E")] <- "E"  ##Electrical or mechanical

# This new variable, Cause, has to be a factor
Cause <- as.factor(Cause)

# Lets look at the default treatment coding
contrasts(Cause)

# What is the base case for Cause?



# Write a model to predict ACCDMG in terms of Cause 
xdmgnd.lm5<-lm(ACCDMG~Cause,data=xdmgnd)
summary(xdmgnd.lm5) 

tapply(as.numeric(xdmgnd$ACCDMG), as.factor(Cause), mean)

# How do we interperet the model xdmgnd. lm5?



#Chage based case to H ----
contrasts(Cause)<-matrix(c(1,0,0,0,0, 0,0,1,0,0, 0,0,0,1,0,  0,0,0,0,1),nrow=5)
colnames(contrasts(Cause)) <-matrix(c("E","M","S","T"),ncol=4)
contrasts(Cause)

xdmgnd.lm6<-lm(ACCDMG~Cause,data=xdmgnd)
summary(xdmgnd.lm6)

#How do we interperet the model xdmgnd.lm6?


#More qualitative variables:
xdmgnd.lm7<-lm(ACCDMG~as.factor(WEATHER),data=xdmgnd)
summary(xdmgnd.lm7) #What is the base case?


##*****************************************Exercise 1*****************************************

# Recode the qualitative variable Weather using the categories 
# in accident descriptions pdf document


# What is the base case?


# Build a model with the qualitative variable weather



# What is your conclusion?



# Build a new model to test the hypothesis: 
# "Accidents of type derailment do not increase 
# the severity of rail road accidents."




# What is the base case?




# What is your conclusion?





## Create 2 ANCOVA models:

#attach the dataset to the R search path. 
attach(xdmgnd) 

#remove it from the search path
# detach(xdmgnd) 

#now you can use variable names directly
summary(ACCDMG) 


# Build a main effects model Cause + TEMP + TRNSPD +  CARS + HEADEND1
xdmgnd.lm1 <-lm(ACCDMG~Cause + TEMP + TRNSPD +  CARS + HEADEND1,data=xdmgnd)
summary(xdmgnd.lm1)

# Build a main effects + interaction model Cause + TEMP + TRNSPD + CARS + HEADEND1 
xdmgnd.lm2<-lm(ACCDMG~(Cause+TEMP + TRNSPD + CARS + HEADEND1)^2,data=xdmgnd)
summary(xdmgnd.lm2)
vif(xdmgnd.lm2)

#Perform a Partial F Test: xdmgnd.lm1 vs. xdmgnd.lm2
anova(xdmgnd.lm1,xdmgnd.lm2)

# Which model do you choose?


## Use stepwise regression on your main effects + interaction model xdmgnd.lm1
xdmgnd.lm1.step <- step(xdmgnd.lm2)

# What is your new model?
summary(xdmgnd.lm1.step)
vif(xdmgnd.lm1.step)


xdmgnd.lm.Derail<-lm(ACCDMG~(Derail+TEMP + TRNSPD + CARS + HEADEND1)^2,data=xdmgnd)
summary(xdmgnd.lm.Derail)
vif(xdmgnd.lm.Derail)

xdmgnd.lm1<-lm(ACCDMG~(Derail+TEMP + TRNSPD + CARS + HEADEND1)^2,data=xdmgnd)
summary(xdmgnd.lm1)
vif(xdmgnd.lm1)

xdmgnd.lm1.step <- step(xdmgnd.lm1)
vif(xdmgnd.lm1.step)

anova(xdmgnd.lm1, xdmgnd.lm1.step)


##*****************************************Exercise 2*****************************************
# Choose 1 of your project 1 hypotheses (or assignment 2) based on an actionable, 
# controllable variable.  Build a model with both qualitative
# and quantitative variables to test your hypothesis.  Make sure 
# to address the following steps:
# 1.  Variable selection for linear models.
# 2.  Treatment of categorical variables for your linear model.
# 3.  Measure the performance of the models.
# 4.  Adjust your models based on analytical and graphical diagnostics.
# 5.  Reassess models based on adjustments.
# 6.  Compare your best models.
# 7.  Use your best model to reject or accept your hypothesis and provide a 
#     recommendation supported by statistical evidence.

