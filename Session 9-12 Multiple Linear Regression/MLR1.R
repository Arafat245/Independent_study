#    			
#			
#	 Multiple Linear Regression 1
#
#******************************************************

##load data
sourcedir <- "C:/Users/jgh6ds/Desktop/Independent/Session 9-12 Multiple Linear Regression"
setwd(sourcedir)
source("AccidentInput.R")

#load libraries
library(ggplot2)
library(GGally)
library(devtools) # for ggbiplot
library(ggbiplot)

traindir <- "C:/Users/jgh6ds/Desktop/Independent/Sessions 2-5 Data and Visualization/TrainData"
acts <- file.inputl(traindir)

totacts <- combine.data(acts)

##Build a data frame with only extreme accidents for ACCDMG

dmgbox <-boxplot(totacts$ACCDMG)

ggplot(as.data.frame(totacts$ACCDMG), aes(x=totacts$ACCDMG)) + 
  geom_boxplot(col= "steelblue") + theme(plot.title = element_text(hjust = 0.5)) + coord_flip()

xdmg <- totacts[totacts$ACCDMG > dmgbox$stats[5],]

## Remove duplicates from xdmg and call new data frame xdmgnd
xdmgnd <- xdmg[!(duplicated(xdmg[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])),]

# Setup categorical variables
xdmgnd$Cause <- rep(NA, nrow(xdmgnd))

xdmgnd$Cause[which(substr(xdmgnd$CAUSE, 1, 1) == "M")] <- "M"
xdmgnd$Cause[which(substr(xdmgnd$CAUSE, 1, 1) == "T")] <- "T"
xdmgnd$Cause[which(substr(xdmgnd$CAUSE, 1, 1) == "S")] <- "S"
xdmgnd$Cause[which(substr(xdmgnd$CAUSE, 1, 1) == "H")] <- "H"
xdmgnd$Cause[which(substr(xdmgnd$CAUSE, 1, 1) == "E")] <- "E"

# This new variable, Cause, has to be a factor

xdmgnd$Cause <- factor(xdmgnd$Cause)

xdmgnd$Type <- factor(xdmgnd$TYPE, labels = c("Derailment", "HeadOn", "Rearend", "Side", "Raking", "BrokenTrain", "Hwy-Rail", "GradeX", "Obstruction", "Explosive", "Fire","Other","SeeNarrative"))

#***********************************************************
#  	Possible predictors of damage	
#***********************************************************

# SPM
#Scatter plot matricies for quantitative predictors and single metric.

source("SPM_Panel.R")

uva.pairs(xdmgnd[,c("ACCDMG", "TRNSPD", "CARS", "TIMEHR", "TEMP")])

ggpairs(xdmgnd[,c("ACCDMG", "TRNSPD", "CARS", "TIMEHR", "TEMP")])


# PCA
# Principal components with the correlation matrix for extreme data with 1 metric and quantitative predictors.

source("PCAplots.R")

pred.pca <- princomp(xdmgnd[,c("ACCDMG", "TRNSPD", "CARS", "TIMEHR", "TEMP")], cor = T )

biplot(pred.pca)


## Which predictors are most correlated with accident damage?


###############################
# Categorical plots

# heatmap
source("http://www.phaget4.org/R/myImagePlot.R")
myImagePlot(table(xdmgnd$Cause, xdmgnd$Type), title = "No. of Accidents by Cause and Type of Accident")

## Which accident causes and types have the highest numbers of extreme accidents?

# Type & TRNSPD
library(lattice)

xyplot(log(ACCDMG)~TRNSPD | Type, data = xdmgnd, type = c("p", "r"))

qplot(TRNSPD, log(ACCDMG), data = xdmgnd) +  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + facet_wrap(~ Type, scales = "free")



# Cause & TRNSPD
xyplot(log(ACCDMG)~TRNSPD | Cause, data = xdmgnd, type = c("p", "r"))

qplot(TRNSPD, log(ACCDMG), data = xdmgnd) +  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + facet_wrap(~ Cause, scales = "free")


##What is notable about the relationship between train speed and accident
##damages for different accident causes and types?

#More complex xyplots

# Cause X Type and TRNSPD
xyplot(log(ACCDMG)~TRNSPD | Cause * Type, data = xdmgnd, type = c("p", "r"))

qplot(log(ACCDMG), TRNSPD, data = xdmgnd) +  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + facet_wrap(~ Cause * Type, scales = "free")


# Create the Derail variable & 
# then look at interactions with Cause
xdmgnd$Derail <- (xdmgnd$Type == "Derailment")

# plot xy with interactions of Derail and Cause
xyplot(log(ACCDMG)~TRNSPD | Cause * Derail, data = xdmgnd, type = c("p", "r"))

qplot(log(ACCDMG), TRNSPD, data = xdmgnd) +  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + facet_wrap(~ Cause * Derail, scales = "free")



# Create a Freight variable
xdmgnd$Freight <- (xdmgnd$TYPEQ == 1)


# Interaction plots

# Plot interaction between Derailment and Cause

interaction.plot(xdmgnd$Derail, xdmgnd$Cause,log(xdmgnd$ACCDMG))

ggplot() +
  aes(x = xdmgnd$Derail, y = log(xdmgnd$ACCDMG), group = xdmgnd$Cause, color = xdmgnd$Cause) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line")


# Interaction plots with quantitative variables
Speed <- cut(xdmgnd$TRNSPD, c(min(xdmgnd$TRNSPD),15,max(xdmgnd$TRNSPD)), include.lowest = T, labels = c("low speed", "high speed"))

Cars <- cut(xdmgnd$CARS, c(min(xdmgnd$CARS),1,max(xdmgnd$CARS)), include.lowest = T, labels = c("low hzd", "high hzd"))

Tons <- cut(xdmgnd$TONS, c(min(xdmgnd$TONS),median(xdmgnd$TONS),max(xdmgnd$TONS)), include.lowest = T, labels = c("low tons", "high tons"))

# Plot interaction between Speed and Cars
interaction.plot(Speed, Cars, log(xdmgnd$ACCDMG))

# First option with seeing points
qplot(x = TRNSPD, y = log(ACCDMG), data = xdmgnd, colour = Cars) +
  geom_point(colour = "gray")+
  geom_smooth(method = "lm") 

# Second option without points

ggplot() +
  aes(x = Speed, y = log(xdmgnd$ACCDMG), group = Cars, color = Cars) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line")



# Plot interaction between Freight and Speed
interaction.plot(xdmgnd$Freight, Speed, log(xdmgnd$ACCDMG))


ggplot() +
  aes(x = xdmgnd$Freight, y = log(xdmgnd$ACCDMG), group = Speed, color = Speed) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line")


# Plot interaction between Derailments and Speed
interaction.plot(xdmgnd$Derail, Speed, log(xdmgnd$ACCDMG))


ggplot() +
  aes(x = xdmgnd$Derail, y = log(xdmgnd$ACCDMG), group = Speed, color = Speed) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line")



# Plot interaction between Tons and Speed
interaction.plot(Speed, Tons, log(xdmgnd$ACCDMG))

ggplot() +
  aes(x = Speed, y = log(xdmgnd$ACCDMG), group = Tons, color = Tons) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line")



## How might these results inform your hypotheses?
## Use the multivariate visualizations as evidence to form at least 1 hypothesis.


####################################
# Linear Models
####################################

# Build linear regression models with different combinations of quantitative predictors to provide evidence for your hypothesis

# Single predictor
xdmgnd.lm1<-lm(ACCDMG~TEMP,data=xdmgnd)
summary(xdmgnd.lm1)
names(xdmgnd.lm1)
coef(xdmgnd.lm1)
sum(xdmgnd.lm1$res^2)


# Two predictors
xdmgnd.lm2<-lm(ACCDMG~TEMP+TRNSPD,data=xdmgnd)
summary(xdmgnd.lm2)
names(xdmgnd.lm2)
coef(xdmgnd.lm2)



#Three predictors
xdmgnd.lm3<-lm(ACCDMG~TEMP+TRNSPD+CARS,data=xdmgnd)
summary(xdmgnd.lm3)
coef(xdmgnd.lm3)


# Interpret your model coefficients.  Do they make sense?



# Interpret your developed models using the model utility test and t-test.



# Write out the null and alternative hypothesis for each of the tests.  



# Do you reject or fail to reject H0 for each test?




####################################
#	Now repeat for TOTKLD + TOTINJ
####################################
