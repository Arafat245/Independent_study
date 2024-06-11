
#******************************************************
#
#  				             Session 3
#				Duplicates, Categorial Variable Relationships &
#				            Extreme values
#
#******************************************************

#***************************
# 0.1 installing and loading the library for the visualization
#***************************
sessionInfo() #information about the version of your R and packages that are loaded in this session

#***************************
# 0.2 installing and loading the library for this session
#***************************
# tidyverse libraries
library(ggplot2)
library(dplyr)
#install.packages("stringr")
library(stringr)
#install.packages("GGally")
library(GGally)
#install.packages("psych")
library(psych)
#install.packages("lattice")
library(lattice)
#install.packages("ggpubr")
library(ggpubr)
#install.packages("gplots", dependencies = T)
library(gplots)



#**********************************************************
# 1. loading data
#**********************************************************


# Set working directory (change to your path)

# traindir <- "D:/GoogleDrive/Julie_SYS4021/2020/Data/Trains"
# sourcedir <-"D:/GoogleDrive/Julie_SYS4021/2020/R Code"

traindir <- "C:/Users/jgh6ds/Desktop/Independent/Sessions 2-5 Data and Visualization/TrainData"
sourcedir <- "C:/Users/jgh6ds/Desktop/Independent/Sessions 2-5 Data and Visualization"
setwd(sourcedir)

# Source AccidentInput
source("AccidentInput.R")

# you should have two data structures in working memory
# First - a list of data frames for each year of accident data
acts <- file.inputl(traindir)

# Next a data frame with all accidents from all years from 2001 - 2019

# the combined data frame
totacts <- combine.data(acts)

dim_desc(totacts) #dim(totacts)


#*************************************************
#		2. More Data Cleaning
#*************************************************

#***********************************************************
#		2.1 Setup Categorical Variables
#***********************************************************

# Accident type
# convert to factor 
# recoding 
class(totacts$TYPE)
levels(totacts$TYPE)
totacts$TYPE <- recode(totacts$TYPE,"Derailment", "HeadOn", "Rearend", "Side", "Raking", "BrokenTrain", "Hwy-Rail", "GradeX", "Obstruction", "Explosive", "Fire","Other","SeeNarrative") %>% as.factor()
class(totacts$TYPE)
levels(totacts$TYPE)

# Type of train
class(totacts$TYPEQ)
totacts$TYPEQ <- as.numeric(totacts$TYPEQ) #convert it to numeric
class(totacts$TYPEQ)

# Now convert to factor
class(totacts$TYPEQ)
levels(totacts$TYPEQ)
totacts$TYPEQ <- recode(totacts$TYPEQ,"Freight", "Passenger", "Commuter", "Work",  "Single", "CutofCars", "Yard", "Light", "Maint") %>% as.factor()
class(totacts$TYPEQ)
levels(totacts$TYPEQ) # now we can get different categories of train

# Accident cause

# The function case_when is part of the dplyr package and str_detect is part of the stringr package
# Both dplyr and stringr are part of the tidyverse package
totacts <- totacts %>% mutate(Cause = 
    case_when(
      str_detect(CAUSE, "M") ~ "M",
      str_detect(CAUSE, "T") ~ "T",
      str_detect(CAUSE, "S") ~ "S",
      str_detect(CAUSE, "H") ~ "H",
      str_detect(CAUSE, "E") ~ "E")
    )
# This new variable, Cause, has to be a factor
totacts$Cause <- as.factor(totacts$Cause)

#***********************************************************
#		2.2 Extreme data points
#***********************************************************
ggplot(as.data.frame(totacts$ACCDMG), aes(x=totacts$ACCDMG)) + geom_histogram()

#Look also at TOTKLD and TOTINJ
#what is the more frequent values?

ggplot(totacts, aes(y=ACCDMG)) + geom_boxplot()
# Get the values in the box plot for ACCDMG
dmgbox <- ggplot(totacts, aes(y=ACCDMG)) + geom_boxplot()
dmgbox

#takes the plot object, and performs all steps necessary to produce an object that can be rendered. 
#This function outputs two pieces: a list of data frames (one for each layer), 
#and a panel object, which contain all information about axis limits, breaks etc.
dmgbox.built <- ggplot_build(dmgbox)

names(dmgbox.built$data[[1]]) # what variables are associated with the boxplot?
tmp = dmgbox.built$data[[1]]


#***********************************************************
#		2.2.1 find only the extremes - the points above the upper whisker
#***********************************************************

# ymax is the upper whisker - anything above that is an outlier
upper <- dmgbox.built$data[[1]]$ymax

# create a new data frame with only the outliers
xdmg <- totacts %>% filter(ACCDMG > upper)
  
# how many outliers are there
count(xdmg)

# What proportion of accidents are extreme?
count(xdmg)/count(totacts)

# Proportion of costs

#summation of extreme ACCDMG 
extreme.sum <- totacts %>% select(ACCDMG) %>% filter(ACCDMG > dmgbox.built$data[[1]]$ymax) %>% sum()
#summation total ACCDMG
total.sum <- sum(totacts$ACCDMG)

#proportion of cost of extreme accident
extreme.sum/total.sum

# Let's look at the most extreme cost accidents. Are there any of particular interest?

# returns row index
which(xdmg$ACCDMG > 15e6)

# returns values filtered on condition
xdmg %>% select(ACCDMG) %>% filter(ACCDMG> 15e6)

#***********************************************************
#		2.2.2 Duplicate data points
#***********************************************************

# The max
which(xdmg$ACCDMG == max(xdmg$ACCDMG))

# how many entries have a max values
xdmg %>% select(ACCDMG) %>% filter(ACCDMG == max(ACCDMG))

# Look at the narrative
as.matrix(names(xdmg)) 
narrative = xdmg %>% select(ACCDMG,c(122:136)) %>% filter(ACCDMG == max(ACCDMG))
# or we can select columns start with NARR
narrative = xdmg %>%  select(ACCDMG,starts_with("NARR")) %>% filter(ACCDMG == max(ACCDMG))


# Are there any duplication in these selected columns?
duplicated(xdmg[1:100, c("YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])


# What about longitude and latitude?


# select an observation with an incident number?
which(xdmg$INCDTNO == "110058")
xdmg[which(xdmg$INCDTNO == "110058"),  c("YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")]
xdmg %>% select(INCDTNO,YEAR, MONTH, DAY, TIMEHR, TIMEMIN) %>% filter(INCDTNO == "110058")

# Are there any duplication in these selected columns?
duplicated(xdmg[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])

# Not duplicated
!(duplicated(xdmg[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")]))

# remove the duplicates

# base R
xdmgnd <- xdmg[!(duplicated(xdmg[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])),]
# tidyverse
#	keep all variables in .data. If a combination of ... is not distinct, this keeps the first row of values.
xdmgnd <- xdmg %>% distinct(INCDTNO,YEAR, MONTH, DAY, TIMEHR, TIMEMIN, .keep_all = TRUE)

# check out the number of duplicated entries
# dimensions of xdmg and xdmgnd
dim_desc(xdmg)
dim_desc(xdmgnd)

# number of duplicates
count(xdmg) - count(xdmgnd)


#*************************************************
#		3. Visualization
#      Look at the graphs of these extreme accidents
#*************************************************


#histogram
ggplot(as.data.frame(xdmg$ACCDMG), aes(xdmg$ACCDMG)) + geom_histogram()

# boxplot
xdmgbox <- ggplot(as.data.frame(xdmg$ACCDMG), aes(x=xdmg$ACCDMG)) + 
  geom_boxplot() + ggtitle("Accidents with Extreme Damage") +
  labs(x = "Cost ($)") + theme(plot.title = element_text(hjust = 0.5))

names(ggplot_build(xdmgbox)$data[[1]]) # note boxplot variables now have x instead of y in front

# time series
# summation of all damages per year
df <- xdmg %>% select(YEAR,ACCDMG) %>%  group_by(YEAR) %>% summarize(damages = sum(ACCDMG))
ggplot(data=df, aes(x=YEAR, y=damages)) + geom_line() + geom_point()


# also plot number of accidents per year.
ggplot(as.data.frame(table(totacts$YEAR)), aes(x = Var1, y= Freq)) + 
  geom_bar(stat="identity")+
  xlab("Year")

# Frequency of accident types
# compare with the totacts plot
# reorder its first argument as a categorical variable, 
#and reorders its levels based on the values of a second variable, usually numeric.
ggplot(as.data.frame(table(xdmg$TYPE)), aes(x = reorder(Var1,-Freq), y= Freq)) + 
  geom_bar(stat="identity")+
  xlab("Accident Type")


# SPM of metrics & train variables
ggpairs(xdmg[,c("ACCDMG", "TOTKLD", "TOTINJ", "TRNSPD")])
pairs.panels(xdmg[,c("ACCDMG", "TOTKLD", "TOTINJ", "TRNSPD")])


#*************************************************
#		3.1. Categorical variables
#*************************************************

# Cause
ggplot(data = xdmg, aes(x = Cause, y = ACCDMG)) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_grey(start = 0.5, end = 0.8) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Box Plots of Accident Damage by Cause") +
  labs(x = "Damage ($)", y = "Accident Cause")

# Repeat for Type of accident





#*************************************************
#		3.2. Use of jitter
#        Jittering for Visualizations Only
#        As mentioned before, jittering adds some random noise to data, 
#        which can be beneficial when we want to visualize data in a scatterplot. By using the jitter function, 
#        we can get a better picture of the true underlying relationship between two variables in a dataset
#*************************************************

bwplot(as.factor(YEAR)~jitter(ACCDMG, amount = 2.5e5), data = xdmg, main = "Box Plots of Extreme Accident Damage by Year (with jitter)", xlab = "Damage ($)", ylab = "Year")

ggplot(data = xdmg, aes(x = as.factor(YEAR), y = jitter(ACCDMG, amount = 2.5e5))) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_grey(start = 0.5, end = 0.8) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Box Plots of Extreme Accident Damage by Year (with jitter)") +
  labs(x = "Damage ($)", y = "Year")


par(mfrow = c(1,2))
boxplot(jitter(xdmg$ACCDMG, amount = 2.5e5), col = "steelblue", main = "Extreme Accident Damage with Jitter")
boxplot(xdmg$ACCDMG, col = "steelblue", main = "Extreme Accident Damage without Jitter")
par(mfrow = c(1,1))


p <- ggplot(as.data.frame(jitter(xdmg$ACCDMG, amount = 2.5e5)), aes(x = jitter(xdmg$ACCDMG, amount = 2.5e5))) +
  geom_boxplot(col= "steelblue") + ggtitle("Extreme Accident Damage with Jitter") +
  theme(plot.title = element_text(hjust = 0.5)) + coord_flip()

q <- ggplot(as.data.frame(xdmg$ACCDMG), aes(x=xdmg$ACCDMG)) +
  geom_boxplot(col= "steelblue") + ggtitle("Extreme Accident Damage without Jitter") +
  theme(plot.title = element_text(hjust = 0.5)) + coord_flip()

ggarrange(p, q, nrow = 1, ncol = 2)


#*************************************************
#		3.3. Conditioning on categorical variables
#*************************************************

# on Cause
xyplot(ACCDMG~TRNSPD | Cause, main = "Extreme Damage vs. Train Speed Conditioned on Cause", xlab = "Train Speed", ylab = "Total Accident Damage", data = xdmg)

# use facet_wrap for condition
qplot(ACCDMG, TRNSPD, data = xdmg) + facet_wrap(~ Cause, scales = "free") +
  ggtitle("Extreme Damage vs. Train Speed Conditioned on Cause") + 
  labs(x = "Total Accident Damage", y = "Total Accident Damage") +
  theme(plot.title = element_text(hjust = 0.5))

# on type of accident


# Repeat the above extreme point analysis but use TOTINJ + TOTKLD
# But wait until we do more cleaning


#*************************************************
#		3.4. Heatmaps for categorical variabels
#*************************************************
table(xdmg$Cause, xdmg$TYPE)

heatmap(table(xdmg$Cause, xdmg$TYPE), Rowv = NA, Colv = NA)
# With legend (optional)

heatmap.2(table(xdmg$Cause, xdmg$TYPE), Rowv = F, Colv = F)

source("http://www.phaget4.org/R/myImagePlot.R")
# if you are getting "invalid graphics state" error, try executing dev.off() on the Console
myImagePlot(table(xdmg$Cause, xdmg$TYPE), title = "No. of Accidents by Cause and Type of Accident")




