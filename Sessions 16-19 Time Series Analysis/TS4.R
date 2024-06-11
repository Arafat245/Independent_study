#************************************************************
#
#      	Session 20
#			Time Series Analysis 4 - Multivariate
#
#************************************************************


#*****************************
#
# Load the data & source files
#
#*****************************
datadir <- "J:/private/SYS4021/2021/Data/VAweather/"
sourcedir <-"J:/private/SYS4021/2021/R_Code"

library(mtsdi)
library(MTS)
library(forecast)
library(ggplot2)
library(lubridate)
library(ggfortify)
library(ggpubr)
library(tseries)
library(tidyverse)

##Load the Virginia weather data
setwd(datadir)
VAweather <- read.table('VirginiaWeatherData.csv',header=T,sep=',')
setwd(sourcedir)

#Replace -999s with NAs
VAweather[VAweather == -999.0] <- NA
VAweather <- VAweather[1:(dim(VAweather)[1]-7),]

# create date column for plotting
date <- VAweather %>% select(Year, Month) %>% mutate(date = make_datetime(Year,Month))
VAweather$date <- date$date

# Impute NAs
f <- ~ C_Precip + C_Tmax + C_Tmin + R_Precip + R_Tmax + R_Tmin
t <- c(seq(1,dim(VAweather)[1]))
imputedData <- mnimput(f, VAweather[,3:8], eps=1e-3, ts=TRUE, method="gam", 
                       ga.control=list(formula=paste(names(VAweather[,3:8]),'~ns(t,6)')))
VAweather[,3:8] <- imputedData$filled.dataset

# Create time series of monthly Richmond precipitation and minimum temperature
Richmond.Tmin <- VAweather$R_Tmin
Richmond.Tmax <- VAweather$R_Tmax

##Use the ts() command to get a time series of Richmond.Tmin
Tmin.ts<-ts(Richmond.Tmin)

##Use the ts() command to get a time series of Richmond.Tmax
Tmax.ts<-ts(Richmond.Tmax)


#************************************************
#
# Simulating from Time Series Models
#
#************************************************

# Build univariate time series of min and max Richmond temperature
time.temp <- c(1:(length(Tmin.ts)-6))
Tmin.lm <- lm(Tmin.ts[time.temp] ~ time.temp + sin(2*pi*time.temp/12) + 
                cos(2*pi*time.temp/12))
Tmax.lm <- lm(Tmax.ts[time.temp] ~ time.temp + sin(2*pi*time.temp/12) + 
                cos(2*pi*time.temp/12))

# Build arima models to residuals to Tmin.lm and Tmax.lm
e.Tmin.lm <- auto.arima(Tmin.lm$residuals,approximation=FALSE)
e.Tmax.lm <- auto.arima(Tmax.lm$residuals,approximation=FALSE)

summary(e.Tmin.lm) # ARIMA(2,0,1)
summary(e.Tmax.lm) # ARIMA(1,0,0)

# See if the residuals are correlated
allResiduals <- data.frame(Tmin.lm$residuals, Tmax.lm$residuals)
colnames(allResiduals) <- c("Tmin","Tmax")
cor(allResiduals)

# Simulate 50 years of monthly minimum and maximum temperature residuals
e.Tmin.sim <- arima.sim(n=12*50, list(ar=c(e.Tmin.lm$coef[1],e.Tmin.lm$coef[2]), ma=c(e.Tmin.lm$coef[3])), 
                        sd=sqrt(e.Tmin.lm$sigma2))
e.Tmax.sim <- arima.sim(n=12*50, list(ar=c(e.Tmax.lm$coef[1])), sd=sqrt(e.Tmax.lm$sigma2))

# See if the simulated residuals are correlated
allSimulations <- data.frame(e.Tmin.sim, e.Tmax.sim)
colnames(allSimulations) <- c("Tmin","Tmax")
cor(allSimulations)


#************************************************
#
# Multivariate Time Series Models
#
#************************************************

# Build VARMA model to minimum and maximum temperature residuals
AICmatrix <- matrix(NA, 3, 4)
for(p in 1:3){ # rows of AICmatrix
  for(q in 0:3){ # columns of AICmatrix
    varma.model <- VARMACpp(allResiduals, p=p, q=q, include.mean=F)
    AICmatrix[p,q+1] <- varma.model$aic
  }
}

# Pick the model with the lowest AIC
AICmatrix

varma.model <- VARMACpp(allResiduals, p=1, q=0, include.mean=F)

# Simulate 50 years of monthly temperature residuals from VARMA model
T.sim = VARMAsim(12*50,phi=varma.model$Phi,theta=varma.model$Theta,sigma=varma.model$Sigma)

# Compare correlation of simulated residuals to actual residuals
cor(T.sim$series)
cor(allResiduals)

# Add mean predictions and plot simulation of Tmin and Tmax
time.50yr <- c(1:(50*12))
time.50yr.df <- data.frame(time.temp = time.50yr)

Tmin.mean <- predict(Tmin.lm, newdata=time.50yr.df)
Tmax.mean <- predict(Tmax.lm, newdata=time.50yr.df)

ggplot() + 
  geom_line(aes(x=time.temp[1:length(e.Tmin.sim)],y=T.sim$series[,1] + Tmin.mean),color="blue") + 
  geom_line(aes(x=time.temp[1:length(e.Tmax.sim)],y=T.sim$series[,2] + Tmax.mean),color="red") + 
  xlab("Time Step") + ylab("Temperature")

# Diagnostics
# compute fitted values (true - residual; lose 1st observation because p=1)
Tmin.fitted = allResiduals[2:dim(allResiduals)[1],1] - varma.model$residuals[,1]
Tmax.fitted = allResiduals[2:dim(allResiduals)[1],2] - varma.model$residuals[,2]

# Residuals vs Fitted
Tmin_resid_v_fitted = ggplot() + geom_point(aes(x=Tmin.fitted+Tmin.lm$fitted.values[2:length(Tmin.lm$fitted.values)], 
                          y=varma.model$residuals[,1])) +
                          xlab("Tmin Fitted Values") + ylab("Tmin Residuals")

Tmax_resid_v_fitted = ggplot() + geom_point(aes(x=Tmax.fitted+Tmax.lm$fitted.values[2:length(Tmax.lm$fitted.values)], 
                          y=varma.model$residuals[,2]))  +
                          xlab("Tmax Fitted Values") + ylab("Tmax Residuals")

ggarrange(Tmin_resid_v_fitted, Tmax_resid_v_fitted, nrow=2, ncol=1)

# QQ plot of residuals
TminQQ = qplot(sample=varma.model$residuals[,1]) +
  stat_qq_line(color="red") + ggtitle("Tmin Residuals QQ")

TmaxQQ = qplot(sample=varma.model$residuals[,2]) +
  stat_qq_line(color="red") + ggtitle("Tmax Residuals QQ")

ggarrange(TminQQ, TmaxQQ, nrow=2, ncol=1)

# independence of residuals
MTSdiag(varma.model)

# Build another model (next best AIC) and compare diagnostics
varma.model2 <- VARMACpp(allResiduals, p=2, q=0, include.mean=F)
MTSdiag(varma.model2)

#The Ljung-Box statistic is different from 0 for all lags in this model, 
#so we choose the previous.

# Forecast the next 6 months with the first model
# Use VARMApred to forecast the next 6 months of temperature residuals
varma.fcst <- VARMApred(varma.model, h=6)

# Prediction for the next 6 months:

# The test period in months
next.6mo.time <- c((length(Tmin.ts)-5):(length(Tmin.ts)))

# The test data frame
next.6mo.Tmin <- data.frame(time.temp = next.6mo.time, Tmin = Tmin.ts[next.6mo.time])
next.6mo.Tmax <- data.frame(time.temp = next.6mo.time, Tmax = Tmax.ts[next.6mo.time])

# The actual time series for the test period
next.6mo.Tmin.ts <- ts(next.6mo.Tmin$Tmin)
next.6mo.Tmax.ts <- ts(next.6mo.Tmax$Tmax)

E_Y.pred.Tmin <- predict(Tmin.lm, newdata=next.6mo.Tmin) # mean from linear model
e_t.pred.Tmin <- varma.fcst$pred[,1]
e_t.pred.Tmin.lower <- varma.fcst$pred[,1] - 1.96*varma.fcst$se.err[,1]
e_t.pred.Tmin.upper <- varma.fcst$pred[,1] + 1.96*varma.fcst$se.err[,1]
next.6mo.prediction.Tmin <- E_Y.pred.Tmin + e_t.pred.Tmin

E_Y.pred.Tmax <- predict(Tmax.lm, newdata=next.6mo.Tmin) # mean from linear model
e_t.pred.Tmax <- varma.fcst$pred[,2]
e_t.pred.Tmax.lower <- varma.fcst$pred[,2] - 1.96*varma.fcst$se.err[,2]
e_t.pred.Tmax.upper <- varma.fcst$pred[,2] + 1.96*varma.fcst$se.err[,2]
next.6mo.prediction.Tmax <- E_Y.pred.Tmax + e_t.pred.Tmax

# MSE:

mean((next.6mo.prediction.Tmin-next.6mo.Tmin$Tmin)^2)
mean((next.6mo.prediction.Tmax-next.6mo.Tmax$Tmax)^2)

# Plot actual values and predicted values
Tmin.predictions <- ggplot() + 
  geom_line(aes(x=next.6mo.Tmin$time.temp,y=next.6mo.Tmin$Tmin),color="black") + 
  geom_line(aes(x=next.6mo.Tmin$time.temp,y=next.6mo.prediction.Tmin),color="red") + 
  geom_line(aes(x=next.6mo.Tmin$time.temp,y=E_Y.pred.Tmin + e_t.pred.Tmin.lower),
            color="red",linetype="dashed") + 
  geom_line(aes(x=next.6mo.Tmin$time.temp,y=E_Y.pred.Tmin + e_t.pred.Tmin.upper),
            color="red",linetype="dashed") +
  xlab("") + ylab("Richmond Tmin") + 
  ggtitle("Tmin Trend and Seasonality Model + VARMA of Residuals")

Tmax.predictions <- ggplot() + 
  geom_line(aes(x=next.6mo.Tmax$time.temp,y=next.6mo.Tmax$Tmax),color="black") + 
  geom_line(aes(x=next.6mo.Tmax$time.temp,y=next.6mo.prediction.Tmax),color="red") + 
  geom_line(aes(x=next.6mo.Tmax$time.temp,y=E_Y.pred.Tmax + e_t.pred.Tmax.lower),
            color="red",linetype="dashed") + 
  geom_line(aes(x=next.6mo.Tmax$time.temp,y=E_Y.pred.Tmax + e_t.pred.Tmax.upper),
            color="red",linetype="dashed") +
  xlab("") + ylab("Richmond Tmin") + 
  ggtitle("Tmax Trend and Seasonality Model + VARMA of Residuals")

ggarrange(Tmin.predictions,Tmax.predictions,nrow=2,ncol=1)
