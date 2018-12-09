## packages ------
library(tidyverse)
library(tseries) # for handling ts data, e.g. plot.ts()
library(lubridate) # for nice dates
library(xts) # for handling ts data, e.g. with dates
library(readxl) # to read excel xls data
library(dplyr) # e.g. to use join
library(zoo) # basis of xts
library(stats) # e.g. for lagging time series
library(Hmisc) # lag
library(texreg) # to save regression results
# library(quantmod) # e.g. function Next to advance time series
# library(lattice)

## load and clean data -------------------------------

# load data (load function)
Vol_raw <- read.csv2("data-raw/OxfordManRealizedVolatilityIndices0.2.csv", sep = ",", dec = ".", na.strings=c("","NA"))
Vix1_raw <- read_excel("data-raw/vixarchive.xls")
Vix2_raw <- read.csv2("data-raw/vixcurrent.csv", sep = ",")

# Make Volatility Dataset Ready
Vol <- (Vol_raw[-(1:2),1:2]) # take out columns and rows I do not need
colnames(Vol) <- c("Date", "RealizedVariance") # rename columns
Vol$Date <- as.Date(as.character(Vol$Date), "%Y%m%d") # turn Date into appropriate time-based object
Vol$RealizedVariance <- as.numeric(as.character(Vol$RealizedVariance)) # turn RealizedVola (daily) to numeric
rm(Vol_raw)

# Make VIX dataset ready (2 datasets as in 2001 they opened a new dataset)
Vix1 <- Vix1_raw[,-(2:4)] # remove what I do not need
colnames(Vix1) <- c("Date", "VIX.Close") # rename columns
Vix1$Date <- as.Date(Vix1$Date) # turn Date into appropriate time-based object
Vix1$VIX.Close <- as.numeric(as.character(Vix1$VIX.Close)) # turn Close into numeric
Vix1 <- as.data.frame(Vix1) # turn Vix1 into data.frame (before: tbl.df, tbl, data.frame)
Vix2 <- Vix2_raw[,-(2:4)] # remove what I do not need
colnames(Vix2) <- c("Date", "VIX.Close") # rename columns
Vix2$Date <- as.Date(Vix2$Date, format = "%m/%d/%Y") # turn Date into appropriate time-based object
Vix2$VIX.Close <- as.numeric(as.character(Vix2$VIX.Close)) # turn Close into numeric
rm(Vix1_raw)
rm(Vix2_raw)

# join all the datasets and turn into time series
head(Vix1)
tail(Vix2)
Vix <- rbind(Vix1,Vix2) # combine the vix datasets to one big one
df_temp <- left_join(Vol, Vix, by = "Date") %>% as.data.frame() # attach the Vix dataset to the Vol -> drop the Vix which have no Vol associated
Df <- as.xts(df_temp[,-1], order.by=df_temp[,1]) # turn into xts object, remove first column with obersvation number
# Df_ts <- ts(df_temp, freq = 365) # second version: turn into ts object
rm(df_temp)

## plot data -------------------------------

par(mfrow = c(2,1))
plot(Df$VIX.Close, grid.col = NA, main = "Daily VIX")
plot(Df$RealizedVariance, grid.col = NA, main = "Daily Realized Variance", yaxis.left = T)

plot_vol <- autoplot(Df$RealizedVariance) +
  theme_classic() +
  ggtitle("Daily Realized Variance") +
  xlab("Year") +
  ylab("Variance")
# ggsave("Vol.png", plot = last_plot(), path = "written/pictures")

plot_vix <- autoplot(Df$VIX.Close) +
  theme_classic() +
  ggtitle("Daily VIX (Close)") +
  xlab("Year") +
  ylab("VIX")
# ggsave("VIX.png", plot = last_plot(), path = "written/pictures")

# graphic analysis shows large peak in 2009, otherwise smaller peaks -> linear model good?

## time series observation ----

# basic time series observation
index(Df)
start(Df)
end(Df)
frequency(Df)
periodicity(Df)
nmonths(Df)
str(Df)

# use endpoints to calculate period values -> rollapply works better I think
ep1 <- endpoints(Df,on="weeks",k=2)
ep2 <- endpoints(Df,on="months")
period.apply(Df$RealizedVolatility, INDEX=ep1, FUN=mean)
period.apply(Df$RealizedVolatility,INDEX=ep2,FUN=mean)

# check for stationarity: decompose time series
# stationarity: mean is constant over time, variance does not increase over time, seasonality effect is minimal

decompose(Df_full, type = "mult") # should decompose time series in trend, season and irregular component
decompose(log(Df$SP500))

# seasonal decomposition
stl(log(Df_full$RealizedVolatility), s.window="months")
stl(Df$SP500)

# cound not find a seasonality with both tests

# p-value: probability, that when the H0 is true, this statistical summary or more extreme values would be observed
# check stationarity with augmented dickey fuller test
adf.test(na.omit(Df$RealizedVolatility)) # looks stationary (reject H0 that it is not stationary)
adf.test(Df$SP500) # does not look stationary (can not reject H0 that it is stationary)
adf.test(Df$VIX.Close) # looks stationary (reject H0 that it is not stationary)
adf.test(na.omit(diff(log(Df$SP500)))) # difference (stabilize trend) of log (stabilize variance) is stationary
rollapply(data= Df$RealizedVolatility,width = 900 ,FUN = mean,na.rm = T) %>% range(na.rm =T) # range of mean is pretty large

# plot ACF and PACF
# PACF: correlation between a variable and a lag of itself that is not explained by correlations at all lower-order-lags (lower-order: close to time t)
acf(Df_full)
pacf(Df_full)
acf(Df_full$RealizedVolatility) # tail off
pacf(Df_full$RealizedVolatility) # cutt off around 9
Df$RealizedVolatility %>% rollapply(5,mean,na.rm = T) %>% na.omit() %>% acf
Df$RealizedVolatility %>% rollapply(5,mean,na.rm = T) %>% na.omit() %>% pacf
Df$RealizedVolatility %>% rollapply(20,mean,na.rm = T) %>% na.omit() %>% acf
Df$RealizedVolatility %>% rollapply(20,mean,na.rm = T) %>% na.omit() %>% pacf

# plot, take into account trend and seasonality
plot(Df$SP500)
plot(log(Df$SP500))
plot(diff(log(Df$SP500))) # quasi log(1+r) -> should be distributed log-normally
plot(density(na.omit(diff(log(Df$SP500)))))


## regress data -------------------------------

## tested 2 Alternatives, both seem to give the same result (which is positive as I expected from theory)

# Alternative 1: RV(t+1) = RV(t) + RV(w, incl.t) + RV(m, incl.t) + VIX(t)

lm1 <- lm(Hmisc::Lag(Df$RealizedVariance, shift=-1) ~ Df$RealizedVariance + Df$RealizedVariance %>% rollapply(5,mean,na.rm = T) + Df$RealizedVariance %>% rollapply(20,mean,na.rm = T))
summary(lm1)
lm2 <- lm(Hmisc::Lag(Df$RealizedVariance, shift=-1) ~ Df$RealizedVariance + Df$RealizedVariance %>% rollapply(5,mean,na.rm = T) + Df$RealizedVariance %>% rollapply(20,mean,na.rm = T) + Df$VIX.Close)
summary(lm2)

# Alternative 2: Rv(t) = RV(t-1) + RV(w, starting t-1) + RV(m, starting t-1) + VIX(t-1)

lm1b <- lm(Df$RealizedVariance ~ Df$RealizedVariance %>% lag(1) + Df$RealizedVariance %>% rollapply(5,mean,na.rm = T) %>% lag(1) + Df$RealizedVariance %>% rollapply(20,mean,na.rm = T) %>% lag(1))
summary(lm1b)
lm2b <- lm(Df$RealizedVariance ~ Df$RealizedVariance %>% lag(1) + Df$RealizedVariance %>% rollapply(5,mean,na.rm = T) %>% lag(1) + Df$RealizedVariance %>% rollapply(20,mean,na.rm = T) %>% lag(1) + Df$VIX.Close %>% lag(1))
summary(lm2b)


## HAR-RV model  ----------

## in order to fit an AR model, the data has to be stationary -> augmented dickey fuller test pointed to stationarity

# determine the order of the lags: look at ACF and PACF

# in time series observation, I found out

ARModel <- ar(Df_full, aic = TRUE, order.max = NULL,
              method = "yule-walker")

ARModel2 <- sarima(Df_full$RealizedVolatility, 1,0,0) #1 = AR order, 0 = differince order, 0 = MA order
# I have super high residual correlation with AR(1)
ARModel2$ttable

Df$RealizedVariance <- Df$RealizedVolatility * Df$RealizedVolatility
Df_full <- na.omit(Df)
HARModel1 <- harModel(Df_full$RealizedVariance, periods = c(1,5,22), RVest = c("rCov"), type = "HARRV")
HARModel1 %>% summary()


## generic -------
# # data type checks (generic execute whenever one wants)
# sapply(df.ts,mode)
# sapply(df.ts,class)
# sapply(df,mode)
# sapply(df,class)

# set plot window
# par(mfrow = c(1,2))

# clear environment
# rm(list=ls())

# look at ts
# start(df$RealizedVariance)
# end(df$RealizedVariance)
# frequency(df$RealizedVariance)
# periodicity(df$RealizedVariance)
#
# # testing the lag : works
# lag_day <- lag(df$RealizedVariance,1)
# lag_day[1:10]
# df$RealizedVariance[1:10]
#
# lag_week <- lag(df$WeeklyVariance,5)
# lag_week[1:20]
# df$WeeklyVariance[1:20]
#

