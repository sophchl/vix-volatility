## without drake
library(tidyverse)
library(tseries) # for handling ts data, e.g. plot.ts()
library(lubridate) # for nice dates
library(xts) # for handling ts data, e.g. with dates
library(readxl) # to read excel xls data
library(dplyr) # e.g. to use join
library(zoo) # basis of xts
library(stats) # e.g. for lagging time series
# library(lattice)

## load and clean data----------------

# load data (load function)
Vol_raw <- read.csv2("data-raw/OxfordManRealizedVolatilityIndices0.2.csv", sep = ",", dec = ".", na.strings=c("","NA"))
Vix1_raw <- read_excel("data-raw/vixarchive.xls")
Vix2_raw <- read.csv2("data-raw/vixcurrent.csv", sep = ",")

# Make Volatility Dataset Ready
Vol <- (Vol_raw[-(1:2),1:2]) # take out columns and rows I do not need
colnames(Vol) <- c("Date", "RealizedVariance") # rename columns
Vol$Date <- as.Date(as.character(Vol$Date), "%Y%m%d") # turn Date into appropriate time-based object
Vol$RealizedVariance <- as.numeric(as.character(Vol$RealizedVariance)) # turn RealizedVola (daily) to numeric
Vol <- Vol %>% group_by (yw = paste(year(Date), week(Date), sep = "")) # add unique week index (temp)
Vol <- Vol %>% group_by(ym = paste(year(Date), month(Date), sep = "")) # add unique month index (temp)
Vol$yw <- as.numeric(Vol$yw) # turn the unique week index in numeric
Vol$ym <- as.numeric(Vol$ym) # turn the unique month index in numeric
Vol$WeeklyVariance <- ave(Vol$RealizedVariance, Vol$yw, FUN=function(x) mean (x,na.rm = T))
Vol$MonthlyVariance <- ave(Vol$RealizedVariance, Vol$ym, FUN=function(x) mean (x,na.rm = T))
# Vol <- Vol[,-(3:4)] # remove week and month index

# Make VIX dataset ready (2 datasets as in 2001 they opened a new dataset)
Vix1 <- Vix1_raw[,-(2:4)] # remove what I do not need
colnames(Vix1) <- c("Date", "VIX.Close") # rename columns
Vix1$Date <- as.Date(Vix1$Date) # turn Date into appropriate time-based object
Vix1$VIX.Close <- as.numeric(as.character(Vix1$VIX.Close)) # turn Close into numeric
Vix2 <- Vix2_raw[,-(2:4)] # remove what I do not need
colnames(Vix2) <- c("Date", "VIX.Close") # rename columns
Vix2$Date <- as.Date(Vix2$Date, format = "%m/%d/%Y") # turn Date into appropriate time-based object
Vix2$VIX.Close <- as.numeric(as.character(Vix2$VIX.Close)) # turn Close into numeric

# join the datasets and turn into time series
Vix2[1:5,]
tail(Vix2,5)
Vix <- rbind(Vix1,Vix2) # combine the vix datasets to one big one
df_temp <- left_join(Vol, Vix, by = "Date") %>% as.data.frame() # attach the Vix dataset to the Vol -> drop the Vix which have no Vol associated
df <- as.xts(df_temp[,-1], order.by=df_temp[,1]) # turn into xts object
df_ts <- ts(df_temp, freq = 365) # second version: turn into ts object
rm(df_temp)

## observe data -------

par(mfrow = c(2,1))
plot(df$VIX.Close, grid.col = NA, main = "Daily VIX")
plot(df$RealizedVariance, grid.col = NA, main = "Daily Realized Variance", yaxis.left = T)
plot(df$WeeklyVariance, grid.col = NA, main = "Weekly Realized Variance")
plot(df$MonthlyVariance, grid.col = NA, main = "Monthly Realized Variance")
# graphic analysis shows large peak in 2009, otherwise smaller peaks -> linear model good?

## check for stationarity: decompose time series ----
# stationarity: mean is constant over time, variance does not increase over time, seasonality effect is minimal

decomposedR <- decompose(df, type = "mult") # soll Zeitreihe in Trend, Saison und irreguläre Komponente zerlegen

## regress data ----

lm1 <- lm(df$RealizedVariance ~ df$RealizedVariance %>% lag(1) + df$RealizedVariance %>% rollapply(5,mean,na.rm = T) + df$RealizedVariance %>% rollapply(20,mean,na.rm = T))
summary(lm1)
lm2 <- lm(df$RealizedVariance ~ df$RealizedVariance %>% lag(1) + df$RealizedVariance %>% rollapply(5,mean,na.rm = T) + df$RealizedVariance %>% rollapply(20,mean,na.rm = T) + df$VIX.Close %>% lag(1))
summary(lm2)







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

