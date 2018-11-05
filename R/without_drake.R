## without drake
library(tidyverse)
library(tseries) # for handling ts data, e.g. plot.ts()
library(lubridate) # for nice dates
library(xts) # for handling ts data, e.g. with dates
library(readxl) # to read excel xls data
library(dplyr) # e.g. to use join
library(zoo) # basis of xts
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
Vol$WeeklyVariance <- ave(as.numeric(levels(Vol$RealizedVariance)[Vol$RealizedVariance]), Vol$yw, FUN=function(x) mean(x, na.rm=T)) # add weekly average
Vol$MonthlyVariance <- ave(as.numeric(levels(Vol$RealizedVariance)[Vol$RealizedVariance]), Vol$ym, FUN=function(x) mean(x, na.rm=T)) # add monthly average
Vol <- Vol[,-(3:4)] # remove week and month index

# Make VIX dataset ready
Vix1 <- Vix1_raw[,-(2:4)] # remove what I do not need
colnames(Vix1) <- c("Date", "VIX.Close")
Vix1$Date <- as.Date(Vix1$Date)
Vix1$VIX.Close <- as.numeric(as.character(Vix1$VIX.Close))
Vix2 <- Vix2_raw[,-(2:4)] # remove what I do not need
colnames(Vix2) <- c("Date", "VIX.Close")
Vix2$Date <- as.Date(Vix2$Date, format = "%m/%d/%Y")
Vix2$VIX.Close <- as.numeric(as.character(Vix2$VIX.Close))

# join the datasets and turn into time series
Vix2[1:5,]
tail(Vix2,5)
Vix <- rbind(Vix1,Vix2)
df_temp <- left_join(Vol, Vix, by = "Date") %>% as.data.frame()
df <- as.xts(df_temp[,-1], order.by=df_temp[,1]) # turn into xts object
rm(df_temp)

## observe data -------

par(mfrow = c(2,1))
plot(df$RealizedVariance, grid.col = NA, main = "Daily Realized Variance", yaxis.left = T)
plot(df$VIX.Close, grid.col = NA, main = "Daily VIX")
plot(df$WeeklyVariance, grid.col = NA, main = "Weekly Realized Variance")
plot(df$MonthlyVariance, grid.col = NA, main = "Monthly Realized Variance")



## generic -------
# # data type checks (generic execute whenever one wants)
# sapply(df.ts,mode)
# sapply(df.ts,class)
# sapply(df,mode)
# sapply(df,class)

# # net plot window
# par(mfrow = c(1,2))

# clear environment
# rm(list=ls())




