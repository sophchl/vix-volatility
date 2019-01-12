Df_regress <- Df
Df_regress$day <- Df_regress$RealizedVolatility %>% lag(1)
Df_regress$week  <- Df_regress$RealizedVolatility %>% rollapply(5,mean,na.rm = T) %>% lag(1)
Df_regress$month <- Df_regress$RealizedVolatility %>% rollapply(22,mean,na.rm = T) %>% lag(1)
Df_regress$dayVIX <- Df_regress$VIX.Close %>% lag(1)
Df_regress$weekVIX  <- Df_regress$VIX.Close %>% rollapply(5,mean,na.rm = T) %>% lag(1)
Df_regress$monthVIX <- Df_regress$VIX.Close %>% rollapply(22,mean,na.rm = T) %>% lag(1)
Df_regress$crisis <- c( ifelse (year(Df_regress) >= 2008 & year(Df_regress) <= 2012, 1,0) )
Df_regress <- na.omit(Df_regress)

Df_regress_table <- as.data.frame(Df_regress)
Df_regress_table <- na.omit(Df_regress_table)

colnames(Df_regress_table) <- c("RV", "VIX", "SP500", "Returns", "Daily RV", "Weekly RV", "Monthly RV", "Daily VIX", "Weekly VIX", "Monthly VIX")

# summary table for all time periods

stargazer(Df_regress_table[,c(1,2, 6, 7, 9, 10)], summary.stat = c("n", "min", "max", "mean", "sd"), title = "summary statistics")

Df_regress_table_crisis <- Df_regress[which(year(Df_regress) >= 2008 & year(Df_regress) <= 2012)] %>% as.data.frame()
colnames(Df_regress_table_crisis) <- c("RV", "VIX", "SP500", "Returns", "Daily RV", "Weekly RV", "Monthly RV", "Daily VIX", "Weekly VIX", "Monthly VIX")
stargazer(Df_regress_table_crisis[,c(1,2, 6, 7, 9, 10)], summary.stat = c("n", "min", "max", "mean", "sd"), title = "summary statistics")

Df_regress_table_not_crisis <- Df_regress[which(year(Df_regress) <= 2007 | year(Df_regress) >= 2013)] %>% as.data.frame()
colnames(Df_regress_table_not_crisis) <- c("RV", "VIX", "SP500", "Returns", "Daily RV", "Weekly RV", "Monthly RV", "Daily VIX", "Weekly VIX", "Monthly VIX")
stargazer(Df_regress_table_not_crisis[,c(1,2, 6, 7, 9, 10)], summary.stat = c("n", "min", "max", "mean", "sd"), title = "summary statistics")


# table skewness and kurtosis

skewness(Df_regress_table[,c(1,2, 6, 7, 9, 10)]) %>% round(3)
kurtosis(Df_regress_table[,c(1,2, 6, 7, 9, 10)]) %>% round(3)

skewness(Df_regress_table_crisis[,c(1,2, 6, 7, 9, 10)]) %>% round(3)
kurtosis(Df_regress_table_crisis[,c(1,2, 6, 7, 9, 10)]) %>% round(3)

skewness(Df_regress_table_not_crisis[,c(1,2, 6, 7, 9, 10)]) %>% round(3)
kurtosis(Df_regress_table_not_crisis[,c(1,2, 6, 7, 9, 10)]) %>% round(3)

# the same for all the log values

Df_regress_table_log <- Df_regress_table
colnames(Df_regress_table_log) <- c("RV", "VIX", "SP500", "Returns", "Daily RV", "Weekly RV", "Monthly RV", "Daily VIX", "Weekly VIX", "Monthly VIX")
Df_regress_table_log$RV <- Df_regress_table_log$RV %>% log()
Df_regress_table_log$VIX <- Df_regress_table_log$VIX %>% log()
Df_regress_table_log$`Daily RV` <- Df_regress_table_log$`Weekly RV` %>% log()
Df_regress_table_log$`Monthly RV` <- Df_regress_table_log$`Monthly RV`%>% log()
Df_regress_table_log$`Weekly VIX` <- Df_regress_table_log$`Weekly VIX` %>% log()
Df_regress_table_log$`Monthly VIX` <- Df_regress_table_log$`Monthly VIX` %>% log()

# summary statistics all time periods

stargazer(Df_regress_table_log[,c(1,2, 6, 7, 9, 10)], summary.stat = c("n", "min", "max", "mean", "sd"), title = "summary statistics")

Df_regress_table_log_crisis <- Df_regress[which(year(Df_regress) >= 2008 & year(Df_regress) <= 2012)] %>% as.data.frame()
colnames(Df_regress_table_log_crisis) <- c("RV", "VIX", "SP500", "Returns", "Daily RV", "Weekly RV", "Monthly RV", "Daily VIX", "Weekly VIX", "Monthly VIX")
stargazer(Df_regress_table_log_crisis[,c(1,2, 6, 7, 9, 10)], summary.stat = c("n", "min", "max", "mean", "sd"), title = "summary statistics")

Df_regress_table_log_not_crisis <- Df_regress[which(year(Df_regress) <= 2007 | year(Df_regress) >= 2013)] %>% as.data.frame()
colnames(Df_regress_table_log_not_crisis) <- c("RV", "VIX", "SP500", "Returns", "Daily RV", "Weekly RV", "Monthly RV", "Daily VIX", "Weekly VIX", "Monthly VIX")
stargazer(Df_regress_table_log_not_crisis[,c(1,2, 6, 7, 9, 10)], summary.stat = c("n", "min", "max", "mean", "sd"), title = "summary statistics")

# table skewness and kurtosis

skewness(Df_regress_table_log[,c(1,2, 6, 7, 9, 10)]) %>% round(3)
kurtosis(Df_regress_table_log[,c(1,2, 6, 7, 9, 10)]) %>% round(3)

skewness(Df_regress_table_log_crisis[,c(1,2, 6, 7, 9, 10)]) %>% round(3)
kurtosis(Df_regress_table_log_crisis[,c(1,2, 6, 7, 9, 10)]) %>% round(3)

skewness(Df_regress_table_not_crisis[,c(1,2, 6, 7, 9, 10)]) %>% round(3)
kurtosis(Df_regress_table_not_crisis[,c(1,2, 6, 7, 9, 10)]) %>% round(3)


# correlation matrixes

cor(Df_regress_table)[c(1,2,5,6,7,8,9,10), c(1,2,5,6,7,8,9,10)] %>% stargazer()

# autocorrelation functions

head(Df_regress)
colnames(Df_regress) <- c("RV", "VIX", "SP500", "Returns", "DayRV", "WeekRV", "MonthRV", "DayVIX", "WeekVIX", "MonthVIX", "crisis")
Df_regress_temp <- Df_regress[,c(1,2,6,7,9,10)]
head(Df_regress_temp)

acf(Df_regress_temp)
pacf(Df_regress_temp)
class(Df_full)


# Plot the distributions against the normal distribution

ggplot(Df_frame, aes(x = Df_frame$RealizedVolatility)) + geom_density()

ggplot(Df_frame, aes(x = Df_frame$RealizedVolatility %>% log())) + geom_density()



# Augmented Dickey Fuller Test

adf.test(Df_full$RealizedVolatility) # p.value small, reject H0 of unit root at 99% level
adf.test(Df_full$VIX.Close)



head(Df_regress_table_not_crisis)
