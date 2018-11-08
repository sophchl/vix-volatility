# # # with data.table
#
# library(data.table)
# df <-as.data.table(df_raw[-(1:2),1:2])
# colnames(df) <- c("Date", "RealizedVariance")
#
# # add mean per week and per month for data.table
# as.Date(as.character(df$Date), "%Y%m%d")
# df$Date <- as.Date(as.character(df$Date), "%Y%m%d")
# df <- df %>% group_by (yw = paste(year(Date), week(Date), sep = "-"))
# df <- df %>% group_by( ym = paste(year(Date), month(Date), sep = "-"))
#
# # # with factor instead of numeric
#
# # add realized variance on weekly and monthly basis: average over respective period
# # weekly
# mean(as.numeric(levels(df2$RealizedVariance[1:5])[df2$RealizedVariance[1:5]]),na.rm=T) # mean of first week
#
# Vol$WeeklyVariance <- ave(as.numeric(levels(Vol$RealizedVariance)[Vol$RealizedVariance]), Vol$yw, FUN=function(x) mean(x, na.rm=T)) # add weekly average
# Vol$MonthlyVariance <- ave(as.numeric(levels(Vol$RealizedVariance)[Vol$RealizedVariance]), Vol$ym, FUN=function(x) mean(x, na.rm=T)) # add monthly average
#
# weekly_var <- vector()
# n <- 1
# while(n<length(df2$RealizedVariance)) {
#   weekly_var <- c(weekly_var,mean(as.numeric(levels(df2$RealizedVariance[n:(n+4)])[df2$RealizedVariance[n:(n+4)]]),na.rm=T))
#   n<-n+5
# }
# rm(n)
#
# # monthly
# monthly_var <- vector()
# n <- 1
# while(n<length(df2$RealizedVariance)) {
#   weekly_var <- c(weekly_var,mean(as.numeric(levels(df2$RealizedVariance[n:(n+4)])[df2$RealizedVariance[n:(n+4)]]),na.rm=T))
#   n<-n+5
# }
# rm(n)


