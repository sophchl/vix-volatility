# # # with data.table--------------------------
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
# # # with factor instead of numeric --------------------------
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
#
# # # monthly and weekly averages (non-moving) (first part belongs to creation of dataset, second to graphics) -------------

# Vol <- Vol %>% group_by (yw = paste(year(Date), week(Date), sep = "")) # add unique week index (temp)
# Vol <- Vol %>% group_by(ym = paste(year(Date), month(Date), sep = "")) # add unique month index (temp)
# Vol$yw <- as.numeric(Vol$yw) # turn the unique week index in numeric
# Vol$ym <- as.numeric(Vol$ym) # turn the unique month index in numeric
# Vol$WeeklyVariance <- ave(Vol$RealizedVariance, Vol$yw, FUN=function(x) mean (x,na.rm = T)) # create weekly average
# Vol$MonthlyVariance <- ave(Vol$RealizedVariance, Vol$ym, FUN=function(x) mean (x,na.rm = T)) # create monthly average
# Vol <- Vol[,-(3:4)] # remove week and month index
#
# plot(Df$WeeklyVariance, grid.col = NA, main = "Weekly Realized Variance")
# plot(Df$MonthlyVariance, grid.col = NA, main = "Monthly Realized Variance")


