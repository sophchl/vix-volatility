create_week <- function(x) {
  weekly_var <- vector()
  n <- 1
  while(n<length(x$RealizedVariance)) {
    weekly_var <- c(weekly_var,mean(as.numeric(levels(x$RealizedVariance[n:(n+4)])[x$RealizedVariance[n:(n+4)]]),na.rm=T))
    n<-n+5
  }
  rm(n)
}

create_month <- function(x) {
  monthly_var <- vector()
  n <- 1
  while(n<length(x$RealizedVariance)) {
    monthly_var <- c(weekly_var,mean(as.numeric(levels(x$RealizedVariance[n:(n+4)])[x$RealizedVariance[n:(n+4)]]),na.rm=T))
    n<-n+5
  }
  rm(n)
}


