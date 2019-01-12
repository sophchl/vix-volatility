#' Create a dataset for the regression containing the averages and lagged RV and VIX data
#'
#' @param Data
#'
#' @return
#' @export
#'
#' @examples
RegressionDataRVVIX <- function(Data) {
  Df_regress <- Data
  Df_regress$day <- Df_regress$RealizedVolatility %>% lag(1)
  Df_regress$week  <- Df_regress$RealizedVolatility %>% rollapply(5,mean,na.rm = T) %>% lag(1)
  Df_regress$month <- Df_regress$RealizedVolatility %>% rollapply(22,mean,na.rm = T) %>% lag(1)
  Df_regress$dayVIX <- Df_regress$VIX.Close %>% lag(1)
  Df_regress$weekVIX  <- Df_regress$VIX.Close %>% rollapply(5,mean,na.rm = T) %>% lag(1)
  Df_regress$monthVIX <- Df_regress$VIX.Close %>% rollapply(22,mean,na.rm = T) %>% lag(1)
  Df_regress$crisis <- c( ifelse (year(Df_regress) >= 2008 & year(Df_regress) <= 2012, 1,0) )
  Df_regress <- na.omit(Df_regress)
return(Df_regress)
}
