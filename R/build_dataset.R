#' build one comprehensive dataset as a time series
#'
#' @param infile1: VIx1
#' @param infile2: Vix2
#' @param infile3: Vol
#' @param infile4: SP
#'
#' @return a time.series object, Df (class: xts, zoo - from xts)
#' @export
#'
#' @examples
#' Build_df(Vix1, Vix2, Vol, SP)
#'
Build_df <- function(infile1, infile2, infile3, infile4) {
  Vix <- rbind(infile1,infile2) # combine the vix datasets to one big one (vertical)
  df_temp <- left_join(infile3, Vix, by = "Date") %>% as.data.frame() # attach the Vix dataset to the Vol -> drop the Vix which have no Vol associated
  df_temp <- inner_join(df_temp, infile4, by = "Date") # attach the SP500 to the dataset -> drop the SP wich have no Vol and Vix associated
  Df <- as.xts(df_temp[,-1], order.by=df_temp[,1]) # turn into xts object, remove first column with observation number
  Df$Returns <- CalculateReturns(Df$SP500, method = "log")
  return(Df)
}



#' build one comprehensive dataset as a data.frame (same as Build_df1 just missing the last step)
#'
#' @param infile1 Vix1
#' @param infile2 Vix2
#' @param infile3 Vol
#' @param infile4: SP
#'
#' @return a dataframe
#' @export
#'
#' @examples
#' Build_df1(Vix1, Vix2, Vol, SP)
#'
Build_df2 <- function(infile1, infile2, infile3, infile4) {
  Vix <- rbind(infile1,infile2) # combine the vix datasets to one big one
  df_temp <- left_join(infile3, Vix, by = "Date") %>% as.data.frame() # attach the Vix dataset to the Vol -> drop the Vix which have no Vol associated
  df_temp <- inner_join(df_temp, infile4, by = "Date") # attach the SP500 to the dataset -> drop the SP wich have no Vol and Vix associated
  return(df_temp)
}

