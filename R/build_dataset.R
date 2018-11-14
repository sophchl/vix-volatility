#' build one comprehensive dataset as a time series (in 2 formats)
#'
#' @param infile1: VIx1
#' @param infile2: Vix2
#' @param infile3: Vix3
#'
#' @return 2 time.series objects, df (class: xts, zoo - from xts) and df_ts (class: mts, ts, matrix - from ts)
#' @export
#'
#' @examples
#' Build_df(Vix1, Vix2, Vol)
Build_df <- function(infile1, infile2, infile3) {
  Vix <- rbind(infile1,infile2) # combine the vix datasets to one big one
  df_temp <- left_join(infile3, Vix, by = "Date") %>% as.data.frame() # attach the Vix dataset to the Vol -> drop the Vix which have no Vol associated
  Df <- as.xts(df_temp[,-1], order.by=df_temp[,1]) # turn into xts object,remove first column with observation number
  return(Df)
}

