#' Cleans the VIX data with date format YYYY-MM-DD
#'
#' @param infile
#'
#' @return a data.frame (tbl_df, tbl, data.frame) with Date (class: Date) and VIX.Close (class: numeric), VIX.Close keeps 2 decimals
#' @export
#'
#' @examples
#' Clean_Vix1(Vix1_raw)
Clean_Vix1 <- function(infile) {
  Vix1 <- infile[,-(2:4)] # remove what I do not need
  colnames(Vix1) <- c("Date", "VIX.Close") # rename columns
  Vix1$Date <- as.Date(Vix1$Date) # turn Date into appropriate time-based object
  Vix1$VIX.Close <- as.numeric(as.character(Vix1$VIX.Close)) # turn Close into numeric
  Vix1$VIX.Close <- Vix1$VIX.Close/sqrt(252) # turn from annualized to daily
  Vix1 <- as.data.frame(Vix1) # turn Vix1 into data.frame (before: tbl.df, tbl, data.frame)
  return(Vix1)
}
