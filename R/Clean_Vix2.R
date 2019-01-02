#' Cleans the VIX data with date format D/M/YYYY
#'
#' @param infile
#'
#' @return a data.frame with Date (class: Date) and VIX.Close (class: numeric), VIX.Close keeps 2 decimals
#' @export
#'
#' @examples
#' Clean_Vix2(Vix2_raw)
Clean_Vix2 <- function(infile) {
  Vix2 <- infile[,-(2:4)] # remove what I do not need
  colnames(Vix2) <- c("Date", "VIX.Close") # rename columns
  Vix2$Date <- as.Date(Vix2$Date, format = "%m/%d/%Y") # turn Date into appropriate time-based object
  Vix2$VIX.Close <- as.numeric(as.character(Vix2$VIX.Close)) # turn Close into numeric
  Vix2$VIX.Close <- Vix2$VIX.Close/sqrt(252) # turn from annualized to daily
  return(Vix2)
}
