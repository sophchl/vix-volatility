#' Cleans the Volatility data
#'
#' @param infile
#'
#' @return a data.frame with a Date (class: Date) and a Volatility (Volatility, class: numeric)
#' @export
#'
#' @examples
#' Clean_Vol(Vol_raw)
Clean_Vol <- function(infile) {
  Vol <- (infile[-(1:2),1:2]) # take out columns and rows I do not need
  colnames(Vol) <- c("Date", "RealizedVariance") # rename columns
  Vol$Date <- as.Date(as.character(Vol$Date), "%Y%m%d") # turn Date into appropriate time-based object
  #Vol$RealizedVariance <- as.numeric(as.character(Vol$RealizedVariance)) * 100 * 100
  Vol$RealizedVolatility <- as.numeric(as.character(Vol$RealizedVariance)) %>% sqrt() # turn RealizedVola (daily) to numeric -> Vola = sd
  Vol$RealizedVolatility <- Vol$RealizedVolatility*100
  Vol <- Vol[,c(1,3)]
  return(Vol)
}
