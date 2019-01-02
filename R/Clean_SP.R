#' Cleans the S&P500 Data
#'
#' @param infile a data.frame
#'
#' @return a data.frame with 2 columns: Date and S&P500
#' @export
#'
#' @examples
#' Clean_SP(SP_raw)
#'
Clean_SP <- function(infile) {
  SP <- infile[,c(1,4)] # column 1 is date, column 4 is Close S&P500
  colnames(SP) <- c("Date", "SP500")
  SP <- as.data.frame(SP)
  SP[,1] <- as.Date(SP[,1])
  return(SP)
}
