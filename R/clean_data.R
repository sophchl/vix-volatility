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
  colnames(Vol) <- c("Date", "RealizedVolatility") # rename columns
  Vol$Date <- as.Date(as.character(Vol$Date), "%Y%m%d") # turn Date into appropriate time-based object
  Vol$RealizedVolatility <- as.numeric(as.character(Vol$RealizedVolatility)) %>% sqrt() # turn RealizedVola (daily) to numeric -> Vola = sd
  Vol$RealizedVolatility <- Vol$RealizedVolatility*100
  return(Vol)
}


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
  Vix1 <- as.data.frame(Vix1) # turn Vix1 into data.frame (before: tbl.df, tbl, data.frame)
  return(Vix1)
}



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
  return(Vix2)
}



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
  SP <- infile[,c(1,4)]
  colnames(SP) <- c("Date", "SP500")
  SP <- as.data.frame(SP)
  SP[,1] <- as.Date(SP[,1])
  return(SP)
}


