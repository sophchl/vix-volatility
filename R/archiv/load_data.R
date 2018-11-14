#' Loads data from csv files
#'
#' @param infile the datafile
#'
#' @return a data.frame df of the file
#' @export
#'
#' @examples
#' infile <- "data-raw/sandp500.csv"
#' load_data(infile)
load_data <- function(infile) {
  df <- read.csv2(infile, sep = ",")
  df
}


#' cleans data (removes all columns and lines that are irrelevant)
#'
#' @param infile the data.frame
#'
#' @return a data.dable containing only date and Realized Variance based on 5min intervals
#' @export
#'
#' @examples
clean_data <- function(x){
  df2 <-as.data.table(x[-(1:2),1:2])
  colnames(df2) <- c("Date", "RealizedVariance")
  df2
}
