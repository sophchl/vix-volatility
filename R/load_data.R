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
  df <- read.csv2(infile)
  df
}
