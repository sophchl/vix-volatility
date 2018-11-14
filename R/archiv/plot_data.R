#' plots time series data
#'
#' @param x the input we want to visualize
#'
#' @return
#' @export
#'
#' @examples
plot_data_ts <- function(infile) {
  par(mfrow = c(2,1))
  plot(df$VIX.Close, grid.col = NA, main = "Daily VIX")
  plot(df$RealizedVariance, grid.col = NA, main = "Daily Realized Variance", yaxis.left = T)
  plot(df$WeeklyVariance, grid.col = NA, main = "Weekly Realized Variance")
  plot(df$MonthlyVariance, grid.col = NA, main = "Monthly Realized Variance")
}
