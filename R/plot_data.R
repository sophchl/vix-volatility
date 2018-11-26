#' Plots and saves time series data, out of one variable
#'
#' @param variable the variance that should be plotted
#' @param title the title as it is written on top of the graph that is plotted
#' @param save_name the name under which the output should be saved, including file type (e.g.".png")
#'
#' @return a file to the latex written folder, in the format specified in save_name
#' @export
#'
#' @examples
#' plot_data1(Df$RealizedVariance, "Realized Variance", "var.png")
#'
plot_data1 <- function(variable, title, save_name) {
  plot <- autoplot(variable) +
    theme_classic() +
    ggtitle(title) +
    xlab("Year") +
    ylab(title)
  ggsave(save_name, plot = last_plot(), path = "written/pictures")
}



plot_data2 <- function(variable1, variable2, title, save_name) {
  plot <- autoplot(variable1, variable2) +
    theme_classic() +
    ggtitle(title) +
    xlab("Year") +
    ylab(title)
  return(plot)
}



