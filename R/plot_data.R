#' Plots and saves time series data
#'
#' @param variable
#' @param title
#' @param save_name
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
