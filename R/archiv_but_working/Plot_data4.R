#' plots and saves Data.frame data, VIX and RV together
#'
#' @param infile a data.frame
#'
#' @return
#' @export
#'
#' @examples
#' plot_data4(Df_frame)
#'
Plot_data4 <- function(infile, save_name){
  plot <-  ggplot(infile, aes(x = Date)) +
    geom_line(aes(y = RealizedVolatility, color = "Realized Volatility"), size = 1) + # RV in green
    geom_line(aes(y = VIX.Close,  color = "VIX (Close)"), size  = 1) +  ## vix in orange
    scale_y_continuous(breaks = c(seq(from = 1, to = 9, by = 1)), limits = c(0,9)) +
    scale_x_date(breaks = "2 years", date_labels = "%Y") +
    scale_color_manual(values = c("darkseagreen3", "#E7B800")) +
    labs(y = "VIX (Close)", x = "Year", color = "Legend") +
    theme(legend.position = c(0.8,0.9), panel.grid.major = element_line(colour = "grey92"), panel.grid.minor = element_line(colour = "grey92"),
          text = element_text(size = 15), axis.text = element_text(size = 15))
  ggsave(save_name, plot = last_plot(), width = 16, path = "written/pictures")
}
