#' plots and saves Data.frame data, VIX and SP500 together
#'
#' @param infile a data.frame
#'
#' @return a nice graph
#' @export
#'
#' @examples
#' plot_data2(Df_frame)
#'
Plot_data2a <- function(infile, save_name){
  plot <-  ggplot(infile, aes(x = Date)) +
    geom_line(aes(y = VIX.Close,  color = "VIX (Close)"), size  = 1) +  ## vix in orange
    geom_line(aes(y = SP500/500, color = "SP500"), size = 1) + ## SP500 in blue
    scale_y_continuous(sec.axis = sec_axis(~.*500, name = "S&P 500", breaks = c(seq(from = 500, to =5000, by = 500))), breaks = c(seq(from = 1, to = 5, by = 1))) +
    scale_x_date(breaks = "2 years", date_labels = "%Y") +
    scale_color_manual(values = c("#00AFBB", "#E7B800")) +
    labs(y = "VIX (Close)", x = "Year", color = "Legend") +
    theme(legend.position = c(0.8,0.9), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          text = element_text(size = 25), axis.text = element_text(size = 25), rect = element_rect(fill = "transparent"))
  ggsave(save_name, plot = last_plot(), width = 16, path = "written/pictures")
}

