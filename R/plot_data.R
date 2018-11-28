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
plot_data2 <- function(infile, save_name){
 plot <-  ggplot(infile, aes(x = Date)) +
    geom_line(aes(y = VIX.Close,  color = "VIX (Close)"), size  = 1) +  ## vix in orange
    geom_line(aes(y = SP500/20, color = "SP500"), size = 1) + ## SP500 in blue
    scale_y_continuous(sec.axis = sec_axis(~.*20, name = "S&P 500", breaks = c(400,800,1200,1600,2000)), breaks = c(20,40,60,80,100)) +
    scale_x_date(breaks = "2 years", date_labels = "%Y") +
    scale_color_manual(values = c("#00AFBB", "#E7B800")) +
    labs(y = "VIX (Close)", x = "Year", color = "Legend") +
    theme(legend.position = c(0.8,0.9), panel.grid.major = element_line(colour = "grey92"), panel.grid.minor = element_line(colour = "grey92"),
          text = element_text(size = 15), axis.text = element_text(size = 15))
  ggsave(save_name, plot = last_plot(), width = 16, path = "written/pictures")
}



#' plots and saves Data.frame data, VIX and RV and SP500 together
#'
#' @param infile a data.frame
#'
#' @return
#' @export
#'
#' @examples
#' plot_data3(Df_frame)
#'
plot_data3 <- function(infile, save_name){
  plot <- ggplot(infile, aes(x = Date)) +
    geom_line(aes(y = VIX.Close,  color = "VIX (Close)"), size  = 1) +  ## vix in orange
    geom_line(aes(y = SP500/20, color = "SP500"), size = 1) + ## SP500 in blue
    geom_line(aes(y = RealizedVariance*200, color = "Realized Volatility"), size = 1) + # RV in green
    scale_y_continuous(sec.axis = sec_axis(~.*20, name = "S&P 500", breaks = c(400,800,1200,1600,2000)), breaks = c(20,40,60,80,100)) +
    scale_x_date(breaks = "2 years", date_labels = "%Y") +
    scale_color_manual(values = c("darkseagreen3", "#00AFBB", "#E7B800")) +
    labs(y = "VIX (Close)", x = "Year", color = "Legend") +
    theme(legend.position = c(0.8,0.9), panel.grid.major = element_line(colour = "grey92"), panel.grid.minor = element_line(colour = "grey92"),
          text = element_text(size = 15), axis.text = element_text(size = 15))
  ggsave(save_name, plot = last_plot(), width = 16, path = "written/pictures")
}



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
plot_data4 <- function(infile, save_name){
 plot <-  ggplot(infile, aes(x = Date)) +
    geom_line(aes(y = VIX.Close/20,  color = "VIX (Close)"), size  = 1) +  ## vix in orange
    geom_line(aes(y = RealizedVariance, color = "Realized Volatility"), size = 1) +
    scale_y_continuous(sec.axis = sec_axis(~.*20, name = "VIX", breaks = c(20,40,60,80,100)), breaks = c(0.1, 0.2, 0.3, 0.4, 0.5), limits = c(0,0.5)) +
    scale_x_date(breaks = "2 years", date_labels = "%Y") +
    scale_color_manual(values = c("darkseagreen3", "#E7B800")) +
    labs(y = "VIX (Close)", x = "Year", color = "Legend") +
    theme(legend.position = c(0.8,0.9), panel.grid.major = element_line(colour = "grey92"), panel.grid.minor = element_line(colour = "grey92"),
          text = element_text(size = 15), axis.text = element_text(size = 15))
  ggsave(save_name, plot = last_plot(), width = 16, path = "written/pictures")
}


