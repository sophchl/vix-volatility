#'Create a dataset for the regression containing only the data from wednesday every two weeks (not same option twice in regression)
#'
#' @param Data
#'
#' @return
#' @export
#'
#' @examples
#' CreateNonOverlapping(Df)
CreateNonOverlapping <- function(Data) {
Df_non_overlapping <- Data[which(weekdays(index(Data)) == "Mittwoch")]
weekdays(index(Data))
weekdays(index(Df_non_overlapping))
ToDelete <- seq(0, length(Df_non_overlapping), 2)
Df_non_overlapping <- Df_non_overlapping[-ToDelete,]
return(Df_non_overlapping)
}


