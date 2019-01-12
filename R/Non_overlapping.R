CreateNonOverlapping <- function(Data) {
Df_non_overlapping <- Df[which(weekdays(index(Df)) == "Mittwoch")]
weekdays(index(Df))
weekdays(index(Df_non_overlapping))
head(Df_non_overlapping)
ToDelete <- seq(0, length(Df_non_overlapping), 2)
Df_non_overlapping <- Df_non_overlapping[-ToDelete,]
head(Df_non_overlapping)
}

# Regress_data_newey(Df_non_overlapping, "written/tables/overlap1.tex", "written/tables/overlap2.tex")

