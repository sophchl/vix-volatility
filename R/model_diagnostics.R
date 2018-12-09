CalculateBICandAIC <- function(ModelList, ModelNameList) {
  AIC_list <- ModelList %>% map(AIC)
  BIC_list <- ModelList %>% map(BIC)
  AIC_table <- data.frame(matrix(unlist(AIC_list), nrow=length(ModelList), byrow=T))
  BIC_table <- data.frame(matrix(unlist(BIC_list), nrow=length(ModelList), byrow=T))
  Both_table <- merge.data.frame(AIC_table, BIC_table, by = "row.names")
  Both_table <- Both_table[,2:3]
  colnames(Both_table) <- c("AIC", "BIC")
  rownames(Both_table) <- ModelNameList
  print(Both_table)
  print(xtable(Both_table, type = "latex"), file = "written/tables/AICandBIC.tex")
}




