#' conduct the F-test for Hypothesis 3: The VIX subsumes the information content from the historic volatilities
#'
#' @param Modell1
#' @param Modell2
#' @param SavePath1
#' @param SavePath2
#'
#' @return
#' @export
#'
#' @examples
#' Ftesting(lm1[[3]], lm1[[6]], "written/tables/ftest1.tex", "written/tables/ftest2.tex")
#'
FTesting2 <- function(Modell1, Modell2, Caption1, Caption2, SavePath1, SavePath2) {
Hnull1 <- c("day = 0", "week = 0", "month = 0", "dayVIX=1")
Hnull2 <- c("day %>% log() = 0", "week %>% log() = 0", "month %>% log() = 0", "dayVIX %>% log() = 1")
FtestLevel <- linearHypothesis(Modell1, Hnull1, test = "F")
FtestLog <- linearHypothesis(Modell2, Hnull2, test = "F")
print(xtable(FtestLevel, type = "latex", caption = c(Caption1)), caption.placement = getOption("xtable.caption.placement", "top"), table.placement = getOption("xtable.table.placement", "htbp!"), file = SavePath1)
print(xtable(FtestLog, type = "latex", caption = c(Caption2)), caption.placement = getOption("xtable.caption.placement", "top"), table.placement = getOption("xtable.table.placement", "htbp!"), file = SavePath2)
}


