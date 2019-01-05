#' Estimate my 2 models with newey west standard errors and produce latex output
#'
#' @param Df
#'
#' @return 4 regressions and 2 latex ouput
#' @export
#'
#' @examples
Regress_data_newey <- function(Data) {
  Df_regress <- Data
  Df_regress$day <- Df_regress$RealizedVolatility %>% lag(1)
  Df_regress$week  <- Df_regress$RealizedVolatility %>% rollapply(5,mean,na.rm = T) %>% lag(1)
  Df_regress$month <- Df_regress$RealizedVolatility %>% rollapply(22,mean,na.rm = T) %>% lag(1)
  Df_regress$crisis <- c( ifelse (year(Df_regress) >= 2008 & year(Df_regress) <= 2012, 1,0) )
  Df_regress <- na.omit(Df_regress)
  MyRegression1 <- lm(RealizedVolatility ~ day + week + month + crisis, data = Df_regress)
  MyRegression2 <- lm(RealizedVolatility ~ day + week + month + crisis + VIX.Close %>% lag(1), data = Df_regress)
  MyRegression3 <- lm(RealizedVolatility %>% log() ~ day %>% log() + week %>% log() + month %>% log() + crisis, data = Df_regress)
  MyRegression4 <- lm(RealizedVolatility %>% log() ~ day %>% log() + week %>% log() + month %>% log() + crisis + VIX.Close  %>% lag(1)  %>% log(), data = Df_regress)
  MyNeweyWest1 <- coeftest(MyRegression1, vcov=NeweyWest(MyRegression1))
  MyNeweyWest2 <- coeftest(MyRegression2, vcov=NeweyWest(MyRegression2))
  MyNeweyWest3 <- coeftest(MyRegression3, vcov=NeweyWest(MyRegression3))
  MyNeweyWest4 <- coeftest(MyRegression4, vcov=NeweyWest(MyRegression4))
  Robust_se1 <- MyNeweyWest1[,2]
  Robust_t1 <- MyNeweyWest1[,3]
  Robust_p1 <- MyNeweyWest1[,4]
  Robust_se2 <- MyNeweyWest2[,2]
  Robust_t2 <- MyNeweyWest2[,3]
  Robust_p2 <- MyNeweyWest2[,4]
  Robust_se3 <- MyNeweyWest3[,2]
  Robust_t3 <- MyNeweyWest3[,3]
  Robust_p3 <- MyNeweyWest3[,4]
  Robust_se4 <- MyNeweyWest4[,2]
  Robust_t4 <- MyNeweyWest4[,3]
  Robust_p4 <- MyNeweyWest4[,4]
  stargazer(MyRegression1, MyRegression2, se = list(Robust_se1, Robust_se2), t = list(Robust_t1, Robust_t2), p = list(Robust_p1, Robust_p2), add.lines=list(c("AIC", round(AIC(MyRegression1),1), round(AIC(MyRegression2),1))), dep.var.labels = c("Realized Volatility"), covariate.labels = c("Intercept", "$RV^{d}$", "$RV^{w}$", "$RV^{m}$", "crisis", "VIX"), omit.stat = "f", intercept.bottom = FALSE, intercept.top = TRUE, title = "level regression", out = "written/tables/newey1.tex")
  stargazer(MyRegression3, MyRegression4, se = list(Robust_se3, Robust_se4), t = list(Robust_t3, Robust_t4), p = list(Robust_p3, Robust_p4), add.lines=list(c("AIC", round(AIC(MyRegression3),1), round(AIC(MyRegression4),1))), dep.var.labels = c("Realized Volatility"), covariate.labels = c("Intercept", "$RV^{d}_{log}$", "$RV^{w}_{log}$", "$RV^{m}_{log}$", "crisis", "$VIX_{log}$"), omit.stat = "f", intercept.bottom = FALSE, intercept.top = TRUE, title = "logarithmic regression", out = "written/tables/newey2.tex")
  Results <- list(MyRegression1, MyRegression2, MyRegression3, MyRegression4, MyNeweyWest1, MyNeweyWest2, MyNeweyWest3, MyNeweyWest4)
  return(Results)
}


