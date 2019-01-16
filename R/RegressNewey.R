#' Estimate my 2 models with newey west standard errors and produce latex output
#'
#' @param Data
#' @param SavePath1
#' @param SavePath2
#'
#' @return 4 regressions and 2 latex ouput
#' @export Results
#'
#' @examples
#' RegressNewey(Df_regress, "written/tables/reg1", "wirtten/tabels/reg2")
RegressNewey <- function(Data, SavePath1, SavePath2) {
  MyRegression1a <- lm(RealizedVolatility ~ day + week + month + crisis, data = Data)
  MyRegression2a <- lm(RealizedVolatility ~ crisis + dayVIX, data = Data)
  MyRegression3a <- lm(RealizedVolatility ~ day + week + month + crisis + dayVIX, data = Data)
  MyRegression1b <- lm(RealizedVolatility %>% log() ~ day %>% log() + week %>% log() + month %>% log() + crisis, data = Data)
  MyRegression2b <- lm(RealizedVolatility %>% log() ~ crisis + dayVIX %>% log(), data = Data)
  MyRegression3b <- lm(RealizedVolatility %>% log() ~ day %>% log() + week %>% log() + month %>% log() + crisis + dayVIX %>% log(), data = Data)
  MyNeweyWest1a <- coeftest(MyRegression1a, vcov=NeweyWest(MyRegression1a))
  MyNeweyWest2a <- coeftest(MyRegression2a, vcov=NeweyWest(MyRegression2a))
  MyNeweyWest3a <- coeftest(MyRegression3a, vcov=NeweyWest(MyRegression3a))
  MyNeweyWest1b <- coeftest(MyRegression1b, vcov=NeweyWest(MyRegression1b))
  MyNeweyWest2b <- coeftest(MyRegression2b, vcov=NeweyWest(MyRegression2b))
  MyNeweyWest3b <- coeftest(MyRegression3b, vcov=NeweyWest(MyRegression3b))
  Robust_se1a <- MyNeweyWest1a[,2]
  Robust_t1a <- MyNeweyWest1a[,3]
  Robust_p1a <- MyNeweyWest1a[,4]
  Robust_se2a <- MyNeweyWest2a[,2]
  Robust_t2a <- MyNeweyWest2a[,3]
  Robust_p2a <- MyNeweyWest2a[,4]
  Robust_se3a <- MyNeweyWest3a[,2]
  Robust_t3a <- MyNeweyWest3a[,3]
  Robust_p3a <- MyNeweyWest3a[,4]
  Robust_se1b <- MyNeweyWest1b[,2]
  Robust_t1b <- MyNeweyWest1b[,3]
  Robust_p1b <- MyNeweyWest1b[,4]
  Robust_se2b <- MyNeweyWest2b[,2]
  Robust_t2b <- MyNeweyWest2b[,3]
  Robust_p2b <- MyNeweyWest2b[,4]
  Robust_se3b <- MyNeweyWest3b[,2]
  Robust_t3b <- MyNeweyWest3b[,3]
  Robust_p3b <- MyNeweyWest3b[,4]
  stargazer(MyRegression1a, MyRegression2a, MyRegression3a, se = list(Robust_se1a, Robust_se2a, Robust_se3a), t = list(Robust_t1a, Robust_t2a, Robust_t3a), p = list(Robust_p1a, Robust_p2a, Robust_p3a), add.lines=list(c("AIC", round(AIC(MyRegression1a),1), round(AIC(MyRegression2a),1), round(AIC(MyRegression3a),1))), dep.var.labels = c("Realized Volatility"), covariate.labels = c("Intercept", "$RV^{d}_{t}$", "$RV^{w}_{t}$", "$RV^{m}_{t}$", "$crisis$", "$VIX_{t}$"), column.labels = c("Reg1a", "Reg2a", "Reg3a"), omit.stat = "f", intercept.bottom = FALSE, intercept.top = TRUE, title = "Level regression", out = SavePath1)
  stargazer(MyRegression1b, MyRegression2b, MyRegression3b, se = list(Robust_se1b, Robust_se2b, Robust_se3b), t = list(Robust_t1b, Robust_t2b, Robust_t3b), p = list(Robust_p1b, Robust_p2b, Robust_p3b), add.lines=list(c("AIC", round(AIC(MyRegression1b),1), round(AIC(MyRegression2b),1), round(AIC(MyRegression3b),1))), dep.var.labels = c("Realized Volatility"), covariate.labels = c("Intercept", "$ln(RV^{d}_{t})$", "$ln(RV^{w}_{t})$", "$ ln(RV^{m}_{t})$", "$crisis$", "$ln(VIX_{t})$"), column.labels = c("Reg1b", "Reg2b", "Reg3b"), omit.stat = "f", intercept.bottom = FALSE, intercept.top = TRUE, title = "Logarithmic regression", out = SavePath2)
  Results <- list(MyRegression1a, MyRegression2a, MyRegression3a, MyRegression1b, MyRegression2b, MyRegression3b, MyNeweyWest1a, MyNeweyWest2a, MyNeweyWest3a, MyNeweyWest1b, MyNeweyWest2b, MyNeweyWest3b)
  return(Results)
}


