# t-tests: just use p-values from regression
# F-tests: conduct here
# significant increase of adjusted R^2: conduct here

loadd(c(lm1, lm3))
lm1[[3]] %>% summary()
lm1[[6]] %>% summary()

lm3[[3]] %>% summary()
lm3[[6]] %>% summary()

# F-test, Hypothesis 4
Hnull1 <- c("day = 0", "week = 0", "month = 0", "dayVIX=1")
FtestLevel1 <- linearHypothesis(lm1[[3]], Hnull1, test = "F")   # level model all
linearHypothesis(lm3[[3]], Hnull1, test = "F") # level model overlapp
FtestLevel1[2,7]
colnames(FtestLevel1)


Hnull2 <- c("day %>% log() = 0", "week %>% log() = 0", "month %>% log()= 0", "dayVIX %>% log() = 1")
linearHypothesis(lm1[[6]], Hnull2) # log model all
linearHypothesis(lm3[[6]], Hnull2) # log model overlapp

loadd(Df_regress)


