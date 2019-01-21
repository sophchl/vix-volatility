library(usethis)
library(devtools)
library(drake)
library(tidyverse)
library(readxl)
library(tseries)
library(xts)
library(highfrequency)
library(PerformanceAnalytics)
library(xtable)
library(HARModel)
library(sandwich)
library(lubridate)
library(lmtest)
library(stargazer)
library(car)

library(VIXVolatility)

pkgconfig::set_config("drake::strings_in_dots" = "literals")

theme_set(theme_light())

my_plan <- drake_plan(
  height = 5,
  width = 7,

  # load data
  Vol_raw = read.csv2(file_in("data-raw/OxfordManRealizedVolatilityIndices0.2.csv"), sep = ",", dec = ".", na.strings=c("","NA")),
  Vix1_raw = read_excel(file_in("data-raw/vixarchive.xls")),
  Vix2_raw = read.csv2(file_in("data-raw/vixcurrent.csv"), sep = ","),
  SP_raw = read_excel(file_in("data-raw/sandp500.xls"), range = "A767:D8197", col_names = FALSE),

  # clean data
  Vol = Clean_Vol(Vol_raw),
  Vix1 =  Clean_Vix1(Vix1_raw),
  Vix2 = Clean_Vix2(Vix2_raw),
  SP = Clean_SP(SP_raw),
  Df = Build_df1(Vix1, Vix2, Vol, SP),
  Df_frame = Build_df2(Vix1, Vix2, Vol, SP),
  Df_full = na.omit(Df),

  # plot data and save plot
  plot_sp_and_vix = Plot_data2a(Df_frame, "SPandViX.png"),
  plot_sp_and_vol_and_vix = Plot_data3a(Df_frame, "SPandVolandViX.png"),

  # descriptive statistics

  # regress data and save plot
  Df_regress = RegressionDataRVVIX(Df),
  Df_no_overlap = CreateNonOverlapping(Df_regress),
  lm1 = RegressNewey2(Df_regress, "tab:newey1", "tab:newey2", "Level regression (whole sample)", "Logarithmic regression (whole sample)", "written/tables/newey1.tex", "written/tables/newey2.tex"),
  lm2 = RegressNewey2(Df_no_overlap, "tab:overlap1", "tab:overlap2", "Level regression (non-overlapping sample)", "Logarithmimc regression (non-overlapping sample)", "written/tables/overlap1.tex", "written/tables/overlap2.tex"),

  # F-tests
  Ftest1 = FTesting(lm1[[3]], lm1[[6]], "F-test Reg3a", "F-test Reg3b", "tab:ftest1", "tab:ftest2", "written/tables/ftest1.tex", "written/tables/ftest2.tex"),
  Ftest2 = FTesting(lm2[[3]], lm2[[6]], "F-test Reg3a non-overlapping sample", "F-test Reg3b non-overlapping sample", "tab:ftest1over","tab:ftest2over", "written/tables/ftestOverlap1.tex", "written/tables/ftestOverlap2.tex")

)

make(my_plan)
vis_drake_graph(drake_config(my_plan))

loadd(c(Df,Df_frame, Df_full))
loadd(lm1)
loadd(lm2)


# rm(list = ls())

## Merkzettel --------------------------------
# file_in - Wenn ich Sachen von außerhalb der R umgebung einlese, file_out: wenn ich Sachen abspeichere
# Umgebung: normal, dass da keine Objekte auftauchen
# Funktionen: für kleine Funtkionen kann man auch direkt in drake die baseR funktionen schreiben
# für jeden Rechenschritt braucht es ein object/target (save <- savefunction(file_out))
# bei file_in/file_out -> klammer nur um den pfad, restlichen input in die äußere klammer
