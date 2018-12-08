library(usethis)
library(devtools)
library(drake)
library(tidyverse)
library(readxl)
library(xts)
library(tseries)
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
  Vix1 =  Clean_VixA(Vix1_raw),
  Vix2 = Clean_VixB(Vix2_raw),
  SP = Clean_SP(SP_raw),
  Df = Build_df(Vix1, Vix2, Vol, SP),
  Df_frame = Build_df2(Vix1, Vix2, Vol, SP),
  Df_full = na.omit(Df),

  # plot data and save plot
  plot_var = plot_data1(Df$RealizedVolatility, "Realized Volatility", "vol.png"),
  plot_vix = plot_data1(Df$VIX.Close, "VIX Close", "vix.png"),
  plot_sp_and_vix = plot_data2(Df_frame, "SPandViX.png"),
  plot_sp_and_vol_and_vix = plot_data3(Df_frame, "SPandVolandViX.png"),
  plot_vol_and_vix = plot_data4(Df_frame,"VolandViX.png"),

  # regress data and save plot
  lm1 = regress_data_harvix2(Df, file_out("written/tables/regression_harvix.tex")),
  lm2 = regress_data_harvixln2(Df, file_out("written/tables/regression_harvixln.tex"))

  # graphical exploration of time series data

  # HAR-RV model

)

make(my_plan)
vis_drake_graph(drake_config(my_plan))

loadd(c(Df,Df_frame, Df_full))

# rm(list = ls())






## Merkzettel --------------------------------
# file_in - Wenn ich Sachen von außerhalb der R umgebung einlese, file_out: wenn ich Sachen abspeichere
# Umgebung: normal, dass da keine Objekte auftauchen
# Funktionen: für kleine Funtkionen kann man auch direkt in drake die baseR funktionen schreiben
# für jeden Rechenschritt braucht es ein object/target (save <- savefunction(file_out))
# bei file_in/file_out -> klammer nur um den pfad, restlichen input in die äußere klammer
