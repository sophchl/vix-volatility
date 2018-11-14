library(usethis)
library(devtools)
library(drake)
library(tidyverse)
library(VIXVolatility)
pkgconfig::set_config("drake::strings_in_dots" = "literals")

theme_set(theme_light())

my_plan <- drake_plan(
  height = 5,
  width = 7,

  # load data
  Vol_raw = read.csv2(file_in("data-raw/OxfordManRealizedVolatilityIndices0.2.csv", sep = ",", dec = ".", na.strings=c("","NA"))),
  Vix1_raw = read_excel(file_in("data-raw/vixarchive.xls")),
  Vix2_raw = read.csv2(file_in("data-raw/vixcurrent.csv", sep = ",")),

  # clean data
  Vol = Clean_Vol(Vol_raw),
  Vix1 =  Clean_Vix1(Vix1_raw),
  Vix2 = Clean_Vix2(Vix2_raw),
  Df = Build_df(Vix1, Vix2, Vol)

  # plot data


  # save the plot


  # regress data


)

make(my_plan)
readd(df2)
loadd(df2)

## Merkzettel --------------------------------
# file_in - Wenn ich Sachen von außerhalb der R umgebung einlese, file_out: wenn ich Sachen abspeichere
# Umgebung: normal, dass da keine Objekte auftauchen
# Funktionen: für kleine Funtkionen kann man auch direkt in drake die baseR funktionen schreiben
# für jeden Rechenschritt braucht es ein object/target (save <- savefunction(file_out))
