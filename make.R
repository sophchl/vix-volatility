## with drake

library(drake)
library(tidyverse)
library(VIXVolatility)
pkgconfig::set_config("drake::strings_in_dots" = "literals")

theme_set(theme_light())

my_plan <- drake_plan(
  height = 5,
  width = 7,

  # load and clean data
  df = load_data(file_in("data-raw/OxfordManRealizedVolatilityIndices0.2.csv")),
  df2 = clean_data(df)


  # plot data


  # save the plot


  # regress data


)

make(my_plan)
readd(df2)
loadd(df2)

