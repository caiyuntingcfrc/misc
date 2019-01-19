rm(list = ls())
library(ggmap)
library(tidyverse)

TC.df <- haven::read_spss(file = "Twin Cities/3.雙城正式_全年級20180704final_han&Lung.sav")
TC.variables <- labelled::var_label(TC.df)
class(TC.variables)

home.df <- read.delim(file = "home/HDATA106", sep = "\t", header = FALSE)
home.df <- read.table(file = "home/HDATA106", header = FALSE)
home.df <- read.fwf("home/HDATA106", widths = rep(1, 80), header = FALSE)[1:10, ]
head(home.df)
home.df[1]