library(tidyverse)
library(summarytools)
library(stargazer)

rm(list = ls())

# load datafile and codebook
load("AA170042/code_tbl.RData")
load("AA170042/inc106.RData")

# set global options of summarytools
st_options()
st_options("style", "grid")
st_options("round.digits", 4)
st_options("freq.report.nas", F)

#add new variabel by structure of families
df.inc106 <- df.inc106 %>% mutate(
        SF = case_when(df.inc106$a18 %in% c("101", "102") ~ 1, # single
                       df.inc106$a18 %in% c("201", "202") ~ 2, # couple
                       df.inc106$a18 %in% c("321", "322", "331", "332") ~ 3, # singleParent
                       df.inc106$a18 %in% c("421", "422", "431", "432") ~ 4, # core
                       df.inc106$a18 %in% c("511", "512", "531", "532") ~ 5, # grandParent
                       df.inc106$a18 %in% c("611", "612", "621", "622","631", "632") ~ 6, # threeGen
                       df.inc106$a18 %in% c("701", "702") ~ 7) # others
)

# calculate
a8 <- df.inc106 %>% group_by(SF) %>% summarise_at(vars(a8), funs(mean)) # a8
a9 <- df.inc106 %>% group_by(SF) %>% summarise_at(vars(a9), funs(mean)) # a9

