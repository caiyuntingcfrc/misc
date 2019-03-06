library(tidyverse)
library(summarytools)
library(stargazer)

rm(list = ls())

# load datafile and codebook
# code book
load("AA170042/code_tbl.RData") 
# data
load("AA170042/inc106.RData")

# set global options of summarytools
st_options()
st_options("style", "grid")
st_options("round.digits", 4)
st_options("freq.report.nas", F)

#add new variabel by structure of families
df.inc106 <- df.inc106 %>% mutate(
        SF = case_when(a18 %in% c("101", "102") ~ 1, # single
                       a18 %in% c("201", "202") ~ 2, # couple
                       a18 %in% c("321", "322", "331", "332") ~ 3, # singleParent
                       a18 %in% c("421", "422", "431", "432") ~ 4, # core
                       a18 %in% c("511", "512", "531", "532") ~ 5, # grandParent
                       a18 %in% c("611", "612", "621", "622","631", "632") ~ 6, # threeGen
                       a18 %in% c("701", "702") ~ 7), # others
        minor = a8 - a12) %>% 
        # group by structure of families
        group_by(SF)

# calculate
df <- df.inc106 %>% group_by(SF)
# sf
N_SF <- df %>% count()
# めず计 (a8)
NP_domestic <- df %>% summarise_at(vars(a8), funs(mean))
# 碞穨计 (a9)
NP_employed <- df %>% summarise_at(vars(a9), funs(mean))
# 獶Θ计 (a8 - a12)
NP_minor <- df %>% summarise_at(vars(minor), funs(mean))
# ┮眔Μ计 (a13)
NP_income <- df %>% summarise_at(vars(a13), funs(mean))
# せき烦计 (a19)
NP_elder <- df %>% summarise_at(vars(a19), funs(mean))
# Μめゑㄒ  (b19_)
d <- df %>% select(starts_with("b19_"), starts_with("b20_"), c("SF")) %>% filter(b19_1 == "09", b20_1 == "00") %>% count(.$SF)
dd <- df.inc106[ , grep("itm951|^b19_*|^b4_*", names(df.inc106))] %>% filter_all(any_vars(. == "09"))
ddd <- df.inc106[ , grep("itm951|^b19_*", names(df.inc106))] %>% filter_all(any_vars(. == "10"))
b <- df.inc106[ , grep("a8|itm300|itm301|itm309|itm680|itm681|itm951|^b19_1$|^b20_1$", names(df.inc106))] %>% 
        filter(b19_1 == "10" & b20_1 %in% c("00", "01") & a8 == 1)
x <- df.inc106[ , grep("a8|itm951|itm681|^b20_1$", names(df.inc106))] %>% filter(b20_1 == "01" & a8 == 1) ; fix(x)
a <- filter(ddd, between(itm951, 8000, 9000))
fix(dd)
fix(ddd)
fix(b)
