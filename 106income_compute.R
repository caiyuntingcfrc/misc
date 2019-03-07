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
df <- df.inc106 %>% mutate(
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

# sf
N_SF <- df %>% count()
# 戶內人口數 (a8)
NP_domestic <- df %>% filter(!is.na(a8)) %>% summarise_at(vars(a8), funs(mean))
# 就業人口數 (a9)
NP_employed <- df %>% filter(!is.na(a9)) %>% summarise_at(vars(a9), funs(mean))
# 非成年人口數 (a8 - a12)
NP_minor <- df %>% filter(!is.na(a8), !is.na(a12)) %>% summarise_at(vars(minor), funs(mean))
# 所得收入者人口數 (a13)
NP_income <- df %>% filter(!is.na(a13)) %>% summarise_at(vars(a13), funs(mean))
# 六十五歲以上人口數 (a19)
NP_elder <- df %>% filter(!is.na(a19)) %>% summarise_at(vars(a19), funs(mean))
# 低收入戶比例  (b19_)
d <- df %>% select(starts_with("b19_"), starts_with("b20_"), starts_with("b2_"), c("SF")) %>% 
        filter(b19_1 == "09", b20_1 == "00", b2_1 == "01")

dd <- df %>% select(starts_with("b19_"), starts_with("b20_"), starts_with("b2_"), starts_with("itm"), c("SF")) %>% 
        filter_at(vars(starts_with("b19_")), any_vars(. == "09")) %>% filter(SF == 1)
x <- df %>% filter(SF == 1, d1 == 1) %>% filter_at(vars(starts_with("b19_")), any_vars(. %in% c("09")))
x <- df %>% filter(d1 == 7)
table(df$d1)
# 中低收入戶比例

# 從政府經常性移轉收入佔所得收入比例 itm430 / itm400
PP_Gov_inc <- df %>% filter(!is.na(itm400)) %>% filter(!is.na(itm430)) %>% 
        mutate(PP_Gov_inc = itm430 / itm400) %>% summarise_at(vars(PP_Gov_inc), funs(mean))
# 社會保險給付經常移轉收入佔所得收入比例  itm450 / itm400
PP_Insurance_inc <- df %>% filter(!is.na(itm400)) %>% filter(!is.na(itm450)) %>%
        mutate(PP_Insurance_inc = itm450 / itm400) %>% summarise_at(vars(PP_Insurance_inc), funs(mean))
# 對政府經常性移轉支出佔所得收入比例 itm580 / itm 400
PP_Gov_exp <- df %>% filter(!is.na(itm400)) %>% filter(!is.na(itm580)) %>% 
        mutate(PP_Gov_exp = itm580 / itm400) %>% summarise_at(vars(PP_Gov_exp), funs(mean))
# 社會保險經常移轉支出佔所得收入比例 itm640 / itm400
PP_Insurance_exp <- df %>% filter(!is.na(itm400)) %>% filter(!is.na(itm640)) %>% 
        mutate(PP_Insurance_exp = itm640 / itm400) %>% summarise_at(vars(PP_Insurance_exp), funs(mean))