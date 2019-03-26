library(tidyverse)
library(summarytools)
library(stargazer)
library(haven)

rm(list = ls())

# load datafile and codebook
# code book
load("AA170042/code_tbl.RData") 
# data
load("AA170042/inc106.RData")

# load sav
# df.inc106 <- read_spss("AA170042/inc106.sav")

# set global options of summarytools
st_options(style = "simple",
           round.digits = 4,
           ctable.prop = "c")


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
NP_domestic <- df %>% summarise_at(vars(a8), funs(mean))
# 就業人口數 (a9)
NP_employed <- df %>% filter(!is.na(a9)) %>% summarise_at(vars(a9), funs(mean))
# 非成年人口數 (a8 - a12)
NP_minor <- df %>% filter(!is.na(a8), !is.na(a12)) %>% summarise_at(vars(minor), funs(mean))
# 所得收入者人口數 (a13)
NP_income <- df %>% filter(!is.na(a13)) %>% summarise_at(vars(a13), funs(mean))
# 六十五歲以上人口數 (a19)
NP_elder <- df %>% filter(!is.na(a19)) %>% summarise_at(vars(a19), funs(mean))
# 低所得比例  (每人可支配所得中位數 * 0.6 ; 106 = 284,228; dome_num_avg = 3.07)
# 中低所得比例（低所得 * 1.5）
# a <- df %>% mutate(dom_inc = (itm500 - itm600 - (itm1042 - itm390)))
df <- df %>% filter(!is.na(itm400) & !is.na(itm600)) %>% 
        mutate(dom_inc = (itm400 - itm600), 
               indi_inc = (itm400 - itm600) / a8)

# calculate median
a <- df$a8
b <- df$indi_inc

x <- vector("list", length(a))
for(i in 1:length(a)){
        x[[i]] <- rep(b[i], times = a[i])
}
x <- unlist(x)
m <- median(x)

# 
low <- m * 0.6
m_low <- low * 1.5
df <- df %>% 
        mutate(
                low_inc = case_when(
                        indi_inc <= low ~ "1", 
                        indi_inc > low & indi_inc <= m_low  ~ "2", 
                        TRUE ~ "99"
                        )
                ) %>% 
        group_by(SF)

low_inc <- df %>%
        mutate(counts = n()) %>%
        count(SF, low_inc, counts) %>%
        spread(low_inc, n, sep = "_") %>%
        mutate(
                p_1 = low_inc_1 / counts, 
                p_2 = low_inc_2 / counts
                ) %>%
        round(4)
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