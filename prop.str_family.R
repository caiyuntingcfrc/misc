rm(list = ls())
setwd("d:/R_wd/tw_inc/R data files/")
source("d:/R_wd/R studies/R scripts/func_ins.pack.R")
ins.pack("tidyverse", "expss", "epiDisplay", "sjPlot")

# load --------------------------------------------------------------------

load("df_inc107.RData")

# recode ------------------------------------------------------------------
d <- df.inc107 %>% 
        mutate(str_family = case_when(a18 %in% c(101, 102) ~ 1L, 
                                      a18 %in% c(201, 202) ~ 2L, 
                                      a18 %in% c(321, 322, 331, 332) ~ 3L, 
                                      a18 %in% c(421, 422, 431, 432) ~ 4L, 
                                      a18 %in% c(511, 512, 531, 532) ~ 5L, 
                                      a18 %in% c(611, 612, 621, 622, 631, 632) ~ 6L, 
                                      a18 %in% c(701, 702) ~ 7L, 
                                      TRUE ~ NA_integer_))
# apply labels
d <- apply_labels(d, 
                  str_family = "structure of families", 
                  str_family = c("單人戶" = 1L, 
                                 "夫婦戶" = 2L, 
                                 "廣義單親戶" = 3L, 
                                 "核心家庭" = 4L, 
                                 "祖孫家庭" = 5L, 
                                 "三代家庭" = 6L, 
                                 "其他" = 7L))

# weigh -------------------------------------------------------------------

s <- d$str_family
w <- d$a20
weighed <- s[rep(1:length(s), times = w)]
w1 <- weight(s, w, digits = 0)

# freq --------------------------------------------------------------------

tab1(weighed, 
     decimal = 2,
     sort.group = "decreasing", 
     bar.values = "percent", 
     main = "臺灣家庭組織型態（2018）")
tab1(w1, 
     decimal = 2,
     sort.group = "decreasing", 
     bar.values = "percent", 
     main = "臺灣家庭組織型態（2018）")
# plot --------------------------------------------------------------------

plot_frq(d$str_family, weight.by = d$a20)
