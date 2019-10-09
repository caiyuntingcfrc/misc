rm(list = ls())
source("~/github/misc/func_PovertyRate.R")

setwd("~/R_wd/tw_inc/")

# load Rdata --------------------------------------------------------------

load("AA170025/inc89.RData")
load("AA170030/inc94.RData")
load("AA170035/inc99.RData")
load("AA170036/inc100.RData")
load("AA170040/inc104.RData")
load("AA170042/inc106.RData")
load("AA170043/inc107.RData")

# poverty rates -----------------------------------------------------------

poverty_rate(df = df.inc89, weight = "a21", year = 89) %>% 
        round(digits = 2)
poverty_rate(df = df.inc94, weight = "a20", year = 94) %>% 
        round(digits = 2)
poverty_rate(df = df.inc99, weight = "a20", year = 99) %>% 
        round(digits = 2)
poverty_rate(df = df.inc100, weight = "a20", year = 100) %>% 
        round(digits = 2)
poverty_rate(df = df.inc104, weight = "a20", year = 104) %>% 
        round(digits = 2)
poverty_rate(df = df.inc106, weight = "a20", year = 106) %>% 
        round(digits = 2)
poverty_rate(df = df.inc107, weight = "a20", year = 107) %>% 
        round(digits = 2)



