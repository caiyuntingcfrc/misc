
# prep and options --------------------------------------------------------

# rm and clear console
rm(list = ls()); cat("\14")
# source("~/Github_CFRC/misc/func_ins.pack.R")
devtools::source_url("https://raw.githubusercontent.com/caiyuntingcfrc/misc/function_poverty/func_ins.pack.R")
# setwd
setwd("d:/R_wd/")
# options
options(scipen = 999)

ins.pack("haven", 
         "data.table", 
         "tidyverse", 
         "readxl")

# read the file -----------------------------------------------------------
# year
year <- read_xls("fertility rate/TW_ASFR.xls", range = "B5:B77", col_names = FALSE)
colnames(year) <- "year"
# asfr
asfr_1 <- read_xls("fertility rate/TW_ASFR.xls", range = "D4:J77") %>% 
        # rename with select
        select(`15_19` = `15～19`, 
               `20_24` = `20～24`,
               `25_29` = `25～29`,
               `30_34` = `30～34`,
               `35_39` = `35～39`,
               `40_44` = `40～44`,
               `45_49` = `45～49`,
               everything())
# cbind
asfr <- cbind(year, asfr_1)
# rm
rm(asfr_1, year)

# calc --------------------------------------------------------------------


for(i in 1:nrow(asfr)){
        
        f15_19 <- sum(asfr[1:5, 2]) * 5 / 1000
        f15_19 <- sum(asfr[1:5, 3]) * 5 / 1000
        f15_19 <- sum(asfr[1:5, 4]) * 5 / 1000
        f15_19 <- sum(asfr[1:5, 5]) * 5 / 1000
        f15_19 <- sum(asfr[1:5, 6]) * 5 / 1000
        f15_19 <- sum(asfr[1:5, 7]) * 5 / 1000
}

sum(asfr[1:5, 2]) * 5 / 1000
sum(asfr[6:10, 3]) * 5 / 1000


# read file ---------------------------------------------------------------



df <- read_sav("women/105年AA150018/women105.sav") %>% 
        # remove variable labels
        zap_label() %>% 
        # remove value labels
        zap_labels()

# filter ------------------------------------------------------------------

# filter by age
df <- df %>% filter(15 <= a3 & a3 < 50)

# sum: all children
df <- df %>% 
        mutate( # children_all = rowSums(.[ , grep("^b2a_a|^b2a_b", names(df))], na.rm = TRUE), 
                children_check = rowSums(select(., b2a_a, b2a_b), na.rm = TRUE))

