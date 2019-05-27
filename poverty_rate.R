##### author: CAI YUN-TING ######
##### poverty rate 2000 #####
##### prep and options #####
# set working directory
setwd("D:/R_wd/")
# clear objects
rm(list = ls())
# load the packages
l <- c("tidyverse", "DescTools", "ggplot2", 
       "car", "userfriendlyscience", "summarytools", 
       "magrittr", "haven", "gridExtra")
lapply(l, require, character.only= TRUE)
rm(l)
# options
options(scipen = 999)
# options for summarytools
st_options(style = "simple",
           round.digits = 4,
           ctable.prop = "c")

##### 89 #####
##### load the RData file #####
# code book
load("AA170025/code_tbl_89.RData")
# data file
load("AA170025/inc89.RData")

##### Equivalised income #####
df.inc89 <- df.inc89 %>% 
        mutate(sqrt_scale = sqrt(a8) ,
               indi_inc = (itm400 - itm600) / sqrt_scale
               )

##### function -- poverty threshold #####
p.threshold <- function(df, w, inc) {
        # weight
        a <- df[ , w]
        b <- df[ ,inc]
        x <- vector("list", length = nrow(a))
        for(i in 1:nrow(a)) {
                x[[i]] <- rep(b[i, ], times = a[i, ])
                }
        x <- unlist(x)
        # calculate the median
        m <- median(x, na.rm = TRUE)
        return(m)
}

##### functioin -- proportion #####
p.prop <- function(df, w, inc, m) {
        # weight
        a <- df[ , w]
        b <- df[ ,inc]
        x <- vector("list", length = nrow(a))
        for(i in 1:nrow(a)) {
                x[[i]] <- rep(b[i, ], times = a[i, ])
        }
        x <- unlist(x)
        y <- x < m * 0.5
        p <- Freq(y, useNA = "always") %>% 
                print(digits = 2) %>% 
                .[.$level == "TRUE", ] %>% 
                select( - one_of(c("freq", "cumfreq", "cumperc")))
        return(p)
        }

##### median #####
m <- p.threshold(df.inc89, "a21", "indi_inc")

##### overall households #####
p_all <- p.prop(df.inc89, "a21", "indi_inc", m)

##### male headed households #####
# filter by head's sex
df <- df.inc89 %>% filter(a7 == 1)
p_m_head <- p.prop(df, "a21", "indi_inc", m)

##### female headed households #####
# filter by head's sex
df <- df.inc89 %>% filter(a7 == 2)
p_f_head <- p.prop(df, "a21", "indi_inc", m)

##### house with aged #####
# filter by number of aged
df <- df.inc89 %>% filter(a19 >= 1)
p_with_aged <- p.prop(df, "a21", "indi_inc", m)

##### house without aged #####
# filter by number of aged
df <- df.inc89 %>% filter(a19 < 1)
p_without_aged <- p.prop(df, "a21", "indi_inc", m)

##### overall single-parent families ######
df <- df.inc89 %>% 
        filter(b2_1 == "01") %>% 
        filter(b16_1 %in% c(91, 94, 95, 96)) %>% 
        filter_at(vars(starts_with("b4_")), any_vars(. < 18)) %>% 
        filter(a18 %in% c(321, 322))
p_single_parent <- p.prop(df, "a21", "indi_inc", m)

##### male single-parent families ######
df <- df.inc89 %>% 
        filter(a7 == 1) %>% 
        filter(b2_1 == "01") %>% 
        filter(b16_1 %in% c(91, 94, 95, 96)) %>% 
        filter_at(vars(starts_with("b4_")), any_vars(. < 18)) %>% 
        filter(a18 %in% c(321, 322))
p_single_male_parent <- p.prop(df, "a21", "indi_inc", m)

##### female single-parent families ######
df <- df.inc89 %>% 
        filter(a7 == 2) %>% 
        filter(b2_1 == "01") %>% 
        filter(b16_1 %in% c(91, 94, 95, 96)) %>% 
        filter_at(vars(starts_with("b4_")), any_vars(. < 18)) %>% 
        filter(a18 %in% c(321, 322))
p_single_female_parent <- p.prop(df, "a21", "indi_inc", m)

d <- bind_rows(p_all, p_m_head, p_f_head, 
               p_with_aged, p_without_aged, 
               p_single_parent, p_single_male_parent, p_single_female_parent)

d$level <- c("Overall household", "Male headed households", "female headed households", 
             "Household with aged", "Household without aged", 
             "Overall single-parent", "Male single-parent", "Female single-parent")
colnames(d) <- c("Poverty rate (%)", "2000")

##### print #####
pdf("data_output.pdf")
grid.table(d)
dev.off()

##### load the RData file #####
# code book
load("AA170030/code_tbl_94.RData")
# data file
load("AA170030/inc94.RData")

##### Equivalised income #####
df.inc94 <- df.inc94 %>% 
        mutate(sqrt_scale = sqrt(a8),
               indi_inc = (itm400 - itm600) / sqrt_scale
        )

# calculate median
a <- df.inc94$a20
b <- df.inc94$indi_inc

x <- vector("list", length(a))
for(i in 1:length(a)){
        x[[i]] <- rep(b[i], times = a[i])
}
x <- unlist(x) %>% enframe()
m <- median(x$value, na.rm = TRUE)
x <- x %>% mutate(poverty = case_when(value < m * 0.5 ~ 1,
                                      value >= m * 0.5 ~ 0)
                  )
PercTable(x$poverty, digits = 2)
##### overall household #####
# m <- median(df.inc94$indi_inc, na.rm = TRUE)
df.inc94 <- df.inc94 %>% mutate(
        poverty = case_when(indi_inc < m * 0.5 ~ 1,
                            indi_inc >= m * 0.5 ~ 0)
        )
# as.factor
df.inc94[ , "poverty"] %<>% mutate_if(is.numeric, as.factor)
PercTable(df.inc94$poverty, digits = 2)

##### load the RData file #####
# code book
load("AA170040/code_tbl_104.RData")
# data file
load("AA170040/inc104.RData")

##### Equivalised income #####
df.inc104 <- df.inc104 %>% 
        mutate(sqrt_scale = sqrt(a8),
               indi_inc = (itm400 - itm600) / sqrt_scale
        )

# calculate median
a <- df.inc104$a20
b <- df.inc104$indi_inc

x <- vector("list", length(a))
for(i in 1:length(a)){
        x[[i]] <- rep(b[i], times = a[i])
}
x <- unlist(x) %>% enframe()
m <- median(x$value, na.rm = TRUE) %>% round(2)
x <- x %>% mutate(poverty = case_when(value < m * 0.5 ~ 1,
                                      value >= m * 0.5 ~ 0)
                  )
PercTable(x$poverty, digits = 2)

##### overall household #####
# m <- median(df.inc104$indi_inc, na.rm = TRUE)
df.inc104 <- df.inc104 %>% mutate(
        poverty = case_when(indi_inc < m * 0.5 ~ 1,
                            indi_inc >= m * 0.5 ~ 0)
        )
# as.factor
df.inc104[ , "poverty"] %<>% mutate_if(is.numeric, as.factor)
PercTable(df.inc104$poverty, digits = 2)