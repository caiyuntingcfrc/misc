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
       "magrittr", "haven", "gridExtra", 
       "microbenchmark")
lapply(l, require, character.only= TRUE)
rm(l)
# options
options(scipen = 999)
# options for summarytools
st_options(style = "simple",
           round.digits = 4,
           ctable.prop = "c")

##### load the RData file #####
# code book
load("AA170025/code_tbl_89.RData")
# data file
load("AA170025/inc89.RData")

##### Equivalised income #####
df.inc89 <- df.inc89 %>% 
        mutate(sqrt_scale = sqrt(a8) ,
               indi_inc = (itm400 - itm600) / sqrt_scale)

##### function -- poverty threshold #####
p.threshold <- function(df, w, inc) {
        # weight
        a <- df[[w]]
        b <- df[[inc]]
        # replicate income by weight
        x <- b[rep(1:length(b), times = a)]
        # calculate the median
        m <- median(x, na.rm = TRUE)
        return(m)
        }

##### (vanilla) function -- poverty threshold #####
# p.threshold <- function(df, w, inc) {
#         # weight
#         a <- df[[w]]
#         b <- df[[inc]]
#         x <- vector("list", length = length(a))
#         for(i in 1:length(a)) {
#                 x[[i]] <- rep(b[i], times = a[i])
#                 }
#         x <- unlist(x)
#         # calculate the median
#         m <- median(x, na.rm = TRUE)
#         return(m)
# }

##### functioin -- proportion #####
p.prop <- function(df, w, inc, m) {
        # weight
        a <- df[[w]]
        b <- df[[inc]]
        # replicate income by weight
        x <- b[rep(1:length(b), times = a)]
        y <- x < m * 0.5
        # calculate percentage
        p <- length(y[y == TRUE]) / length(y) * 100
        return(p)
        }

##### (vanilla) functioin -- proportion #####
# p.prop <- function(df, w, inc, m) {
#         # weight
#         a <- df[[w]]
#         b <- df[[inc]]
#         x <- vector("list", length = length(a))
#         for(i in 1:length(a)) {
#                 x[[i]] <- rep(b[i], times = a[i])
#         }
#         x <- unlist(x)
#         y <- x < m * 0.5
#         p <- Freq(y, useNA = "always") %>% 
#                 print(digits = 2) %>% 
#                 .[.$level == "TRUE", ] # %>% 
#                 # select( - one_of(c("freq", "cumfreq", "cumperc")))
#         return(p)
#         }

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
        # single parent families
        filter(a18 %in% c(321, 322, 331, 332)) %>% 
        # number of children is larger than 0
        filter((a8 - a12) != 0) %>% 
        # with children that are under 18
        filter_at(vars(starts_with("b4_")), any_vars(. < 18))
p_single_parent <- p.prop(df, "a21", "indi_inc", m)

##### male single-parent families ######
df <- df.inc89 %>% 
        # male head
        filter(a7 == 1) %>% 
        # single parent families
        filter(a18 %in% c(321, 322, 331, 332)) %>% 
        # number of children is larger than 0
        filter((a8 - a12) > 0) %>% 
        # with children that are under 18
        filter_at(vars(starts_with("b4_")), any_vars(. < 18))
p_single_male_parent <- p.prop(df, "a21", "indi_inc", m)

##### female single-parent families ######
df <- df.inc89 %>% 
        # female head
        filter(a7 == 2) %>% 
        filter(a18 %in% c(321, 322, 331, 332)) %>% 
        filter((a8 - a12) > 0) %>% 
        filter_at(vars(starts_with("b4_")), any_vars(. < 18))
p_single_female_parent <- p.prop(df, "a21", "indi_inc", m)

##### overall population #####
df <- df.inc89
a <- df[["indi_inc"]]
b <- df[["a8"]]
c <- df[["a21"]]
# weight by n of people in the house
x <- a[rep(1:length(a), times = b)]
y <- c[rep(1:length(c), times = b)]
# weight by weight
z <- x[rep(1:length(x), times = y)]
# below threshold
r <- z < m * 0.5
# calculate the proportion
p_overall <- length(r[r == TRUE]) / length(r) * 100

##### elderly population #####
df <- df.inc89
a <- df[["indi_inc"]]
b <- df[["a19"]]
c <- df[["a21"]]
# weight by n of people in the house
x <- a[rep(1:length(a), times = b)]
y <- c[rep(1:length(c), times = b)]
# weight by weight
z <- x[rep(1:length(x), times = y)]
# below threshold
r <- z < m * 0.5
# calculate the proportion
p_elderly <- length(r[r == TRUE]) / length(r) * 100

# d <- bind_rows(p_all, p_m_head, p_f_head,
#                p_with_aged, p_without_aged,
#                p_single_parent, p_single_male_parent, p_single_female_parent)

# d$level <- c("Overall household", "Male headed households", "female headed households",
#              "Household with aged", "Household without aged",
#              "Overall single-parent", "Male single-parent", "Female single-parent")
# colnames(d) <- c("Poverty rate (%)", "2000")
#
# ##### print #####
# pdf("data_output.pdf")
# grid.table(d)
# dev.off()