##### author: CAI YUN-TING ######
##### poverty rate 2000 #####
##### prep and options #####
# set working directory
setwd("D:/R_wd/")
# clear objects
rm(list = ls())
# load the packages
l <- c("tidyverse", "magrittr", "haven", 
       "gridExtra", "summarytools")
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
load("tw_inc/R data files/code_tbl_107.RData")
# data file
load("tw_inc/R data files/df_inc107.RData")

df <- df.inc107

##### Equivalised income #####
df <- df %>% 
        mutate(sqrt_scale = sqrt(a8) ,
               inc = itm400 - itm600,
               indi_inc = (itm400 - itm600) / sqrt_scale)
# threshold
# t <- median(df$indi_inc, na.rm = TRUE) * 0.5

# overall household
a <- df[["inc"]]
b <- df[["a20"]]
x <- a[rep(1:length(a), times = b)]
summary(x)
# overall population
a <- df[["inc"]]
b <- df[["a8"]]
c <- df[["a20"]]
# inc
x <- a[rep(1:length(a), times = b)]
# weight
y <- c[rep(1:length(c), times = b)]
# n in the house
w <- b[rep(1:length(b), times = b)]
z <- x / w
s <- z[rep(1:length(z), times = y)]
summary(s)

# weighing by number of members in the house
a <- df[["indi_inc"]]
b <- df[["a8"]]
c <- df[["a20"]]
x <- a[rep(1:length(a), times = b)]
y <- c[rep(1:length(c), times = b)]
z <- x[rep(1:length(x), times = y)]
t <- median(z, na.rm = TRUE) * 0.5
r <- z < t
length(r[r == TRUE]) / length(r) * 100
summary(z)

# test
# rdata
load(url("https://cfrc.tbcloud.tk/index.php/s/TZ4E2aQ3bzTnETj/download"))
df <- df.inc107
##### Equivalised income #####
df <- df %>% 
        mutate(sqrt_scale = sqrt(a8) ,
               inc = itm400 - itm600,
               indi_inc = (itm400 - itm600) / sqrt_scale) %>% 
        select(inc, indi_inc, a8, a20)
test <- df[rep(1:nrow(df), times = df$a8), ]
test3 <- df[rep(1:nrow(df), times = df$a20), ]
test2 <- test[rep(1:nrow(test), times = test$a20), ]
test2 <- test2 %>% 
        mutate(mean_inc = inc / a8)
summary(test2$mean_inc); rm(test2)
summary(test3$inc); rm(test3)


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

##### functioin -- proportion #####
p.prop <- function(df, w, inc, m) {
        # weight
        a <- df[[w]]
        b <- df[[inc]]
        # replicate income by weight
        x <- b[rep(1:length(b), times = a)]
        y <- x < (m * 0.5)
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