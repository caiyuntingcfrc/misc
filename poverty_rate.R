##### author: CAI YUN-TING ######
##### culculate poverty rates #####

##### prep and options #####
# set working directory
setwd("~/R_wd/")
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
load("tw_inc/R data files/code_tbl_89.RData")
load("tw_inc/R data files/df_inc89.RData")
# df <- readRDS("tw_inc/R data files/df_inc89.rds")
df <- df.inc89

# Equivalized income ------------------------------------------------------

df <- df %>% 
        mutate(sqrt_scale = sqrt(a8) ,
               inc = itm400 - itm600,
               indi_inc = (itm400 - itm600) / sqrt_scale)

# calculate poverty threshold ---------------------------------------------
# parameters
weight <- "a21"
n.all <- "a8"

w <- df[[weight]]
i <- df[["indi_inc"]]
n <- df[[n.all]]
# replicate income by weight
w1 <- i[rep(1:length(i), times = w)]
w2 <- n[rep(1:length(n), times = w)]
weighed <- w1[rep(1:length(w1), times = w2)]
# calculate the median and poverty threshold
t <- median(w1, na.rm = TRUE) * 0.5

p.prop <- function(df, w, inc, t) {
        # weight
        a <- df[[w]]
        b <- df[[inc]]
        # replicate income by weight
        x <- b[rep(1:length(b), times = a)]
        y <- x < t
        p <- length(y[y == TRUE]) / length(y) * 100
        return(p)
        }

# overall households ------------------------------------------------------

d <- df
p_all <- p.prop(d, "a21", "indi_inc", t)

# male headed households --------------------------------------------------

# filter by head's sex
d <- df %>% filter(a7 == 1)
p_m_head <- p.prop(d, "a21", "indi_inc", t)

# female headed households ------------------------------------------------

# filter by head's sex
d <- df %>% filter(a7 == 2)
p_f_head <- p.prop(d, "a21", "indi_inc", t)

# house with aged ---------------------------------------------------------

# filter by number of aged
d <- df %>% filter(a19 >= 1)
d <- df %>% filter_at(vars(matches("^b4_")), any_vars(. >= 65))
p_with_aged <- p.prop(d, "a21", "indi_inc", t)

# house with aged (>= 75) ---------------------------------------------------

d <- df %>% filter_at(vars(matches("^b4_")), any_vars(. >= 75))
p_with_aged75 <- p.prop(d, "a21", "indi_inc", t)

# house with aged (>= 85) -------------------------------------------------

d <- df %>% filter_at(., 
                      vars(matches("^b4_")), 
                      any_vars(. >= 85))
p_with_aged85 <- p.prop(d, "a21", "indi_inc", t)


# house without aged ------------------------------------------------------

# filter by number of aged
d <- df %>% filter(a19 < 1)
p_without_aged <- p.prop(d, "a21", "indi_inc", t)

# overall single-parent families ------------------------------------------

d <- df %>% 
        # single parent families
        filter(a18 %in% c(321, 322, 331, 332)) %>% 
        # number of children is larger than 0
        filter((a8 - a12) != 0) %>% 
        # with children that are under 18 
        filter_at(vars(starts_with("b4_")), any_vars(. < 18))
p_single_parent <- p.prop(d, "a21", "indi_inc", t)

# male single-parent families ---------------------------------------------

d <- df %>% 
        # male head
        filter(a7 == 1) %>% 
        # single parent families
        filter(a18 %in% c(321, 322, 331, 332)) %>% 
        # number of children is larger than 0
        filter((a8 - a12) > 0) %>% 
        # with children that are under 18
        filter_at(vars(starts_with("b4_")), any_vars(. < 18))
p_single_male_parent <- p.prop(d, "a21", "indi_inc", t)

# female single-parent families -------------------------------------------

d <- df %>% 
        # female head
        filter(a7 == 2) %>% 
        filter(a18 %in% c(321, 322, 331, 332)) %>% 
        filter((a8 - a12) > 0) %>% 
        filter_at(vars(starts_with("b4_")), any_vars(. < 18))
p_single_female_parent <- p.prop(d, "a21", "indi_inc", t)


# household with children -------------------------------------------------

d <- df %>% 
        # having 1 or more children
        filter((a8 - a12) > 0) %>% 
        filter_at(vars(matches("^b4_")), any_vars(. < 18)) %>% 
        .[.[[type]] %in% c(321, 322, 331, 332), ]
p.prop(d, "a21", "indi_inc", t)

# test
n.all <- "a8"
n.adult <- "a12"
type <- "a18"
d <- df %>% 
        .[.[[n.all]] - .[[n.adult]] > 0, ] %>%
        filter_at(vars(matches("^b4_")), any_vars(. < 18)) %>% 
        filter_at(type, all_vars(. %in% c(321, 322, 331, 332)))
p.prop(d, "a21", "indi_inc", t)

# household with children (0 - 5) -----------------------------------------

d <- df %>% 
        # having 1 or more children
        filter((a8 - a12) > 0) %>% 
        filter_at(vars(matches("^b4_")), any_vars(. < 6 & . >= 0))
        # filter((a8 - a12) > 0)
p.prop(d, "a21", "indi_inc", t)


# household with children (0 - 11) ----------------------------------------

d <- df %>% 
        # having 1 or more children
        filter((a8 - a12) > 0) %>% 
        filter_at(vars(matches("^b4_")), any_vars(. < 12 & . >= 0))
# filter((a8 - a12) > 0)
p.prop(d, "a21", "indi_inc", t)

# household with children (0 - 17) ----------------------------------------

d <- df %>% 
        # having 1 or more children
        filter((a8 - a12) > 0) %>% 
        filter_at(vars(matches("^b4_")), any_vars(. < 18 & . >= 0))
# filter((a8 - a12) > 0)
p.prop(d, "a21", "indi_inc", t)

# overall population ------------------------------------------------------

d <- df
i <- d[["indi_inc"]]
n <- d[["a8"]]
w <- d[["a21"]]
# weigh by weight ("a21")
w1 <- i[rep(1:length(i), times = w)]
w2 <- n[rep(1:length(n), times = w)]
# weigh by numbers of people in the house ("a8")
weighed <- w1[rep(1:length(w1), times = w2)]
# below threshold
r <- weighed < t
# calculate the proportion
p_overall <- length(r[r == TRUE]) / length(r) * 100

# elderly population ------------------------------------------------------

d <- df
i <- d[["indi_inc"]]
n <- d[["a19"]]
w <- d[["a21"]]
# weigh by weight ("a21")
w1 <- i[rep(1:length(i), times = w)]
w2 <- n[rep(1:length(n), times = w)]
# weigh by numbers of aged in the house ("a8")
weighed <- w1[rep(1:length(w1), times = w2)]
# below threshold
r <- weighed < t
# calculate the proportion
p_elderly <- length(r[r == TRUE]) / length(r) * 100

# d <- bind_rows(p_all, p_m_head, p_f_head,
#                p_with_aged, p_without_aged,
#                p_single_parent, p_single_male_parent, p_single_female_parent)

# d$level <- c("Overall household", "Male headed households", "female headed households",
#              "Household with aged", "Household without aged",
#              "Overall single-parent", "Male single-parent", "Female single-parent")
# colnames(d) <- c("Poverty rate (%)", "2000")