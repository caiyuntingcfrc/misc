##### culculate poverty rates #####

# prep and options --------------------------------------------------------

# set working directory
setwd("~/")
# clear objects
rm(list = ls())
# load the packages
library(tidyverse)
# options
options(scipen = 999)

# load the data file ------------------------------------------------------

# data file 107 (dataframe: df.inc107)
load(url("https://disk.cfrc.ntu.edu.tw/index.php/s/dETZkK3XiD7fnMa/download"))
# code book 107
load(url("https://disk.cfrc.ntu.edu.tw/index.php/s/TZ4E2aQ3bzTnETj/download"))

df <- df.inc107

# Equivalized income ------------------------------------------------------
# recode new variables: sqrt_scale, inc, eq_inc
df <- df %>% 
        mutate(sqrt_scale = sqrt(a8) ,
               inc = itm400 - itm600,
               eq_inc = (itm400 - itm600) / sqrt_scale)

# calculate poverty threshold ---------------------------------------------

# parameters
weight <- "a20"
n.all <- "a8"

w <- df[[weight]]
i <- df[["eq_inc"]]
n <- df[[n.all]]

# replicate income by weight
w1 <- i[rep(1:length(i), times = w)]
# wight by numbers of people in the household (weighed 2nd time)
w2 <- n[rep(1:length(n), times = w)]
weighed <- w1[rep(1:length(w1), times = w2)]
# calculate the median and poverty threshold
t <- median(w1, na.rm = TRUE) * 0.5
t2 <- median(weighed, na.rm = TRUE) * 0.5

# funtcion p.prop
p.prop <- function(data, weight, inc, threshold) {
        # weight
        w <- data[[weight]]
        i <- data[[inc]]
        # replicate income by weight
        w1 <- i[rep(1:length(i), times = w)]
        y <- w1 < threshold
        p <- length(y[y == TRUE]) / length(y) * 100
        return(p)
        }

# overall households ------------------------------------------------------

d <- df
p_all <- p.prop(data = d, 
                weight = "a20", 
                inc = "eq_inc", 
                threshold = t)

# male headed households --------------------------------------------------

# filter by head's sex
d <- df %>% filter(a7 == 1)
p_m_head <- p.prop(data = d, 
                   weight = "a20", 
                   inc = "eq_inc", 
                   threshold = t)

# female headed households ------------------------------------------------

# filter by head's sex
d <- df %>% filter(a7 == 2)
p_f_head <- p.prop(data = d, 
                   weight = "a20", 
                   inc = "eq_inc", 
                   threshold = t)

# house with aged ---------------------------------------------------------

# filter by number of aged
# at least one aged (b4_: age of the interviewee)
d <- df %>% filter_at(vars(matches("^b4_")), any_vars(. >= 65))
p_with_aged <- p.prop(data = d, 
                      weight = "a20", 
                      inc = "eq_inc", 
                      threshold = t)

# house with aged (>= 75) ---------------------------------------------------

d <- df %>% filter_at(vars(matches("^b4_")), any_vars(. >= 75))
p_with_aged75 <- p.prop(data = d, 
                        weight = "a20", 
                        inc = "eq_inc", 
                        threshold = t)

# house with aged (>= 85) -------------------------------------------------

d <- df %>% filter_at(vars(matches("^b4_")), any_vars(. >= 85))
p_with_aged85 <- p.prop(data = d, 
                        weight = "a20", 
                        inc = "eq_inc", 
                        threshold = t)

# house without aged ------------------------------------------------------
# filter by number of aged
d <- df %>% filter(a19 == 0)
p_without_aged <- p.prop(data = d, 
                         weight = "a20", 
                         inc = "eq_inc", 
                         threshold = t)

# overall single-parent families ------------------------------------------

d <- df %>% 
        # single parent families
        filter(a18 %in% c(321, 322, 331, 332)) %>% 
        # with children that are under 18 
        filter_at(vars(starts_with("b4_")), any_vars(. < 18))

p_single_parent <- p.prop(data = d, 
                          weight = "a20", 
                          inc = "eq_inc", 
                          threshold = t)

# male single-parent families ---------------------------------------------

d <- df %>% 
        # male head
        filter(a7 == 1) %>% 
        # single parent families
        filter(a18 %in% c(321, 322, 331, 332)) %>% 
        # with children that are under 18
        filter_at(vars(starts_with("b4_")), any_vars(. < 18))

p_single_male_parent <- p.prop(data = d, 
                               weight = "a20", 
                               inc = "eq_inc", 
                               threshold = t)

# female single-parent families -------------------------------------------

d <- df %>% 
        # female head
        filter(a7 == 2) %>% 
        filter(a18 %in% c(321, 322, 331, 332)) %>% 
        filter_at(vars(starts_with("b4_")), any_vars(. < 18))

p_single_female_parent <- p.prop(data = d, 
                                 weight = "a20", 
                                 inc = "eq_inc", 
                                 threshold = t)

# household with children -------------------------------------------------

d <- df %>% 
        # having 1 or more children
        filter_at(vars(matches("^b4_")), any_vars(. < 18 & . >= 0))

p_with_children <- p.prop(data = d, 
                          weight = "a20", 
                          inc = "eq_inc", 
                          threshold = t)

# household with children (<6) --------------------------------------------

d <- df %>% 
        # having 1 or more children
        filter_at(vars(matches("^b4_")), any_vars(. < 6 & . >= 0))

p_with_children5 <- p.prop(data = d, 
                           weight = "a20", 
                           inc = "eq_inc", 
                           threshold = t)

# household with children (<12) -------------------------------------------

d <- df %>% 
        # having 1 or more children
        filter_at(vars(matches("^b4_")), any_vars(. < 12 & . >= 0))

p_with_children11 <- p.prop(data = d, 
                            weight = "a20", 
                            inc = "eq_inc", 
                            threshold = t)

# household with children by group of numbers -----------------------------
d <- df %>% 
        # having 1 or more children
        filter_at(vars(matches("^b4_")), any_vars(. < 18 & . >= 0))

# grep
l <- grep("^b4_", names(d))

# nmbers of children
d$`n.children` <- NA
for(i in 1:nrow(d)) {
        d$`n.children`[i] <- length(which(d[i, l] < 18 & d[i, l] >= 0))
        }
# mutate by groups of age
d <- d %>% 
        mutate(g.children = case_when(n.children > 0 & n.children <= 1 ~ 1,
                                      n.children > 1 & n.children <= 2 ~ 2, 
                                      n.children >= 3 ~ 3)) %>%         
        mutate_at(vars(matches("g.children")), as.factor)
# summary
summarytools::freq(d$g.children)

# having 1 child
d1 <- d %>% 
        filter(g.children == 1)

p_with_children_1c <- p.prop(data = d1, 
                             weight = "a20", 
                             inc = "eq_inc", 
                             threshold = t)
# having 2 children
d2 <- d %>% 
        filter(g.children == 2)

p_with_children_2c <- p.prop(data = d2, 
                             weight = "a20", 
                             inc = "eq_inc", 
                             threshold = t)

# having 3 or more children
d3 <- d %>% 
        filter(g.children == 3)

p_with_children_3c <- p.prop(data = d3, 
                             weight = "a20", 
                             inc = "eq_inc", 
                             threshold = t)

# household with children(<6) by group of numbers -------------------------
d <- df %>% 
        # having 1 or more children
        filter_at(vars(matches("^b4_")), any_vars(. < 6 & . >= 0))

# grep
l <- grep("^b4_", names(d))

# nmbers of children
d$`n.children` <- NA
for(i in 1:nrow(d)) {
        d$`n.children`[i] <- length(which(d[i, l] < 6 & d[i, l] >= 0))
        }
# mutate by groups of age
d <- d %>% 
        mutate(g.children = case_when(n.children > 0 & n.children <= 1 ~ 1,
                                      n.children > 1 & n.children <= 2 ~ 2, 
                                      n.children >= 3 ~ 3))
# summary
summarytools::freq(d$g.children)
# having 1 child
d1 <- d %>% 
        filter(g.children == 1)

p_with_children5_1c <- p.prop(data = d1, 
                             weight = "a20", 
                             inc = "eq_inc", 
                             threshold = t)
# having 2 children
d2 <- d %>% 
        filter(g.children == 2)

p_with_children5_2c <- p.prop(data = d2, 
                             weight = "a20", 
                             inc = "eq_inc", 
                             threshold = t)

# having 3 or more children
d3 <- d %>% 
        filter(g.children == 3)

p_with_children5_3c <- p.prop(data = d3, 
                             weight = "a20", 
                             inc = "eq_inc", 
                             threshold = t)

# household with children(<12) by group of numbers -------------------------
d <- df %>% 
        # having 1 or more children
        filter_at(vars(matches("^b4_")), any_vars(. < 12 & . >= 0))

# grep
l <- grep("^b4_", names(d))

# nmbers of children
d$`n.children` <- NA
for(i in 1:nrow(d)) {
        d$`n.children`[i] <- length(which(d[i, l] < 12 & d[i, l] >= 0))
        }
# mutate by groups of age
d <- d %>% 
        mutate(g.children = case_when(n.children > 0 & n.children <= 1 ~ 1,
                                      n.children > 1 & n.children <= 2 ~ 2, 
                                      n.children >= 3 ~ 3))
# summary
tbl <- summarytools::freq(d$g.children, 
                          report.nas = FALSE); tbl
# having 1 child
d1 <- d %>% 
        filter(g.children == 1)

p_with_children11_1c <- p.prop(data = d1, 
                             weight = "a20", 
                             inc = "eq_inc", 
                             threshold = t)
# having 2 children
d2 <- d %>% 
        filter(g.children == 2)

p_with_children11_2c <- p.prop(data = d2, 
                             weight = "a20", 
                             inc = "eq_inc", 
                             threshold = t)

# having 3 or more children
d3 <- d %>% 
        filter(g.children == 3)

p_with_children11_3c <- p.prop(data = d3, 
                             weight = "a20", 
                             inc = "eq_inc", 
                             threshold = t)

# overall population ------------------------------------------------------

d <- df
i <- d[["eq_inc"]]
n <- d[["a8"]]
w <- d[["a20"]]
# weigh by weight
w1 <- i[rep(1:length(i), times = w)]
w2 <- n[rep(1:length(n), times = w)]
# weigh by numbers of people in the house
weighed <- w1[rep(1:length(w1), times = w2)]
# below threshold
r <- weighed < t
# calculate the proportion
p_overall <- length(r[r == TRUE]) / length(r) * 100


# children population -----------------------------------------------------

d <- df %>% 
        # having 1 or more children
        filter_at(vars(matches("^b4_")), any_vars(. < 18 & . >= 0))

# grep
l <- grep("^b4_", names(d))
# nmbers of children
d$`n.children` <- NA
for(i in 1:nrow(d)) {
        d$`n.children`[i] <- length(which(d[i, l] < 18 & d[i, l] >= 0))
        }

# weigh
i <- d[["eq_inc"]]
n <- d[["n.children"]]
w <- d[["a20"]]
w1 <- i[rep(1:length(i), times = w)]
w2 <- n[rep(1:length(n), times = w)]
# weigh by numbers of children in the house
weighed <- w1[rep(1:length(w1), times = w2)]
# below threshold
r <- weighed < t
# calculate the proportion
p_children <- length(r[r == TRUE]) / length(r) * 100


# children population (0-5) -----------------------------------------------

d <- df %>% 
        # having 1 or more children
        filter_at(vars(matches("^b4_")), any_vars(. < 6 & . >= 0))
# grep
l <- grep("^b4_", names(d))
# nmbers of children
d$`n.children` <- NA
for(i in 1:nrow(d)) {
        d$`n.children`[i] <- length(which(d[i, l] < 6 & d[i, l] >= 0))
        }
# weigh
i <- d[["eq_inc"]]
n <- d[["n.children"]]
w <- d[["a20"]]
w1 <- i[rep(1:length(i), times = w)]
w2 <- n[rep(1:length(n), times = w)]
# weigh by numbers of children in the house
weighed <- w1[rep(1:length(w1), times = w2)]
# below threshold
r <- weighed < t
# calculate the proportion
p_children5 <- length(r[r == TRUE]) / length(r) * 100


# children population (0-11) ----------------------------------------------

d <- df %>% 
        # having 1 or more children
        filter_at(vars(matches("^b4_")), any_vars(. < 11 & . >= 0))
# grep
l <- grep("^b4_", names(d))
# nmbers of children
d$`n.children` <- NA
for(i in 1:nrow(d)) {
        d$`n.children`[i] <- length(which(d[i, l] < 11 & d[i, l] >= 0))
        }
# weigh
i <- d[["eq_inc"]]
n <- d[["n.children"]]
w <- d[["a20"]]
w1 <- i[rep(1:length(i), times = w)]
w2 <- n[rep(1:length(n), times = w)]
# weigh by numbers of children in the house
weighed <- w1[rep(1:length(w1), times = w2)]
# below threshold
r <- weighed < t
# calculate the proportion
p_children11 <- length(r[r == TRUE]) / length(r) * 100

# elderly population ------------------------------------------------------

d <- df %>% 
        # having 1 or more children
        filter_at(vars(matches("^b4_")), any_vars(. >= 65))
# grep
l <- grep("^b4_", names(d))
# nmbers of children
d$`n.elder` <- NA
for(i in 1:nrow(d)) {
        d$`n.elder`[i] <- length(which(d[i, l] >= 65))
        }

i <- d[["eq_inc"]]
n <- d[["n.elder"]]
w <- d[["a20"]]
# weigh by weight ("a21")
w1 <- i[rep(1:length(i), times = w)]
w2 <- n[rep(1:length(n), times = w)]
# weigh by numbers of aged in the house
weighed <- w1[rep(1:length(w1), times = w2)]
# below threshold
r <- weighed < t
# calculate the proportion
p_elderly <- length(r[r == TRUE]) / length(r) * 100


# elderly population (>=75) -----------------------------------------------

d <- df %>% 
        # having 1 or more children
        filter_at(vars(matches("^b4_")), any_vars(. >= 75))

# grep
l <- grep("^b4_", names(d))
# nmbers of children
d$`n.elder` <- NA
for(i in 1:nrow(d)) {
        d$`n.elder`[i] <- length(which(d[i, l] >= 75))
}

i <- d[["eq_inc"]]
n <- d[["n.elder"]]
w <- d[["a20"]]
# weigh by weight ("a21")
w1 <- i[rep(1:length(i), times = w)]
w2 <- n[rep(1:length(n), times = w)]
# weigh by numbers of aged in the house
weighed <- w1[rep(1:length(w1), times = w2)]
# below threshold
r <- weighed < t
# calculate the proportion
p_elderly75 <- length(r[r == TRUE]) / length(r) * 100

# elderly population (>=85) -----------------------------------------------

d <- df %>% 
        # having 1 or more children
        filter_at(vars(matches("^b4_")), any_vars(. >= 85))

# grep
l <- grep("^b4_", names(d))
# nmbers of children
d$`n.elder` <- NA
for(i in 1:nrow(d)) {
        d$`n.elder`[i] <- length(which(d[i, l] >= 85))
}

i <- d[["eq_inc"]]
n <- d[["n.elder"]]
w <- d[["a20"]]
# weigh by weight ("a21")
w1 <- i[rep(1:length(i), times = w)]
w2 <- n[rep(1:length(n), times = w)]
# weigh by numbers of aged in the house
weighed <- w1[rep(1:length(w1), times = w2)]
# below threshold
r <- weighed < t
# calculate the proportion
p_elderly85 <- length(r[r == TRUE]) / length(r) * 100