rm(list = ls())
setwd("d:/R_wd/tw_inc/R data files/")
source("d:/R_wd/R studies/R scripts/func_ins.pack.R")
ins.pack("feather", "tidyverse", "magrittr")
# load --------------------------------------------------------------------
# file path
# file_path <- dir() %>% 
#         grep("^df_.*.feather", ., value = TRUE) %>% 
#         paste(getwd(), "/", ., sep = "")
# read feather
# l <- vector("list", length(file_path))
# l <- lapply(file_path, read_feather)
inc107 <- read_feather("df_inc107.feather")

# recode test -------------------------------------------------------------

df <- inc107

# factor to numeric (bxx_)
l <- grep("^b|itm101$", names(df), value = TRUE)
df[ , l] <- lapply(df[ , l], as.character) %>% lapply(., as.numeric)

# numbers of people -------------------------------------------------------

l <- grep("^b4_", names(df))
df$`n.all` <- NULL
n.all <- vector("numeric", nrow(df))
for(i in 1:nrow(df)) { 
        n.all[i] <- length(which(!is.na(df[i, l])))
        }
df$`n.all` <- n.all


# numbers of adults -------------------------------------------------------

df$`n.adults` <- NULL
n.adults <- vector("numeric", nrow(df))
for(i in 1:nrow(df)) {
        n.adults[i] <- length(which(df[i, l] >= 18))
        }
df$`n.adults` <- n.adults

# numbers of children -----------------------------------------------------

df$`n.children` <- NULL
n.children <- vector("numeric", nrow(df))
for(i in 1:nrow(df)) {
        n.children[i] <- length(which(df[i, l] < 18))
        }
df$`n.children` <- n.children

# numbers of the elder ----------------------------------------------------

df$`n.elder` <- NULL
n.elder <- vector("numeric", nrow(df))
for(i in 1:nrow(df)) {
        n.elder[i] <- length(which(df[i, l] >= 65))
        }
df$`n.elder` <- n.elder


# head's var number and sex -----------------------------------------------


l <- grep("^b1_", names(df), value = TRUE)
df$`h.sex` <- NULL
df$`h.num` <- NULL
h.sex <- vector("numeric", nrow(df))
h.num <- vector("numeric", nrow(df))
for(i in 1:nrow(df)) {
        n <- names(df[i, l][ , which(df[i, l] == df$itm101[i])]) %>%
                str_split(., "_", simplify = TRUE)
        n <- as.numeric(n[ , 2])
        s <- paste("b3_", n, sep = "")
        f <- df[[s]][i]
        h.num[i] <- n
        h.sex[i] <- f
        # h_sex[i] <- df[[s]][i] %>% 
        #         as.character() %>% 
        #         as.numeric()
        }
df$`h.num` <- h.num
df$`h.sex` <- h.sex

# gen ---------------------------------------------------------------------
# having grand children: g1
gen <- vector("numeric", nrow(df))
df$`gen` <- NULL
l <- grep("^b2_", names(df))
for(i in 1:nrow(df)) {
        if(sum(df[i, l] %in% c(6), na.rm = TRUE) > 0) {
                gen[i] <- 3
                } else if (sum(df[i, l] %in% c(4), na.rm = TRUE) > 0) {
                        gen[i] <- 1
                        }
        }
table(gen)
l1 <- grep("^b2_", names(df))
l2 <- grep("^b16_", names(df))
a <- df[3, l1] %>% as.integer()
b <- df[157, l2] %>% as.integer()
table(a)

test <- inc107 %>% slice(3)
test$a18
t <- test %>% select(matches("^b2_|a18"))
# filter test single-parent -----------------------------------------------

# l1 <- grep("^b2_", names(df))
# l2 <- grep("^b16_", names(df))
# d1 <- df[ , l1]
# d2 <- df[ , l2]
# c1 <- d1 == 1 | d1==3 | d1 == 5 | d1 == 7
# c2 <- d2 == 91 | d2 == 97
# cc <- c1 & c2
# tc <- apply(cc, 1, all, na.rm = TRUE)
# dtest <- df[tc, ]
# test <- df %>% 
#         .[unique(which(c, arr.ind = TRUE)[ , 1]), ] 
#         .[which(df[ , l2] == "91" | df[ , l] == "97"), ]

# SF-single-person --------------------------------------------------------

d <- df
d %<>% mutate(sf = case_when(# single-person
                             n.all == 1 & h.sex == 1 ~ "101", 
                             n.all == 1 & h.sex == 2 ~ "102", 
                             # married-couple
                             n.all == 2 & 
                                     n.adults == 2 & 
                                     b2_1 %in% c(1, 2) & b2_2 %in% c(1, 2) &
                                     h.sex == 1 ~ "201", 
                             n.all == 2 & 
                                     n.adults == 2 & 
                                     b2_1 %in% c(1, 2) & b2_2 %in% c(1, 2) &
                                     h.sex == 2 ~ "202", 
                             
                             TRUE ~ NA_character_))
# singel-parent
l1 <- grep("^b2_", names(d))
l2 <- grep("^b16_", names(d))
t <- vector("character", nrow(d))
df$t <- t
for(i in 1:nrow(d)) {
        if(!(d$sf[i] %in% c("101", "102", "201", "202")) &
           length(grep("^1$|^3$", df[i, l1])) == df$n.all[i] &
           length(grep("^[9][1]|^[9][7]", df[i, l2])) == df$n.all[i]) {
                df$t[i] <- "T"
                }
        }
table(df$t)
table(df$a18)
test0 <- df %>% filter(t == "T") %>% select(-t)
table(test0$a18)
test1 <- df %>% 
        filter(a18 %in% c(531, 532, 431 ,432, 631, 632))
tt <- test0[!(test0$x1 %in% test$x1), ]
# test --------------------------------------------------------------------

# one-person male headed
d1 <- df %>% filter(h_sex == 1 & n.all == 1)
d2 <- inc107 %>% filter(a18 == 101)
table(d1$a18 == d2$a18)
# one-person female headed
d1 <- df %>% filter(h_sex == 2 & n.all == 1)
d2 <- inc107 %>% filter(a18 == 102)
table(d1$a18 == d2$a18)

# married-couple
# male headed
d1 <- df %>% filter(h_sex == 1 & n.all == 2)
d2 <- inc107 %>% filter(a18 %in% c(201, 202))
test <- d1 %>% slice(1)
l1 <- grep("^b1_", names(df))
l16 <- grep("^b16_", names(df))
a <- test[ ,l1]
b <- test[ ,l16]
a * b
which(a * b)
with(test, b1_1 * b16_1 == b1_2 * b16_2)
with(test, b1_2 * b16_2)
test$b1_1
test$b1_2
test$b16_1
test$b16_2

# recode ------------------------------------------------------------------
for(i in 1:9) {
        l[[i]] %<>% 
        mutate(str_family = case_when(a18 %in% c(101, 102) ~ 1L, 
                                      a18 %in% c(201, 202) ~ 2L, 
                                      a18 %in% c(321, 322, 331, 332) ~ 3L, 
                                      a18 %in% c(421, 422, 431, 432) ~ 4L, 
                                      a18 %in% c(511, 512, 531, 532) ~ 5L, 
                                      a18 %in% c(611, 612, 621, 622, 631, 632) ~ 6L, 
                                      a18 %in% c(701, 702) ~ 7L, 
                                      TRUE ~ NA_integer_))
        }
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

# weigh revised -----------------------------------------------------------

s <- d$str_family
w <- d$a20
x <- round(xtabs(w ~ s), digits = 0); x
n <- names(x)
weighed <- mapply(rep, x = n, times = x)
l <- unlist(weighed, use.names = FALSE)
table <- epiDisplay::tab1(l, decimal = 2, 
                          sort.group = "decreasing", 
                          graph = FALSE)
# freq --------------------------------------------------------------------

tab1(weighed, 
     decimal = 2,
     sort.group = "decreasing", 
     # bar.values = "percent", 
     main = "臺灣家庭組織型態（2018）")
tab1(w1, 
     decimal = 2,
     sort.group = "decreasing", 
     # bar.values = "percent", 
     main = "臺灣家庭組織型態（2018）")
# plot --------------------------------------------------------------------

plot_frq(d$str_family, weight.by = d$a20)
