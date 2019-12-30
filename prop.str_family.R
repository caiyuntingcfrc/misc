
# prep --------------------------------------------------------------------
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
h.marital <- vector("numeric", nrow(df))
for(i in 1:nrow(df)) {
        n <- names(df[i, l][ , which(df[i, l] == df$itm101[i])]) %>%
                str_split(., "_", simplify = TRUE)
        n <- as.numeric(n[ , 2])
        s <- paste("b3_", n, sep = "")
        m <- paste("b16_", n, sep = "")
        f <- df[[s]][i]
        h.num[i] <- n
        h.sex[i] <- f
        h.marital[i] <- df[[m]][i]
        # h_sex[i] <- df[[s]][i] %>% 
        #         as.character() %>% 
        #         as.numeric()
        }
df$`h.num` <- h.num
df$`h.sex` <- h.sex
df$`h.marital` <- h.marital

# # head's ID and marital status --------------------------------------------
# df$`h.marital` <- NULL
# h.id <- vector("numeric", nrow(df))
# h.marital <- vector("numeric", nrow(df))
# l <- grep("^b2_", names(df))
# for(i in 1:nrow(df)) { 
#         # search for head
#         n <- names(df[i, l][ , which(df[i, l] == 1)]) %>% 
#                 str_split(., "_", simplify = TRUE)
#         # heawd's marital status
#         m <- paste("b16_", n[ , 2], sep = "")
#         id <- paste("b1_", n[ , 2], sep = "")
#         h.id[i] <- df[[id]][i]
#         h.marital[i] <- df[[m]][i]
#         }
# df$`h.id` <- h.id
# df$`h.marital` <- h.marital
# table(h.marital)
# table(h.id)
# 

# head's parents and grand parents' marital status ------------------------

# head's parents's variable name of marital status
df$`p.marital` <- NULL
p.marital <- vector("numeric", nrow(df))
l <- grep("^b2", names(df))
for(i in 1:nrow(df)) { 
        # search for head (identity: parents)
        n <- names(df[i, l][ , which(df[i, l] == 5)])
        # head had only one of the parents
        if( length(n) == 1 ) { 
                n <- str_split(n, "_", simplify = TRUE)
                n <- paste("b16_", n[ , 2], sep = "")
                p.marital[i] <- df[[n]][i]
                } else { p.marital[i] <- NA }
        }
df$`p.marital` <- p.marital
table(p.marital, useNA = "ifany")

# head's grandparents
df$`g.marital` <- NULL
g.marital <- vector("numeric", nrow(df))
l <- grep("^b2", names(df))
for(i in 1:nrow(df)) { 
        # search for head (identity: grandparents)
        n <- names(df[i, l][ , which(df[i, l] == 6)])
        # head had only one of the grandparents
        if( length(n) == 1 ) { 
                n <- str_split(n, "_", simplify = TRUE)
                n <- paste("b16_", n[ , 2], sep = "")
                g.marital[i] <- df[[n]][i]
                } else { g.marital[i] <- NA }
        }
df$`g.marital` <- g.marital
table(g.marital, useNA = "ifany")

# if they are overlapping
# stem family and both the parent and the grandparent are single, widow or widower
test <- df %>% filter(!is.na(p.marital) & !is.na(g.marital))

# test: single parent family ----------------------------------------------
# the head is 3rd gen
l <- grep("^b2_", names(df))
l2 <- grep("^b16_", names(df))
sf <- vector("character", nrow(df))
for( i in 1:nrow(df)) {
        # only one of the parents
        if(length(grep("^[5]$", df[i, l])) == 1 &
           # only one of the parents, the head, and siblings
           length(grep("^[1]$|^[5]$|^[7]$", df[i, l])) == n.all[i] & 
           # only unmarried, divorced, or widowed
           length(grep("^[9][1]$|^[9][7]$", df[i, l2])) == n.all[i] &
           # only the parent is divorced or widowed
           sum(df[i, l2] %in% c(97)) == 1 & 
           # the head is unmarried
           h.marital[i] == 91 &
           # the head is male
           h.sex[i] == 1 ) {
                
                sf[i] <- "331"
                
                } else if (length(grep("^[5]$", df[i, l])) == 1 &
                           length(grep("^[1]$|^[5]$|^[7]$", df[i, l])) == n.all[i] & 
                           length(grep("^[9][1]$|^[9][7]$", df[i ,l2])) == n.all[i] &
                           sum(df[i, l2] %in% c(97)) == 1 & 
                           h.marital[i] == 91 &
                           h.sex[i] == 2) { 
                                
                                sf[i] <- "332" 
                                
                                }
        }
df$`sf` <- sf

d.single <- df %>% filter(sf %in% c(331, 332)) %>% 
        select(matches("^x1|^b2_|^b16_|a18|sf"))
d.single2 <- inc107 %>% filter(a18 %in% c(331, 332)) %>% 
        select(matches("^x1|^b2_|^b16_|a18"))
diff <- d.single[!(d.single$x1 %in% d.single2$x1), ]

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
                                     h.sex == 2 ~ "202"
                             
                             ))

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
