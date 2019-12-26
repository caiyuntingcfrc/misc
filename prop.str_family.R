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
# numbers of people
l <- grep("^b4_", names(df))
df$`n.all` <- NA
n.all <- vector("numeric", nrow(df))
for(i in 1:nrow(df)) { 
        n.all[i] <- length(which(!is.na(df[i, l])))
        }
df$`n.all` <- n.all

# numbers of child
df$`n.children` <- NULL
n.children <- vector("numeric", nrow(df))
for(i in 1:nrow(df)) {
        n.children[i] <- length(which(df[i, l] < 18))
        }
df$`n.children` <- n.children

p <- Sys.time()
# head's sex
l <- grep("^b|itm101$", names(df), value = TRUE)
# factor to numeric
df[ , l] <- lapply(df[ , l], as.character) %>% lapply(., as.numeric)
l <- grep("^b1_", names(df), value = TRUE)
df$`h_sex` <- NULL
h_sex <- vector("numeric", nrow(df))
for(i in 1:nrow(df)) {
        n <- names(df[i, l][ , which(df[i, l] == df$itm101[i])]) %>%
                str_split(., "_", simplify = TRUE)
        n <- n[ , 2]
        s <- paste("b3_", n, sep = "")
        f <- df[[s]][i]
        # factor to numeric
        h_sex[i] <- as.numeric(as.character(f))
        # h_sex[i] <- df[[s]][i] %>% 
        #         as.character() %>% 
        #         as.numeric()
        }
df$`h_sex` <- h_sex
Sys.time() - p

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
d2 <- inc107 %>% filter(a18 == 201)
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
