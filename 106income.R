rm(list = ls())
library(tidyverse)

#read fixed width file (temp)
df <- read_fwf("home/HDATA106", fwf_positions(start = c(1, 4, 9, 79), end = c(8, 8, 78, 80))) 
colnames(df) <- paste("V", seq(1, 4), sep = "")
#split by groups (list)
df_g <- df %>% split(., .[ , "V2"])
#test with g1
df_g1 <- as.data.frame(df_g$`00001`)
test <- spread(df_g1, V4, V3)
#create a function for splitting by group into a list (tibble forma)
split_tibble <- function(tibble, col) tibble %>% split(., .[ , col])
df_g <- split_tibble(df, "X2")

head(home.df); tail(home.df)
home.df[1]

#tidyr spread gather
stocks <- data.frame(
    time = as.Date('2009-01-01') + 0:9,
    X = rnorm(10, 0, 1),
    Y = rnorm(10, 0, 2),
    Z = rnorm(10, 0, 4)
)
stocksm <- stocks %>% gather(stock, price, -time)
stocksm %>% spread(stock, price)
stocksm %>% spread(time, price)
