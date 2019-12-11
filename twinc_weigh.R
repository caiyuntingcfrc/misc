rm(list = ls())
setwd("d:/R_wd/tw_inc/")
library(tidyverse)
library(parallel)
load("R data files/df_inc107.RData")

df <- df.inc107 %>% 
        mutate(sqrt_scale = sqrt(a8),
               eq_inc = (itm400 - itm600) / sqrt_scale) %>% 
        mutate(str_family = case_when(a18 %in% c(101, 102) ~ 1L, 
                                      a18 %in% c(201, 202) ~ 2L, 
                                      a18 %in% c(321, 322, 331, 332) ~ 3L, 
                                      a18 %in% c(421, 422, 431, 432) ~ 4L, 
                                      a18 %in% c(511, 512, 531, 532) ~ 5L, 
                                      a18 %in% c(611, 612, 621, 622, 631, 632) ~ 6L, 
                                      a18 %in% c(701, 702) ~ 7L, 
                                      TRUE ~ NA_integer_))
weight <- "a20"
inc <- "eq_inc"
n.all <- "a8"
x <- round(xtabs(df[[weight]] ~ df[[inc]]))
a <- names(x)
l <- mapply(rep, times = x, x = a, SIMPLIFY = TRUE)
l <- as.numeric(unlist(l, use.names = FALSE))
# threshold
t <- median(l, na.rm = TRUE) * 0.5

tab <- round(xtabs(df[[weight]] ~ df[[inc]]))
n <- names(tab)
l <- mapply(rep, times = tab, x = n, SIMPLIFY = TRUE)
l <- as.numeric(unlist(l, use.names = FALSE))
r <- l < t
p_overallH <- length(r[r == TRUE]) / length(r) * 100
# double weighing
n <- df[[n.all]]
w <- df[[weight]]
i <- df[[inc]]
i1 <- i[rep(1:length(i), times = n)]
w1 <- w[rep(1:length(w), times = n)]
xxtab <- round(xtabs(w1 ~ i1))
n3 <- names(xxtab)
ll <- mapply(rep, times = xxtab, x = n3, SIMPLIFY = TRUE)
ll <- as.numeric(unlist(ll, use.names = FALSE))
r <- ll < t
p_overallP <- length(r[r == TRUE]) / length(r) * 100

# structure of families
xfami <- round(xtabs(df$a20 ~ df$str_family))
f <- names(xfami)
l <- mapply(rep, times = xfami, x = f, SIMPLIFY = TRUE)
l <- as.numeric(unlist(l, use.names = FALSE))
epiDisplay::tab1(l, decimal = 2)
summary(l)
