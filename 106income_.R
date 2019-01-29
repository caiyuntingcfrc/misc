library(tidyverse)
library(docxtractr)
library(LaF)

rm(list = ls())

###### create the codebook ######
# codebook
path_code <- "AA170042/code106.docx"
code_tbl <- read_docx(path_code) %>% docx_extract_tbl() %>% .[complete.cases(.), ]
# add card_num
code_tbl <- rbind(code_tbl, c(NA, "card_num", "#/79-80", "?d??", NA, NA))
# colnames
colnames(code_tbl) <- c("q_num", "variable", "card_pos", "label", "level", "note")
# variable b1_# - b19_#
code_tbl$variable[17:36] <- code_tbl$variable[17:36] %>% str_split("#", simplify = TRUE) %>%
        .[ , 1] %>% as.character()
# start
code_tbl$start <- code_tbl$card_pos %>% str_split("/", simplify = TRUE) %>% 
        .[ , 2] %>% str_split("-", simplify = TRUE) %>% .[ ,1] %>% as.integer()
# end
code_tbl$end <- code_tbl$card_pos %>% str_split("/", simplify = TRUE) %>% 
        .[ , 2] %>% str_split("-", simplify = TRUE) %>% .[ ,2] %>% as.integer()
# replace NA in `end`
code_tbl$end <- with(code_tbl, if_else(is.na(`end`), `start`, `end`))

###### data processing ######
# data raw and card_num
path_dat <- "AA170042/inc106.dat"
df.list <- read_fwf(path_dat, fwf_positions(c(1, 79), c(80, 80), 
                                                    col_names = c("raw", "card_num")),
                            col_types = cols(card_num = col_integer()))
#card 01
l1 <- filter(df.list, card_num == 1)[ ,1]
df1 <- lapply(l1, read_fwf, fwf_positions(code_tbl$start[c(1, 2:16, 95)], 
                                          code_tbl$end[c(1, 2:16, 95)],
                                          col_names = code_tbl$variable[c(1, 2:16, 95)]), 
              col_types = cols(card_num = col_integer()))
# bind_rows and order
df1 <- do.call(bind_rows, df1)
df1 <- df1 %>% .[order(.$`x1`), ]
View(df1[1:5, ])

# card 02-20        
l2 <- filter(df.list, card_num %in% 2:20)[ ,1]
df2 <- lapply(l2, read_fwf, fwf_positions(code_tbl$start[c(1, 17:36, 95)], 
                                   code_tbl$end[c(1, 17:36, 95)],
                                   col_names = code_tbl$variable[c(1, 17:36, 95)]), 
              col_types = cols(card_num = col_integer()))
# bind_rows and order
df2 <- do.call(bind_rows, df2)
df2 <- df2 %>% .[order(.$`card_num`), ]
View(df2[1:5, ])

# split by card_num
# n <- names(table(df2$card_num - 1))
l <- split(df2, df2$card_num, drop = FALSE)
View(l[[1]][1:10, ])
for(i in 1:14){
    colnames(l[[i]]) <- c("x1", paste(colnames(l[[i]])[-c(1, 22)], l[[i]]$card_num[1]-1, sep = ""), 
                          "card_num")
}

#x <- df2[df2$card_num == 2, c(1, 4, 22)]
#d <- spread(x, "card_num", "b3_")
#x <- list()
#b <- c(1:5, 8, 9, 10, 12:16, 19:25)

#for(i in 2:20){
#    for(j in 2:21){
#        y <- df2[df2$card_num == i, c(1, j, 22)]
#        x[[i-1]] <- spread(y, "card_num", b[j-1])
#    }
#}

#x <- list()
#b <- paste("b", c(1:5, 8, 9, 10, 12:16, 19:25), "_", sep = "")
#for(i in 2:20){
#    for(j in 2:21){
#        x[[i-1]] <- spread(df2[c(1, j, 22)], "card_num", b[j-1])
#    }
#}
# spread(df2[c(1, 2, 22)], "card_num", "b2_")

#tibbles into a list 
#l.df2 <- list()
#for(i in 2:20) {
#     l.df2[i-1] <- lapply(df2, filter, card_num == i)
#     # remove variable "card_num"
#     l.df2[[i-1]] <- l.df2[[i-1]] %>% select(-one_of("card_num"))
#}
# merge
#df2 <- Reduce(function(...) merge(..., by = "x1", all = TRUE), l)

# card 21
l21 <- filter(df.list, card_num == 21)[ ,1]
df21 <- lapply(l1, read_fwf, fwf_positions(code_tbl$start[c(1, 37:67)], 
                                          code_tbl$end[c(1, 37:67)],
                                          col_names = code_tbl$variable[c(1, 37:67)]), 
              col_types = cols())
# bind_rows and order
df21 <- do.call(bind_rows, df21)
df21 <- df21 %>% .[order(.$`x1`), ]
View(df21[1:5, ])

# card 22
l22 <- filter(df.list, card_num == 22)[ ,1]
df22 <- lapply(l1, read_fwf, fwf_positions(code_tbl$start[c(1, 68:88)], 
                                           code_tbl$end[c(1, 68:88)],
                                           col_names = code_tbl$variable[c(1, 68:88)]), 
               col_types = cols(c5c = col_integer()))
# bind_rows and order
df22 <- do.call(bind_rows, df22)
df22 <- df22 %>% .[order(.$`x1`), ]
View(df22[1:5, ])

# card 23-99
l23 <- filter(df.list, card_num %in% 23:99)[ ,1]

x <- list()
for(i in 0:4){
        x[i+1] <- lapply(l23, read_fwf, fwf_positions(c(1, 13 + i * 14, 9 + i * 14),
                                                    c(8, 22 + i * 14, 12 + i * 14),
                                                    col_names = c("x1", "exp", "item")
                                                    ),
                       col_types = cols())
        df23 <- do.call(bind_rows, x) %>% distinct()
        gc()
}
rm(x)

#spread (transpose)
df23 <- df23 %>% spread("item", "exp")
df23$`0000` <- NULL
c <- colnames(df23)[-1] ; c
colnames(df23) <- c("x1", paste("item_", c, sep = ""))
colnames(df23)
View(df23[1:5, ])

# order
df23 <- df23 %>% .[order(.$`x1`), ]

rm(list = c("df.list", "l1", "l2", "l21", "l22", "l23"))

# merge
# df23 <- Reduce(function(...) merge(..., by = "x1", all = TRUE), l.df23)

###### LaF approach ######
a <- laf_open_fwf("home/HDATA106", column_types = c("string", "string"), column_widths = c(1, 9))
View(a[1])                  
