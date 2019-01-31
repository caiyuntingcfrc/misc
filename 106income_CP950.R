library(tidyverse)
library(docxtractr)
library(LaF)
library(readtext)
library(data.table)

rm(list = ls())

###### create the codebook ######
# codebook
path_code <- "AA170042/code106.docx"
code_tbl <- read_docx(path_code) %>% docx_extract_tbl() %>% .[complete.cases(.), ]
# add row: card_num
code_tbl <- rbind(code_tbl, c(NA, "card_num", "#/79-80", NA, NA, NA))
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

###### item_xxx ######
doc.text.parts <- readtext(path_code)$text %>% strsplit("\n") %>% .[[1]]
doc.items <- grep("*:", doc.text.parts, value = TRUE)[-c(1:12, 808:810)]
doc.items.part1 <- strsplit(doc.items, ":") %>% unlist() %>% 
        .[2*(1:length(doc.text.parts)) -1 ] %>% .[!is.na(.)]
doc.items.part2 <- strsplit(doc.items, ":") %>% unlist() %>% 
        .[2*(1:length(doc.text.parts))] %>% .[!is.na(.)]

###### data processing and manipulation ######
# data raw and card_num
path_dat <- "AA170042/inc106.dat"
df.list <- read_fwf(path_dat, fwf_positions(c(1, 79), c(80, 80), 
                                                    col_names = c("raw", "card_num")),
                    # card_num as integer
                    col_types = cols(card_num = col_integer())) %>% 
        # order by card_num
        .[order(.$`card_num`), ]
#card 01
l1 <- filter(df.list, card_num == 1)[ ,1]
df1 <- lapply(l1, read_fwf, fwf_positions(code_tbl$start[c(1, 2:16, 95)], 
                                          code_tbl$end[c(1, 2:16, 95)],
                                          col_names = code_tbl$variable[c(1, 2:16, 95)]), 
              # card number as integer
              col_types = cols(card_num = col_integer())) 
# bind_rows and order
df1 <- do.call(bind_rows, df1)
df1 <- df1 %>% .[order(.$`x1`), ]
df1$card_num <- NULL

###### test 2 ######
t <- function(c, d = c - 1){
        if(c == df.list$card_num){
                x <- filter(df.list, card_num == c)[ ,1]
                tmp <- read_fwf(x$raw, fwf_positions(code_tbl$start[c(1, 17:36, 95)], 
                                                          code_tbl$end[c(1, 17:36, 95)]
                                                          )
                                 #col_types = cols(card_num = col_integer())
                )
        }
        else {
                tmp <- as.tibble(matrix(ncol = 22))
                tmp[ , 22] <- c
        }
        colnames(tmp) <- c("x1", paste(code_tbl$variable[17:36], d, sep = ""), "card_num")
        tmp$card_num <- as.integer(tmp$card_num)
        tmp$x1 <- "00000001"
        return(tmp)
}
g <- t(c = 13)
x <- filter(df.list, card_num == 2)[ , 1]
y <- subset(df.list, card_num == 2)
class(y)
lapply(y$raw, read_fwf, fwf_positions(79, 80))
file.exists(x)

######
l2 <- filter(df.list, card_num %in% 2:20)[ ,1]
df2 <- lapply(l2, read_fwf, fwf_positions(code_tbl$start[c(1, 17:36, 95)], 
                                   code_tbl$end[c(1, 17:36, 95)],
                                   col_names = code_tbl$variable[c(1, 17:36, 95)]), 
              col_types = cols(card_num = col_integer()))
# bind_rows and order
df2 <- do.call(bind_rows, df2)
df2 <- df2 %>% .[order(.$`card_num`), ]
# split by card_num
l <- split(df2, df2$card_num)
for(i in 1:14){
    colnames(l[[i]]) <- c("x1", paste(colnames(l[[i]])[-c(1, 22)], l[[i]]$card_num[1]-1, sep = ""), 
                          "card_num")
    # remove card_num (prevent duplicated variable while merging data in the list
    #l[[i]]$card_num <- NULL
}
# merge function and with reduce function
df2 <- Reduce(function(...) merge(..., by = "x1", all = TRUE), l)
gc()

# card 21
l21 <- filter(df.list, card_num == 21)[ ,1]
df21 <- lapply(l1, read_fwf, fwf_positions(code_tbl$start[c(1, 37:67)], 
                                          code_tbl$end[c(1, 37:67)],
                                          col_names = code_tbl$variable[c(1, 37:67)]), 
              col_types = cols())
# bind_rows and order
df21 <- do.call(bind_rows, df21)
df21 <- df21 %>% .[order(.$`x1`), ]

# card 22
l22 <- filter(df.list, card_num == 22)[ ,1]
df22 <- lapply(l1, read_fwf, fwf_positions(code_tbl$start[c(1, 68:88)], 
                                           code_tbl$end[c(1, 68:88)],
                                           col_names = code_tbl$variable[c(1, 68:88)]), 
               col_types = cols(c5c = col_integer()))
# bind_rows and order
df22 <- do.call(bind_rows, df22)
df22 <- df22 %>% .[order(.$`x1`), ]

# card 23-99
l23 <- filter(df.list, card_num %in% 23:99)[ ,1]

x <- list()
for(i in 0:4){
        x[i+1] <- lapply(l23, read_fwf, fwf_positions(c(1, 13 + i * 14, 9 + i * 14),
                                                    c(8, 22 + i * 14, 12 + i * 14),
                                                    col_names = c("x1", "exp", "item"))
                         )
        df23 <- do.call(bind_rows, x) %>% distinct()
}
gc()

# list for grep
sym <- c("{", grep("[A-R]", LETTERS, value = TRUE), "}")
digi <- c(0:9, 1:9, 0)
positive <- c(rep("+", 10), rep("-", 10))
grepbook <- data.frame(sym, digi, positive)

# replace symbols with digits (positive)
for(i in 2:10){
        df23$exp[grep("*\\{", df23$exp)] <- df23$exp[grep("*\\{", df23$exp)] %>% 
                gsub("*\\{", "0", .)
        strs <- paste("*", grepbook$sym[i], sep = "")
        df23$exp[grep(strs, df23$exp)] <- df23$exp[grep(strs, df23$exp)] %>%
                gsub(strs, grepbook$digi[i], .)
}
# replace symbols with digits (negative)
for(i in 11:19){
        df23$exp[grep("*\\}", df23$exp)] <- df23$exp[grep("*\\}", df23$exp)] %>% 
                gsub("*\\}", "0", .) %>% paste("-", ., sep = "")
        strs <- paste("*", grepbook$sym[i], sep = "")
        df23$exp[grep(strs, df23$exp)] <- df23$exp[grep(strs, df23$exp)] %>%
                gsub(strs, grepbook$digi[i], .) %>% paste("-", ., sep = "")
}

# spread (transpose)
df23 <- df23 %>% spread("item", "exp")
df23$`0000` <- NULL
c <- as.integer(colnames(df23)[-1])

# items with no observations
d <- doc.items.part1 %>% as.integer() # item names of all
v <- paste("itm" ,d[! d %in% c], sep = "") # 
dd <- data.frame(matrix(nrow = nrow(df23), ncol = length(v)))
colnames(dd) <- v
dd$x1 <- df23$x1


colnames(df23) <- c("x1", paste("itm", c, sep = ""))
# order
df23 <- df23 %>% .[order(.$`x1`), ]

# remove
#rm(list = c("df.list", "l", "l1", "l2", "l21", "l22", "l23", "x"))
# merge
data.list <- list(df1, df2, df21, df22, df23, dd)
dfinc106 <- Reduce(function(...) merge(..., by = "x1", all = TRUE), data.list)
dfinc106$year <- as.integer(106)

###### save ###### 

###### LaF approach ######
#a <- laf_open_fwf("AA170042/inc106.dat", column_types = c("string", "string"), column_widths = c(1, 9))
#x <- "abcdefg"
#laf_open_fwf(x, column_types = c("character", "character"), column_widths = c(1, 2))
#View(a[1]) 