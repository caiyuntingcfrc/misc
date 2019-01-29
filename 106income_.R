library(tidyverse)
library(docxtractr)
#library(data.table)

rm(list = ls())

# codebook
<<<<<<< HEAD
path_code <- "c:/Users/leots/Documents/R data/AA170042/code106.docx"
=======
path_code <- "c:/Users/user/Downloads/AA170042/code106.docx"
>>>>>>> parent of c758f89... Add files via upload
code_tbl <- read_docx(path_code) %>% docx_extract_tbl() %>% .[complete.cases(.), ]

# add card_num
code_tbl <- rbind(code_tbl, c(NA, "card_num", "#/79-80", "¥d§Ç", NA, NA))

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
<<<<<<< HEAD

#read_fwf(path_dat, fwf_positions(56, 56))

# replace NA in `end`
code_tbl$end <- with(code_tbl, if_else(is.na(`end`), `start`, `end`))

# width 
code_tbl$width <- code_tbl$end - code_tbl$start + 1

# data
path_dat <- "c:/Users/leots/Documents/R data/AA170042/inc106.dat"
df.list <- read_fwf(path_dat, fwf_positions(c(1, 79), c(80, 80), 
                                                    col_names = c("raw", "card_num")),
                            col_types = cols(card_num = col_integer()))

=======
# replace NA in `end`
code_tbl$end <- with(code_tbl, if_else(is.na(`end`), `start`, `end`))

# data
path_dat <- "home/HDATA106"
df.list <- read_fwf(path_dat, fwf_positions(c(1, 79), c(80, 80), 
                                                    col_names = c("raw", "card_num")),
                            col_types = cols(card_num = col_integer()))
>>>>>>> parent of c758f89... Add files via upload
#card 01
l1 <- filter(df.list, card_num == 1)[ ,1]
df1 <- lapply(l1, read_fwf, fwf_positions(code_tbl$start[c(1, 1:16, 95)], 
                                          code_tbl$end[c(1, 1:16, 95)],
                                          col_names = code_tbl$variable[c(1, 1:16, 95)]), 
              col_types = cols(card_num = col_integer()))

# card 02-20        
<<<<<<< HEAD
l2 <- vector("list", 50459)
l2 <- filter(df.list, card_num %in% 2:20)[ , 1]
df2 <- vector("list", 50459)
df2 <- lapply(l2, read_fwf, fwf_positions(code_tbl$start[c(1, 17:36, 95)], 
                                   code_tbl$end[c(1, 17:36, 95)],
                                   col_names = code_tbl$variable[c(1, 17:36, 95)]),
              col_types = cols(card_num = col_integer())) 
df2_ <- do.call(bind_rows, df2) %>% unique()
test <- spread(df2_, "card_num", "b1_")

=======
l2 <- filter(df.list, card_num %in% 2:20)[ ,1]
df2 <- lapply(l2, read_fwf, fwf_positions(code_tbl$start[c(1, 17:36, 95)], 
                                   code_tbl$end[c(1, 17:36, 95)],
                                   col_names = code_tbl$variable[c(1, 17:36, 95)]), 
              col_types = cols(card_num = col_integer()))
>>>>>>> parent of c758f89... Add files via upload
#tibbles into a list 
l.df2 <- list()
for(i in 2:20) {
     l.df2[i-1] <- lapply(df2, filter, card_num == i)
     # remove variable "card_num"
     l.df2[[i-1]] <- l.df2[[i-1]] %>% select(-one_of("card_num"))
}
# merge
<<<<<<< HEAD
df2 <- Reduce(function(...) spread(..., "card_num"), l)
=======
#df2 <- Reduce(function(...) merge(..., by = "x1", all = TRUE), l)
>>>>>>> parent of c758f89... Add files via upload

# card 21
l21 <- filter(df.list, card_num == 21)[ ,1]
df21 <- lapply(l1, read_fwf, fwf_positions(code_tbl$start[c(1, 37:67)], 
                                          code_tbl$end[c(1, 37:67)],
                                          col_names = code_tbl$variable[c(1, 37:67)]), 
              col_types = cols())

# card 22
l22 <- filter(df.list, card_num == 22)[ ,1]
df22 <- lapply(l1, read_fwf, fwf_positions(code_tbl$start[c(1, 68:88)], 
                                           code_tbl$end[c(1, 68:88)],
                                           col_names = code_tbl$variable[c(1, 68:88)]), 
               col_types = cols(c5c = col_integer()))

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
dd <- df23 %>% spread("item", "exp")
dd$`0000` <- NULL
c <- colnames(dd)[-1]
colnames(dd) <- paste("item_", c, sep = "")
colnames(dd)
# merge
# df23 <- Reduce(function(...) merge(..., by = "x1", all = TRUE), l.df23)

#LaF mehod
a <- laf_open_fwf("home/HDATA106", column_types = c("string", "string"), column_widths = c(1, 9))
View(a[1])                  
