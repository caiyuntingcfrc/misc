library(tidyverse)
library(docxtractr)

rm(list = ls())

# codebook
path_code <- "c:/Users/user/Downloads/AA170042/code106.docx"
code_tbl <- read_docx(path_code) %>% docx_extract_tbl() %>% .[complete.cases(.), ]

# add card_num
code_tbl <- rbind(code_tbl, c(NA, "card_num", "#/79-80", "卡序", NA, NA))

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

# data
path_dat <- "c:/Users/user/Downloads/AA170042/inc106.dat"
df.list <- read_fwf(path_dat, fwf_positions(c(1, 79), c(80, 80), 
                                                    col_names = c("raw", "card_num")),
                            col_types = cols(card_num = col_integer()))

df1 <- read_fwf(df.list$raw[df.list$card_num == 1], 
                fwf_positions(code_tbl$start[c(1:16, 95)], 
                              code_tbl$end[c(1:16, 95)], 
                              col_names = code_tbl$variable[c(1:16, 95)]),
                col_types = cols(card_num = col_integer()))

x <- df.list$raw[df.list$card_num == 15]
df2 <- read_fwf(x, 
                fwf_positions(code_tbl$start[c(17:36, 95)], 
                              code_tbl$end[c(17:36, 95)], 
                              col_names = code_tbl$variable[c(17:36, 95)]),
                col_types = cols(card_num = col_integer()))

l <- list()
while(i %in% 2:20) {
                l[[i]] <- read_fwf(df.list$raw[df.list$card_num == i], 
                                     fwf_positions(code_tbl$start[c(17:36, 95)], 
                                                   code_tbl$end[c(17:36, 95)], 
                                                   col_names = c(paste(code_tbl$variable[17:36], i), 
                                                                 code_tbl$variable[95])))
                
}
x <- do.call(rbind, l)

#df <- read_fwf(path_dat, fwf_positions(code_tbl$start[c(1:16, 95)], 
#                                       code_tbl$end[c(1:16, 95)], 
#                                       col_names = code_tbl$variable[c(1:16, 95)])) %>%
#        .[.$`card_num`== "01", ]

# leading zero
i <- sprintf("%02d", 1:10)

# readlines
x <- read_lines(path_dat)
class(x)
xx <- read_fwf(x, fwf_positions(79, 80), progress = TRUE)
