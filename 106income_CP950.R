###### CAI YUN-TING ######

###### install packages !!! one time only ######
# Mark out lines of codes below if these packages are installed.
# p <- c("tidyverse", "docxtractr", "readtext", "haven")
# install.packages(p)
# Mark out lines of codes below if these packages are installed.


###### clear objects first ######
rm(list = ls())

###### loading packages ######
library(tidyverse)
library(docxtractr)
library(readtext)
library(haven)


ptm <- proc.time()
###### create the codebook ######
# codebook
path_code <- "AA170042/code106.docx" # file is in the default working dirctory
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

###### names of item_xxx ######
doc.text.parts <- readtext(path_code)$text %>% strsplit("\n") %>% .[[1]]
doc.items <- grep("*:", doc.text.parts, value = TRUE)[-c(1:12, 808:810)]
# item numbers
doc.items.part1 <- strsplit(doc.items, ":") %>% unlist() %>% 
        .[2*(1:length(doc.text.parts)) -1 ] %>% .[!is.na(.)]
# item contents
doc.items.part2 <- strsplit(doc.items, ":") %>% unlist() %>% 
        .[2*(1:length(doc.text.parts))] %>% .[!is.na(.)]

###### data processing and manipulation ######
# data raw and card_num
path_dat <- "AA170042/inc106.dat"
df.source <- read_fwf(path_dat, fwf_positions(c(1, 79), c(80, 80), 
                                                    col_names = c("raw", "card_num")),
                    # card_num as integer
                    col_types = cols(card_num = col_integer())) %>% 
        # order by card_num
        .[order(.$`card_num`), ]

#card 01
x <- filter(df.source, card_num == 1)[ ,1] %>% .$raw
y <- tempfile("tp", fileext = ".dat")
write(x, file = y)
df1 <- read_fwf(y, fwf_positions(code_tbl$start[c(1, 2:16, 95)], 
                                 code_tbl$end[c(1, 2:16, 95)],
                                 col_names = code_tbl$variable[c(1, 2:16, 95)]), 
                # card number as integer
                col_types = cols(card_num = col_integer())) 
# bind_rows and order
df1 <- df1 %>% .[order(.$`x1`), ]
df1$card_num <- NULL

# card 02
# function t 
f2 <- function(c, d = c - 1){
        # if else
        if(c %in% df.source$card_num){
                # filter out and create a temporary .dat file
                x <- filter(df.source, card_num == c)[ ,1] %>% .$raw
                y <- tempfile("tmp", fileext = ".dat")
                write(x, file = y)
                tmp <- read_fwf(y, fwf_positions(code_tbl$start[c(1, 17:36, 95)], 
                                                 code_tbl$end[c(1, 17:36, 95)]
                                                 )
                                #col_types = cols(card_num = col_integer())
                )
                }
        else {
                tmp <- as.tibble(matrix(ncol = 22))
                tmp[ , 22] <- c
                tmp[ , 1] <- "00000001"
                }
        colnames(tmp) <- c("x1", paste(code_tbl$variable[17:36], d, sep = ""), "card_num")
        tmp$card_num <- as.integer(tmp$card_num)
        return(tmp)
        }
# for loop and merge 
# card number = 2:20
df2 <- list()
for(i in 2:20){
        df2[[i-1]] <- f2(i)
        df2[[i-1]]$`card_num` <- NULL
}
df2 <- Reduce(function(...) merge(..., by = "x1", all = TRUE), df2)
gc() # free up ram

# card 21
x <- filter(df.source, card_num == 21)[ ,1] %>% .$raw
y <- tempfile("tmp", fileext = ".dat")
write(x, file = y)
df21 <- read_fwf(y, fwf_positions(code_tbl$start[c(1, 37:67)], 
                                  code_tbl$end[c(1, 37:67)],
                                  col_names = code_tbl$variable[c(1, 37:67)])
                 )
# order
df21 <- df21 %>% .[order(.$`x1`), ]

# card 22
x <- filter(df.source, card_num == 22)[ ,1] %>% .$raw
y <- tempfile("tmp", fileext = ".dat")
write(x, file = y)
df22 <- read_fwf(y, fwf_positions(code_tbl$start[c(1, 68:88)], 
                                  code_tbl$end[c(1, 68:88)],
                                  col_names = code_tbl$variable[c(1, 68:88)]), 
                 col_types = cols(c5c = col_integer()))
# order
df22 <- df22 %>% .[order(.$`x1`), ]

# card 23-99
x <- filter(df.source, card_num %in% 23:99)[ ,1] %>% .$raw
y <- tempfile("tmp", fileext = ".dat")
write(x, file = y)
x <- list()
for(i in 0:4){
        x[[i+1]] <- read_fwf(y, fwf_positions(c(1, 13 + i * 14, 9 + i * 14),
                                            c(8, 22 + i * 14, 12 + i * 14),
                                            col_names = c("x1", "exp", "item")
                                            )
                           )
        df23 <- do.call(bind_rows, x) %>% distinct()
        }
gc() # free up ram

# list for grep (grepbook)
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
gc() # free up ram

# spread (transpose)
df23 <- df23 %>% spread("item", "exp")
df23$`0000` <- NULL

# items with no observations
# names of all the items
colnames(df23)[-1] <- colnames(df23)[-1] %>% as.integer() %>% paste("itm", ., sep = "")
itms_all <- doc.items.part1 %>% as.integer() %>% paste("itm" , ., sep = "")
df.itm.all <- as.tibble(matrix("NA", nrow = nrow(df23), ncol = length(itms_all)))
df.itm.all$x1 <- df23$x1
colnames(df.itm.all) <- c(itms_all, "x1")
df23 <- df23 %>% left_join(df.itm.all) 
# order
df23 <- df23 %>% .[order(.$`x1`), ]

# merge
data.list <- list(df1, df2, df21, df22, df23)
df.inc106 <- Reduce(function(...) merge(..., by = "x1", all = TRUE), data.list)
df.inc106$year <- as.integer(106)
# remove
rm(df.source, x, df.itm.all, df1, df2, df21, df22, df23, data.list)
gc()

# savefile
save(df.inc106, file = "AA170042/inc106.RData")

# load
load(file = "AA170042/inc106.RData")

###### define class of each column ######
variables <- colnames(df.inc106)

# b1_ (character)
b1 <- grep("^b1_", variables) 
df.inc106[ , b1] <- sapply(df.inc106[ , b1], as.character)
# b4_ (numeric)
b4 <- grep("^b4_", variables)
df.inc106[ , b4] <- sapply(df.inc106[ , b4], as.numeric)
# b21_ (numeric)
b21 <- grep("^b21_", variables)
df.inc106[ , b21] <- sapply(df.inc106[ , b21], as.numeric)
# b23_ (numeric)
b23 <- grep("^b23_", variables)
df.inc106[ , b23] <- sapply(df.inc106[ , b23], as.numeric)
# b25_ (numeric)
b25 <- grep("^b25_", variables)
df.inc106[ , b25] <- sapply(df.inc106[ , b25], as.numeric)

# b2_, b3_ ... (factor)
l <- paste("b", c(2:3, 5:20, 22), "_", sep = "") %>% 
        paste("|", sep = "", collapse = "") %>% paste("b24_", sep = "")
bb <- grep(l, variables)
df.inc106[ , bb] <- sapply(df.inc106[ , bb], as.factor)

# fxx
f <- grep("^f", variables)
# (numeric)
df.inc106[ , f] <- sapply(df.inc106[ , f], as.numeric)
# f57, f61 (factor)
df.inc106$f57 <- sapply(df.inc106$f57, as.factor)
df.inc106$f61 <- sapply(df.inc106$f61, as.factor)

# c 
c <- grep("^c", variables)
# (numeric)
df.inc106[ , c] <- sapply(df.inc106[ , c], as.numeric)
# (factor)
df.inc106$c1 <- sapply(df.inc106$c1, as.factor)
df.inc106$c2 <- sapply(df.inc106$c2, as.factor)
df.inc106$c4 <- sapply(df.inc106$c4, as.factor)

# d
d <- grep("^d", variables)
# (numeric)
df.inc106[ , d] <- sapply(df.inc106[ , d], as.numeric)
# (factor)
df.inc106$d1 <- sapply(df.inc106$d1, as.factor)
df.inc106$d5 <- sapply(df.inc106$d5, as.factor)
df.inc106$d6 <- sapply(df.inc106$d6, as.factor)

# itm
itm <- grep("^itm", variables)
df.inc106[ , itm] <- sapply(unlist(df.inc106[ , itm]), as.numeric)

# view data
fix(df.inc106)

###### time ######
proc.time() - ptm

###### save ###### 
# .RData
save(df.inc106, file = "AA170042/inc106.RData")
# .csv format
# write_csv(df.inc106, "inc106.csv", col_names = TRUE, na = "")
# .sas7bdat format
# write_sas(df.inc106, "inc106.sas7bdat")
# .sav format
write_sav(df.inc106, "inc106.sav", compress = FALSE)

###### time ######
proc.time() - ptm

###### remove all objects ######
# rm(list = ls())

###### load ######
