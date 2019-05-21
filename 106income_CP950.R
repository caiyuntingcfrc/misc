###### CAI YUN-TING ######

###### install packages !!! one time only ######
# Mark out lines of codes below if these packages are installed.
# p <- c("tidyverse", "docxtractr", "readtext", "haven", "expss")
# install.packages(p)
# Mark out lines of codes below if these packages are installed.

##### prep and options #####
# set working directory
setwd("D:/R_wd/")

# clear objects
rm(list = ls())

# loading packages
# expss must be loaded after haven
l <- c("tidyverse", "docxtractr", "readtext", 
       "haven", "expss", "microbenchmark")
lapply(l, require, character.only = TRUE)
rm(l)

# options
options(readr.show_progress = TRUE)

# processing time
ptm <- proc.time()

###### create the codebook ######
# codebook
# file is in the default working dirctory
path_code <- "AA170042/code106.docx" 
code_tbl <- read_docx(path_code) %>% docx_extract_tbl() %>% .[complete.cases(.), ]
# add row: card_num
code_tbl <- rbind(code_tbl, c(NA, "card_num", "#/79-80", NA, NA, NA))
# colnames
colnames(code_tbl) <- c("q_num", "variable", "card_pos", "label", "level", "note")
# variable b1_# - b19_#
code_tbl$`variable`[17:36] <- code_tbl$`variable`[17:36] %>% 
        str_split("#", simplify = TRUE) %>%
        .[ , 1] %>% as.character()
# start
code_tbl$`start` <- code_tbl$`card_pos` %>% 
        str_split("/", simplify = TRUE) %>% 
        .[ , 2] %>% str_split("-", simplify = TRUE) %>% 
        .[ ,1] %>% as.integer()
# end
code_tbl$`end` <- code_tbl$`card_pos` %>% 
        str_split("/", simplify = TRUE) %>% 
        .[ , 2] %>% str_split("-", simplify = TRUE) %>% 
        .[ ,2] %>% as.integer()
# replace NA in `end`
code_tbl$`end` <- with(code_tbl, if_else(is.na(`end`), `start`, `end`))

###### names of item_xxx ######
doc.text.parts <- readtext(path_code)$`text` %>% 
        strsplit("\n") %>% .[[1]]
doc.items <- grep("*:", doc.text.parts, value = TRUE) %>% 
        .[-c(1:12, 808:810)]
# item numbers
doc.items.part1 <- strsplit(doc.items, ":") %>% 
        unlist() %>% 
        .[2 * (1:length(doc.text.parts)) -1 ] %>% 
        .[!is.na(.)]
# item contents
doc.items.part2 <- strsplit(doc.items, ":") %>% 
        unlist() %>% 
        .[2 * (1:length(doc.text.parts))] %>% 
        .[!is.na(.)]

###### data processing and manipulation ######
# data raw and card_num
path_dat <- "AA170042/inc106.dat"
df.source <- read_fwf(path_dat, fwf_positions(start = c(1, 79), 
                                              end = c(80, 80), 
                                              col_names = c("raw", "card_num")
                                              ), 
                      # card_num as integer
                      col_types = cols(card_num = "i")
                      ) %>% 
        # order by card_num
        .[order(.$`card_num`), ]

##### card 01 #####
# filter out card_num == 1
x <- filter(df.source, card_num == 1) %>% .[ ,1] %>% .$`raw`
# define tempfile name and format
y <- tempfile("tp", fileext = ".dat")
# write the tempfile
write(x, file = y)
# read card 1
df1 <- read_fwf(y, fwf_positions(code_tbl$`start`[c(1:16, 95)], 
                                 code_tbl$`end`[c(1:16, 95)],
                                 col_names = code_tbl$`variable`[c(1:16, 95)]), 
                # define column types (variable classes) in df1
                col_types = cols(card_num = "i", 
                                 a4 = "f", a5 = "f", a6 = "n", 
                                 a7 = "c", a8 = "n", a9 = "n", 
                                 a11 = "f", a12 = "n", a13 = "n", 
                                 a16 = "f", a17 = "f", a18 = "f", 
                                 a19 = "n", a20 = "n")
                ) 
# bind_rows and order
df1 <- df1 %>% .[order(.$`x1`), ]
# remove card_num
df1$card_num <- NULL

##### card 02 #####
# card_num 02:20
# function f2
f2 <- function(c, d = c - 1) {
        # if 
        if(c %in% df.source$`card_num`) {
                # filter out and create a temporary .dat file
                x <- filter(df.source, card_num == c) %>% .[ ,1] %>% .$`raw`
                y <- tempfile("tmp", fileext = ".dat")
                write(x, file = y)
                # read file
                tmp <- read_fwf(y, fwf_positions(code_tbl$`start`[c(1, 17:36, 95)], 
                                                 code_tbl$`end`[c(1, 17:36, 95)])
                                )
                # if input c (card_num) is not in the raw data, 
                # then create a temp file wiith "000000001" (for merging data), 
                # which will fill NA.
                # matrix with 22 columns c(1, 17:36, 95)
                } else {tmp <- matrix(ncol = 22) %>% as.tibble()
                        tmp[ , 22] <- c
                        tmp[ , 1] <- "00000001"            
                }
        # name the columns (b1_1, b1_2, b1_3 ......)
        # eg. b1_# , card_num == 2, then # == 1, get b1_1 (#: 1:19)
        colnames(tmp) <- c("x1", paste(code_tbl$`variable`[17:36], d, sep = ""), "card_num")
        # tmp$card_num <- as.integer(tmp$`card_num`)
        return(tmp)
        }
# for loop and left_join (dplyr) 
# card number = 2:20
df2 <- list() 
for(i in 2:20) {
        df2[[i - 1]] <- f2(i)
        df2[[i - 1]]$`card_num` <- NULL
        }
# left_joing with reduce
df2 <- Reduce(function(...) left_join(..., by = "x1"), df2)

# benchmark
# microbenchmark(a <- Reduce(function(...) left_join(..., by = "x1"), df2), unit = "s")
# microbenchmark(b <- Reduce(function(...) merge(..., by = "x1", all = TRUE), df2), unit = "s")
# free up ram
gc()

##### card 21 #####
x <- filter(df.source, card_num == 21) %>% .[ ,1] %>% .$raw
y <- tempfile("tmp", fileext = ".dat")
write(x, file = y)
# code_tbl[37:67, ]
df21 <- read_fwf(y, fwf_positions(code_tbl$`start`[c(1, 37:67)], 
                                  code_tbl$`end`[c(1, 37:67)],
                                  # variable names
                                  col_names = code_tbl$`variable`[c(1, 37:67)]), 
                 # column types
                 cols(f57 = "f", f61 = "f")
                 ) %>% 
        # order
        .[order(.$`x1`), ]

# card 22
x <- filter(df.source, card_num == 22)[ ,1] %>% .$raw
y <- tempfile("tmp", fileext = ".dat")
write(x, file = y)
df22 <- read_fwf(y, fwf_positions(code_tbl$start[c(1, 68:88)], 
                                  code_tbl$end[c(1, 68:88)],
                                  col_names = code_tbl$variable[c(1, 68:88)]), 
                 col_types = cols(c5c = "i"))
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

###### factor label and values ######
s <- which(!(code_tbl$level == ""))
lev <- list() #[1-7], a_xx; [8-22], b_xx; [23-24], f; [25-27], c; [28-30], d
lab <- list() #[1-7], a_xx; [8-22], b_xx; [23-24], f; [25-27], c; [28-30], d
for(i in 1:length(s)){
        lev[[i]] <- code_tbl$level[s[i]] %>% str_extract_all("[0-9]+") %>% .[[1]]
        lab[[i]] <- code_tbl$level[s[i]] %>% str_split("[0-9]+\\. ") %>% .[[1]] %>% .[-1]
        }


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
df.inc106$f57 <- sapply(df.inc106$f57, factor, levels = lev[[23]])
df.inc106$f61 <- sapply(df.inc106$f61, factor, levels = lev[[24]])

# c 
c <- grep("^c", variables)
# (numeric)
df.inc106[ , c] <- sapply(df.inc106[ , c], as.numeric)
# (factor)
df.inc106$c1 <- sapply(df.inc106$c1, factor, levels = lev[[25]])
df.inc106$c2 <- sapply(df.inc106$c2, factor, levels = lev[[26]])
df.inc106$c4 <- sapply(df.inc106$c4, factor, levels = lev[[27]])

# d
d <- grep("^d", variables)
# (numeric)
df.inc106[ , d] <- sapply(df.inc106[ , d], as.numeric)
# (factor)
df.inc106$d1 <- sapply(df.inc106$d1, factor, levels = lev[[28]])
df.inc106$d5 <- sapply(df.inc106$d5, factor, levels = lev[[29]])
df.inc106$d6 <- sapply(df.inc106$d6, factor, levels = lev[[30]])

# itm
itm <- grep("^itm", variables)
df.inc106[ , itm] <- sapply(unlist(df.inc106[ , itm]), as.numeric)

# view data
# fix(df.inc106)

###### time ######
proc.time() - ptm

###### save ###### 
# .RData
#save(df.inc106, file = "AA170042/inc106.RData")
# save(code_tbl, file = "AA170042/code_tbl.RData")
# .csv format
# write_csv(df.inc106, "inc106.csv", col_names = TRUE, na = "")
# .sas7bdat format
# write_sas(df.inc106, "inc106.sas7bdat")
# .sav format
# write_sav(df.inc106, "inc106.sav", compress = TRUE)

###### time ######
proc.time() - ptm

###### remove all objects ######
# rm(list = ls())