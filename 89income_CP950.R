##### author: CAI YUN-TING ######

##### The Survey of Family Income and Expenditure, 2005 #####
##### prep and options #####
# set working directory
setwd("~/R_wd/")

# loading packages
# expss must be loaded after haven
list.packages <- c("tidyverse", "docxtractr", "readtext", 
                   "haven", "expss", "microbenchmark", "hablar")
# check if the packages are installed
new.packages <- list.packages[!(list.packages %in% installed.packages()[ , "Package"])]
# install new packages
if(length(new.packages)) install.packages(new.packages)
# load the packages
lapply(list.packages, require, character.only = TRUE)
# remove lists
rm(list.packages, new.packages)

# options
options(readr.show_progress = TRUE)
# do not show scientific notation
options(scipen = 999)
# timestamp
timestamp <- format(Sys.time(), "%m%d-%H%M")
# processing time
ptm <- proc.time()

##### create the codebook ######
# codebook
# file is in the default working dirctory
path_code <- "AA170025/code89.docx" 
code_tbl_89 <- read_docx(path_code) %>% docx_extract_tbl() %>% .[complete.cases(.), ]
# add row: card_num
code_tbl_89 <- rbind(code_tbl_89, c(NA, "card_num", "#/79-80", NA, NA, NA))
# colnames
colnames(code_tbl_89) <- c("q_num", "variable", "card_pos", "label", "level", "note")
# variable b1_# - b25_#
code_tbl_89$`variable`[26:54] <- code_tbl_89$`variable`[26:54] %>% 
        str_split("#", simplify = TRUE) %>%
        .[ , 1] %>% as.character()
# start
code_tbl_89$`start` <- code_tbl_89$`card_pos` %>% 
        str_split("/", simplify = TRUE) %>% 
        .[ , 2] %>% str_split("-", simplify = TRUE) %>% 
        .[ ,1] %>% as.integer()
# end
code_tbl_89$`end` <- code_tbl_89$`card_pos` %>% 
        str_split("/", simplify = TRUE) %>% 
        .[ , 2] %>% str_split("-", simplify = TRUE) %>% 
        .[ ,2] %>% as.integer()
# replace NA in `end`
code_tbl_89$`end` <- with(code_tbl_89, if_else(is.na(`end`), `start`, `end`))

##### names of item_xxx ######
doc.text.parts <- readtext(path_code)$`text` %>% 
        strsplit("\n") %>% .[[1]]
# items: [71:819]
doc.items <- grep("?G", doc.text.parts, value = TRUE) %>% 
        .[71:819]
# item numbers
doc.items.part1 <- strsplit(doc.items, "?G") %>% 
        unlist() %>% 
        .[2 * (1:length(doc.text.parts)) -1 ] %>% 
        .[!is.na(.)]
# item contents
doc.items.part2 <- strsplit(doc.items, "?G") %>% 
        unlist() %>% 
        .[2 * (1:length(doc.text.parts))] %>% 
        .[!is.na(.)]

##### data processing and manipulation ######
# data raw and card_num
path_dat <- "AA170025/inc89.dat"
df.source <- read_fwf(path_dat, fwf_positions(start = c(1, 79), 
                                              end = c(80, 80), 
                                              col_names = c("raw", "card_num")
                                              ), 
                      # card_num as integer
                      col_types = cols(card_num = "i", .default = "c")
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
# code_tbl_89[2:25]
df1 <- read_fwf(y, fwf_positions(code_tbl_89$`start`[2:25], 
                                 code_tbl_89$`end`[2:25],
                                 col_names = code_tbl_89$`variable`[2:25]), 
                # define column types (variable classes) in df1
                col_types = cols(x1 = "c", area = "f", stage = "f", 
                                 id = "c", a1 = "f", a2 = "f", 
                                 a3 = "f", a4 = "f", 
                                 a5 = "f", a6 = "n", a7 = "f", 
                                 a8 = "n", a9 = "n", a11 = "f", 
                                 a12 = "n", a13 = "n", a14 = "f", 
                                 a15 = "f", a16 = "f", a17 = "f", 
                                 a18 = "f", a19 = "n", a20 = "n", a21 = "c")
                ) %>% 
        # order
        .[order(.$`x1`), ]

# free up ram
gc()

##### card 02 #####
# card_num 02:20
# function f2
f2 <- function(c, d = c - 1) {
        # if input c (card_num) is not in the raw data, 
        # then create a temp file wiith "000000001" (for merging data), 
        # which will fill NA.
        # matrix with 29 columns c(2, 26:54)
        if(c %in% df.source$`card_num`) {
                # filter out card_num == 02:20 and create a temporary .dat file
                x <- filter(df.source, card_num == c) %>% .[ ,1] %>% .$`raw`
                y <- tempfile("tmp", fileext = ".dat")
                write(x, file = y)
                # read file [2, 26:54]
                tmp <- read_fwf(y, fwf_positions(code_tbl_89$`start`[c(2, 26:54)], 
                                                 code_tbl_89$`end`[c(2, 26:54)])
                                )
                } else {tmp <- matrix(ncol = 30) %>% as_tibble(.name_repair = NULL)
                        tmp[ , 1] <- "00000001"        
                        }
        # name the columns (b1_1, b1_2, b1_3 ......)
        # eg. b1_# , card_num == 2, then # == 1, get b1_1 (#: 1:19)
        colnames(tmp) <- c("x1", paste(code_tbl_89$`variable`[26:54], d, sep = ""))
        # tmp$card_num <- as.integer(tmp$`card_num`)
        return(tmp)
        }
# for loop and left_join (dplyr) 
# card number = 2:20
df2 <- list() 
for(i in 2:20) {
        df2[[i - 1]] <- f2(i)
        }
# left_joing with reduce
df2 <- Reduce(function(...) left_join(..., by = "x1"), df2)
# column types
# b1_, b4_, b21_, b23_, b25_
df2 <- df2 %>% convert(chr(x1),
                       num(contains("b4_")), 
                       num(contains("b17_")), 
                       num(contains("b18_")), 
                       num(contains("b21_")), 
                       num(contains("b23_")), 
                       num(contains("b25_")), 
                       num(contains("b27_")), 
                       num(contains("b28_")),  
                       chr(contains("b29_")) 
                       )
# b2_, b3_ ... (factor)
variables <- colnames(df2)
l <- paste("b", c(1:3, 5:16, 19:20, 22, 24), "_", sep = "") %>% 
        paste("|", sep = "", collapse = "") %>% paste("b26_", sep = "")
bb <- grep(l, variables)
# mutate_if
df2[ , bb] %<>% mutate_if(is.character, as.factor) %>% 
        mutate_if(is.numeric, as.factor)

# benchmark
# microbenchmark(a <- Reduce(function(...) left_join(..., by = "x1"), df2), unit = "s")
# microbenchmark(b <- Reduce(function(...) merge(..., by = "x1", all = TRUE), df2), unit = "s")

# free up ram
gc()

##### card 21 #####
# filter out card_num == 21
x <- filter(df.source, card_num == 21) %>% .[ ,1] %>% .$raw
y <- tempfile("tmp", fileext = ".dat")
write(x, file = y)
# code_tbl_89[55:85]
df21 <- read_fwf(y, fwf_positions(code_tbl_89$`start`[c(2, 55:85)], 
                                  code_tbl_89$`end`[c(2, 55:85)],
                                  # variable names
                                  col_names = code_tbl_89$`variable`[c(2, 55:85)]), 
                 # define column types
                 cols(x1 = "c", f57 = "f", f58 = "f", .default = "n")
                 ) %>% 
        # order by x1
        .[order(.$`x1`), ]

# free up ram
gc()

##### card 22 #####
# filter out card_num == 22
x <- filter(df.source, card_num == 22) %>% .[ ,1] %>% .$raw
y <- tempfile("tmp", fileext = ".dat")
write(x, file = y)
# code_tbl_89[86:112]
df22 <- read_fwf(y, fwf_positions(code_tbl_89$`start`[c(2, 86:112)], 
                                  code_tbl_89$`end`[c(2, 86:112)],
                                  # variable names
                                  col_names = code_tbl_89$`variable`[c(2, 86:112)]), 
                 # define column types
                 col_types = cols(x1 = "c", c15 = "n", c16 = "n",
                                  c17 = "n", c18 = "n", c19 = "n",
                                  c22 = "n", .default = "f")) %>% 
        # order by x1
        .[order(.$`x1`), ]

# free up ram
gc()

##### card 23-99 ####
# filter out card_num %in% 23:99
x <- filter(df.source, card_num %in% 23:99) %>% .[ ,1] %>% .$raw
y <- tempfile("tmp", fileext = ".dat")
write(x, file = y)
x <- list()
# for loop (5 sections)
# item 101 = "1010" (9, 12) , take posistion (9, 11)
for(i in 0:4) {
        x[[i + 1]] <- read_fwf(y, fwf_positions(c(1, 9 + i * 14, 13 + i * 14),
                                            c(8, 11 + i * 14, 22 + i * 14),
                                            col_names = c("x1", "item", "exp")), 
                               col_types = cols(x1 = "c", item = "c", exp = "c")
                           )
        df23 <- do.call(bind_rows, x) %>% distinct()
        }
# free up ram
gc()

##### list for grep (grepbook) #####
sym <- c("{", grep("[A-R]", LETTERS, value = TRUE), "}")
digi <- c(0:9, 1:9, 0)
positive <- c(rep("+", 10), rep("-", 10))
grepbook <- tibble(sym, digi, positive)
# pattern for grep and gsub
pattern <- grepbook$`sym` %>% .[2:19]
grepbook$pattern <- c("\\{$", sapply(pattern, paste, "$", sep = ""), "\\}$")

##### replace symbols with digits (positive) ####
p <- grepbook$pattern
r <- grepbook$digi
for(i in 1:10) {
        # postitive [1:10]
        df23$exp[grep(p[i], df23$exp)] <- gsub(pattern = p[i], 
                                               replacement = r[i], 
                                               x = grep(p[i], df23$exp, value = TRUE))
        # negative [11:20]
        df23$exp[grep(p[i + 10], df23$exp)] <- gsub(pattern = p[i + 10], 
                                               replacement = r[i + 10], 
                                               x = grep(p[i + 10], df23$exp, value = TRUE)) %>% 
                paste("-", ., sep = "")
        }

# spread (transpose)
df23 <- df23 %>% spread(key = "item", value = "exp")
# remove column `0000`
df23 <- df23 %>% select( - one_of("000"))

##### items with no observations #####
# df23 (626 variables) but all items are 800
# names of all the items
colnames(df23)[-1] <- colnames(df23)[-1] %>% as.integer() %>% paste("itm", ., sep = "")
itms_all <- doc.items.part1 %>% as.integer() %>% paste("itm" , ., sep = "")
# create a tibble for those who are not in df23
df.itm.all <- matrix("NA", nrow = nrow(df23), ncol = length(itms_all)) %>% 
        as_tibble(.name_repair = NULL)
# create x1 column for merging
df.itm.all$`x1` <- df23$`x1`
# name the columns with all item names
colnames(df.itm.all) <- c(itms_all, "x1")
# merge
df23 <- df23 %>% left_join(df.itm.all) %>% 
        # column types (hablar::convert)
        convert(chr(x1), num(contains("itm"))) %>% 
        # order
        .[order(.$`x1`), ]
# free up ram
gc()

##### merge #####
data.list <- list(df1, df2, df21, df22, df23)
df.inc89 <- Reduce(function(...) left_join(..., by = "x1"), data.list)
# add year column
df.inc89$year <- as.integer(89)
# remove
rm(df.source, x, df.itm.all, df1, df2, df21, df22, df23, data.list)
# free up memory
gc()

##### factor label and values ######
s <- which(!(code_tbl_89$level == ""))
lev <- list() #[1-7], a_xx; [8-22], b_xx; [23-24], f; [25-27], c; [28-30], d
lab <- list() #[1-7], a_xx; [8-22], b_xx; [23-24], f; [25-27], c; [28-30], d
for(i in 1:length(s)){
        lev[[i]] <- code_tbl_89$level[s[i]] %>% str_extract_all("[0-9]+") %>% .[[1]]
        lab[[i]] <- code_tbl_89$level[s[i]] %>% str_split("[0-9]+\\. ") %>% .[[1]] %>% .[-1]
        }

##### save ###### 
# .RData
save(df.inc89, file = "AA170025/inc89.RData")
save(code_tbl_89, file = "AA170025/code_tbl_89.RData")
# .csv format
# write_csv(df.inc106, "inc106.csv", col_names = TRUE, na = "")
# .sas7bdat format
# write_sas(df.inc106, "inc106.sas7bdat")
# .sav format
# write_sav(df.inc106, "inc106.sav", compress = TRUE)

##### time ######
proc.time() - ptm

##### remove all objects ######
# rm(list = ls())