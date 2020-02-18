
# author: CAI YUN-TING ----------------------------------------------------
# The Survey of Family Income and Expenditure, 1976 -----------------------

# prep and options --------------------------------------------------------
# rm
rm(list = ls()); cat("\14")
# set working directory
setwd("~/R_wd/tw_inc/")
# soure func: ins.pack
devtools::source_url("https://raw.githubusercontent.com/caiyuntingcfrc/misc/function_poverty/func_ins.pack.R")
# loading packages
ins.pack("tidyverse", "docxtractr", "readtext", 
         "haven", "hablar", "data.table")
# options
options(readr.show_progress = TRUE)
# do not show scientific notation
options(scipen = 999)
# timestamp
timestamp <- format(Sys.time(), "%m%d-%H%M")
# processing time
ptm <- proc.time()
# data source
path_code <- "AA170001/code65.docx"
path_dat <- "AA170001/inc65.dat"
year <- 65

# create the codebook -----------------------------------------------------

# codebook
# file is in the default working dirctory
code_tbl <- read_docx(path_code) %>% 
        docx_extract_tbl() %>% 
        .[complete.cases(.), ]
# add row: card_num
code_tbl <- rbind(code_tbl, c(NA, "card_num", "#/79-80", NA, NA, NA))
# colnames
colnames(code_tbl) <- c("q_num", "variable", "card_pos", 
                           "label", "level", "note")
# variable b1_# - b25_#
l <- grep("^b", code_tbl$variable)
code_tbl$variable[l] %<>% 
        str_split("#", simplify = TRUE) %>%
        .[ , 1] %>% as.character()
rm(l)

# start
code_tbl$start <- code_tbl$card_pos %>% 
        str_split("/", simplify = TRUE) %>% 
        .[ , 2] %>% str_split("-", simplify = TRUE) %>% 
        .[ ,1] %>% as.integer()
# end
code_tbl$end <- code_tbl$card_pos %>% 
        str_split("/", simplify = TRUE) %>% 
        .[ , 2] %>% str_split("-", simplify = TRUE) %>% 
        .[ ,2] %>% as.integer()
# replace NA in end
code_tbl$end <- with(code_tbl, if_else(is.na(end), start, end))

# list for grep (grepbook) ------------------------------------------------

sym <- c("{", grep("[A-R]", LETTERS, value = TRUE), "}")
digi <- c(0:9, 1:9, 0)
positive <- c(rep("+", 10), rep("-", 10))
grepbook <- tibble(sym, digi, positive)
# pattern for grep and gsub
pattern <- grepbook$sym %>% .[2:19]
grepbook$pattern <- c("\\{$", sapply(pattern, paste, "$", sep = ""), "\\}$")

# names of item_xxx -------------------------------------------------------

doc.text.parts <- readtext(path_code)$text %>% 
        strsplit("\n") %>% .[[1]]
# items
doc.items <- doc.text.parts %>% 
        # begin with at least two digits
        grep("^[1-9][0-9]{1,3}：", ., value = TRUE) %>% 
        strsplit(., "：") %>% 
        unlist() 
        
# item numbers
doc.items.digits <- doc.items %>% 
        .[2 * (1:length(.)) - 1] %>% 
        .[!is.na(.)] %>% 
        .[-(1:27)]
# item contents
doc.items.contents <- doc.items %>% 
        .[2 * (1:length(doc.text.parts))] %>% 
        .[!is.na(.)] %>% 
        .[-(1:27)]

# spss file ---------------------------------------------------------------

d.spss <- read_spss("AA170001/inc65.sav") %>% 
        zap_formats() %>% 
        zap_labels()

# data processing and manipulation ----------------------------------------
# data raw and card_num
df.source <- read_fwf(path_dat, fwf_positions(start = c(1, 79), 
                                              end = c(80, 80), 
                                              col_names = c("raw", "card_num")
                                              ), 
                      # card_num as integer
                      col_types = cols(card_num = "i", .default = "c"))

# card 01 -----------------------------------------------------------------
# filter out card_num == 1
x <- filter(df.source, card_num == 1) %>% .[ ,1] %>% .$raw
# define tempfile name and format
y <- tempfile("tp", fileext = ".dat")
# write the tempfile
write(x, file = y)
# read card 1
# code_tbl[2:25]
l <- grep("x1|area|stage|id|^a[0-9]", code_tbl$variable)
df1 <- read_fwf(y, fwf_positions(code_tbl$start[l], 
                                 code_tbl$end[l],
                                 col_names = code_tbl$variable[l]), 
                # define column types (variable classes) in df1
                col_types = cols(x1 = "c", area = "c", stage = "f", 
                                 id = "c", 
                                 a1 = "f", a2 = "f", a3 = "f", 
                                 a21 = "n")) %>% 
        # order data
        .[order(.$x1), ]
# free up ram
gc()


# card 02 -----------------------------------------------------------------
# card_num 02:20
# function f2
f2 <- function(c, d = c - 1) {
        # variabl names
        # l <- grep("^x1|^b", code_tbl$variable)
        l1 <- grep("^x1|^b.*_1", code_tbl$variable, value = TRUE)
        l2 <- grep("^x1|^b.*_2", code_tbl$variable, value = TRUE)
        # start and end
        s1 <- code_tbl$start[code_tbl$variable %in% l1]
        e1 <- code_tbl$end[code_tbl$variable %in% l1]
        s2 <- code_tbl$start[code_tbl$variable %in% l2]
        e2 <- code_tbl$end[code_tbl$variable %in% l2]
        # colnames
        name1 <- str_split(l1[-1], "_", simplify = TRUE) %>% 
                .[ , 1] %>% 
                paste(., "_", (c - 1) * 2 - 1, sep = "")
        name2 <- str_split(l2[-1], "_", simplify = TRUE) %>% 
                .[ , 1] %>% 
                paste(., "_", (c - 1) * 2, sep = "")
        
        # if input c (card_num) is not in the raw data, 
        # then create a temp file with a "first row" (for merging data), 
        # which will fill NA.
        if(c %in% df.source$card_num) {
                # filter out card_num == 02:20 and create a temporary .dat file
                x <- filter(df.source, card_num == c) %>% .[ ,1] %>% .$raw
                # tempfile
                y <- tempfile("tmp", fileext = ".dat")
                write(x, file = y)
                # read file
                tmp1 <- read_fwf(y, fwf_positions(start = s1, 
                                                  end = e1, 
                                                  col_names = c("x1", name1)))
                # in some occasion X1 will have wrong type (such as num)
                # and then it will cause error while executing left_join
                # so here we convert all the x1 variable into chr type
                tmp1$x1 <- as.character(tmp1$x1)
                tmp2 <- read_fwf(y, fwf_positions(start = s2, 
                                                  end = e2, 
                                                  col_names = c("x1", name2)))
                tmp2$x1 <- as.character(tmp2$x1)
                tmp <- left_join(tmp1, tmp2, by = "x1")
                } else {l <- grep("^x1|^b", code_tbl$variable)
                        tmp <- matrix(ncol = length(l)) %>% 
                        as_tibble(.name_repair = NULL)
                        colnames(tmp) <- c("x1", name1, name2)
                        tmp[ , 1] <- "01100001"        
                        }
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
                       num(contains("b15_")))
# b2_, b3_ ... (factor)
variables <- colnames(df2)
l <- grep("x1|b4|b15", variables)
# mutate_if
df2[ , variables[-l]] %<>% mutate_if(is.character, as.factor) %>% 
        mutate_if(is.numeric, as.factor)
# free up ram
gc()

# card 21 -----------------------------------------------------------------
# filter out card_num == 21
x <- filter(df.source, card_num == 21) %>% .[ ,1] %>% .$raw
y <- tempfile("tmp", fileext = ".dat")
write(x, file = y)
l <- grep("x1|^f", code_tbl$variable)
df21 <- read_fwf(y, fwf_positions(code_tbl$start[l], 
                                  code_tbl$end[l],
                                  # variable names
                                  col_names = code_tbl$variable[l]), 
                 # define column types
                 cols(x1 = "c", .default = "n")
                 ) %>% 
        # order by x1
        .[order(.$x1), ]

# free up ram
gc()


# card 22 -----------------------------------------------------------------
# filter out card_num == 22
x <- filter(df.source, card_num == 22) %>% .[ ,1] %>% .$raw
y <- tempfile("tmp", fileext = ".dat")
write(x, file = y)
# code_tbl[86:112]
l <- grep("x1|^c[0-9]|^d[0-9]", code_tbl$variable)
df22 <- read_fwf(y, fwf_positions(code_tbl$start[l], 
                                  code_tbl$end[l],
                                  # variable names
                                  col_names = code_tbl$variable[l]), 
                 # define column types
                 col_types = cols(x1 = "c", 
                                  c28 = "n", c29 = "n", c30 = "n", 
                                  c21 = "n", 
                                  .default = "f")) %>% 
        # order by x1
        .[order(.$x1), ]

# free up ram
gc()

# card 23-99 --------------------------------------------------------------
# filter out card_num %in% 23:99
x <- filter(df.source, card_num %in% 23:99) %>% .[ ,1] %>% .$raw
y <- tempfile("tmp", fileext = ".dat")
write(x, file = y)
d <- vector("list", length = length(x))
l <- grep("^x1|^itm|^amt", code_tbl$variable)
s <- c(code_tbl$start[l])
e <- c(code_tbl$end[l])
d <- read_fwf(y, fwf_positions(start = s, 
                               end = e), 
              col_types = cols(.default = "c"))
d.test <- d %>% 
        filter(X1 == "01100001") %>% 
        t() %>% 
        as_tibble() %>% 
        gather(d.test)
names(d.test) <- unlist(c(d.test[1, 2], "value"))
d.test %<>% .[-1, ]
d.test$d.test <- "key"
spread(d.test, key = "d.test", value = "value")
d$time <- with(d, ave(rep(1, nrow(d)), X1, FUN = seq_along))
dL <- melt(d, id.vars = c("X1", "time"))
dd <- dcast(dL, X1 ~ variable + time)

# test
# mydf <- read_csv("~/Documents/test.csv")
# mydf$time <- with(mydf, ave(rep(1, nrow(mydf)), 
#                Prefix, FUN = seq_along))
# dfL <- melt(mydf, id.vars=c("Prefix", "Exchange", "time"))
# dcast(dfL, Prefix ~ variable + time)

# 
test <- d %>% reshape(direction = "wide", 
                      idvar = c("X1"))
test <- d %>% pivot_wider()
n1 <- rep(c("itm", "atm"), each = 1, times = (ncol(d) - 1) / 2)
n2 <- rep(1:(length(n1) / 2), each = 2)
col1 <- paste("x1", n1, n2, sep = "")
colnames(d) <- col1
for(i in 1:length(x)) { 
        d[[i]] <- read_fwf(y, fwf_positions(start = s, 
                                            end = e))
        }
# for loop (5 sections)
# item 101 = "1010" (9, 12) , take posistion (9, 11)
# x <- list()
# for(i in 0:6) {
#         x[[i + 1]] <- read_fwf(y, fwf_positions(c(1, 9 + i * 10, 12 + i * 10),
#                                             c(8, 11 + i * 10, 18 + i * 10),
#                                             col_names = c("x1", "item", "exp")), 
#                                col_types = cols(x1 = "c", item = "c", exp = "c")
#                            )
#         df23 <- rbindlist(x) %>% distinct()
#         }
# # free up ram
# gc()
x <- list()
for(i in 0:6) {
        x[[i + 1]] <- read_fwf(y, fwf_positions(c(1, 9 + i * 10, 12 + i * 10),
                                            c(8, 11 + i * 10, 18 + i * 10),
                                            col_names = c("x1", "item", "exp")), 
                               col_types = cols(x1 = "c", item = "c", exp = "c")
                           )
        df23 <- rbindlist(x) %>% distinct()
        }
# free up ram
gc()

##### replace symbols with digits #####
p <- grepbook$pattern
r <- grepbook$digi
for(i in 1:10) {
        # postitive [1:10]
        a <- grep(p[i], df23$exp)
        df23$exp[a] <- gsub(pattern = p[i], 
                            replacement = r[i], 
                            x = grep(p[i], df23$exp, value = TRUE))
        # negative [11:20]
        b <- grep(p[i + 10], df23$exp)
        df23$exp[b] <- gsub(pattern = p[i + 10], 
                            replacement = r[i + 10], 
                            x = grep(p[i + 10], df23$exp, value = TRUE)) %>% 
                paste("-", ., sep = "")
}

# spread (transpose)
df23 <- df23 %>% distinct() %>% spread(key = "item", value = "exp", drop = FALSE)

d.test <- df23 %>% 
        filter(x1 == "01100001") %>% 
        group_by(x1) %>% 
        spread(key = x1, value = exp) %>% 
        as.data.table()
colnames(d.test) <- c("item", "x1")
d.test[ , c := paste0(item, x1)]

d <- spread(d.test, x1, exp) %>% 
        group_by() %>% 
        as.data.table()
d

# df23 <- df23 %>% filter(x1 %in% c("01100001", "01100002"))
d$time <- with(d, ave(rep(1, nrow(d)), item, FUN = seq_along))

# spread (transpose)
df23 <- df23 %>% distinct() %>% spread(key = "item", value = "exp")

d.test <- spread(df23, x1, exp, drop = TRUE)
# 

df23 %<>% select(-x1)
dL <- melt(df23, id.vars = c("item", "time"))
dd <- dcast(dL, variable ~ item + time, value.var = "value")

# replace symbols with digits ---------------------------------------------

p <- grepbook$pattern
r <- grepbook$digi
for(i in 1:10) {
        # postitive [1:10]
        a <- grep(p[i], df23$exp)
        df23$exp[a] <- gsub(pattern = p[i], 
                            replacement = r[i], 
                            x = grep(p[i], df23$exp, value = TRUE))
        # negative [11:20]
        b <- grep(p[i + 10], df23$exp)
        df23$exp[b] <- gsub(pattern = p[i + 10], 
                            replacement = r[i + 10], 
                            x = grep(p[i + 10], df23$exp, value = TRUE)) %>% 
                paste("-", ., sep = "")
        }

# spread (transpose)
test <- df23 %>% filter(x1 == "01100001")
test %>% spread(key = x1, value = "exp")
df23[1:10, ] %>% spread(x1, value = "exp") %>% 
        mutate(card = "01100001") %>% 
        spread(key = "card", value = "01100001")
dtest <- df23 %>% spread(key = "x1", value = "exp")
df23[c(259, 166917), ]
# remove column `0000`
df23 <- df23 %>% select( - one_of("0000"))

# items without observations ----------------------------------------------
# names of all the items
colnames(df23)[-1] <- colnames(df23)[-1] %>% 
        as.integer() %>% 
        paste("itm", ., sep = "")
itms_all <- doc.items.digits %>% 
        as.integer() %>% 
        paste("itm" , ., sep = "")
# create a tibble for those who are not in df23
df.itm.all <- matrix("NA", nrow = nrow(df23), ncol = length(itms_all)) %>% 
        as_tibble(.name_repair = NULL)
# create x1 column for merging
df.itm.all$x1 <- df23$x1
# name the columns with all item names
colnames(df.itm.all) <- c(itms_all, "x1")
# merge
df23 <- df23 %>% left_join(df.itm.all) %>% 
        # column types (hablar::convert)
        convert(chr(x1), num(contains("itm"))) %>% 
        # order
        .[order(.$x1), ]
# free up ram
gc()

# merge -------------------------------------------------------------------
data.list <- list(df1, df2, df21, df22, df23)
df.inc <- Reduce(function(...) left_join(..., by = "x1"), data.list)
# add year column
df.inc$year <- year
#
df.inc107 <- df.inc
code_tbl_107 <- code_tbl
# remove
rm(df.source, x, df.itm.all, 
   df1, df2, df21, df22, df23, 
   data.list, df.inc, code_tbl)
# free up memory
gc()


# save --------------------------------------------------------------------
# .RData
# save df.inc
save(df.inc107, file = "AA170043/df_inc107.RData")
save(df.inc107, file = "R data files/df_inc107.RData")
# save code_tbl
save(code_tbl_107, file = "AA170043/code_tbl_107.RData")
save(code_tbl_107, file = "R data files/code_tbl_107.RData")
# .csv format
# write_csv(df.inc106, "inc106.csv", col_names = TRUE, na = "")
# .sas7bdat format
# write_sas(df.inc106, "inc106.sas7bdat")
# .sav format
# write_sav(df.inc106, "inc106.sav", compress = TRUE)

##### time ######
proc.time() - ptm

##### remove all objects ######
l <- grep("^df.inc|^code_tbl", ls())
rm(list = ls()[-l])