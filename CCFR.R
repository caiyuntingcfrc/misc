
# prep and options --------------------------------------------------------

# rm and clear console
rm(list = ls()); cat("\14")
# source("~/Github_CFRC/misc/func_ins.pack.R")
devtools::source_url("https://raw.githubusercontent.com/caiyuntingcfrc/misc/function_poverty/func_ins.pack.R")
# setwd
setwd("d:/R_wd/women/")
# options
options(scipen = 999)
# load packages
ins.pack("haven", "data.table", "tidyverse", 
         "magrittr", "docxtractr")
# data source
path_code <- "AA150018/code105.docx"
path_dat <- "AA150018/Women105.dat"
year <- 105


# codebook ----------------------------------------------------------------

# extract table in docx
code_tbl <- read_docx("AA150018/code105.docx") %>% 
        docx_extract_tbl() %>% 
        na.omit()
# names
names(code_tbl) <- c("q_num", "var_name", "var_pos", 
                     "var_label", "val_label", "note")

# start and end
m <- str_split(code_tbl$var_pos, "-", simplify = TRUE)
w <- which(m == "", arr.ind = TRUE)[ , 1]
m[w, ][ , 2] <- m[w, ][ , 1]
code_tbl$start <- as.integer(m[ , 1])
code_tbl$end <- as.integer(m[ , 2])

# read the file -----------------------------------------------------------

dat <- read_fwf(file = path_dat, 
                col_positions = fwf_positions(start = code_tbl$start, 
                                              end = code_tbl$end, 
                                              col_names = code_tbl$var_name), 
                col_types = cols(a3 = "d"))

# calc: n.children --------------------------------------------------------

# replace na with 0
dat[ , c("b2a_a", "b2a_b")] %<>% expss::if_na(., 0)

# check weather the weights are numeric
if(sum(sapply(dat[ , c("weight1", "weight2")], is.character)) == 2) {
        dat[ , c("weight1", "weight2")] %<>% sapply(., as.numeric)
}

# dplyr approach
dat %<>% rowwise() %>% mutate(n.children = sum(b2a_a, b2a_b, na.rm = TRUE))
# data.table approach
setDT(dat)
dat[ , n.children_rev := rowSums(.SD, na.rm = TRUE), .SDcols = c("b2a_a", "b2a_b")]
# compare the result
setdiff(dat$n.children, dat$n.children_rev)

# var: haveChildren
dat[ , haveChildren := if_else(n.children > 0, 1, 0)]


# calc: childless ---------------------------------------------------------

# filter age == 46 (cohort 1970)
df <- dat %>% 
        filter(a3 == 46) %>% 
        filter(a0 %in% c(1, 2))

# sample freq table
epiDisplay::tab1(df$haveChildren, decimal = 2, graph = FALSE)

# wtab approach
# weight table
wtab <- round(xtabs(df[["weight2"]] ~ df[["haveChildren"]]))
# replicate by weight
i <- names(wtab)
weighed <- unlist(mapply(rep, x = i, times = wtab))
# weighed <- unlist(weighed, use.names = TRUE)

# pop freq table
epiDisplay::tab1(weighed, decimal = 2, graph = FALSE)

# dplyr approach
# weigh table
wtab <- df %>% 
        group_by(haveChildren) %>%
        summarise(w = round(sum(weight2, na.rm = TRUE)), .groups = "keep")
# replicate by weight
weighed <- unlist(mapply(rep, x = wtab$haveChildren, times = wtab$w))
# pop freq table
epiDisplay::tab1(weighed, decimal = 2, graph = FALSE)

# calc: CCFR (cohort 1970) ------------------------------------------------

df <- dat %>% 
        filter(a3 %in% 45:49)

# n.women
n.women <- sum(df$weight2, na.rm = TRUE)

# n.children
wtab <- round(xtabs(df[["weight2"]] ~ df[["n.children"]]))
n.children <- sum(as.numeric(names(wtab)) * as.numeric(wtab))

ccfr70 <- n.children / n.women

# dplyr
wtab <- df %>% 
        group_by(n.children) %>% 
        summarise(w = round(sum(weight2, na.rm = TRUE)), .groups = "keep")
n.children <- sum(with(wtab, n.children * w))

ccfr70 <- n.children / n.women
