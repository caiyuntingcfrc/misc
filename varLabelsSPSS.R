# prep and options --------------------------------------------------------

# rm and clear console
rm(list = ls()); cat("\14")
devtools::source_url("https://raw.githubusercontent.com/caiyuntingcfrc/misc/function_poverty/func_ins.pack.R")
# setwd
setwd("d:/R_wd/misc/working/")
# option: scipen
options(scipen = 999)
# load packages
ins.pack("tidyverse", "docxtractr")

# labels ------------------------------------------------------------------

doc <- read_docx("codebook.docx") %>% 
        docx_extract_tbl(preserve = TRUE) %>% 
        .[complete.cases(.), ] %>% 
        select(-1)

# GET FILE ----------------------------------------------------------------

# .sav file
# s <- "GET FILE='d:\\R_wd\\misc\\working\\CFRC_FSE14_W1_data_20200918.sav'.\n"
# cat(s, file = "syntax.sps")

# .csv file
getfile <- "SET UNICODE=ON.
GET DATA
 /TYPE = TXT
 /FILE = 'D:/R_wd/misc/working/data.dat'
 /DELCASE = LINE
 /DELIMITERS = '\\t'
 /ARRANGEMENT = DELIMITED
 /FIRSTCASE = 1
 /IMPORTCASE = ALL
 /VARIABLES = "
cat(getfile, file = "syntax.sps")

# variables ---------------------------------------------------------------

doc <- doc %>% 
        mutate(format = case_when(
                備註 == "字串" ~ "A", 
                備註 == "數字" ~ "F2"))
# format for
fmt <- paste0(doc$變項名稱, " ", doc$format, "\n", " ")
cat("\n", " ", fmt, file = "syntax.sps", ".", append = TRUE, sep = "")
cat("\n", "CACHE.\n", "EXECUTE.\n", file = "syntax.sps", append = TRUE, sep = "")

# variable labels ---------------------------------------------------------

vars <- paste0(doc$題號, " ", doc$變項說明)
varLabs <- paste0("VARIABLE LABELS", " ", doc$變項名稱, " ", "'", vars, "'", ".", "\n")
cat("\n", varLabs, file = "syntax.sps", append = TRUE, sep = "")

# # delete "." --------------------------------------------------------------
# 
# var_lab <- read_lines("val.txt") %>% 
#         str_split(pattern = "\\-*+[\\.]\\s", simplify = TRUE)
# # paste0
# lab <- paste0(var_lab[ , 1], 
#               ifelse(rowSums(var_lab == "") == 1, "", " "), 
#               var_lab[ , 2])
# # save file
# writeLines(lab, "varLabs.txt")
# cat("\n", lab)

# value labels ------------------------------------------------------------

# return index by which()
i <- which(!(doc$選項數值說明 == "無"))
# apply gsub, replace "=" with white space
strings <- lapply(doc$選項數值說明[i], gsub, pattern = "=", replacement = " ") %>% 
        # replace "\n" with "\n " (white space)
        lapply(., gsub, pattern = "\n", replacement = "\n ")
# write SPSS syntax: VALUE LBELS
valLabs <- paste0("VALUE LABELS", " ", doc$變項名稱[i], "\n ", strings, ".", "\n")
# export
cat("\n", valLabs, file = "syntax.sps", append = TRUE, sep = "")

# missing values ----------------------------------------------------------

# if_else, "()" means deleting defined missing values
m <- if_else(doc$遺漏 == "無", "()", paste0("(", doc$遺漏, ")")) 
# write SPSS syntax: MISSING VALUES
missing <- paste0("MISSING VALUES", " ", doc$變項名稱, " ", m, ".", "\n")
# export
cat("\n", missing, file = "syntax.sps", append = TRUE, sep = "")

# measure -----------------------------------------------------------------

doc <- doc %>% 
        mutate(measure = case_when(
                測量 == "名義" ~ "(NOMINAL)", 
                測量 == "序數" ~ "(ORDINAL)", 
                測量 == "尺度" ~ "(SCALE)"))
# write SPSS syntax: VARIABLE LEVEL
measure <- paste0("VARIABLE LEVEL", " ", doc$變項名稱, " ", doc$measure, ".\n")
# export
cat("\n", measure, file = "syntax.sps", append = TRUE, sep = "")

# save .sav file ----------------------------------------------------------

# save <- "SAVE OUTFILE = 'D:/R_wd/misc/working/CFRC_FSE14_W1_data_test.sav'."
# cat("\n", save, file = "syntax.sps", append = TRUE, sep = "")
