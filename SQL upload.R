
# prep and options --------------------------------------------------------

# list of packages
list.packages <- c("tidyverse", "magrittr", "haven", "DBI")
# check if the packages are installed
new.packages <- list.packages[!(list.packages %in% installed.packages()[ , "Package"])]
# install new packages
if(length(new.packages)) install.packages(new.packages)
# load the packages
lapply(list.packages, require, character.only = TRUE)
# remove lists
rm(list.packages, new.packages)

# load data file ----------------------------------------------------------

load("tw_inc/R data files/df_inc104.RData")
# MySQL Upload test -------------------------------------------------------
con <- odbc::dbConnect(odbc(), "group1_DB")
rm(df2)
l <- sapply(df.inc104, typeof)
l[l=="character"] <- "text"
l
df2 <- df.inc104[756:1246]
        # select(1:765) %>% 
        # slice(2)
# dbWriteTable()
table <- names(df2)[1:10]
dbWriteTable(conn = con, name = "df3", value = df2, overwrite = TRUE, row.names = FALSE) 
odbc::dbWriteTable(conn = con, name = "iris", value = iris, overwrite = TRUE) 
# DBI::dbAppendTable(conn = con, name = "df", value = df2, append = TRUE) 
gc()

rm(list = ls())
file_path_inc <- "https://cfrc.tbcloud.tk/index.php/s/bcqXSXEB3FjzakX/download"
load(url(file_path_inc))

write_csv(df.inc104, path = "104.inc.csv")
names(df.inc104)
sapply(df.inc104, typeof)

write_csv(iris, path = "iris.csv", col_names = TRUE)
names(iris)
s <- sapply(iris, typeof)
dbSendStatement(conn = con, "CREATE TABLE `test` (`col1` double, `col2` doubl)")

