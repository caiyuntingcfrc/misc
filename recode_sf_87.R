
# prep --------------------------------------------------------------------

rm(list = ls())
setwd("d:/R_wd/tw_inc/R data files/")
# soure func: ins.pack
devtools::source_url("https://raw.githubusercontent.com/caiyuntingcfrc/misc/function_poverty/func_ins.pack.R")
ins.pack("feather", "tidyverse", "magrittr", "data.table")

# load --------------------------------------------------------------------

inc87 <- read_feather("df_inc87.feather")
# dt
dt <- inc87
# setDT
setDT(dt)

# factor to numeric (bxx_)
lb101 <- grep("^b|itm101$", names(dt))
dt[ , (lb101) := lapply(.SD, as.character), .SDcols = lb101]
dt[ , (lb101) := lapply(.SD, as.numeric), .SDcols = lb101]
# dt[ , l] <- lapply(dt[ , l], as.character) %>% lapply(., as.numeric)


# numbers of people -------------------------------------------------------

# grep
lb4 <- grep("^b4_", names(dt), value = TRUE)
# numbers of people in the household
dt[ , n.all := rowSums(!is.na(.SD)), .SDcols = lb4]

# numbers of children -----------------------------------------------------

lb4 <- grep("^b4_", names(dt), value = TRUE)
dt[ , n.children := rowSums(.SD < 18, na.rm = TRUE), .SDcols = lb4]

# numbers of the elderly --------------------------------------------------

lb4 <- grep("^b4_", names(dt), value = TRUE)
dt[ , n.elderly := rowSums(.SD >= 65, na.rm = TRUE), .SDcols = lb4]

# head's marital status ---------------------------------------------------
# convert b2_ to numeric

lb2 <- grep("^b2_", names(dt))
m <- which(dt[ , ..lb2] == 1, arr.ind = TRUE)

# row and column
r <- m[ , 1]
c <- m[ , 2]
# recode h.select
dt[r , h.select := c]
# recode h.marital
dt[ , h.marital := .SD[[paste0("b16_", .BY$h.select)]], by = h.select]

# heawd's sex -------------------------------------------------------------

dt[ , h.sex := .SD[[paste0("b3_", .BY$h.select)]], by = h.select]

# head's parents and grand parents' marital status ------------------------

df$`p.marital1` <- NULL
df$`p.marital2` <- NULL
p.marital1 <- vector("numeric", nrow(df))
p.marital2 <- vector("numeric", nrow(df))
l <- grep("^b2_", names(df))
for(i in 1:nrow(df)) { 
        # search for head (identity: parents)
        n <- names(df[i, l][ , which(df[i, l] == 5)])
        # head had only one of the parents
        if( length(n) == 1 ) { 
                n <- str_split(n, "_", simplify = TRUE)
                n1 <- paste("b16_", n[1 , 2], sep = "")
                p.marital1[i] <- df[[n1]][i]
        } else if ( length(n) == 2) {
                n <- str_split(n, "_", simplify = TRUE)
                n2 <- paste("b16_", n[2 , 2], sep = "")
                p.marital2[i] <- df[[n2]][i]
        } else { 
                p.marital1[i] <- NA 
                p.marital2[i] <- NA 
        }
}
df$`p.marital1` <- p.marital1
df$`p.marital2` <- p.marital2

# recode: single person and married couple --------------------------------

# single-person
c101 <- with(dt, n.all == 1 & h.sex == 1)
c102 <- with(dt, n.all == 1 & h.sex == 2)
dt[ , sf := ifelse(c101, "101", NA_character_)][ , sf := ifelse(c102, "102", sf)]

# married couple
c201 <- with(dt, n.all == 2 & 
                     b2_1 %in% c(1, 2) & 
                     b2_2 %in% c(1, 2) & 
                     h.sex == 1)

c202 <- with(dt, n.all == 2 & 
                     b2_1 %in% c(1, 2) & 
                     b2_2 %in% c(1, 2) & 
                     h.sex == 2)

dt[ , sf := ifelse(c201, "201", sf)][ , sf := ifelse(c202, "202", sf)]

# recode: single-parent family --------------------------------------------

# the head is the 2nd gen
l <- grep("^b2_", names(df))
l2 <- grep("^b16_", names(df))
for( i in 1:nrow(df)) {
        # only one of the parents
        if( 
                # at least two in the household
                df$n.all[i] >= 2 &
                # only the head and the head's children
                sum(df[i, l] %in% c(3), na.rm = TRUE) >= 1 &
                # at least one child is unmarried
                sum(df[i, l2][which(df[i, l] == 3)] %in% c(91), na.rm = TRUE) >= 1 &
                # the head is divorced or widowed
                df$h.marital[i] %in% c(94, 95, 96) &
                # the head is male
                df$h.sex[i] == 1 ) {
                
                df$sf[i] <- "321"
                
        } else if ( 
                df$n.all[i] >= 2 &
                sum(df[i, l] %in% c(3), na.rm = TRUE) >= 1 &
                sum(df[i, l2][which(df[i, l] == 3)] %in% c(91), na.rm = TRUE) >= 1 &
                df$h.marital[i] %in% c(94, 95, 96) &
                # the head is female
                df$h.sex[i] == 2 ) { 
                
                df$sf[i] <- "322" 
                
        }
}

# the head is 3rd gen
l <- grep("^b2_", names(df))
l2 <- grep("^b16_", names(df))
# sf <- vector("character", nrow(df))
for( i in 1:nrow(df)) {
        # only one of the parents
        if(sum(df[i, l] == 5, na.rm = TRUE) == 1 &
           # length(grep("^[5]$", df[i, l])) == 1 &
           # only one of the parents, the head, and siblings
           sum(df[i, l] %in% c(1, 5), na.rm = TRUE) >= 2 &
           # length(grep("^[1]$|^[5]$|^[7]$", df[i, l])) == n.all[i] & 
           # only unmarried, divorced, or widowed
           sum(df[i, l2] %in% c(91), na.rm = TRUE) >= 1 &
           # length(grep("^[9][1]$|^[9][7]$", df[i, l2])) == n.all[i] &
           # only the parent is divorced or widowed
           # sum(df[i, l2] %in% 97, na.rm = TRUE) == 1 & 
           # sum(df[i, l2][which(df[i, l] %in% c(1, 7))] == 91, na.rm = TRUE) >= 1 &
           # the head is unmarried
           h.marital[i] == 91 &
           # the head is male
           h.sex[i] == 1 ) {
                
                df$sf[i] <- "331"
                
        } else if ( sum(df[i, l] == 5, na.rm = TRUE) == 1 &
                    sum(df[i, l] %in% c(1, 5), na.rm = TRUE) >= 2 &
                    sum(df[i, l2] %in% c(91), na.rm = TRUE) >= 1 &
                    # sum(df[i, l2] %in% 97, na.rm = TRUE) == 1 & 
                    # sum(df[i, l2][which(df[i, l] %in% c(1, 7))] == 91, na.rm = TRUE) >= 1 &
                    h.marital[i] == 91 &
                    h.sex[i] == 2) { 
                
                df$sf[i] <- "332" 
                
        }
}

# recode: core family -----------------------------------------------------

# the head is the 2nd gen
l <- grep("^b2_", names(df))
l2 <- grep("^b16_", names(df))
for( i in 1:nrow(df)) {
        if( # at least 3 in the household
                df$n.all[i] >= 3 &
                # the head and the head's spouse, the head's children and others
                sum(df[i, l] %in% c(4, 5, 9, 11), na.rm = TRUE) == 0 &
                # at least one of the children is unmarried
                sum(df[i, l2][which(df[i, l] == 3)] == 91, na.rm = TRUE) >= 1 &
                # the head is married
                !(h.marital[i] %in% c(91, 92, 97)) &
                # the head is male
                h.sex[i] == 1 ) {
                
                df$sf[i] <- "421"
                
        } else if ( df$n.all[i] >= 3 &
                    sum(df[i, l] %in% c(4, 5, 9, 11), na.rm = TRUE) == 0 &
                    sum(df[i, l2][which(df[i, l] == 3)] == 91, na.rm = TRUE) >= 1 &
                    !(h.marital[i] %in% c(91, 92, 97)) &
                    h.sex[i] == 2 ) { 
                
                df$sf[i] <- "422" 
                
        }
}

# the head is the 3rd gen
l <- grep("^b2_", names(df))
l2 <- grep("^b16_", names(df))
for( i in 1:nrow(df)) {
        # at least 3 in the household
        if( df$n.all[i] >= 3 &
            # at least one of the parents
            sum(df[i, l] == 5, na.rm = TRUE) >= 1 &
            # no grand parents
            sum(df[i, l] == 6, na.rm = TRUE) == 0 &
            # length(grep("^[5]$", df[i, l])) >= 1 &
            # length(grep("^[6]$", df[i, l])) == 0 &
            # one of the parents are married
            sum(df$p.marital1[i] %in% c(91, 92, 97), na.rm = TRUE) == 0 &
            sum(df$p.marital2[i] %in% c(91, 92, 97), na.rm = TRUE) == 0 &
            # at least one of the siblings is unmarried
            sum(df[i, l2][which(df[i, l] %in% c(1, 7))] == 91, na.rm = TRUE) >= 1 &
            df$h.marital[i] == 91 &
            # the head is male
            df$h.sex[i] == 1 ) { 
                
                df$sf[i] <- "431"
                
        } else if ( df$n.all[i] >= 3 &
                    sum(df[i, l] == 5, na.rm = TRUE) >= 1 &
                    sum(df[i, l] == 6, na.rm = TRUE) == 0 &
                    sum(df$p.marital1[i] %in% c(91, 92, 97), na.rm = TRUE) == 0 &
                    sum(df$p.marital2[i] %in% c(91, 92, 97), na.rm = TRUE) == 0 &
                    sum(df[i, l2][which(df[i, l] %in% c(1, 7))] == 91, na.rm = TRUE) >= 1 &
                    df$h.marital[i] == 91 &
                    df$h.sex[i] == 2 ) { 
                
                df$sf[i] <- "432" 
                
        }
}

# recode: grand-parent famils --------------------------------------------

# the head is the 1st gen
l <- grep("^b2_", names(df))
l2 <- grep("^b16_", names(df))
for( i in 1:nrow(df)) {
        # none of the 2nd gen
        if( # length(grep("^[3]$", df[i, l])) == 0 &
                sum(df[i, l] == 3, na.rm = TRUE) == 0 &
                # at least one grand child
                sum(df[i, l] == 4, na.rm = TRUE) >= 1 &
                # at least one grand child is unmarried
                sum(df[i, l2][which(df[i, l] == 4)] == 91, na.rm = TRUE) >= 1 &
                # length(grep("^[1-2]$|^[4-5]$|^[7-9]$|^[1][1-4]$", df[i, l])) == df$n.all[i] &
                # the head is not single
                !(df$h.marital[i] == 91) &
                # the head is male
                df$h.sex[i] == 1) {
                
                df$sf[i] <- "511" 
                
        } else if (sum(df[i, l] == 3, na.rm = TRUE) == 0 &
                   sum(df[i, l] == 4, na.rm = TRUE) >= 1 &
                   sum(df[i, l2][which(df[i, l] == 4)] == 91, na.rm = TRUE) >= 1 &
                   # length(grep("^[1-2]$|^[4-5]$|^[7-9]$|^[1][1-4]$", df[i, l])) == df$n.all[i] &
                   !(df$h.marital[i] == 91) &
                   # the head is female
                   df$h.sex[i] == 2) {
                
                df$sf[i] <- "512"        
                
        }
}

# the head is the 3rd gen
l <- grep("^b2_", names(df))
l2 <- grep("^b16_", names(df))
for( i in 1:nrow(df)) {
        # none of the 2nd gen
        if( sum(df[i, l] == 5, na.rm = TRUE) == 0 &
            # at least one grand parent
            sum(df[i, l] == 6, na.rm = TRUE) >= 1 &
            # at least the head or one of the head's siblings is single
            sum(df[i, l2][which(df[i, l] %in% c(1, 7))] == 91, na.rm = TRUE) >= 1 &
            # length(grep("^[9][1]$", df[i, l2])) >= 1 &
            # all but not the 2nd gen
            # sum(!(df[i, l] == 5), na.rm = TRUE) == df$n.all[i] &
            # the head is single
            df$h.marital[i] == 91 &
            # the head is male
            df$h.sex[i] == 1) {
                
                df$sf[i] <- "531" 
                
        } else if (sum(df[i, l] == 5, na.rm = TRUE) == 0 &
                   # at least one grand parent
                   sum(df[i, l] == 6, na.rm = TRUE) >= 1 &
                   # at least the head or one of the head's siblings is single
                   sum(df[i, l2][which(df[i, l] %in% c(1, 7))] == 91, na.rm = TRUE) >= 1 &
                   # all but not the 2nd gen
                   # sum(!(df[i, l] == 5), na.rm = TRUE) == df$n.all[i]&
                   df$h.marital[i] == 91 &
                   # the head is female
                   df$h.sex[i] == 2) {
                
                df$sf[i] <- "532"        
                
        }
}

# recode: stem familie ----------------------------------------------------

# the head is the 1st gen
l <- grep("^b2_", names(df))
l2 <- grep("^b16_", names(df))

for(i in 1:nrow(df)) {
        # sf is NA
        if( is.na(df[i, "sf"]) & 
            # one of the grand children is single
            sum(df[i, l2][which(df[i, l] == 4)] == 91, na.rm = TRUE) >= 1 &
            # the head has at least one child
            sum(df[i, l] == 3, na.rm = TRUE) >= 1&
            df$h.sex[i] == 1) {
                
                df$sf[i] <- "611"
                
        } else if( is.na(df[i, "sf"]) & 
                   # one of the grand children is single
                   sum(df[i, l2][which(df[i, l] == 4)] == 91, na.rm = TRUE) >= 1 &
                   sum(df[i, l] == 3, na.rm = TRUE) >= 1&
                   df$h.sex[i] == 2) {
                
                df$sf[i] <- "612"
                
        }
}

# the head is the 2nd gen
l <- grep("^b2_", names(df))
l2 <- grep("^b16_", names(df))
for(i in 1:nrow(df)) {
        # sf is NA
        if( is.na(df[i, "sf"]) & 
            # one of the children is single
            sum(df[i, l2][which(df[i, l] == 3)] == 91, na.rm = TRUE) >= 1 &
            # the head has at least one parent or spouse's parent
            sum(df[i, l] %in% c(5, 11), na.rm = TRUE) >= 1 &
            # the head is not single
            !(df$h.marital[i] %in% c(91)) &
            # the head is male
            df$h.sex[i] == 1) {
                
                df$sf[i] <- "621"
                
        } else if( is.na(df[i, "sf"]) & 
                   # one of the grand children is single
                   sum(df[i, l2][which(df[i, l] == 3)] == 91, na.rm = TRUE) >= 1 &
                   sum(df[i, l] %in% c(5, 11), na.rm = TRUE) >= 1 &
                   !(df$h.marital[i] %in% c(91)) &
                   # the head is female
                   df$h.sex[i] == 2) {
                
                df$sf[i] <- "622"
                
        }
}

# the head is the 3rd gen
l <- grep("^b2_", names(df))
l2 <- grep("^b16_", names(df))
for(i in 1:nrow(df)) {
        # sf is NA
        if( is.na(df[i, "sf"]) & 
            # one of the children is single
            sum(df[i, l2][which(df[i, l] %in% c(1, 7))] == 91, na.rm = TRUE) >= 1 &
            # the head has at least one parent or spouse's parent
            sum(df[i, l] %in% c(5, 11), na.rm = TRUE) >= 1 &
            # the head has at least one grand parent
            sum(df[i, l] %in% c(6), na.rm = TRUE) >= 1 &
            # the head is not single
            !(df$h.marital[i] %in% c(92, 31:39)) &
            # the head is male
            df$h.sex[i] == 1) {
                
                df$sf[i] <- "631"
                
        } else if( is.na(df[i, "sf"]) & 
                   # one of the children is single
                   sum(df[i, l2][which(df[i, l] %in% c(1, 7))] == 91, na.rm = TRUE) >= 1 &
                   # the head has at least one parent or spouse's parent
                   sum(df[i, l] %in% c(5, 11), na.rm = TRUE) >= 1 &
                   # the head has at least one grand parent
                   sum(df[i, l] %in% c(6), na.rm = TRUE) >= 1 &
                   !(df$h.marital[i] %in% c(92, 31:39)) &
                   # the head is female
                   df$h.sex[i] == 2 ) {
                
                df$sf[i] <- "632"
                
        }
}

# recode: others ----------------------------------------------------------

df %<>% mutate(sf = case_when(!is.na(sf) ~ sf, 
                              h.sex == 1 ~ "701", 
                              h.sex == 2 ~ "702"))


# recode ------------------------------------------------------------------

df$a7 <- df$h.sex
df$a8 <- df$n.all
df$a12 <- df$n.adults
dt$a18 <- dt$sf
df$a19 <- df$n.elder

# recode:sf ---------------------------------------------------------------

dt %<>% mutate(str_family = case_when(a18 %in% c(101, 102) ~ 1L, 
                                      a18 %in% c(201, 202) ~ 2L, 
                                      a18 %in% c(321, 322, 331, 332) ~ 3L, 
                                      a18 %in% c(421, 422, 431, 432) ~ 4L, 
                                      a18 %in% c(511, 512, 531, 532) ~ 5L, 
                                      a18 %in% c(611, 612, 621, 622, 631, 632) ~ 6L, 
                                      a18 %in% c(701, 702) ~ 7L, 
                                      TRUE ~ NA_integer_))
# df %<>% apply_labels(df, 
#                      str_family = "structure of families", 
#                      str_family = c("single-person" = 1L, 
#                                     "married couple" = 2L, 
#                                     "single-parent" = 3L, 
#                                     "core" = 4L, 
#                                     "grandparent" = 5L, 
#                                     "stem" = 6L, 
#                                     "other" = 7L))
s <- dt$str_family
w <- dt$a21
x <- round(xtabs(w ~ s), digits = 0); x
n <- names(x)
weighed <- mapply(rep, x = n, times = x)
l <- unlist(weighed, use.names = FALSE)
table <- epiDisplay::tab1(l, decimal = 2, 
                          graph = TRUE)


# savethe file ------------------------------------------------------------

write_feather(df, "df_inc87.feather")
