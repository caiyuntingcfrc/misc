
# prep --------------------------------------------------------------------

rm(list = ls())
setwd("D:/R_wd/tw_inc/R data files/")
# soure func: ins.pack
devtools::source_url("https://raw.githubusercontent.com/caiyuntingcfrc/misc/function_poverty/func_ins.pack.R")
ins.pack("feather", "tidyverse", "magrittr")

# load --------------------------------------------------------------------
# file path
# file_path <- dir() %>% 
#         grep("^df_.*.feather", ., value = TRUE) %>% 
#         paste(getwd(), "/", ., sep = "")
# read feather
# l <- vector("list", length(file_path))
# l <- lapply(file_path, read_feather)
inc105 <- read_feather("df_inc105.feather")


# Recode:inc105 -----------------------------------------------------------

df <- inc105

# factor to numeric (bxx_)
l <- grep("^b|itm101$", names(df), value = TRUE)
df[ , l] <- lapply(df[ , l], as.character) %>% lapply(., as.numeric)


# Checkbox ----------------------------------------------------------------

check <- tempfile(fileext = ".dat")

# numbers of people -------------------------------------------------------

l <- grep("^b4_", names(df))
df$`n.all` <- NULL
n.all <- vector("numeric", nrow(df))
for(i in 1:nrow(df)) { 
        n.all[i] <- length(which(!is.na(df[i, l])))
        }
df$`n.all` <- n.all
# diff
d1 <- df %>% select(x1, a8)
d2 <- df %>% select(x1, n.all)
diff1 <- nrow(d1[!(d1$x1 %in% d2$x1), ])
diff2 <- nrow(d2[!(d2$x1 %in% d1$x1), ])
# check
if(diff1 == 0 & diff2 == 0) { 
        cat("1\n", file = check, append = FALSE) 
        } else {
                cat("n.all\n", file = check, append = FALSE)
                }


# numbers of adults -------------------------------------------------------

df$`n.adults` <- NULL
n.adults <- vector("numeric", nrow(df))
for(i in 1:nrow(df)) {
        n.adults[i] <- length(which(df[i, l] >= 18))
        }
df$`n.adults` <- n.adults
# diff
d1 <- df %>% select(x1, a12)
d2 <- df %>% select(x1, n.adults)
diff1 <- nrow(d1[!(d1$x1 %in% d2$x1), ])
diff2 <- nrow(d2[!(d2$x1 %in% d1$x1), ])
# check
if(diff1 == 0 & diff2 == 0) { 
        cat("1\n", file = check, append = TRUE) 
        } else {
               cat("n.adults\n", file = check, append = TRUE)
                }

# numbers of children -----------------------------------------------------

df$`n.children` <- NULL
n.children <- vector("numeric", nrow(df))
for(i in 1:nrow(df)) {
        n.children[i] <- length(which(df[i, l] < 18))
        }
df$`n.children` <- n.children

# numbers of the elder ----------------------------------------------------

df$`n.elder` <- NULL
n.elder <- vector("numeric", nrow(df))
for(i in 1:nrow(df)) {
        n.elder[i] <- length(which(df[i, l] >= 65))
        }
df$`n.elder` <- n.elder
# diff
d1 <- df %>% select(x1, a19)
d2 <- df %>% select(x1, n.elder)
diff1 <- nrow(d1[!(d1$x1 %in% d2$x1), ])
diff2 <- nrow(d2[!(d2$x1 %in% d1$x1), ])
# check
if(diff1 == 0 & diff2 == 0) { 
        cat("1\n", file = check, append = TRUE) 
        } else {
                cat("n.elder\n", file = check, append = TRUE)
                }

# head's var number and sex -----------------------------------------------

l <- grep("^b1_", names(df), value = TRUE)
df$`h.sex` <- NULL
df$`h.num` <- NULL
h.sex <- vector("numeric", nrow(df))
h.num <- vector("numeric", nrow(df))
h.marital <- vector("numeric", nrow(df))
for(i in 1:nrow(df)) {
        n <- names(df[i, l][ , which(df[i, l] == df$itm101[i])]) %>%
                str_split(., "_", simplify = TRUE)
        n <- as.numeric(n[ , 2])
        s <- paste("b3_", n, sep = "")
        m <- paste("b16_", n, sep = "")
        f <- df[[s]][i]
        h.num[i] <- n
        h.sex[i] <- f
        h.marital[i] <- df[[m]][i]
        # h_sex[i] <- df[[s]][i] %>% 
        #         as.character() %>% 
        #         as.numeric()
        }
df$`h.num` <- h.num
df$`h.sex` <- h.sex
df$`h.marital` <- h.marital

# diff
d1 <- df %>% select(x1, a7)
d2 <- df %>% select(x1, h.sex)
diff1 <- nrow(d1[!(d1$x1 %in% d2$x1), ])
diff2 <- nrow(d2[!(d2$x1 %in% d1$x1), ])
# check
if(diff1 == 0 & diff2 == 0) { 
        cat("1\n", file = check, append = TRUE) 
        } else {
                cat("h.sex\n", file = check, append = TRUE)
                }
# # head's ID and marital status --------------------------------------------
# df$`h.marital` <- NULL
# h.id <- vector("numeric", nrow(df))
# h.marital <- vector("numeric", nrow(df))
# l <- grep("^b2_", names(df))
# for(i in 1:nrow(df)) { 
#         # search for head
#         n <- names(df[i, l][ , which(df[i, l] == 1)]) %>% 
#                 str_split(., "_", simplify = TRUE)
#         # heawd's marital status
#         m <- paste("b16_", n[ , 2], sep = "")
#         id <- paste("b1_", n[ , 2], sep = "")
#         h.id[i] <- df[[id]][i]
#         h.marital[i] <- df[[m]][i]
#         }
# df$`h.id` <- h.id
# df$`h.marital` <- h.marital
# table(h.marital)
# table(h.id)
# 

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

# head's grandparents
df$`g.marital1` <- NULL
df$`g.marital2` <- NULL
g.marital1 <- vector("numeric", nrow(df))
g.marital2 <- vector("numeric", nrow(df))
l <- grep("^b2_", names(df))
for(i in 1:nrow(df)) { 
        # search for head (identity: parents)
        n <- names(df[i, l][ , which(df[i, l] == 6)])
        # head had only one of the parents
        if( length(n) == 1 ) { 
                n <- str_split(n, "_", simplify = TRUE)
                n1 <- paste("b16_", n[1 , 2], sep = "")
                g.marital1[i] <- df[[n1]][i]
                } else if ( length(n) == 2) {
                        n <- str_split(n, "_", simplify = TRUE)
                        n2 <- paste("b16_", n[2 , 2], sep = "")
                        g.marital2[i] <- df[[n2]][i]
                        } else { 
                                g.marital1[i] <- NA 
                                g.marital2[i] <- NA 
                                }
        }
df$`g.marital1` <- g.marital1
df$`g.marital2` <- g.marital2

# if they are overlapping
# stem family and both the parent and the grandparent are single, widow or widower
# test <- df %>% filter(!is.na(p.marital) & !is.na(g.marital))


# recode: single person and married couple --------------------------------

df %<>% mutate(sf = case_when(
        # single-person
        n.all == 1 & h.sex == 1 ~ "101", 
        n.all == 1 & h.sex == 2 ~ "102", 
        
        # married-couple
        n.all == 2 & 
                # n.adults == 2 & 
                # couple
                b2_1 %in% c(1, 2) & 
                b2_2 %in% c(1, 2) &
                # 
                b16_1 %in% c(1, 2, 31, 51, 91:93, 97) & 
                b16_2 %in% c(1, 2, 31, 51, 91:93, 97) &
                h.sex == 1 ~ "201", 
        n.all == 2 & 
                # n.adults == 2 & 
                b2_1 %in% c(1, 2) & 
                b2_2 %in% c(1, 2) & 
                # 
                b16_1 %in% c(1, 2, 31, 51, 91:93, 97) & 
                b16_2 %in% c(1, 2, 31, 51, 91:93, 97) &
                h.sex == 2 ~ "202"
                )
        )

# diff 1xx
d1 <- df %>% filter(sf %in% c(101, 102)) %>% select(x1)
d2 <- inc105 %>% filter(a18 %in% c(101, 102)) %>% select(x1)
diff1 <- nrow(d1[!(d1$x1 %in% d2$x1), ])
diff2 <- nrow(d2[!(d2$x1 %in% d1$x1), ])
# check
if(diff1 == 0 & diff2 == 0) { 
        cat("1\n", file = check, append = TRUE) 
        } else {
                cat("1xx\n", file = check, append = TRUE)
                }    
# diff 2xx
d1 <- df %>% filter(sf %in% c(201, 202)) %>% select(matches("^x1"))
d2 <- inc105 %>% filter(a18 %in% c(201, 202)) %>% select(matches("^x1"))
diff1 <- nrow(d1[!(d1$x1 %in% d2$x1), ])
diff2 <- nrow(d2[!(d2$x1 %in% d1$x1), ])
# check
if(diff1 == 0 & diff2 == 0) { 
        cat("1\n", file = check, append = TRUE) 
        } else {
                cat("2xx\n", file = check, append = TRUE)
                }   
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
            sum(df[i, l] %in% c(1, 3), na.rm = TRUE) == df$n.all[i] &
            # all of the children is unmarried
            sum(df[i, l2][which(df[i, l] == 3)] == 91, na.rm = TRUE) == df$n.all[i] - 1 &
            # the head is divorced or widowed
            df$h.marital[i] == 97 &
            # the head is male
            df$h.sex[i] == 1 ) {
                
                df$sf[i] <- "321"
                
                } else if ( 
                            df$n.all[i] >= 2 &
                            sum(df[i, l] %in% c(1, 3), na.rm = TRUE) == df$n.all[i] &
                            sum(df[i, l2][which(df[i, l] == 3)] == 91, na.rm = TRUE) == df$n.all[i] - 1&
                            df$h.marital[i] == 97 &
                            # the head is female
                            df$h.sex[i] == 2 ) { 
                        
                                df$sf[i] <- "322" 
                                
                                }
        }
# diff
d1 <- df %>% filter(sf %in% c(321, 322)) %>% 
        select(matches("^x1|a18|^b2_|^b16_")) 
d2 <- inc105 %>% filter(a18 %in% c(321, 322)) %>%
        select(matches("^x1|a18|^b2_|^b16_")) 
diff1 <- nrow(d1[!(d1$x1 %in% d2$x1), ])
diff2 <- nrow(d2[!(d2$x1 %in% d1$x1), ])
# check
if(diff1 == 0 & diff2 == 0) { 
        cat("1\n", file = check, append = TRUE) 
        } else {
                cat("32x\n", file = check, append = TRUE)
                }     

# the head is 3rd gen
l <- grep("^b2_", names(df))
l2 <- grep("^b16_", names(df))
# sf <- vector("character", nrow(df))
for( i in 1:nrow(df)) {
        # only one of the parents
        if(length(grep("^[5]$", df[i, l])) == 1 &
           # only one of the parents, the head, and siblings
           length(grep("^[1]$|^[5]$|^[7]$", df[i, l])) == n.all[i] & 
           # only unmarried, divorced, or widowed
           length(grep("^[9][1]$|^[9][7]$", df[i, l2])) == n.all[i] &
           # only the parent is divorced or widowed
           sum(df[i, l2] %in% 97, na.rm = TRUE) == 1 & 
           # the head is unmarried
           h.marital[i] == 91 &
           # the head is male
           h.sex[i] == 1 ) {
                
                df$sf[i] <- "331"
                
                } else if ( length(grep("^[5]$", df[i, l])) == 1 &
                            length(grep("^[1]$|^[5]$|^[7]$", df[i, l])) == n.all[i] & 
                            length(grep("^[9][1]$|^[9][7]$", df[i ,l2])) == n.all[i] &
                            sum(df[i, l2] %in% 97, na.rm = TRUE) == 1 & 
                            h.marital[i] == 91 &
                            h.sex[i] == 2) { 
                                
                                df$sf[i] <- "332" 
                                
                                }
        }

# diff
d1 <- df %>% filter(sf %in% c(331, 332)) %>% select(x1)
d2 <- inc105 %>% filter(a18 %in% c(331, 332)) %>% select(x1)
diff1 <- nrow(d1[!(d1$x1 %in% d2$x1), ])
diff2 <- nrow(d2[!(d2$x1 %in% d1$x1), ])
# check
if(diff1 == 0 & diff2 == 0) { 
        cat("1\n", file = check, append = TRUE) 
        } else {
                cat("33x\n", file = check, append = TRUE)
                }   


# recode: core family -----------------------------------------------------

# the head is the 2nd gen
l <- grep("^b2_", names(df))
l2 <- grep("^b16_", names(df))
for( i in 1:nrow(df)) {
        if( length(grep("^[1]$", df[i, l])) == 1 &
            # at least 3 in the household
            df$n.all[i] >= 3 &
            # the head and the head's spouse, the head's children and others
            length(grep("^[1]$|^[2]$|^[3]$|^[6]$|^[7]$|^[8]$|^[1][0]$|^[1][2-4]$", df[i, l])) == n.all[i] & 
            # at least one of the children is unmarried
            sum(df[i, l] %in% 3, na.rm = TRUE) >= 1 & 
            sum(df[i, l2] %in% 91, na.rm = TRUE) >= 1 & 
            # the head is married
            !(h.marital[i] %in% c(91, 92, 97)) &
            # the head is male
            h.sex[i] == 1 ) {
                
                df$sf[i] <- "421"
                
                } else if ( length(grep("^[1]$", df[i, l])) == 1 &
                            df$n.all[i] >= 3 &
                            length(grep("^[1]$|^[2]$|^[3]$|^[6]$|^[7]$|^[8]$|^[1][0]$|^[1][2-4]$", df[i, l])) == n.all[i] & 
                            sum(df[i, l] %in% 3, na.rm = TRUE) >= 1 & 
                            sum(df[i, l2] %in% 91, na.rm = TRUE) >= 1 & 
                            !(h.marital[i] %in% c(91, 92, 97)) &
                            h.sex[i] == 2 ) { 
                                
                                df$sf[i] <- "422" 
                                
                                }
        }

# diff
d1 <- df %>% filter(sf %in% c(421, 422)) %>% select(x1)
d2 <- inc105 %>% filter(a18 %in% c(421, 422)) %>% select(x1)
diff1 <- nrow(d1[!(d1$x1 %in% d2$x1), ])
diff2 <- nrow(d2[!(d2$x1 %in% d1$x1), ])
# check
if(diff1 == 0 & diff2 == 0) { 
        cat("1\n", file = check, append = TRUE) 
        } else {
                cat("42x\n", file = check, append = TRUE)
                }   

# the head is the 3rd gen
l <- grep("^b2_", names(df))
l2 <- grep("^b16_", names(df))
for( i in 1:nrow(df)) {
        if( length(grep("^[5]$", df[i, l])) >= 1 &
            length(grep("^[6]$", df[i, l])) == 0 &
            # at least 3 in the household
            df$n.all[i] >= 3 &
            # parents are married
            sum(df$p.marital1[i] %in% c(91, 92, 97), na.rm = TRUE) == 0 &
            sum(df$p.marital2[i] %in% c(91, 92, 97), na.rm = TRUE) == 0 &
            # at least one of the siblings is unmarried
            sum(df[i, l2] %in% 91, na.rm = TRUE) >= 1 & 
            df$h.marital[i] == 91 &
            # the head is male
            df$h.sex[i] == 1 ) { 
                
                df$sf[i] <- "431"
                
                } else if ( length(grep("^[5]$", df[i, l])) >= 1 &
                            length(grep("^[6]$", df[i, l])) == 0 &
                            df$n.all[i] >= 3 &
                            sum(df$p.marital1[i] %in% c(91, 92, 97), na.rm = TRUE) == 0 &
                            sum(df$p.marital2[i] %in% c(91, 92, 97), na.rm = TRUE) == 0 &
                            sum(df[i, l2] %in% 91, na.rm = TRUE) >= 1 & 
                            df$h.marital[i] == 91 &
                            df$h.sex[i] == 2 ) { 
                                
                                df$sf[i] <- "432" 
                                
                                }
        }

# diff
d1 <- df %>% filter(sf %in% c(431, 432)) %>% select(x1)
d2 <- inc105 %>% filter(a18 %in% c(431, 432)) %>% select(x1)
diff1 <- nrow(d1[!(d1$x1 %in% d2$x1), ])
diff2 <- nrow(d2[!(d2$x1 %in% d1$x1), ])
# check
if(diff1 == 0 & diff2 == 0) { 
        cat("1\n", file = check, append = TRUE) 
        } else {
                cat("43x\n", file = check, append = TRUE)
                }   
# recode: grand-parent familie --------------------------------------------

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
d1 <- df %>% filter(sf %in% c(511, 512)) %>% select(matches("^x1"))
d2 <- inc105 %>% filter(a18 %in% c(511, 512)) %>% select(matches("^x1"))
diff1 <- nrow(d1[!(d1$x1 %in% d2$x1), ])
diff2 <- nrow(d2[!(d2$x1 %in% d1$x1), ])
# check
if(diff1 == 0 & diff2 == 0) { 
        cat("1\n", file = check, append = TRUE) 
        } else {
                cat("51x\n", file = check, append = TRUE)
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

d1 <- df %>% filter(sf %in% c(531, 532)) %>% select(x1)
d2 <- inc105 %>% filter(a18 %in% c(531, 532)) %>% select(x1)
diff1 <- nrow(d1[!(d1$x1 %in% d2$x1), ])
diff2 <- nrow(d2[!(d2$x1 %in% d1$x1), ])
# check
if(diff1 == 0 & diff2 == 0) { 
        cat("1\n", file = check, append = TRUE) 
        } else {
        cat("53x\n", file = check, append = TRUE)
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
# diff 
d1 <- df %>% filter(sf %in% c(611, 612)) %>% select(x1)
d2 <- inc105 %>% filter(a18 %in% c(611, 612)) %>% select(x1)
diff1 <- nrow(d1[!(d1$x1 %in% d2$x1), ])
diff2 <- nrow(d2[!(d2$x1 %in% d1$x1), ])
# check
if(diff1 == 0 & diff2 == 0) { 
        cat("1\n", file = check, append = TRUE) 
        } else {
        cat("61x\n", file = check, append = TRUE)
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
# diff 
d1 <- df %>% filter(sf %in% c(621, 622)) %>% select(x1)
d2 <- inc105 %>% filter(a18 %in% c(621, 622)) %>% select(x1)
diff1 <- nrow(d1[!(d1$x1 %in% d2$x1), ])
diff2 <- nrow(d2[!(d2$x1 %in% d1$x1), ])
# check
if(diff1 == 0 & diff2 == 0) { 
        cat("1\n", file = check, append = TRUE) 
        } else {
        cat("62x\n", file = check, append = TRUE)
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
# diff 
d1 <- df %>% filter(sf %in% c(631, 632)) %>% select(matches("^x1|a18|^b2_|^b16_"))
# d1 <- df[sf %in% c(631, 632), grep("^x1|a18|^b2_|^b16_", names(df))]
d2 <- inc105 %>% filter(a18 %in% c(631, 632)) %>% select(matches("^x1|a18|^b2_|^b16_"))
diff1 <- nrow(d1[!(d1$x1 %in% d2$x1), ])
diff2 <- nrow(d2[!(d2$x1 %in% d1$x1), ])
# check
if(diff1 == 0 & diff2 == 0) { 
        cat("1\n", file = check, append = TRUE) 
        } else {
        cat("63x\n", file = check, append = TRUE)
}

# recode: others ----------------------------------------------------------

df %<>% mutate(sf = case_when(!is.na(sf) ~ sf, 
                              h.sex == 1 ~ "701", 
                              h.sex == 2 ~ "702"))
# diff 
d1 <- df %>% filter(sf %in% c(701, 702)) %>% select(x1)
d2 <- inc105 %>% filter(a18 %in% c(701, 702)) %>% select(x1)
diff1 <- nrow(d1[!(d1$x1 %in% d2$x1), ])
diff2 <- nrow(d2[!(d2$x1 %in% d1$x1), ])
# check
if(diff1 == 0 & diff2 == 0) { 
        cat("1\n", file = check, append = TRUE) 
        } else {
        cat("70x\n", file = check, append = TRUE)
}
read_lines(check)

# recode: structure of families -------------------------------------------

for(i in 1:9) {
        l[[i]] %<>% 
        mutate(str_family = case_when(a18 %in% c(101, 102) ~ 1L, 
                                      a18 %in% c(201, 202) ~ 2L, 
                                      a18 %in% c(321, 322, 331, 332) ~ 3L, 
                                      a18 %in% c(421, 422, 431, 432) ~ 4L, 
                                      a18 %in% c(511, 512, 531, 532) ~ 5L, 
                                      a18 %in% c(611, 612, 621, 622, 631, 632) ~ 6L, 
                                      a18 %in% c(701, 702) ~ 7L, 
                                      TRUE ~ NA_integer_))
        }
# apply labels
d <- apply_labels(d, 
                  str_family = "structure of families", 
                  str_family = c("單人戶" = 1L, 
                                 "夫婦戶" = 2L, 
                                 "廣義單親戶" = 3L, 
                                 "核心家庭" = 4L, 
                                 "祖孫家庭" = 5L, 
                                 "三代家庭" = 6L, 
                                 "其他" = 7L))

# weigh -------------------------------------------------------------------

s <- d$str_family
w <- d$a20
weighed <- s[rep(1:length(s), times = w)]
w1 <- weight(s, w, digits = 0)

# weigh revised -----------------------------------------------------------

s <- d$str_family
w <- d$a20
x <- round(xtabs(w ~ s), digits = 0); x
n <- names(x)
weighed <- mapply(rep, x = n, times = x)
l <- unlist(weighed, use.names = FALSE)
table <- epiDisplay::tab1(l, decimal = 2, 
                          sort.group = "decreasing", 
                          graph = FALSE)
# freq --------------------------------------------------------------------

tab1(weighed, 
     decimal = 2,
     sort.group = "decreasing", 
     # bar.values = "percent", 
     main = "臺灣家庭組織型態（2018）")
tab1(w1, 
     decimal = 2,
     sort.group = "decreasing", 
     # bar.values = "percent", 
     main = "臺灣家庭組織型態（2018）")
# plot --------------------------------------------------------------------

plot_frq(d$str_family, weight.by = d$a20)
