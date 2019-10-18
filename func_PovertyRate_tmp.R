##### author: CAI YUN-TING ######
##### prep and options #####
# list of packages
list.packages <- c("tidyverse", "magrittr", "haven", 
       "gridExtra", "summarytools", "vcd")
# check if the packages are installed
new.packages <- list.packages[!(list.packages %in% installed.packages()[ , "Package"])]
# install new packages
if(length(new.packages)) install.packages(new.packages)
# load the packages
lapply(list.packages, require, character.only = TRUE)
# remove lists
rm(list.packages, new.packages)
# options
options(scipen = 999)
# options for summarytools
st_options(style = "simple",
           round.digits = 4,
           ctable.prop = "c")

##### function -- poverty rate #####
poverty_rate <- function(df, weight, 
                         n.all = "a8", sex = "a7", aged = "a19", 
                         type = "a18", n.adult = "a12", year) {
        
        
        ##### equivalised income #####
        n <- df[[n.all]]
        df <- df %>% 
                mutate(sqrt_scale = sqrt(n),
                       indi_inc = (itm400 - itm600) / sqrt_scale)
        
        ##### poverty threshold #####
        w <- df[[weight]]
        i <- df[["indi_inc"]]
        n <- df[[n.all]]
        # replicate income by weight
        w1 <- i[rep(1:length(i), times = w)]
        w2 <- n[rep(1:length(n), times = w)]
        weighed <- w1[rep(1:length(w1), times = w2)]
        # calculate the median and poverty threshold
        t <- median(weighed, na.rm = TRUE) * 0.5
        
        ##### function prop #####
        p.prop <- function(df, w) {
                # weight
                w <- df[[weight]]
                i <- df[["indi_inc"]]
                # replicate income by weight
                weighed <- i[rep(1:length(i), times = w)]
                low <- weighed < t
                p <- length(low[low == TRUE]) / length(low) * 100
                return(p)
        }
        
        ##### prop overall households #####
        d <- df
        p.all_house <- p.prop(df = d, w = weight)
        names(p.all_house) <- "Overall household"
        
        ##### prop male headed household #####
        d <- df[df[[sex]] == 1, ]
        p.m_headed_house <- p.prop(df = d, w = weight)
        names(p.m_headed_house) <- "Male headed households"
        
        ##### prop female headed household #####
        d <- df[df[[sex]] == 2, ]
        p.f_headed_house <- p.prop(df = d, w = weight)
        names(p.f_headed_house) <- "Female headed households"
        
        ##### prop household with aged #####
        d <- df[df[[aged]] >= 1, ]
        p.with_aged <- p.prop(df = d, w = weight)
        names(p.with_aged) <- "Household with aged"
        
        ##### prop household without aged #####
        d <- df[df[[aged]] < 1, ]
        p.without_aged <- p.prop(df = d, w = weight)
        names(p.without_aged) <- "Household without aged"
        
        ##### overall single-parent families #####
        # children are under 18: dependent children
        d <- df
        l <- grep("^b4_", names(d))
        w <- d[ , l] < 18
        d <- d %>% 
                .[unique(which(w, arr.ind = TRUE)[ , 1]), ] %>% 
                # structure of families: single parent
                .[.[[type]] %in% c(321, 322, 331, 332), ] %>% 
                # with children
                .[.[[n.all]] - .[[n.adult]] > 0, ]
        p.single_parent <- p.prop(df = d, w = weight)
        names(p.single_parent) <- "Overall single-parent families"
        
        ##### male single-parent households #####
        # children are under 18: dependent children
        d <- df
        l <- grep("^b4_", names(d))
        w <- d[ , l] < 18
        d <- d %>% 
                .[unique(which(w, arr.ind = TRUE)[ , 1]), ] %>% 
                # male head 
                .[.[[sex]] == 1, ] %>% 
                # structure of families: single parent
                .[.[[type]] %in% c(321, 322, 331, 332), ] %>% 
                # with children
                .[.[[n.all]] - .[[n.adult]] > 0, ]
        p.m_single_parent <- p.prop(df = d, w = weight)
        names(p.m_single_parent) <- "Male single-parent families"
        
        ##### female single-parent families #####
        # children are under 18
        d <- df
        l <- grep("^b4_", names(d))
        w <- d[ , l] < 18
        d <- d %>% 
                .[unique(which(w, arr.ind = TRUE)[ , 1]), ] %>% 
                # female head 
                .[.[[sex]] == 2, ] %>% 
                # structure of families: single parent
                .[.[[type]] %in% c(321, 322, 331, 332), ] %>% 
                # with children
                .[.[[n.all]] - .[[n.adult]] > 0, ]
        p.f_single_parent <- p.prop(df = d, w = weight)
        names(p.f_single_parent) <- "Female single-parent families"
        
        ##### Overall population #####
        # below threshold
        r <- weighed < t
        # calculate the proportion
        p.all_population <- length(r[r == TRUE]) / length(r) * 100
        names(p.all_population) <- "Overall population"
        
        ##### Elderly population #####
        d <- df
        # a <- d[["indi_inc"]]
        w <- df[[weight]]
        i <- df[["indi_inc"]]
        n_aged <- d[[aged]]
        # b <- d[[aged]]
        # c <- d[[weight]]
        # weigh by n of people in the house
        w1 <- i[rep(1:length(i), times = w)]
        w2 <- n_aged[rep(1:length(n_aged), times = w)]
        weighed <- w1[rep(1:length(w1), times = w2)]
        # below threshold
        low <- weighed < t
        # calculate the proportion
        p.all_elderly <- length(low[low == TRUE]) / length(low) * 100
        names(p.all_elderly) <- "Elderly population"
        
        ##### return the results #####
        l <- c(p.all_house, p.m_headed_house, p.f_headed_house, 
               p.with_aged, p.without_aged, p.single_parent, 
               p.m_single_parent, p.f_single_parent, p.all_population, 
               p.all_elderly)
        out.table <- data.frame(l, row.names = names(l))
        colnames(out.table) <- year
        return(out.table)
        }