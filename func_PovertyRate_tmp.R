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
        t <- median(w1, na.rm = TRUE) * 0.5
        # DGBAS
        t2 <- median(weighed, na.rm = TRUE) * 0.5
        
        ##### function prop #####
        p.prop <- function(df, w) {
                # weight
                w <- df[[weight]]
                i <- df[["indi_inc"]]
                # replicate income by weight
                weighed <- i[rep(1:length(i), times = w)]
                low1 <- weighed < t
                low2 <- weighed < t2
                p1 <- length(low1[low1 == TRUE]) / length(low1) * 100
                # DGBAS
                p2 <- length(low2[low2 == TRUE]) / length(low2) * 100
                p <- c(p1, p2)
                return(p)
        }
        
        ##### prop overall households #####
        d <- df
        p.all_house <- p.prop(df = d, w = weight)[1]
        p.all_house_DGBAS <- p.prop(df = d, w = weight)[2]
        names(p.all_house) <- "Overall household"
        names(p.all_house_DGBAS) <- "Overall household (DGBAS)"
        
        ##### prop male headed household #####
        d <- df[df[[sex]] == 1, ]
        p.m_headed_house <- p.prop(df = d, w = weight)[1]
        p.m_headed_house_DGBAS <- p.prop(df = d, w = weight)[2]
        names(p.m_headed_house) <- "Male headed households"
        names(p.m_headed_house_DGBAS) <- "Male headed households (DGBAS)"
        
        ##### prop female headed household #####
        d <- df[df[[sex]] == 2, ]
        p.f_headed_house <- p.prop(df = d, w = weight)[1]
        p.f_headed_house_DGBAS <- p.prop(df = d, w = weight)[2]
        names(p.f_headed_house) <- "Female headed households"
        names(p.f_headed_house_DGBAS) <- "Female headed households (DGBAS)"
        
        ##### prop household with aged #####
        d <- df[df[[aged]] >= 1, ]
        p.with_aged <- p.prop(df = d, w = weight)[1]
        p.with_aged_DGBAS <- p.prop(df = d, w = weight)[2]
        names(p.with_aged) <- "Household with aged"
        names(p.with_aged_DGBAS) <- "Household with aged (DGBAS)"
        
        ##### prop household without aged #####
        d <- df[df[[aged]] < 1, ]
        p.without_aged <- p.prop(df = d, w = weight)[1]
        p.without_aged_DGBAS <- p.prop(df = d, w = weight)[2]
        names(p.without_aged) <- "Household without aged"
        names(p.without_aged_DGBAS) <- "Household without aged (DGBAS)"
        
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
        p.single_parent <- p.prop(df = d, w = weight)[1]
        p.single_parent_DGBAS <- p.prop(df = d, w = weight)[2]
        names(p.single_parent) <- "Overall single-parent families"
        names(p.single_parent_DGBAS) <- "Overall single-parent families (DGBAS)"
        
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
        p.m_single_parent <- p.prop(df = d, w = weight)[1]
        p.m_single_parent_DGBAS <- p.prop(df = d, w = weight)[2]
        names(p.m_single_parent) <- "Male single-parent families"
        names(p.m_single_parent_DGBAS) <- "Male single-parent families (DGBAS)"
        
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
        p.f_single_parent <- p.prop(df = d, w = weight)[1]
        p.f_single_parent_DGBAS <- p.prop(df = d, w = weight)[2]
        names(p.f_single_parent) <- "Female single-parent families"
        names(p.f_single_parent_DGBAS) <- "Female single-parent families (DGBAS)"
        
        ##### Overall population #####
        # below threshold
        low <- weighed < t
        low2 <- weighed < t2
        # calculate the proportion
        p.all_population <- length(low[low == TRUE]) / length(low) * 100
        p.all_population_DGBAS <- length(low2[low2 == TRUE]) / length(low2) * 100
        names(p.all_population) <- "Overall population"
        # DGBAS
        names(p.all_population_DGBAS) <- "Overall population (DGBAS)"
        
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
        low2 <- weighed < t2
        # calculate the proportion
        p.all_elderly <- length(low[low == TRUE]) / length(low) * 100
        p.all_elderly_DGBAS <- length(low2[low2 == TRUE]) / length(low2) * 100
        names(p.all_elderly) <- "Elderly population"
        names(p.all_elderly_DGBAS) <- "Elderly population (DGBAS)"
        
        ##### return the results #####
        l <- c(p.all_house, p.all_house_DGBAS,
               p.m_headed_house, p.m_headed_house_DGBAS,
               p.f_headed_house, p.f_headed_house_DGBAS,
               p.with_aged, p.with_aged_DGBAS, 
               p.without_aged, p.without_aged_DGBAS, 
               p.single_parent, p.single_parent_DGBAS, 
               p.m_single_parent, p.m_single_parent_DGBAS, 
               p.f_single_parent, p.f_single_parent_DGBAS, 
               p.all_population, p.all_population_DGBAS, 
               p.all_elderly, p.all_elderly_DGBAS)
        out.table <- data.frame(l, row.names = names(l))
        colnames(out.table) <- year
        return(out.table)
        }