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
        
        ##### weight by age #####
        n_all <- df[[n.all]] 
        n_adult <- df[[n.adult]] 
        
        ##### equivalised income #####
        n <- df[[n.all]]
        df <- df %>% 
                mutate(sqrt_scale = sqrt(n),
                       indi_inc = (itm400 - itm600) / sqrt_scale)
        
        ##### poverty threshold #####
        w <- df[[weight]]
        i <- df[["indi_inc"]]
        # replicate income by weight
        weighed <- i[rep(1:length(i), times = w)]
        # calculate the median and poverty threshold
        t <- median(weighed, na.rm = TRUE) * 0.5
        
        ##### function prop #####
        p.prop <- function(df, w) {
                # weight
                w <- df[[w]]
                i <- df[["indi_inc"]]
                # replicate income by weight
                weighed <- i[rep(1:length(i), times = w)]
                r <- weighed < t
                p <- length(r[r == TRUE]) / length(r) * 100
                return(p)
                }
        
        ##### prop overall households #####
        d <- df
        p.all_house <- p.prop(df = d, w = weight)
        names(p.all_house) <- "Overall household"
        
        ##### prop male headed household #####
        # d <- df[df[[sex]] == 1, ]
        d <- df %>% filter_at(sex, all_vars(. == 1))
        p.m_headed_house <- p.prop(df = d, w = weight)
        names(p.m_headed_house) <- "Male headed households"
        
        ##### prop female headed household #####
        # d <- df[df[[sex]] == 2, ]
        d <- df %>% filter_at(sex, all_vars(. == 2))
        p.f_headed_house <- p.prop(df = d, w = weight)
        names(p.f_headed_house) <- "Female headed households"
        
        ##### prop household with aged #####
        # d <- df[df[[aged]] >= 1, ]
        d <- df %>% filter_at(aged, all_vars(. >= 1))
        p.with_aged <- p.prop(df = d, w = weight)
        names(p.with_aged) <- "Household with aged"
        
        ##### prop household without aged #####
        # d <- df[df[[aged]] < 1, ]
        d <- df %>% filter_at(aged, all_vars(. >= 1))
        p.without_aged <- p.prop(df = d, w = weight)
        names(p.without_aged) <- "Household without aged"
        
        ##### overall single-parent families #####
        # children are under 18: dependent children
        d <- df %>% 
                # having members who are under 18
                filter_at(vars(matches("^b4_")), any_vars(. < 18)) %>% 
                # structure of family: single parent family
                filter_at(type, all_vars(. %in% c(321, 322, 331, 332))) %>% 
                # with at least 1 child
                .[.[[n.all]] - .[[n.adult]] > 0, ]
        # l <- grep("^b4_", names(d))
        # w <- d[ , l] < 18
        # d <- d %>% 
        #         .[unique(which(w, arr.ind = TRUE)[ , 1]), ] %>% 
        #         # structure of families: single parent
        #         .[.[[type]] %in% c(321, 322, 331, 332), ] %>% 
        #         # with children
        #         .[.[[n.all]] - .[[n.adult]] > 0, ]
        p.single_parent <- p.prop(df = d, w = weight)
        names(p.single_parent) <- "Overall single-parent families"
        
        ##### male single-parent households #####
        # children are under 18: dependent children
        d <- df %>% 
                # having members who are under 18
                filter_at(vars(matches("^b4_")), any_vars(. < 18)) %>% 
                # male head
                filter_at(sex, all_vars(. == 1)) %>% 
                # structure of family: single-parent family
                filter_at(type, all_vars(. %in% c(321, 322, 331, 332))) %>%
                # whith at least 1 child
                .[.[[n.all]] - .[[n.adult]] > 0, ]
        # d <- df
        # l <- grep("^b4_", names(d))
        # w <- d[ , l] < 18
        # d <- d %>% 
        #         .[unique(which(w, arr.ind = TRUE)[ , 1]), ] %>% 
        #         # male head 
        #         .[.[[sex]] == 1, ] %>% 
        #         # structure of families: single parent
        #         .[.[[type]] %in% c(321, 322, 331, 332), ] %>% 
        #         # with children
        #         .[.[[n.all]] - .[[n.adult]] > 0, ]
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
        d <- df
        a <- d[["indi_inc"]]
        b <- d[[n.all]]
        c <- d[[weight]]
        # weight by n of people in the house
        x <- a[rep(1:length(a), times = b)]
        y <- c[rep(1:length(c), times = b)]
        # weight by weight
        z <- x[rep(1:length(x), times = y)]
        # below threshold
        r <- z < t
        # calculate the proportion
        p.all_population <- length(r[r == TRUE]) / length(r) * 100
        names(p.all_population) <- "Overall population"
        
        ##### Elderly population #####
        d <- df
        a <- d[["indi_inc"]]
        b <- d[[aged]]
        c <- d[[weight]]
        # weight by n of people in the house
        x <- a[rep(1:length(a), times = b)]
        y <- c[rep(1:length(c), times = b)]
        # weight by weight
        z <- x[rep(1:length(x), times = y)]
        # below threshold
        r <- z < t
        # calculate the proportion
        p.all_elderly <- length(r[r == TRUE]) / length(r) * 100
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