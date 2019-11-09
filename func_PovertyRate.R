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
        
        ##### prop household with aged (>=65) #####
        # d <- df[df[[aged]] >= 1, ]
        d <- df %>% filter_at(aged, all_vars(. >= 1))
        p.with_aged <- p.prop(df = d, w = weight)
        names(p.with_aged) <- "Household with aged (>=65)"
        
        ##### prop household with aged (>=75) #####
        d <- df %>% filter_at(vars(matches("^b4_")), any_vars(. >= 75))
        p.with_aged75 <- p.prop(df = d, w = weight)
        names(p.with_aged75) <- "Household with aged (>=75)"
        
        ##### prop household with aged (>=85) #####
        d <- df %>% filter_at(vars(matches("^b4_")), any_vars(. >= 85))
        p.with_aged85 <- p.prop(df = d, w = weight)
        names(p.with_aged85) <- "Household with aged (>=85)"
        
        ##### prop household without aged #####
        # d <- df[df[[aged]] < 1, ]
        d <- df %>% filter_at(aged, all_vars(. < 1))
        p.without_aged <- p.prop(df = d, w = weight)
        names(p.without_aged) <- "Household without aged"
        
        ##### overall single-parent families #####
        # children are under 18: dependent children
        d <- df %>% 
                # whith at least 1 child
                # filter((a8 - a12) > 0) %>% 
                # having members who are under 18
                filter_at(vars(matches("^b4_")), any_vars(. < 18)) %>% 
                # structure of family: single parent family
                filter_at(type, all_vars(. %in% c(321, 322, 331, 332)))
        ##### deprecated #####
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
                # whith at least 1 child
                # filter((a8 - a12) > 0) %>% 
                # having members who are under 18
                filter_at(vars(matches("^b4_")), any_vars(. < 18)) %>% 
                # male head
                filter_at(sex, all_vars(. == 1)) %>% 
                # structure of family: single-parent family
                filter_at(type, all_vars(. %in% c(321, 322, 331, 332)))

        p.m_single_parent <- p.prop(df = d, w = weight)
        names(p.m_single_parent) <- "Male single-parent families"
        
        ##### female single-parent families #####
        # children are under 18
        d <- df %>% 
                # whith at least 1 child
                # filter((a8 - a12) > 0) %>% 
                # having members who are under 18
                filter_at(vars(matches("^b4_")), any_vars(. < 18)) %>% 
                # male head
                filter_at(sex, all_vars(. == 2)) %>% 
                # structure of family: single-parent family
                filter_at(type, all_vars(. %in% c(321, 322, 331, 332)))
        p.f_single_parent <- p.prop(df = d, w = weight)
        names(p.f_single_parent) <- "Female single-parent families"
        
        ##### Household with children #####
        d <- df %>% 
                # having 1 or more children
                filter((a8 - a12) > 0) %>% 
                # less than 18 years old
                filter_at(vars(matches("^b4_")), any_vars(. < 18))
        p.with_children <- p.prop(df = d, w = weight)
        names(p.with_children) <- "Household with children (0-17)"
        
        ##### Household with children 0-5 #####
        d <- df %>% 
                # having 1 or more children
                filter((a8 - a12) > 0) %>% 
                # less than 6 years old
                filter_at(vars(matches("^b4_")), any_vars(. < 6 & . >= 0))
        p.with_children5 <- p.prop(df = d, w = weight)
        names(p.with_children5) <- "Household with children (0-5)"
        
        ##### Household with children 0-11 #####
        d <- df %>% 
                # having 1 or more children
                filter((a8 - a12) > 0) %>% 
                # less than 12 years old
                filter_at(vars(matches("^b4_")), any_vars(. < 12 & . >= 0))
        p.with_children11 <- p.prop(df = d, w = weight)
        names(p.with_children11) <- "Household with children (0-11)"
        
        ##### Overall population #####
        d <- df
        i <- d[["indi_inc"]]
        n <- d[[n.all]]
        w <- d[[weight]]
        # weigh by weight ("a21")
        w1 <- i[rep(1:length(i), times = w)]
        w2 <- n[rep(1:length(n), times = w)]
        # weigh by numbers of people in the house ("a8")
        weighed <- w1[rep(1:length(w1), times = w2)]
        # below threshold
        r <- weighed < t
        # calculate the proportion
        p.all_population <- length(r[r == TRUE]) / length(r) * 100
        names(p.all_population) <- "Overall population"
        
        
        ##### Child population #####
        d <- df %>% 
                # having 1 or more children
                filter_at(vars(matches("^b4_")), any_vars(. < 18 & . >= 0))
        
        # grep
        l <- grep("^b4_", names(d))
        # nmbers of children
        d$`n.children` <- NA
        for(i in 1:nrow(d)) {
                d$`n.children`[i] <- length(which(d[i, l] < 18 & d[i, l] >= 0))
                }
        # weigh by weight
        i <- d[["indi_inc"]]
        n <- d[["n.children"]]
        w <- d[[weight]]
        w1 <- i[rep(1:length(i), times = w)]
        w2 <- n[rep(1:length(n), times = w)]
        # weigh by numbers of children in the house
        weighed <- w1[rep(1:length(w1), times = w2)]
        # below threshold
        r <- weighed < t
        # calculate the proportion
        p.all_children <- length(r[r == TRUE]) / length(r) * 100
        names(p.all_children) <- "Child population (0-17)"
        
        ##### Child population (0-5) #####
        d <- df %>% 
                # having 1 or more children
                filter_at(vars(matches("^b4_")), any_vars(. < 6 & . >= 0))
        
        # grep
        l <- grep("^b4_", names(d))
        # nmbers of children
        d$`n.children` <- NA
        for(i in 1:nrow(d)) {
                d$`n.children`[i] <- length(which(d[i, l] < 6 & d[i, l] >= 0))
                }
        # weigh by weight
        i <- d[["indi_inc"]]
        n <- d[["n.children"]]
        w <- d[[weight]]
        w1 <- i[rep(1:length(i), times = w)]
        w2 <- n[rep(1:length(n), times = w)]
        # weigh by numbers of children in the house
        weighed <- w1[rep(1:length(w1), times = w2)]
        # below threshold
        r <- weighed < t
        # calculate the proportion
        p.all_children5 <- length(r[r == TRUE]) / length(r) * 100
        names(p.all_children5) <- "Child population (0-5)"
        
        ##### Child population (0-11) #####
        d <- df %>% 
                # having 1 or more children
                filter_at(vars(matches("^b4_")), any_vars(. < 12 & . >= 0))
        
        # grep
        l <- grep("^b4_", names(d))
        # nmbers of children
        d$`n.children` <- NA
        for(i in 1:nrow(d)) {
                d$`n.children`[i] <- length(which(d[i, l] < 12 & d[i, l] >= 0))
                }
        # weigh by weight
        i <- d[["indi_inc"]]
        n <- d[["n.children"]]
        w <- d[[weight]]
        w1 <- i[rep(1:length(i), times = w)]
        w2 <- n[rep(1:length(n), times = w)]
        # weigh by numbers of children in the house
        weighed <- w1[rep(1:length(w1), times = w2)]
        # below threshold
        r <- weighed < t
        # calculate the proportion
        p.all_children11 <- length(r[r == TRUE]) / length(r) * 100
        names(p.all_children11) <- "Child population (0-11)"
        
        ##### Elderly population (>=65) #####
        d <- df %>% 
                # having 1 or more children
                filter_at(vars(matches("^b4_")), any_vars(. >= 65))
        # grep
        l <- grep("^b4_", names(d))
        # nmbers of children
        d$`n.elder` <- NA
        for(i in 1:nrow(d)) {
                d$`n.elder`[i] <- length(which(d[i, l] >= 65))
                }
        i <- d[["indi_inc"]]
        n <- d[["n.elder"]]
        w <- d[[weight]]
        w1 <- i[rep(1:length(i), times = w)]
        w2 <- n[rep(1:length(n), times = w)]
        # weigh by numbers of aged in the house
        weighed <- w1[rep(1:length(w1), times = w2)]
        # below threshold
        r <- weighed < t
        # calculate the proportion
        p.all_elderly <- length(r[r == TRUE]) / length(r) * 100
        names(p.all_elderly) <- "Elderly population (>=65)"
        
        ##### Elderly population (>=75) #####
        d <- df %>% 
                # having 1 or more children
                filter_at(vars(matches("^b4_")), any_vars(. >= 75))
        # grep
        l <- grep("^b4_", names(d))
        # nmbers of children
        d$`n.elder` <- NA
        for(i in 1:nrow(d)) {
                d$`n.elder`[i] <- length(which(d[i, l] >= 75))
        }
        i <- d[["indi_inc"]]
        n <- d[["n.elder"]]
        w <- d[[weight]]
        w1 <- i[rep(1:length(i), times = w)]
        w2 <- n[rep(1:length(n), times = w)]
        # weigh by numbers of aged in the house
        weighed <- w1[rep(1:length(w1), times = w2)]
        # below threshold
        r <- weighed < t
        # calculate the proportion
        p.all_elderly75 <- length(r[r == TRUE]) / length(r) * 100
        names(p.all_elderly75) <- "Elderly population (>=75)"
        
        ##### Elderly population (>=85) #####
        d <- df %>% 
                # having 1 or more children
                filter_at(vars(matches("^b4_")), any_vars(. >= 85))
        # grep
        l <- grep("^b4_", names(d))
        # nmbers of children
        d$`n.elder` <- NA
        for(i in 1:nrow(d)) {
                d$`n.elder`[i] <- length(which(d[i, l] >= 85))
        }
        i <- d[["indi_inc"]]
        n <- d[["n.elder"]]
        w <- d[[weight]]
        w1 <- i[rep(1:length(i), times = w)]
        w2 <- n[rep(1:length(n), times = w)]
        # weigh by numbers of aged in the house
        weighed <- w1[rep(1:length(w1), times = w2)]
        # below threshold
        r <- weighed < t
        # calculate the proportion
        p.all_elderly85 <- length(r[r == TRUE]) / length(r) * 100
        names(p.all_elderly85) <- "Elderly population (>=85)"
        
        ##### return the results #####
        l <- c(p.all_house, p.m_headed_house, p.f_headed_house, 
               p.with_aged, p.with_aged75, p.with_aged85, 
               p.without_aged, 
               p.with_children, p.with_children5, p.with_children11,
               p.single_parent, p.m_single_parent, p.f_single_parent, 
               p.all_population, 
               p.all_children, p.all_children5, p.all_children11,
               p.all_elderly, p.all_elderly75, p.all_elderly85)
        out.table <- data.frame(l, row.names = names(l))
        colnames(out.table) <- year
        return(out.table)
        }