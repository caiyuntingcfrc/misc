##### author: CAI YUN-TING ######
##### prep and options #####
# list of packages
list.packages <- c("tidyverse", "magrittr")
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
                       inc = itm400 - itm600, 
                       eq_inc = (itm400 - itm600) / sqrt_scale)
        
        ##### poverty threshold #####
        w <- df[[weight]]
        i <- df[["eq_inc"]]
        # replicate income by weight
        weighed <- i[rep(1:length(i), times = w)]
        # calculate the median and poverty threshold
        t <- median(weighed, na.rm = TRUE) * 0.5
        
        ##### function prop #####
        p.prop <- function(data, 
                           w = weight, 
                           inc = "eq_inc", 
                           threshold = t) {
                # weight
                w <- data[[w]]
                i <- data[[inc]]
                # replicate income by weight
                weighed <- i[rep(1:length(i), times = w)]
                r <- weighed < threshold
                p <- length(r[r == TRUE]) / length(r) * 100
                return(p)
        }
        
        ##### deprecated #####
        # l <- grep("^b4_", names(d))
        # w <- d[ , l] < 18
        # d <- d %>% 
        #         .[unique(which(w, arr.ind = TRUE)[ , 1]), ] %>% 
        #         # structure of families: single parent
        #         .[.[[type]] %in% c(321, 322, 331, 332), ] %>% 
        #         # with children
        #         .[.[[n.all]] - .[[n.adult]] > 0, ]
        
        ##### Household with children #####
        d <- df %>% 
                # having 1 or more children
                filter((a8 - a12) > 0) %>% 
                # less than 18 years old
                filter_at(vars(matches("^b4_")), any_vars(. < 18))
        p.with_children <- p.prop(data = d)
        names(p.with_children) <- "Household with children (<18)"
        
        ##### Household with children 0-5 #####
        d <- df %>% 
                # having 1 or more children
                filter((a8 - a12) > 0) %>% 
                # less than 6 years old
                filter_at(vars(matches("^b4_")), any_vars(. < 6 & . >= 0))
        p.with_children5 <- p.prop(data = d)
        names(p.with_children5) <- "Household with children (<6)"
        
        ##### Household with children 0-11 #####
        d <- df %>% 
                # having 1 or more children
                filter((a8 - a12) > 0) %>% 
                # less than 12 years old
                filter_at(vars(matches("^b4_")), any_vars(. < 12 & . >= 0))
        p.with_children11 <- p.prop(data = d)
        names(p.with_children11) <- "Household with children (<12)"
        
        ##### Household with children by group of numbers #####
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
        # mutate by groups of age
        d <- d %>% 
                mutate(g.children = case_when(n.children > 0 & n.children <= 1 ~ 1,
                                              n.children > 1 & n.children <= 2 ~ 2, 
                                              n.children >= 3 ~ 3)) %>%         
                mutate_at(vars(matches("g.children")), as.factor)
        
        ##### Household with children (<18) having 1 child #####
        d1 <- d %>% 
                filter(g.children == 1)
        
        p.with_children_1c <- p.prop(data = d1)
        names(p.with_children_1c) <- "Household with children (<18) (1 child)"
        ##### Household with children (<18) having 2 children #####
        d2 <- d %>% 
                filter(g.children == 2)
        
        p.with_children_2c <- p.prop(data = d2)
        names(p.with_children_2c) <- "Household with children (<18) (2 children)"
        ##### Household with children (<18) having 3 or more children #####
        d3 <- d %>% 
                filter(g.children == 3)
        
        p.with_children_3c <- p.prop(data = d3)
        names(p.with_children_3c) <- "Household with children (<18) (>3 children)"
        
        ##### Household with children (<12) having 1 child #####
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
        # mutate by groups of age
        d <- d %>% 
                mutate(g.children = case_when(n.children > 0 & n.children <= 1 ~ 1,
                                              n.children > 1 & n.children <= 2 ~ 2, 
                                              n.children >= 3 ~ 3)) %>%         
                mutate_at(vars(matches("g.children")), as.factor)
        
        d1 <- d %>% 
                filter(g.children == 1)
        p.with_children11_1c <- p.prop(data = d1)
        names(p.with_children11_1c) <- "Household with children (<12) (1 child)"
        
        ##### Household with children (<12) having 2 children #####
        d2 <- d %>% 
                filter(g.children == 2)
        p.with_children11_2c <- p.prop(data = d2)
        names(p.with_children11_2c) <- "Household with children (<12) (2 children)"
        
        ##### Household with children (<12) having 3 or more children #####
        d3 <- d %>% 
                filter(g.children == 3)
        p.with_children11_3c <- p.prop(data = d3)
        names(p.with_children11_3c) <- "Household with children (<12) (>3 children)"
        
        ##### Household with children (<6) having 1 child #####
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
        # mutate by groups of age
        d <- d %>% 
                mutate(g.children = case_when(n.children > 0 & n.children <= 1 ~ 1,
                                              n.children > 1 & n.children <= 2 ~ 2, 
                                              n.children >= 3 ~ 3)) %>%         
                mutate_at(vars(matches("g.children")), as.factor)
        
        d1 <- d %>% 
                filter(g.children == 1)
        p.with_children5_1c <- p.prop(data = d1)
        names(p.with_children5_1c) <- "Household with children (<6) (1 child)"
        
        ##### Household with children (<6) having 2 children #####
        d2 <- d %>% 
                filter(g.children == 2)
        p.with_children5_2c <- p.prop(data = d2)
        names(p.with_children5_2c) <- "Household with children (<6) (2 children)"
        
        ##### Household with children (<6) having 3 or more children #####
        d3 <- d %>% 
                filter(g.children == 3)
        p.with_children5_3c <- p.prop(data = d3)
        names(p.with_children5_3c) <- "Household with children (<6) (>3 children)"
        ##### return the results #####
        l <- c(p.with_children, p.with_children11, p.with_children5,
               p.with_children_1c, p.with_children_2c, p.with_children_3c, 
               p.with_children11_1c, p.with_children11_2c, p.with_children11_3c, 
               p.with_children5_1c, p.with_children5_2c, p.with_children5_3c)
        out.table <- data.frame(l, row.names = names(l))
        # l <- mget(grep("^p.with", ls(), value = TRUE))
        # out.table <- do.call(rbind, l) %>% 
        #         data.frame(., row.names = sapply(l, names))
        colnames(out.table) <- year
        return(out.table)
}