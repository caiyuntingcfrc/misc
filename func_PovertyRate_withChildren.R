##### author: CAI YUN-TING ######
##### prep and options #####
# options
devtools::source_url("https://raw.githubusercontent.com/caiyuntingcfrc/misc/function_poverty/func_ins.pack.R")
ins.pack("tidyverse")

##### function -- poverty rate #####
poverty_rate_children <- function(df, weight, 
                                  n.all = "a8", sex = "a7", aged = "a19", 
                                  type = "a18", n.adult = "a12") {

        ##### equivalised income #####
        n <- df[[n.all]]
        df <- df %>% 
                mutate(sqrt_scale = sqrt(n),
                       inc = itm400 - itm600, 
                       eq_inc = (itm400 - itm600) / sqrt_scale)
        
        ##### check if the weight is numeric #####
        if(!is.numeric(df[[weight]])) {
                df[[weight]] <- as.numeric(df[[weight]])
        }
        
        ##### poverty threshold #####
        # weight table
        wtab <- round(xtabs(df[[weight]] ~ df[["eq_inc"]]))
        # replicate income by weight
        i <- names(wtab)
        weighed <- mapply(rep, x = i, times = wtab)
        weighed <- as.numeric(unlist(weighed, use.names = TRUE))
        # replicate income by weight
        # weighed <- i[rep(1:length(i), times = w)]
        # calculate the median and poverty threshold
        t <- median(weighed, na.rm = TRUE) * 0.5
        
        ##### function prop #####
        p.prop <- function(data, 
                           w = weight, 
                           inc = "eq_inc", 
                           threshold = t) {
                # weight table
                wtab <- round(xtabs(data[[w]] ~ data[[inc]]))
                # replicate income by weight
                i <- names(wtab)
                weighed <- mapply(rep, x = i, times = wtab)
                weighed <- as.numeric(unlist(weighed, use.names = TRUE))
                # weighed <- i[rep(1:length(i), times = w)]
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
                # less than 18 years old
                filter_at(vars(matches("^b4_")), any_vars(. < 18))
        p.with_children <- p.prop(data = d)
        names(p.with_children) <- "Household with children (<18)"

        ##### Household with children <12 #####
        d <- df %>% 
                # less than 12 years old
                filter_at(vars(matches("^b4_")), any_vars(. < 12 & . >= 0))
        p.with_children11 <- p.prop(data = d)
        names(p.with_children11) <- "Household with children (<12)"
        
        ##### Household with children <6 #####
        d <- df %>% 
                # less than 6 years old
                filter_at(vars(matches("^b4_")), any_vars(. < 6 & . >= 0))
        p.with_children5 <- p.prop(data = d)
        names(p.with_children5) <- "Household with children (<6)"
        
        
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
                                              n.children >= 2 ~ 2)) %>%         
                mutate_at(vars(matches("g.children")), as.factor)
        
        ##### Household with children (<18) having 1 child #####
        d1 <- d %>% 
                filter(g.children == 1)
        
        p.with_children_1c <- p.prop(data = d1)
        names(p.with_children_1c) <- "Household with children (<18) (1 child)"
        ##### Household with children (<18) having >= 2 children #####
        d2 <- d %>% 
                filter(g.children == 2)
        
        p.with_children_2c <- p.prop(data = d2)
        names(p.with_children_2c) <- "Household with children (<18) (>=2 children)"
        
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
                                              n.children >= 2 ~ 2)) %>%         
                mutate_at(vars(matches("g.children")), as.factor)
        
        d1 <- d %>% 
                filter(g.children == 1)
        p.with_children11_1c <- p.prop(data = d1)
        names(p.with_children11_1c) <- "Household with children (<12) (1 child)"
        
        ##### Household with children (<12) having >= 2 children #####
        d2 <- d %>% 
                filter(g.children == 2)
        p.with_children11_2c <- p.prop(data = d2)
        names(p.with_children11_2c) <- "Household with children (<12) (>=2 children)"
        
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
                                              n.children >= 2 ~ 2)) %>%         
                mutate_at(vars(matches("g.children")), as.factor)
        
        d1 <- d %>% 
                filter(g.children == 1)
        p.with_children5_1c <- p.prop(data = d1)
        names(p.with_children5_1c) <- "Household with children (<6) (1 child)"
        
        ##### Household with children (<6) having >= 2 children #####
        d2 <- d %>% 
                filter(g.children == 2)
        p.with_children5_2c <- p.prop(data = d2)
        names(p.with_children5_2c) <- "Household with children (<6) (>=2 children)"
        
        ##### Core family with children #####
        d <- df %>% 
                # core family
                filter_at(type, all_vars(. %in% c(421, 422, 431, 432))) %>%
                # less than 18 years old
                filter_at(vars(matches("^b4_")), any_vars(. < 18))
        core.with_children <- p.prop(data = d)
        names(core.with_children) <- "Core family with children (<18)"

        ##### Core family with children <12 #####
        d <- df %>% 
                # core family
                filter_at(type, all_vars(. %in% c(421, 422, 431, 432))) %>%
                # less than 12 years old
                filter_at(vars(matches("^b4_")), any_vars(. < 12 & . >= 0))
        core.with_children11 <- p.prop(data = d)
        names(core.with_children11) <- "Core family with children (<12)"
        
        ##### Core family with children <6 #####
        d <- df %>% 
                # core family
                filter_at(type, all_vars(. %in% c(421, 422, 431, 432))) %>%
                # less than 6 years old
                filter_at(vars(matches("^b4_")), any_vars(. < 6 & . >= 0))
        core.with_children5 <- p.prop(data = d)
        names(core.with_children5) <- "Core Family with children (<6)"
        
        ##### Core family with children by group of numbers #####
        d <- df %>% 
                # core family
                filter_at(type, all_vars(. %in% c(421, 422, 431, 432))) %>%
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
                                              n.children >= 2 ~ 2)) %>%         
                mutate_at(vars(matches("g.children")), as.factor)
        
        ##### Core family with children (<18) having 1 child #####
        d1 <- d %>% 
                filter(g.children == 1)
        
        core.with_children_1c <- p.prop(data = d1)
        names(core.with_children_1c) <- "Core family with children (<18) (1 child)"
        ##### Core family with children (<18) having >= 2 children #####
        d2 <- d %>% 
                filter(g.children == 2)
        
        core.with_children_2c <- p.prop(data = d2)
        names(core.with_children_2c) <- "Core family with children (<18) (>=2 children)"
        
        ##### Core family with children (<12) having 1 child #####
        d <- df %>% 
                # core family
                filter_at(type, all_vars(. %in% c(421, 422, 431, 432))) %>%
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
                                              n.children >= 2 ~ 2)) %>%         
                mutate_at(vars(matches("g.children")), as.factor)
        
        d1 <- d %>% 
                filter(g.children == 1)
        core.with_children11_1c <- p.prop(data = d1)
        names(core.with_children11_1c) <- "Core family with children (<12) (1 child)"
        
        ##### Core family with children (<12) having >= 2 children #####
        d2 <- d %>% 
                filter(g.children == 2)
        core.with_children11_2c <- p.prop(data = d2)
        names(core.with_children11_2c) <- "Core family with children (<12) (>=2 children)"
        
        ##### Core family with children (<6) having 1 child #####
        d <- df %>% 
                # core family
                filter_at(type, all_vars(. %in% c(421, 422, 431, 432))) %>%
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
                                              n.children >= 2 ~ 2)) %>%         
                mutate_at(vars(matches("g.children")), as.factor)
        
        d1 <- d %>% 
                filter(g.children == 1)
        core.with_children5_1c <- p.prop(data = d1)
        names(core.with_children5_1c) <- "Core family with children (<6) (1 child)"
        
        ##### Core family with children (<6) having >= 2 children #####
        d2 <- d %>% 
                filter(g.children == 2)
        core.with_children5_2c <- p.prop(data = d2)
        names(core.with_children5_2c) <- "Core family with children (<6) (>=2 children)"
        
        ##### stem family with children #####
        d <- df %>% 
                # stem family
                filter_at(type, all_vars(. %in% c(611, 612, 
                                                  621, 622, 
                                                  631, 632))) %>%
                # less than 18 years old
                filter_at(vars(matches("^b4_")), any_vars(. < 18))
        stem.with_children <- p.prop(data = d)
        names(stem.with_children) <- "stem family with children (<18)"

        ##### stem family with children <12 #####
        d <- df %>% 
                # stem family
                filter_at(type, all_vars(. %in% c(611, 612, 
                                                  621, 622, 
                                                  631, 632))) %>%
                # less than 12 years old
                filter_at(vars(matches("^b4_")), any_vars(. < 12 & . >= 0))
        stem.with_children11 <- p.prop(data = d)
        names(stem.with_children11) <- "stem family with children (<12)"
        
        ##### stem family with children <6 #####
        d <- df %>% 
                # stem family
                filter_at(type, all_vars(. %in% c(611, 612, 
                                                  621, 622, 
                                                  631, 632))) %>%
                # less than 6 years old
                filter_at(vars(matches("^b4_")), any_vars(. < 6 & . >= 0))
        stem.with_children5 <- p.prop(data = d)
        names(stem.with_children5) <- "stem Family with children (<6)"
        
        ##### Stem family with children by group of numbers #####
        d <- df %>% 
                # stem family
                filter_at(type, all_vars(. %in% c(611, 612, 
                                                  621, 622, 
                                                  631, 632))) %>%
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
                                              n.children >= 2 ~ 2)) %>%         
                mutate_at(vars(matches("g.children")), as.factor)
        
        ##### Stem family with children (<18) having 1 child #####
        d1 <- d %>% 
                filter(g.children == 1)
        
        stem.with_children_1c <- p.prop(data = d1)
        names(stem.with_children_1c) <- "Stem family with children (<18) (1 child)"
        ##### Stem family with children (<18) having >= 2 children #####
        d2 <- d %>% 
                filter(g.children == 2)
        
        stem.with_children_2c <- p.prop(data = d2)
        names(stem.with_children_2c) <- "Stem family with children (<18) (>=2 children)"
        
        ##### Stem family with children (<12) having 1 child #####
        d <- df %>% 
                # stem family
                filter_at(type, all_vars(. %in% c(611, 612, 
                                                  621, 622, 
                                                  631, 632))) %>%
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
                                              n.children >= 2 ~ 2)) %>%         
                mutate_at(vars(matches("g.children")), as.factor)
        
        d1 <- d %>% 
                filter(g.children == 1)
        stem.with_children11_1c <- p.prop(data = d1)
        names(stem.with_children11_1c) <- "Stem family with children (<12) (1 child)"
        
        ##### Stem family with children (<12) having >= 2 children #####
        d2 <- d %>% 
                filter(g.children == 2)
        stem.with_children11_2c <- p.prop(data = d2)
        names(stem.with_children11_2c) <- "Stem family with children (<12) (>=2 children)"
        
        ##### Stem family with children (<6) having 1 child #####
        d <- df %>% 
                # stem family
                filter_at(type, all_vars(. %in% c(611, 612, 
                                                  621, 622, 
                                                  631, 632))) %>%
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
                                              n.children >= 2 ~ 2)) %>%         
                mutate_at(vars(matches("g.children")), as.factor)
        
        d1 <- d %>% 
                filter(g.children == 1)
        stem.with_children5_1c <- p.prop(data = d1)
        names(stem.with_children5_1c) <- "Stem family with children (<6) (1 child)"
        
        ##### Stem family with children (<6) having >= 2 children #####
        d2 <- d %>% 
                filter(g.children == 2)
        stem.with_children5_2c <- p.prop(data = d2)
        names(stem.with_children5_2c) <- "Stem family with children (<6) (>=2 children)"
        
        ##### return the results #####
        # deprecated
        # l <- c(p.with_children, p.with_children11, p.with_children5,
        #        p.with_children_1c, p.with_children_2c, 
        #        p.with_children11_1c, p.with_children11_2c, 
        #        p.with_children5_1c, p.with_children5_2c, 
        #        core.with_children, core.with_children11, core.with_children5, 
        #        core.with_children_1c, core.with_children_2c, 
        #        core.with_children11_1c, core.with_children11_2c, 
        #        core.with_children5_1c, core.with_children5_2c)
        # out.table <- data.frame(l, row.names = names(l))
        l <- mget(grep("^p.with|^core.with|^stem.with", ls(), value = TRUE))
        out.table <- do.call(rbind, l) %>%
                data.frame(., row.names = sapply(l, names))
        colnames(out.table) <- df$year[1] + 1911L
        return(out.table)
        }