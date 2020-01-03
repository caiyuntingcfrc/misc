##### author: CAI YUN-TING ######
##### prep and options #####
##### source function: ins.pak #####
devtools::source_url("https://raw.githubusercontent.com/caiyuntingcfrc/misc/function_poverty/func_ins.pack.R")
ins.pack("tidyverse")
##### function -- poverty rate #####
poverty_rate <- function(df, weight, 
                         n.all = "a8", sex = "a7", aged = "a19", 
                         type = "a18", n.adult = "a12") {
        
        ##### equivalised income #####
        n <- df[[n.all]]
        df <- df %>% 
                mutate(sqrt_scale = sqrt(n),
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
        # deprecated
        # replicate income by weight
        # weighed <- i[rep(1:length(i), times = w)]
        # calculate the median and poverty threshold
        t <- median(weighed, na.rm = TRUE) * 0.5
        
        ##### function prop #####
        p.prop <- function(df, w) {
                # weight table
                wtab <- round(xtabs(df[[w]] ~ df[["eq_inc"]]))
                # replicate income by weight
                i <- names(wtab)
                weighed <- mapply(rep, x = i, times = wtab)
                weighed <- as.numeric(unlist(weighed, use.names = TRUE))
                # deprecated
                # weighed <- i[rep(1:length(i), times = w)]
                r <- weighed < t
                p <- length(r[r == TRUE]) / length(r) * 100
                return(p)
                }
        
        ##### [households] #####
        ##### overall households #####
        d <- df
        p.all_house <- p.prop(df = d, w = weight)
        names(p.all_house) <- "Overall household"
        
        ##### male headed household #####
        # d <- df[df[[sex]] == 1, ]
        d <- df %>% filter_at(sex, all_vars(. == 1))
        p.m_headed_house <- p.prop(df = d, w = weight)
        names(p.m_headed_house) <- "Male headed households"
        
        ##### female headed household #####
        # d <- df[df[[sex]] == 2, ]
        d <- df %>% filter_at(sex, all_vars(. == 2))
        p.f_headed_house <- p.prop(df = d, w = weight)
        names(p.f_headed_house) <- "Female headed households"
        
        ##### household with aged (>=65) #####
        # d <- df[df[[aged]] >= 1, ]
        d <- df %>% filter_at(vars(matches("^b4_")), any_vars(. >= 65))
        p.with_aged <- p.prop(df = d, w = weight)
        names(p.with_aged) <- "Household with aged (>=65)"
        
        ##### household with aged (>=75) #####
        d <- df %>% filter_at(vars(matches("^b4_")), any_vars(. >= 75))
        p.with_aged75 <- p.prop(df = d, w = weight)
        names(p.with_aged75) <- "Household with aged (>=75)"
        
        ##### household with aged (>=85) #####
        d <- df %>% filter_at(vars(matches("^b4_")), any_vars(. >= 85))
        p.with_aged85 <- p.prop(df = d, w = weight)
        names(p.with_aged85) <- "Household with aged (>=85)"
        
        ##### household without aged #####
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
        
        ##### Household with children (<18) #####
        d <- df %>% 
                # having 1 or more children
                filter((a8 - a12) > 0) %>% 
                # less than 18 years old
                filter_at(vars(matches("^b4_")), any_vars(. < 18))
        p.with_children <- p.prop(df = d, w = weight)
        names(p.with_children) <- "Household with children (<18)"
        
        ##### Household with children (<12) #####
        d <- df %>% 
                # having 1 or more children
                filter((a8 - a12) > 0) %>% 
                # less than 12 years old
                filter_at(vars(matches("^b4_")), any_vars(. < 12 & . >= 0))
        p.with_children11 <- p.prop(df = d, w = weight)
        names(p.with_children11) <- "Household with children (<12)"
        
        ##### Household with children (<6) #####
        d <- df %>% 
                # having 1 or more children
                filter((a8 - a12) > 0) %>% 
                # less than 6 years old
                filter_at(vars(matches("^b4_")), any_vars(. < 6 & . >= 0))
        p.with_children5 <- p.prop(df = d, w = weight)
        names(p.with_children5) <- "Household with children (<6)"
        
        
        ##### Overall population #####
        d <- df
        i <- d[["eq_inc"]]
        n <- d[[n.all]]
        w <- d[[weight]]
        # weigh by numbers of people in the household
        w1 <- i[rep(1:length(i), times = n)]
        w2 <- w[rep(1:length(n), times = n)]
        # weighed xtabs
        wtab <- xtabs(w2 ~ w1)
        v <- names(wtab)
        weighed <- mapply(rep, times = wtab, x = v, SIMPLIFY = TRUE)
        weighed <- as.numeric(unlist(weighed, use.names = FALSE))
        # weighed <- w1[rep(1:length(w1), times = w2)]
        # below threshold
        r <- weighed < t
        # calculate the proportion
        p.all_population <- length(r[r == TRUE]) / length(r) * 100
        names(p.all_population) <- "Overall population"
        
        
        ##### Child population (<18) #####
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
        i <- d[["eq_inc"]]
        n <- d[["n.children"]]
        w <- d[[weight]]
        # weigh by numbers of people in the household
        w1 <- i[rep(1:length(i), times = n)]
        w2 <- w[rep(1:length(n), times = n)]
        # weighed xtabs
        wtab <- xtabs(w2 ~ w1)
        v <- names(wtab)
        weighed <- mapply(rep, times = wtab, x = v, SIMPLIFY = TRUE)
        weighed <- as.numeric(unlist(weighed, use.names = FALSE))
        # below threshold
        r <- weighed < t
        # calculate the proportion
        p.all_children <- length(r[r == TRUE]) / length(r) * 100
        names(p.all_children) <- "Child population (<18)"
        
        ##### Child population (<12) #####
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
        i <- d[["eq_inc"]]
        n <- d[["n.children"]]
        w <- d[[weight]]
        # weigh by numbers of people in the household
        w1 <- i[rep(1:length(i), times = n)]
        w2 <- w[rep(1:length(n), times = n)]
        # weighed xtabs
        wtab <- xtabs(w2 ~ w1)
        v <- names(wtab)
        weighed <- mapply(rep, times = wtab, x = v, SIMPLIFY = TRUE)
        weighed <- as.numeric(unlist(weighed, use.names = FALSE))
        # below threshold
        r <- weighed < t
        # calculate the proportion
        p.all_children11 <- length(r[r == TRUE]) / length(r) * 100
        names(p.all_children11) <- "Child population (<12)"
        
        ##### Child population (<6) #####
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
        i <- d[["eq_inc"]]
        n <- d[["n.children"]]
        w <- d[[weight]]
        # weigh by numbers of people in the household
        w1 <- i[rep(1:length(i), times = n)]
        w2 <- w[rep(1:length(n), times = n)]
        # weighed xtabs
        wtab <- xtabs(w2 ~ w1)
        v <- names(wtab)
        weighed <- mapply(rep, times = wtab, x = v, SIMPLIFY = TRUE)
        weighed <- as.numeric(unlist(weighed, use.names = FALSE))
        # below threshold
        r <- weighed < t
        # calculate the proportion
        p.all_children5 <- length(r[r == TRUE]) / length(r) * 100
        names(p.all_children5) <- "Child population (<6)"
        
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
        i <- d[["eq_inc"]]
        n <- d[["n.elder"]]
        w <- d[[weight]]
        # weigh by numbers of people in the household
        w1 <- i[rep(1:length(i), times = n)]
        w2 <- w[rep(1:length(n), times = n)]
        # weighed xtabs
        wtab <- xtabs(w2 ~ w1)
        v <- names(wtab)
        weighed <- mapply(rep, times = wtab, x = v, SIMPLIFY = TRUE)
        weighed <- as.numeric(unlist(weighed, use.names = FALSE))
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
        i <- d[["eq_inc"]]
        n <- d[["n.elder"]]
        w <- d[[weight]]
        # weigh by numbers of people in the household
        w1 <- i[rep(1:length(i), times = n)]
        w2 <- w[rep(1:length(n), times = n)]
        # weighed xtabs
        wtab <- xtabs(w2 ~ w1)
        v <- names(wtab)
        weighed <- mapply(rep, times = wtab, x = v, SIMPLIFY = TRUE)
        weighed <- as.numeric(unlist(weighed, use.names = FALSE))
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
        i <- d[["eq_inc"]]
        n <- d[["n.elder"]]
        w <- d[[weight]]
        # weigh by numbers of people in the household
        w1 <- i[rep(1:length(i), times = n)]
        w2 <- w[rep(1:length(n), times = n)]
        # weighed xtabs
        wtab <- xtabs(w2 ~ w1)
        v <- names(wtab)
        weighed <- mapply(rep, times = wtab, x = v, SIMPLIFY = TRUE)
        weighed <- as.numeric(unlist(weighed, use.names = FALSE))
        # below threshold
        r <- weighed < t
        # calculate the proportion
        p.all_elderly85 <- length(r[r == TRUE]) / length(r) * 100
        names(p.all_elderly85) <- "Elderly population (>=85)"
        
        #### return the results #####
        l <- mget(grep("^p.all|^p.m|^p.f|^p.with|^p.without",
                       ls(),
                       value = TRUE))
        out.table <- do.call(rbind, l) %>%
                data.frame(., row.names = sapply(l, names))
        colnames(out.table) <- df$year[1] + 1911L
        rm(l)
        gc()
        # l <- c(p.all_house, p.m_headed_house, p.f_headed_house,
        #        p.with_aged, p.with_aged75, p.with_aged85,
        #        p.without_aged,
        #        p.with_children, p.with_children5, p.with_children11,
        #        p.single_parent, p.m_single_parent, p.f_single_parent,
        #        p.all_population,
        #        p.all_children, p.all_children5, p.all_children11,
        #        p.all_elderly, p.all_elderly75, p.all_elderly85)
        # out.table <- data.frame(l, row.names = names(l))
        # colnames(out.table) <- year
        # rm(l); gc()
        return(out.table)
        }