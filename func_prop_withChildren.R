##### proportion of household with children in diffrent structures of families #####

# prep and options --------------------------------------------------------
# options
options(scipen = 999)
devtools::source_url("https://raw.githubusercontent.com/caiyuntingcfrc/misc/function_poverty/func_ins.pack.R")
ins.pack("tidyverse", "summarytools")

# funtion--proportion -----------------------------------------------------

prop.f <- function(df, weight, age) {
        ##### household with children #####
        h <- df
        # grep
        l <- grep("^b4_", names(h))
        # nmbers of children
        h$`n.children` <- NA
        for(i in 1:nrow(h)) {
                h$`n.children`[i] <- length(which(h[i, l] < age & h[i, l] >= 0))
                }
        # mutate: by the numbers of children
        h <- h %>% 
                mutate(g.children = case_when(n.children > 0 & n.children <= 1 ~ 1,
                                              n.children >= 2 ~ 2
                )) %>%         
                mutate_at(vars(matches("g.children")), as.factor)
        # mutate: with or without children
        h <- h %>% 
                mutate(with_children = case_when(n.children >= 1  ~ 1,
                                                 n.children < 1 ~ 0
                )) %>%         
                mutate_at(vars(matches("with_children")), as.factor)

        # weigh
        w <- h[[weight]]
        # check if numeric
        if(!is.numeric(w)) { w <- as.numeric(w) }
        c <- h[["with_children"]]
        # replicate by weight
        wtab <- xtabs(w ~ c)
        i <- names(wtab)
        weighed <- mapply(rep, x = i, times = wtab, SIMPLIFY = TRUE)
        weighed <- as.numeric(unlist(weighed, use.names = TRUE))
        # w1 <- c[rep(1:length(c), times = w)]
        out <- c(h$year[1] + 1911L, summarytools::freq(weighed)[[6]])
        return(out)
        rm(h)
        gc()
        }