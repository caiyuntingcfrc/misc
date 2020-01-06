##### proportion of household with elder in diffrent structures of families #####

# prep and options --------------------------------------------------------
# options
options(scipen = 999)
devtools::source_url("https://raw.githubusercontent.com/caiyuntingcfrc/misc/function_poverty/func_ins.pack.R")
ins.pack("tidyverse", "summarytools")

# funtion--proportion -----------------------------------------------------

prop.e <- function(df, weight, age) {
        ##### household with elder #####
        h <- df
        # grep
        l <- grep("^b4_", names(h))
        # nmbers of elder
        h$`n.elder` <- NA
        for(i in 1:nrow(h)) {
                h$`n.elder`[i] <- length(which(h[i, l] >= age))
                }
        # mutate: by the numbers of elder
        h <- h %>% 
                mutate(g.elder = case_when(n.elder > 0 & n.elder <= 1 ~ 1,
                                              n.elder >= 2 ~ 2
                )) %>%         
                mutate_at(vars(matches("g.elder")), as.factor)
        # mutate: with or without elder
        h <- h %>% 
                mutate(with_elder = case_when(n.elder >= 1  ~ 1,
                                                 n.elder < 1 ~ 0
                )) %>%         
                mutate_at(vars(matches("with_elder")), as.factor)

        # weigh
        w <- h[[weight]]
        # check if numeric
        if(!is.numeric(w)) { w <- as.numeric(w) }
        c <- h[["with_elder"]]
        # replicate by weight
        wtab <- round(xtabs(w ~ c), 0)
        i <- names(wtab)
        weighed <- mapply(rep, x = i, times = wtab, SIMPLIFY = TRUE)
        weighed <- as.numeric(unlist(weighed, use.names = TRUE))
        # w1 <- c[rep(1:length(c), times = w)]
        out <- c(h$year[1] + 1911L, summarytools::freq(weighed)[[6]])
        return(out)
        rm(h)
        gc()
        }