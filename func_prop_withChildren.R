##### proportion of household with children in diffrent structures of families #####


# prep and options --------------------------------------------------------
# list of packages
list.packages <- c("tidyverse")
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
        c <- h[["with_children"]]
        # replicate by weight
        w1 <- c[rep(1:length(c), times = w)]
        out <- c(h$year[1] + 1911L, summarytools::freq(w1)[[6]])
        return(out)
        rm(h)
        gc()
        }