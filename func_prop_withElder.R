##### proportion of household with elder in diffrent structures of families #####


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
        c <- h[["with_elder"]]
        # replicate by weight
        w1 <- c[rep(1:length(c), times = w)]
        out <- c(h$year[1] + 1911L, summarytools::freq(w1)[[6]])
        return(out)
        rm(h)
        gc()
        }