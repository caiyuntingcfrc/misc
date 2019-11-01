
# prep and options --------------------------------------------------------
# rm
rm(list = ls())

# setwd
setwd("~/R_wd/")

# list of packages
list.packages <- c("tidyverse", "magrittr", "ggplot2", "hablar")
# check if the packages are installed
new.packages <- list.packages[!(list.packages %in% 
                                        installed.packages()[ , "Package"])]
# install new packages
if(length(new.packages)){ install.packages(new.packages) }
# load the packages
lapply(list.packages, require, character.only = TRUE)
# remove lists
rm(list.packages, new.packages)

# load file
# load("tw_inc/R data files/povertyRatesTW.RData")
load("tw_inc/R data files/poverty.rate.tw_DGBAS.RData")

l <- grep("DGBAS", rownames(poverty.rate.tw))
df0 <- poverty.rate.tw[-l, ] %>% 
        t() %>% 
        as_tibble()
df <- poverty.rate.tw[l, ] %>% 
        t() %>% 
        as_tibble()
colnames(df) <- colnames(df0)
df <- df0 %>% 
        mutate(year = 2000:2018)
# save(df, file = "tw_inc/R data files/poverty.rate.tw.RData")
# save(df, file = "tw_inc/R data files/poverty.rate.tw.DGBAS.RData")
# plots -------------------------------------------------------------------
df <- gather(df, 
            key = "type of poverty rate", 
            value = "poverty rates", -year)
save(df, file = "shiny/demo_dashboard/poverty.demo.RData")
df <- df %>% convert(int(year))
d <- df %>% 
        filter(year %in% 2000:2008) 
        
p <- ggplot(data = d, aes(x = `year`, y = `poverty rates`)) +
        geom_line(aes(color = `type of poverty rate`)) +
        scale_x_continuous(breaks = 2000:2008) +
        theme_classic()
p
