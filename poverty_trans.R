
# prep and options --------------------------------------------------------

# list of packages
list.packages <- c("tidyverse", "magrittr")
# check if the packages are installed
new.packages <- list.packages[!(list.packages %in% 
                                        installed.packages()[ , "Package"])]
# install new packages
if(length(new.packages)){ install.packages(new.packages) }
# load the packages
lapply(list.packages, require, character.only = TRUE)
# remove lists
rm(list.packages, new.packages)

# load data ---------------------------------------------------------------

load(url("https://cfrc.tbcloud.tk/index.php/s/5S8GiiNjX4ZFiSP/download"))

df <- poverty.rate.tw
l <- grep("(DGBAS)", row.names(df))
df <- df[l, ] %>% 
        t() %>% 
        as_tibble()
df$year <- 2000:2018
poverty.rate.tw_DGBAS <- df
save(poverty.rate.tw_DGBAS, file = "povertyRatesTW.RData")
