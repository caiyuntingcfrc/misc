
# prep and options --------------------------------------------------------
# rm
rm(list = ls())

# setwd
setwd("R_wd/")

# list of packages
list.packages <- c("tidyverse", "magrittr", "ggplot2")
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
load("tw_inc/R data files/povertyRatesTW.RData")

# plots -------------------------------------------------------------------
d <- gather(poverty.rate.tw_DGBAS, 
            key = "type of poverty rate", 
            value = "poverty rates", -year)
p <- ggplot(data = d, aes(x = `year`, y = `poverty rates`)) +
        geom_line(aes(color = `type of poverty rate`)) +
        theme_light()
p
