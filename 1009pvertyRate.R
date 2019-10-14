rm(list = ls())
source("~/Github_CFRC/misc/func_PovertyRate.R")
setwd("D:/R_wd/tw_inc/")

# load Rdata --------------------------------------------------------------

l <- list.files("R data files/", pattern = "^df_")
path_list <- paste(getwd(), "/R data files/", l, sep = "")
lapply(path_list, load, .GlobalEnv)

# poverty rates -----------------------------------------------------------

p2000 <- poverty_rate(df = df.inc89, weight = "a21", year = 2000) %>% 
        round(digits = 2)
p2001 <- poverty_rate(df = df.inc90, weight = "a20", year = 2001) %>% 
        round(digits = 2)
p2002 <- poverty_rate(df = df.inc91, weight = "a20", year = 2002) %>% 
        round(digits = 2)
p2003 <- poverty_rate(df = df.inc92, weight = "a20", year = 2003) %>% 
        round(digits = 2)
p2004 <- poverty_rate(df = df.inc93, weight = "a20", year = 2004) %>% 
        round(digits = 2)
p2005 <- poverty_rate(df = df.inc94, weight = "a20", year = 2005) %>% 
        round(digits = 2)
p2006 <- poverty_rate(df = df.inc95, weight = "a20", year = 2006) %>% 
        round(digits = 2)
p2007 <- poverty_rate(df = df.inc96, weight = "a20", year = 2007) %>% 
        round(digits = 2)
p2008 <- poverty_rate(df = df.inc97, weight = "a20", year = 2008) %>% 
        round(digits = 2)
p2009 <- poverty_rate(df = df.inc98, weight = "a20", year = 2009) %>% 
        round(digits = 2)
p2010 <- poverty_rate(df = df.inc99, weight = "a20", year = 2010) %>% 
        round(digits = 2)
p2011 <- poverty_rate(df = df.inc100, weight = "a20", year = 2011) %>% 
        round(digits = 2)
p2012 <- poverty_rate(df = df.inc101, weight = "a20", year = 2012) %>% 
        round(digits = 2)
p2013 <- poverty_rate(df = df.inc102, weight = "a20", year = 2013) %>% 
        round(digits = 2)
p2014 <- poverty_rate(df = df.inc103, weight = "a20", year = 2014) %>% 
        round(digits = 2)
p2015 <- poverty_rate(df = df.inc104, weight = "a20", year = 2015) %>% 
        round(digits = 2)
p2016 <- poverty_rate(df = df.inc105, weight = "a20", year = 2016) %>% 
        round(digits = 2)
p2017 <- poverty_rate(df = df.inc106, weight = "a20", year = 2017) %>% 
        round(digits = 2)
p2018 <- poverty_rate(df = df.inc107, weight = "a20", year = 2018) %>% 
        round(digits = 2)


# combine data frame ------------------------------------------------------

l <- grep("^p[2][0][0-9][0-9]", names(.GlobalEnv), value = TRUE)
data.list <- do.call("list", mget(l))
p_all <- do.call(cbind, data.list) %>% 
        select(sort(names(.), decreasing = FALSE))
poverty.rate.tw <- p_all
save(poverty.rate.tw, file = "R data files/poverty.rate.tw.RData")


