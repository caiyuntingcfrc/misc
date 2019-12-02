rm(list = ls())
source("~/Github/misc/func_PovertyRate_CnE.R")
setwd("i:/R_wd/tw_inc/")

# load Rdata --------------------------------------------------------------

l <- list.files("R data files/", pattern = "^df_")
path_list <- paste(getwd(), "/R data files/", l, sep = "")
lapply(path_list, load, .GlobalEnv)

# load Rdata --------------------------------------------------------------

# l <- list.files("R data files/", pattern = "^df_")
# path_list <- paste(getwd(), "/R data files/", l, sep = "")
# # mget: multi-get (return the value of a names object)
# l1 <- sapply(path_list, function(x) mget(load(x)), simplify = FALSE)
# ll <- lapply(l1[-9], poverty_rate, weight = "a20")

# poverty rates -----------------------------------------------------------

p2000 <- poverty_rate(df = df.inc89, weight = "a21")
p2001 <- poverty_rate(df = df.inc90, weight = "a20")
p2002 <- poverty_rate(df = df.inc91, weight = "a20")
p2003 <- poverty_rate(df = df.inc92, weight = "a20")
p2004 <- poverty_rate(df = df.inc93, weight = "a20") 
p2005 <- poverty_rate(df = df.inc94, weight = "a20")
p2006 <- poverty_rate(df = df.inc95, weight = "a20")
p2007 <- poverty_rate(df = df.inc96, weight = "a20")
p2008 <- poverty_rate(df = df.inc97, weight = "a20")
p2009 <- poverty_rate(df = df.inc98, weight = "a20")
p2010 <- poverty_rate(df = df.inc99, weight = "a20")
p2011 <- poverty_rate(df = df.inc100, weight = "a20")
p2012 <- poverty_rate(df = df.inc101, weight = "a20")
p2013 <- poverty_rate(df = df.inc102, weight = "a20") 
p2014 <- poverty_rate(df = df.inc103, weight = "a20")
p2015 <- poverty_rate(df = df.inc104, weight = "a20")
p2016 <- poverty_rate(df = df.inc105, weight = "a20")
p2017 <- poverty_rate(df = df.inc106, weight = "a20")
p2018 <- poverty_rate(df = df.inc107, weight = "a20")

# combine data frame ------------------------------------------------------

l <- grep("^p[2][0][0-9][0-9]", names(.GlobalEnv), value = TRUE)
l2 <- grep("^df.inc|^data.list", ls(), value = TRUE)
data.list <- do.call("list", mget(l))
p_all <- do.call(cbind, data.list) %>% 
        round(digits = 2) %>% 
        select(sort(names(.), decreasing = FALSE)) %>% 
        rownames_to_column() %>% 
        rename(type = rowname)
poverty.rate.tw_children <- p_all
rm(list = c(l, l2))
save(poverty.rate.tw_children, 
     file = "R data files/poverty.rate.tw_children.RData")
saveRDS(poverty.rate.tw_children, 
        file = "R data files/poverty.rate.tw_children.rds")

