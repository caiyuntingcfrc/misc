rm(list = ls())
# soure func: ins.pack
devtools::source_url("https://raw.githubusercontent.com/caiyuntingcfrc/misc/function_poverty/func_ins.pack.R")
devtools::source_url("https://raw.githubusercontent.com/caiyuntingcfrc/misc/function_poverty/func_PovertyRate.R")
devtools::source_url("https://raw.githubusercontent.com/caiyuntingcfrc/misc/function_poverty/func_PovertyRate_withChildren.R")
setwd("D:/R_wd/tw_inc/R data files/")
ins.pack("tidyverse", "feather", "parallel")

# load Rdata --------------------------------------------------------------
# list of files1 : 90 - 107
l1 <- list.files(pattern = "^df_inc[1][0][0-7].*.feather|^df_inc[9][0-9].*.feather")
path_list1 <- paste(getwd(), "/", l1, sep = "")
df.list1 <- vector("list", length(path_list1))
for(i in 1:length(path_list1)) {
        df.list1[[i]] <- read_feather(path_list1[i])
        }
# 79-89
l2 <- list.files(pattern = "^df_inc[7][9].*.feather|^df_inc[8][0-9].*.feather")
path_list2 <- paste(getwd(), "/", l2, sep = "")
df.list2 <- vector("list", length(path_list2))
for(i in 1:length(path_list2)) {
        df.list2[[i]] <- read_feather(path_list2[i])
        }

# cluster -----------------------------------------------------------------

cpu.core <- detectCores() - 1L
cl <- makeCluster(cpu.core)
clusterEvalQ(cl, library(tidyverse))
clusterEvalQ(cl, library(magrittr))

# poverty rates -----------------------------------------------------------

pr1 <- parLapply(cl, df.list1, poverty_rate, "a20")
pr2 <- parLapply(cl, df.list2, poverty_rate, "a21")
pr.list1 <- c(pr1, pr2)
# with children
pr3 <- parLapply(cl, df.list1, poverty_rate_children, "a20")
pr4 <- parLapply(cl, df.list2, poverty_rate_children, "a21")
pr.list2 <- c(pr3, pr4)


# stop cluster ------------------------------------------------------------
stopCluster(cl)

# combine data frame ------------------------------------------------------

df1 <- do.call(cbind, pr.list1)
df1 <- df1[ , order(names(df1))] %>% round(2L)
df1$type <- rownames(df1)
write_excel_csv(df1, "prRate.csv")

df2 <- do.call(cbind, pr.list2)
df2 <- df2[ , order(names(df2))] %>% round(2L)
df2$type <- rownames(df2)
write_excel_csv(df2, "prRate_children.csv")

l <- grep("^p[2][0][0-9][0-9]", names(.GlobalEnv), value = TRUE)
data.list <- do.call("list", mget(l))
p_all <- do.call(cbind, data.list) %>% 
        select(sort(names(.), decreasing = FALSE)) %>% 
        rownames_to_column() %>% 
        rename(type = rowname)
poverty.rate.tw <- p_all
save(poverty.rate.tw, file = "R data files/poverty.rate.tw.RData")
saveRDS(poverty.rate.tw, file = "R data files/poverty.rate.tw.rds")
# write_sas(data = poverty.rate.tw, path = "R data files/poverty.sas7bdat")
# write_excel_csv(poverty.rate.tw, path = "R data files/poverty.rate.tw.csv")
