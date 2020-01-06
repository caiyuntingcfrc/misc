rm(list = ls())
# soure func: ins.pack
devtools::source_url("https://raw.githubusercontent.com/caiyuntingcfrc/misc/function_poverty/func_ins.pack.R")
devtools::source_url("https://raw.githubusercontent.com/caiyuntingcfrc/misc/function_poverty/func_PovertyRate.R")
devtools::source_url("https://raw.githubusercontent.com/caiyuntingcfrc/misc/function_poverty/func_PovertyRate_withChildren.R")
setwd("D:/R_wd/tw_inc/R data files/")
ins.pack("tidyverse", "feather", "parallel")

# load Rdata --------------------------------------------------------------

# 79-89
l2 <- list.files(pattern = "^df_inc[7][9].*.feather|^df_inc[8][0-3].*.feather")
path_list2 <- paste(getwd(), "/", l2, sep = "")
df.list2 <- vector("list", length(path_list2))
for(i in 1:length(path_list2)) {
        df.list2[[i]] <- read_feather(path_list2[i])
}

for(i in 1:length(df.list2)) {
        
        l <- grep("^b4_", names(df.list2[[i]]))
        # df <- df.list2[[i]]
        n.elder <- vector("numeric", nrow(df.list2[[i]]))
        
        for(j in 1:nrow(df.list2[[i]])) {n.elder[j] <- length(which(df.list2[[i]][j, l] >= 65))}
        
        df.list2[[i]]$`a19` <- n.elder
        
        write_feather(df.list2[[i]], path = path_list2[i])
}


