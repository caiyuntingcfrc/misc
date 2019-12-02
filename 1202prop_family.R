
# prep and options --------------------------------------------------------
# rm
rm(list = ls())
source("~/Github/misc/func_prop_withChildren.R")
source("~/Github/misc/func_prop_withElder.R")
setwd("i:/R_wd/tw_inc/")
path_list <- list.files("R data files/", pattern = "^df_") %>% 
        paste(getwd(), "/R data files/", ., sep = "")
lapply(path_list, load, .GlobalEnv)

# household with children -------------------------------------------------
l <- mget(grep("^df.inc10|^df.inc9", ls(), value = TRUE))
# rm
rm(list = grep("^df.inc[1][0][0-9]|^df.inc[9][0-9]", ls(), value = TRUE))
# prop.f
ll <- lapply(l, prop.f, weight = "a20", age = 18)
prop.h18 <- do.call(rbind, ll) %>% as.data.frame(row.names = NA)
# df.inc89
p <- prop.f(df.inc89, weight = "a21", age = 18)
# rbind
prop.h18 <- rbind(p, prop.h18) %>% arrange(V1)
colnames(prop.h18) <- c("year", "prop.h18")

# household with children (<12) -------------------------------------------
ll <- lapply(l, prop.f, weight = "a20", age = 12)
prop.h12 <- do.call(rbind, ll) %>% as.data.frame(row.names = NA)
p <- prop.f(df.inc89, weight = "a21", age = 12)
prop.h12 <- rbind(p, prop.h12) %>% arrange(V1)
colnames(prop.h12) <- c("year", "prop.h12")

# household with children (< 6) -------------------------------------------
ll <- lapply(l, prop.f, weight = "a20", age = 6)
prop.h6 <- do.call(rbind, ll) %>% as.data.frame(row.names = NA)
p <- prop.f(df.inc89, weight = "a21", age = 6)
prop.h6 <- rbind(p, prop.h6) %>% arrange(V1)
colnames(prop.h6) <- c("year", "prop.h6")

# core family with children -----------------------------------------------
d.core <- lapply(l, filter_at, "a18", all_vars(. %in% c(421, 422, 431, 432)))
# d89
d89 <- df.inc89 %>% 
        filter_at("a18", all_vars(. %in% c(421, 422, 431, 432)))
prop.core18 <- lapply(d.core, prop.f, weight = "a20", age = 18)
prop.core18 <- prop.core18 %>% 
        do.call(rbind, .) %>% 
        as.data.frame(row.names = NA)
p <- prop.f(d89, weight = "a21", age = 18)
# rbind
prop.core18 <- rbind(p, prop.core18) %>% arrange(V1)
colnames(prop.core18) <- c("year", "prop.core18")

# core family with children (<12) -----------------------------------------
prop.core12 <- lapply(d.core, prop.f, weight = "a20", age = 12)
prop.core12 <- prop.core12 %>% 
        do.call(rbind, .) %>% 
        as.data.frame(row.names = NA)
p <- prop.f(d89, weight = "a21", age = 12)
# rbind
prop.core12 <- rbind(p, prop.core12) %>% arrange(V1)
colnames(prop.core12) <- c("year", "prop.core12")

# core family with children (< 6) -----------------------------------------
prop.core6 <- lapply(d.core, prop.f, weight = "a20", age = 6)
prop.core6 <- prop.core6 %>% 
        do.call(rbind, .) %>% 
        as.data.frame(row.names = NA)
p <- prop.f(d89, weight = "a21", age = 6)
# rbind
prop.core6 <- rbind(p, prop.core6) %>% arrange(V1)
colnames(prop.core6) <- c("year", "prop.core6")
# rm
rm(d.core)


# stem family with children -----------------------------------------------
d.stem <- lapply(l, filter_at, "a18", all_vars(. %in% c(611, 612, 
                                                        621, 622, 
                                                        631, 632)))
# d89
d89 <- df.inc89 %>% 
        filter_at("a18", all_vars(. %in% c(611, 612, 
                                           621, 622, 
                                           631, 632)))
prop.stem18 <- lapply(d.stem, prop.f, weight = "a20", age = 18)
prop.stem18 <- prop.stem18 %>% 
        do.call(rbind, .) %>% 
        as.data.frame(row.names = NA)
p <- prop.f(d89, weight = "a21", age = 18)
# rbind
prop.stem18 <- rbind(p, prop.stem18) %>% arrange(V1)
colnames(prop.stem18) <- c("year", "prop.stem18")

# stem family with children (<12) -----------------------------------------
prop.stem12 <- lapply(d.stem, prop.f, weight = "a20", age = 12)
prop.stem12 <- prop.stem12 %>% 
        do.call(rbind, .) %>% 
        as.data.frame(row.names = NA)
p <- prop.f(d89, weight = "a21", age = 12)
# rbind
prop.stem12 <- rbind(p, prop.stem12) %>% arrange(V1)
colnames(prop.stem12) <- c("year", "prop.stem12")

# stem family with children (< 6) -----------------------------------------
prop.stem6 <- lapply(d.stem, prop.f, weight = "a20", age = 6)
prop.stem6 <- prop.stem6 %>% 
        do.call(rbind, .) %>% 
        as.data.frame(row.names = NA)
p <- prop.f(d89, weight = "a21", age = 6)
# rbind
prop.stem6 <- rbind(p, prop.stem6) %>% arrange(V1)
colnames(prop.stem6) <- c("year", "prop.stem6")
rm(d.stem)
rm(l)

# household with elder -------------------------------------------------
lapply(path_list, load, .GlobalEnv)
l <- mget(grep("^df.inc10|^df.inc9", ls(), value = TRUE))
# rm
rm(list = grep("^df.inc[1][0][0-9]|^df.inc[9][0-9]", ls(), value = TRUE))
# prop.e
ll <- lapply(l, prop.e, weight = "a20", age = 65)
prop.h65 <- do.call(rbind, ll) %>% as.data.frame(row.names = NA)
# df.inc89
p <- prop.e(df.inc89, weight = "a21", age = 65)
# rbind
prop.h65 <- rbind(p, prop.h65) %>% arrange(V1)
colnames(prop.h65) <- c("year", "prop.h65")

# household with elder (>=75) -------------------------------------------
ll <- lapply(l, prop.e, weight = "a20", age = 75)
prop.h75 <- do.call(rbind, ll) %>% as.data.frame(row.names = NA)
p <- prop.e(df.inc89, weight = "a21", age = 75)
prop.h75 <- rbind(p, prop.h75) %>% arrange(V1)
colnames(prop.h75) <- c("year", "prop.h75")

# household with elder (>=85) -------------------------------------------
ll <- lapply(l, prop.e, weight = "a20", age = 85)
prop.h85 <- do.call(rbind, ll) %>% as.data.frame(row.names = NA)
p <- prop.e(df.inc89, weight = "a21", age = 85)
prop.h85 <- rbind(p, prop.h85) %>% arrange(V1)
colnames(prop.h85) <- c("year", "prop.h85")

# core family with elder -----------------------------------------------
d.core <- lapply(l, filter_at, "a18", all_vars(. %in% c(421, 422, 431, 432)))
# d89
d89 <- df.inc89 %>% 
        filter_at("a18", all_vars(. %in% c(421, 422, 431, 432)))
prop.core65 <- lapply(d.core, prop.e, weight = "a20", age = 65)
prop.core65 <- prop.core65 %>% 
        do.call(rbind, .) %>% 
        as.data.frame(row.names = NA)
p <- prop.e(d89, weight = "a21", age = 65)
# rbind
prop.core65 <- rbind(p, prop.core65) %>% arrange(V1)
colnames(prop.core65) <- c("year", "prop.core65")

# core family with elder (>=75) -----------------------------------------
prop.core75 <- lapply(d.core, prop.e, weight = "a20", age = 75)
prop.core75 <- prop.core75 %>% 
        do.call(rbind, .) %>% 
        as.data.frame(row.names = NA)
p <- prop.e(d89, weight = "a21", age = 75)
# rbind
prop.core75 <- rbind(p, prop.core75) %>% arrange(V1)
colnames(prop.core75) <- c("year", "prop.core75")

# core family with elder (>=85) -----------------------------------------
prop.core85 <- lapply(d.core, prop.e, weight = "a20", age = 85)
prop.core85 <- prop.core85 %>% 
        do.call(rbind, .) %>% 
        as.data.frame(row.names = NA)
p <- prop.e(d89, weight = "a21", age = 85)
# rbind
prop.core85 <- rbind(p, prop.core85) %>% arrange(V1)
colnames(prop.core85) <- c("year", "prop.core85")
# rm
rm(d.core)


# stem family with elder -----------------------------------------------
d.stem <- lapply(l, filter_at, "a18", all_vars(. %in% c(611, 612, 
                                                        621, 622, 
                                                        631, 632)))
# d89
d89 <- df.inc89 %>% 
        filter_at("a18", all_vars(. %in% c(611, 612, 
                                           621, 622, 
                                           631, 632)))
prop.stem65 <- lapply(d.stem, prop.e, weight = "a20", age = 65)
prop.stem65 <- prop.stem65 %>% 
        do.call(rbind, .) %>% 
        as.data.frame(row.names = NA)
p <- prop.e(d89, weight = "a21", age = 65)
# rbind
prop.stem65 <- rbind(p, prop.stem65) %>% arrange(V1)
colnames(prop.stem65) <- c("year", "prop.stem65")

# stem family with elder (>= 75) -----------------------------------------
prop.stem75 <- lapply(d.stem, prop.e, weight = "a20", age = 75)
prop.stem75 <- prop.stem75 %>% 
        do.call(rbind, .) %>% 
        as.data.frame(row.names = NA)
p <- prop.e(d89, weight = "a21", age = 75)
# rbind
prop.stem75 <- rbind(p, prop.stem75) %>% arrange(V1)
colnames(prop.stem75) <- c("year", "prop.stem75")

# stem family with elder (>= 85) -----------------------------------------
prop.stem85 <- lapply(d.stem, prop.e, weight = "a20", age = 85)
prop.stem85 <- prop.stem85 %>% 
        do.call(rbind, .) %>% 
        as.data.frame(row.names = NA)
p <- prop.e(d89, weight = "a21", age = 85)
# rbind
prop.stem85 <- rbind(p, prop.stem85) %>% arrange(V1)
colnames(prop.stem85) <- c("year", "prop.stem85")
# rm
rm(d.stem)
rm(l)

save.image("/prop.family.RData")
# cbind -------------------------------------------------------------------

clist <- grep("^prop.core|^prop.h|^prop.stem", ls(), value = TRUE)
c <- mget(clist)
out.table <- Reduce(function(...) left_join(..., by = "year"), c) %>% 
        round(digits = 2) %>% 
        t() %>% 
        .[-1, ] %>% 
        as.data.frame()
colnames(out.table) <- 2000L:2018L
prop.family <- out.table
save(prop.family, file = "R data files/prop.family.tw.RData")
saveRDS(prop.family, file = "R data files/prop.family.tw.rds")
