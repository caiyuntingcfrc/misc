l <- dir("~/github_cfrc/misc/") %>% 
        grep("_UTF8.R$", ., value = TRUE) %>% 
        .[-1]
file_path <- paste("~/github_cfrc/misc/", l, sep = "")
lapply(file_path, source, echo = FALSE)
