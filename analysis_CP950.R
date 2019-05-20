##### remove all #####
rm(list = ls())


##### load the packages #####
l <- c("tidyverse", "DescTools", "ggplot2", "haven", "car", "userfriendlyscience")
lapply(l, require, character.only= TRUE)
rm(l)

##### options #####
options(scipen = 999)
# timestamp
timestamp <- format(Sys.time(), "%m%d-%H%M")
# file names
file_name <- file.choose()
# file directory
file_path <- paste(dirname(file_name), "/", "analysis", sep = "")
# create an "analysis" directory
if(length(dir(file_path)) == 0) {
    dir.create(file_path)
    }
# output text file of anlysis
output_file_path <- paste(file_path, "/", "ANOVA", "_", timestamp, ".txt", sep = "")
# add to analysis file
cat("##### file #####\n", paste("The file is ", file_name, sep = ""), 
    file = output_file_path, sep = "\n", append = FALSE)

##### read data #####
# read .sav file and drop labels
df <- read_sav(file_name, encoding = "UTF-8") %>% zap_labels()
# variable names
var_names <- names(df)

##### structure of families #####
# recode parents and grand parents
df <- df %>% mutate(
    # father
    bd25_dad = case_when(Bd25_1.1 == 1 | Bd25_1.2 == 1 | Bd25_1.3 == 1 ~ 1, 
                         TRUE ~ 0),
    # mother
    bd25_mom = case_when(Bd25_1.1 == 1 | Bd25_1.2 == 1 | Bd25_1.3 == 1 ~ 1, 
                         TRUE ~ 0),
    # grand parents
    bd25_grand = case_when(Bd25_3.1 == 1 | Bd25_3.2 == 1 | Bd25_3.3 == 1 ~ 1,
                           TRUE ~ 0)
)
# recode structure of families
df <- df %>% mutate(
    # nuclear family
    Sf_nuc = case_when(bd25_dad == 1 & bd25_mom == 1 & bd25_grand == 0 ~ 1,
                       TRUE ~ 0), 
    # grandparenting family
    Sf_grand = case_when(bd25_dad == 0 | bd25_mom == 0 & bd25_grand == 1 ~ 1, 
                         TRUE ~ 0), 
    # three generation family
    Sf_three = case_when((bd25_dad == 1 | bd25_mom == 1) & bd25_grand == 1 ~ 1, 
                         TRUE ~ 0), 
    # single parent family
    Sf_sp = case_when((bd25_dad == 1 & bd25_mom == 0 & bd25_grand == 0) | 
                          (bd25_dad == 0 & bd25_mom == 1 & bd25_grand == 0) ~ 1, 
                      TRUE ~ 0), 
    # others
    Sf_other = case_when(Sf_nuc == 0 & Sf_grand == 0 & Sf_three == 0 & Sf_sp == 0 ~ 1,
                         TRUE ~ 0), 
    # cohabitation (a single parent with a cohabitating partner)
    Sf_cosp = case_when(Sf_sp == 1 & Bd25_6 == 1 ~ 1, 
                        TRUE ~ 0), 
    # SF
    Sf = case_when(Sf_nuc == 1 ~ "nuc", 
                   Sf_grand == 1 ~ "grand", 
                   Sf_three == 1 ~ "three", 
                   Sf_sp == 1 ~ "sp", 
                   Sf_other == 1 ~ "other", 
                   Sf_cosp == 1 ~ "cosp")
    )

##### filter and control #####
# Bd30 盉猵盉(Bd30 = 1)
# Bd21 ō (ダ克克) (Bd21 = 1 or 2)
# Ft9 璶酚臮ダ克ダ克(Ft9 = 1 or 2 or 3)
# SF (SF = nuc, three)
# n = 5129
df <- df %>% 
    filter(Bd30 == 1 & Bd21 %in% c(1, 2) & Ft9 %in% c(1, 2, 3))

##### recode Ft9 #####
df <- df %>% mutate(
    Ft9 = case_when(Ft9 == 1 ~ 3, 
                    Ft9 == 3 ~ 1, 
                    TRUE ~ 2)
)

# group by Ft9
df <- df %>% group_by(Ft9)

##### desc SF #####
# factorize Sf
df$Sf <- factor(df$Sf)
# frequency of SF
freq_Sf <- Freq(df$Sf) %>% capture.output()
# add the output to file
cat("\n##### freqency of Sf #####", freq_Sf, 
    file = output_file_path, sep = "\n", append = TRUE)

##### filter out grandparenting families #####
# n: 5129 to 4959
# df <- df %>% filter(Sf %in% c("nuc", "three"))
# n: 5129 to 3456
# df <- df %>% filter(Sf %in% c("nuc"))

##### desc Sf (after filtering out grandparenting families)
# frequency of SF
# freq_Sf <- Freq(df$Sf) %>% capture.output()
# add the output to file
# cat("\n##### freqency of Sf (without Sf_grand ) #####", freq_Sf, 
#    file = output_file_path, sep = "\n", append = TRUE)

##### desc Ft9 #####
# factorize Ft9
df$Ft9 <- factor(df$Ft9)
# count of Ft9
freq_Ft9 <- Freq(df$Ft9) %>% capture.output()
# add the output to file
cat("\n##### freqency of Ft9 #####", freq_Ft9, 
    file = output_file_path, sep = "\n", append = TRUE)

##### cross table #####
# Ft9 * Bd21
out_cross <- with(df, PercTable(Ft9, Bd21)) %>% capture.output()
# add the output to file
cat("\n##### cross table of Ft9 * Bd21 #####", out_cross, 
    file = output_file_path, sep = "\n", append = TRUE)
# Ft9 * Sf
# out_cross <- with(df, PercTable(Ft9, Sf)) %>% capture.output()
# add the output to file
# cat("\n##### cross table of Ft9 * Sf #####", out_cross, 
#    file = output_file_path, sep = "\n", append = TRUE)

##### Ch4 child behaviour #####

##### Levene's test for homogeneity of variance
# Ch4_
ll <- df[272:275]
# group = Ft9
out_lev_Ft9 <- with(df, lapply(ll, LeveneTest, group = Ft9, center = "median")) %>% 
    capture.output()
cat("\n##### Ch4 Levene's test (Ft9) #####\n", out_lev_Ft9, 
    file = output_file_path, sep = "\n", append = TRUE)
# group = Sf
# out_lev_Sf <- with(df, lapply(ll, LeveneTest, group = Sf, center = "median")) %>% 
#     capture.output()
# cat("\n##### Ch4 Levene's test (Sf) #####\n", out_lev_Sf, 
#    file = output_file_path, sep = "\n", append = TRUE)

##### ANOVA #####
##### Ch4_attack_sum #####
attack <- with(df, aov(Ch4_attack_sum ~ Ft9))
out_attack <- attack %>% Anova(type = 2) %>% capture.output()
cat("\n##### Ch4_attack #####\n", out_attack, 
    file = output_file_path, sep = "\n", append = TRUE)

##### Ch4_soc_sum #####
soc <- with(df, aov(Ch4_soc_sum ~ Ft9))
out_soc <- soc %>% Anova(type = 2) %>% capture.output()
cat("\n##### Ch4_soc #####\n", out_soc, 
    file = output_file_path, sep = "\n", append = TRUE)

##### not equal variances #####
##### Ch4_attention_sum #####
# ***
out_attention <- oneway.test(Ch4_attention_sum ~ Ft9, data = df) %>% 
    capture.output()
cat("\n##### Ch4_attention #####\n", out_attention, 
    file = output_file_path, sep = "\n", append = TRUE)

##### Ch4_withdrawn_sum #####
# .
out_withdraw <- oneway.test(Ch4_withdrawn_sum ~ Ft9, data = df) %>% 
    capture.output()
cat("\n##### Ch4_withdraw #####\n", out_withdraw, 
    file = output_file_path, sep = "\n", append = TRUE)

##### post hoc prep #####
# wrapper functon
func_post <- function(x, method) PostHocTest(x, method = method)
# post hoc method: LSD, Tukey, Scheffe
l <- list("hsd")

##### post hoc attack (LSD, Tukey, Scheffe) #####
out_post_attack <- lapply(l, func_post, x = attack) %>% capture.output()
cat("\n##### Ch4 attack post hoc #####\n", out_post_attack, 
    file = output_file_path, sep = "\n", append = TRUE)

##### post hoc attention (games-howell) #####
out_post_attention <- df %>% 
    filter(!is.na(Ft9)) %>% 
    filter(!is.na(Ch4_attention_sum)) %>% 
    with(., oneway(y = Ch4_attention_sum, 
                   x = Ft9, 
                   posthoc = "games-howell", 
                   digits = 4, 
                   posthocLetters = TRUE, 
                   posthocLetterAlpha = .05)
         ) %>% 
    capture.output()
cat("\n##### Ch4 attention post hoc #####\n", out_post_attention, 
    file = output_file_path, sep = "\n", append = TRUE)

##### means #####
df %>% summarize_at(vars(Ch4_attack_sum), funs(mean(., na.rm = TRUE)))
df %>% summarize_at(vars(Ch4_soc_sum), funs(mean(., na.rm = TRUE)))
df %>% summarize_at(vars(Ch4_attention_sum), funs(mean(., na.rm = TRUE)))
df %>% summarize_at(vars(Ch4_withdrawn_sum), funs(mean(., na.rm = TRUE)))

##### Bd44 marriage satisfaction #####
# frequency of Bd44
df$Bd44 <- as.integer(df$Bd44)
freq_Bd44 <- PercTable(df$Bd44) %>% capture.output()
# add the output to file
cat("\n##### frequency of Bd44 #####", freq_Bd44, 
    file = output_file_path, sep = "\n", append = TRUE)
# Bd44
marriage <- with(df, aov(Bd44 ~ Ft9 + factor(Bd21)))
out_marriage <- summary(marriage) %>% capture.output()
cat("\n##### Bd44_Marriage #####\n", out_marriage,
    file = output_file_path, sep = "\n", append = TRUE)

##### post hoc marriage (LSD, Tukey, Scheffe) #####
out_post_marriage <- lapply(l, func_post, x = marriage) %>% capture.output()
cat("\n##### Ch4 marriage post hoc #####\n", out_post_marriage, 
    file = output_file_path, sep = "\n", append = TRUE)

##### wrapper function test #####
# my_func <- function(x, method) PostHocTest(x, method = method)
# l <- list("lsd", "hsd", "scheffe")
# lapply(l, my_func, x = marriage) %>% capture.output()

##### plots #####
p <- ggplot(df, aes(x = Ft9, y = Ch4_attack_sum))
p <- p + 
    xlab("Type of Parenting") + 
    ylab("Aggresive behavior") + 
    geom_boxplot(outlier.color = "red") +
    #geom_point(position = position_jitterdodge()) + 
    scale_fill_manual(values = rep(NA, 3)) +
    theme_classic(base_size = 18) ; p