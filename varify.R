# prep and options --------------------------------------------------------
# rm
rm(list = ls()); cat("\14")
# soure func: ins.pack
devtools::source_url("https://raw.githubusercontent.com/caiyuntingcfrc/misc/function_poverty/func_ins.pack.R")
# ins.pack
ins.pack("tidyverse", "data.table", "epiDisplay", "readxl", "pastecs")
# setwd
setwd("d:/R_wd/tw_inc/")
# option
options(scipen = 999)

# readfile ----------------------------------------------------------------

df <- readRDS("R data files/df_inc108.rds")
setDT(df)

# income (all) ------------------------------------------------------------

nrow(df[is.na(itm500), ])

df[ , itm500 := if_else(is.na(itm500), 0, itm500)]



# weigh
s <- df$itm500
w <- df$a20
x <- round(xtabs(w ~ s), digits = 0)
n <- as.numeric(names(x))
weighed <- mapply(rep, x = n, times = x)

l <- unlist(weighed)

stat.desc(l) %>% 
        round(2) %>% 
        knitr::kable()

sum(l, na.rm = TRUE)


# disposable income (all) --------------------------------------------------

# replace na with 0
df[ , itm600 := if_else(is.na(itm600), 0, itm600)]
df[ , itm400 := if_else(is.na(itm400), 0, itm400)]

# calculate disposable income
df[ , dis_inc := itm400 - itm600]

# weigh
s <- df$dis_inc
w <- df$a20
x <- round(xtabs(w ~ s), digits = 0)
n <- as.numeric(names(x))
weighed <- mapply(rep, x = n, times = x)
l <- unlist(weighed, use.names = FALSE)

stat.desc(l) %>% 
        round(2) %>% 
        knitr::kable()