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

# dplyr approach
t <- df %>% 
        group_by(itm500) %>% 
        summarise(w = round(sum(a20, na.rm = TRUE), 0))
# weigh
weighed <- mapply(rep, x = t$itm500, times = t$w)
l <- unlist(weighed, use.names = FALSE)

stat.desc(l) %>% 
        round(2) %>% 
        knitr::kable()

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

stat.desc(weighed) %>% 
        round(2) %>% 
        knitr::kable()

# dplyr approach
t <- df %>% 
        group_by(dis_inc) %>% 
        summarise(w = round(sum(a20, na.rm = TRUE), 0))
# weigh
weighed <- mapply(rep, x = t$dis_inc, times = t$w)
l <- unlist(weighed, use.names = FALSE)
# statistics
stat.desc(l) %>% 
        round(2) %>% 
        knitr::kable()

# minimum -----------------------------------------------------------------

# average disposable income
df[ , n.all := rowSums(!(is.na(.SD)), na.rm = TRUE), .SDcols = grep("^b1_", names(df))]
df[ , avr_income := dis_inc / n.all / 12]

# weigh
# s1 <- df$avr_income[rep(1:length(df$avr_income), times = df$n.all)]
# w1 <- df$a20[rep(1:length(df$a20), times = df$n.all)]

s1 <- mapply(rep, df$avr_income, df$n.all) %>% unlist()
w1 <- mapply(rep, df$a20, df$n.all) %>% unlist()

x1 <- round(xtabs(w1 ~ s1), 0)
n1 <- as.numeric(names(x1))

weighed <- mapply(rep, x = n1, times = x1)
l <- unlist(weighed, use.names = FALSE)
median(l, na.rm = TRUE) * 0.6

stat.desc(weighed) %>% 
        round(2) %>% 
        knitr::kable()
median(l, na.rm = TRUE) * 0.7
