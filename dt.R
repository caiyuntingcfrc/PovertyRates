# prep and options --------------------------------------------------------

# clearing env and console
rm(list = ls()); cat("\14")
# source function: ins.pack
devtools::source_url("https://raw.githubusercontent.com/caiyuntingcfrc/PovertyRates/master/func_ins.pack.R")
# load packages
ins.pack("tidyverse", "magrittr", "feather", "data.table", "epiDisplay")
# setwd
setwd("D:/R_wd/tw_inc")

# read file ---------------------------------------------------------------

df <- read_feather("R data files/df_inc107.feather")

# data manipulation -------------------------------------------------------

setDT(df)
# grep
l <- grep("^b4_", names(df), value = TRUE)
# numbers of people in the household
df[ , n.all := rowSums(!is.na(.SD)), .SDcols = l]
# numbers of children (< 18)
df[ , n.children := rowSums(.SD < 18, na.rm = TRUE), .SDcols = l]
# numbers of the elderly (>= 65)
df[ , n.elderly := rowSums(.SD >= 65, na.rm = TRUE), .SDcols = l]


# recode:sf ---------------------------------------------------------------

df[a18 %in% c(101, 102), sf := 1L]
df[a18 %in% c(201, 202), sf := 2L]
df[a18 %in% c(321, 322, 331, 332), sf := 3L]
df[a18 %in% c(421, 422, 431, 432), sf := 4L]
df[a18 %in% c(511, 512, 531, 532), sf := 5L]
df[a18 %in% c(611, 612, 621, 622, 631, 632), sf := 6L]
df[a18 %in% c(701, 702), sf := 7L]

# recode sf: single parent families
df[ , sf_narrow := ifelse(sf == 3L & n.children >= 1, 3.1, sf)]
# tab1(df$sf_narrow, graph = TRUE, bar.values = "percent", decimal = 2)

# prop:sf -----------------------------------------------------------------

# weigh: sf
s <- df$sf_narrow
w <- df$a20
# xtab
x <- round(xtabs(w ~ s), digits = 0); x
n <- names(x)
# weigh
weighed <- mapply(rep, n, times = x)
l <- unlist(weighed, use.names = FALSE)
t <- tab1(l, decimal = 2, graph = FALSE) %>% 
        .[["output.table"]] %>% 
        as.data.table()
# remove cum.percentage
t <- t[-9, ]
# add type
t[ , `Cum. percent` := NULL][ , type := c("single-person", 
                                          "married-couple", 
                                          "single-parent (broad)", 
                                          "single-parent (narrow)", 
                                          "nuclear", 
                                          "grandparent", 
                                          "stem", 
                                          "others")]
# set column order
setcolorder(t, c(3, 2, 1))

# prop: single-parent by head's sex ---------------------------------------

d