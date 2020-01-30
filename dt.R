# prep and options --------------------------------------------------------

devtools::source_url("https://raw.githubusercontent.com/caiyuntingcfrc/PovertyRates/master/func_ins.pack.R")
ins.pack("tidyverse", "magrittr", "feather", "data.table")
setwd("~/R_wd/tw_inc")

# read file ---------------------------------------------------------------

df <- read_feather("R data files/df_inc107.feather")

# data manipulation -------------------------------------------------------

setDT(df)
# grep
l <- grep("^b4_", names(df), value = TRUE)
# numbers of children (< 18)
df[ , n.children := rowSums(.SD < 18, na.rm = TRUE), .SDcols = l]
# numbers of the elderly (>= 65)
df[ , n.elderly := rowSums(.SD >= 65, na.rm = TRUE), .SDcols = l]

# Proportion of SF --------------------------------------------------------
# recode sf (data.table)
df[a18 %in% c(101, 102), sf := 1L]
df[a18 %in% c(201, 202), sf := 2L]
df[a18 %in% c(321, 322, 331, 332), sf := 3L]
df[a18 %in% c(421, 422, 431, 432), sf := 4L]
df[a18 %in% c(511, 512, 531, 532), sf := 5L]
df[a18 %in% c(611, 612, 621, 622, 631, 632), sf := 6L]
df[a18 %in% c(701, 702), sf := 7L]

# rec