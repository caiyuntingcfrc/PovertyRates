library(data.table)
library(tidyverse)
library(feather)

setwd("d:/R_wd/tw_inc/")
df <- read_feather("R data files/df_inc107.feather")
df <- read_feather("R data files/df_inc106.feather")
setDT(df)
l <- grep("^b4_", names(df), value = TRUE)
df[ , n.all := rowSums(.SD < 18, na.rm = TRUE), .SDcols = l]
