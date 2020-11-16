# prep and options --------------------------------------------------------
# rm
rm(list = ls()); cat("\14")
# source
source("~/Github/PovertyRates/func_PovertyRates.R")
# ins.pack
ins.pack("tidyverse", "data.table", "parallel", "pbapply", "epiDisplay")
# setwd
setwd("i:/R_wd/tw_inc/R data files/")
# pboptions
pboptions("style" = 1, "use_lb" = TRUE)

# read files --------------------------------------------------------------

# file
df.list1 <- readRDS("df.list(79-89).rds")
df.list2 <- readRDS("df.list(90-108).rds")
threshold.list <- readRDS("povertyThreshold.rds")

# setDT -------------------------------------------------------------------

df.list1 <- sapply(df.list1, setDT)
df.list2 <- sapply(df.list2, setDT)

# year
year1 <- sapply(df.list1, function(...){y <- ...[["year"]][1]}) + 1911L
year2 <- sapply(df.list2, function(...){y <- ...[["year"]][1]}) + 1911L

# threshold (79 - 89)
threshold1 <- threshold.list[match(year1, threshold.list$year), ]$threshold
# threshold (90 - 107)
threshold2 <- threshold.list[match(year2, threshold.list$year), ]$threshold