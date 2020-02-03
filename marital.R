
# prep and options --------------------------------------------------------
# rm
rm(list = ls()); cat("\14")
# source: function prop.sf.all
source("~/Github/PovertyRates/func_prop.sf.all.R")
# source: functioni PovertyRates_single
source("~/Github/PovertyRates/func_PovertyRates_single.R")
# devtools::source_url("https://raw.githubusercontent.com/caiyuntingcfrc/PovertyRates/master/func_prop.sf.all.R")
# load packages
ins.pack("tidyverse", "feather", "parallel", "data.table", "pbapply")
# setwd
setwd("i:/R_wd/tw_inc/R data files/")

# load Rdata --------------------------------------------------------------
# read rds
df.list1 <- readRDS("df.list(90-107).rds")
df.list2 <- readRDS("df.list(79-89).rds")

# filter ------------------------------------------------------------------

dt <- df.list2[[1]]
setDT(dt)
dt <- dt[a18 %in% c(321, 322) & n.children >= 1, ]
# dt[ , h.marital := NULL]
# b2_
lb2 <- grep("^b2_", names(dt))
m <- which(dt[ , ..lb2] == 1, arr.ind = TRUE)
r <- m[ , 1]
c <- m[ , 2]
dt[r , h.select := c]
s <- dt$h.select
dt[ , h.marital2 := .SD[[paste0("b16_", .BY$h.select)]], by = h.select]
setdiff(dt$h.marital, dt$h.marital2)
marital <- dt$h.marital; marital
tab1(marital)

