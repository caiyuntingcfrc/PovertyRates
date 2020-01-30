
# prep and options --------------------------------------------------------
# rm
rm(list = ls()); cat("\14")
# source: function prop.sf.all
source("~/Github_CFRC/PovertyRates/func_prop.sf.all.R")
# devtools::source_url("https://raw.githubusercontent.com/caiyuntingcfrc/PovertyRates/master/func_prop.sf.all.R")
# load packages
ins.pack("tidyverse", "feather", "parallel", "data.table", "pbapply")
# setwd
setwd("d:/R_wd/tw_inc/R data files/")

# cluster -----------------------------------------------------------------

cpu.core <- detectCores() - 1L
cl <- makeCluster(cpu.core)
clusterEvalQ(cl, library(magrittr))
clusterEvalQ(cl, library(data.table))
clusterEvalQ(cl, library(epiDisplay))

# # load Rdata --------------------------------------------------------------
# read rds
df.list1 <- readRDS("df.list(90-107).rds")
df.list2 <- readRDS("df.list(79-89).rds")

# # 90 - 107
# l1 <- list.files(pattern = "^df_inc[1][0][0-7].*.feather|^df_inc[9][0-9].*.feather")
# path_list1 <- paste(getwd(), "/", l1, sep = "")
# df.list1 <- parLapply(cl, path_list1, read_feather)
# 
# # 79-89
# l2 <- list.files(pattern = "^df_inc[7][9].*.feather|^df_inc[8][0-9].*.feather")
# path_list2 <- paste(getwd(), "/", l2, sep = "")
# df.list2 <- parLapply(cl, path_list2, read_feather)
# 
# 
# # save RData --------------------------------------------------------------
# 
# saveRDS(df.list1, file = "df.list(90-107).rds")
# saveRDS(df.list2, file = "df.list(79-89).rds")

# proportion of SF (all) --------------------------------------------------

pboptions("style" = 1, "use_lb" = TRUE)

prop.list1 <- pblapply(df.list1, prop.sf, weight = "a20", cl = cl)
prop.list2 <- pblapply(df.list2, prop.sf, weight = "a21", cl = cl)

prop.table1 = Reduce(function(...) merge(..., all = TRUE), prop.list1)
prop.table2 = Reduce(function(...) merge(..., all = TRUE), prop.list2)

prop.table <- merge(prop.table1, prop.table2, by = "type")
prop.table[ , sort(names(prop.table))]

# stop cluster ------------------------------------------------------------
stopCluster(cl)
