
# prep and options --------------------------------------------------------
# rm
rm(list = ls()); cat("\14")
# source: function prop.sf.all
source("~/Github_CFRC/PovertyRates/func_prop.sf.all.R")
# source: functioni PovertyRates_single
source("~/Github_CFRC/PovertyRates/func_PovertyRates_single.R")
# devtools::source_url("https://raw.githubusercontent.com/caiyuntingcfrc/PovertyRates/master/func_prop.sf.all.R")
# load packages
ins.pack("tidyverse", "feather", "parallel", "data.table", "pbapply", "haven")
# setwd
setwd("d:/R_wd/tw_inc/R data files/")

# cluster -----------------------------------------------------------------

# cpu.core <- detectCores() - 1L
# cl <- makeCluster(cpu.core)
# clusterEvalQ(cl, library(magrittr))
# clusterEvalQ(cl, library(data.table))
# clusterEvalQ(cl, library(epiDisplay))

# load Rdata --------------------------------------------------------------
# # read sav
# l1 <- list.files(pattern = "^inc[7][9].*.sav|^inc[8][0-7].*.sav")
# l2 <- list.files(pattern = "^df_inc[7][9].*.feather|^df_inc[8][0-7].*.feather")
# path_list1 <- paste(getwd(), "/", l1, sep = "")
# path_list2 <- paste(getwd(), "/", l2, sep = "")
# df.list_sav <- lapply(path_list1, read_sav) %>% lapply(setDT)
# df.list_feather <- lapply(path_list2, read_feather) %>% lapply(setDT)
# 
# for(i in 1:length(df.list_sav)) {
# 
#         df.list_sav[[i]][ , a7 := df.list_feather[[i]]$h.sex]
#         df.list_sav[[i]][ , a8 := df.list_feather[[i]]$n.all]
#         df.list_sav[[i]][ , a12 := df.list_feather[[i]]$n.adults]
#         df.list_sav[[i]][ , a18 := df.list_feather[[i]]$sf]
#         df.list_sav[[i]][ , a19 := df.list_feather[[i]]$n.elder]
# 
#         write_feather(df.list_sav[[i]], path_list2[[i]])
# 
#         }

# # 90 - 107
# l1 <- list.files(pattern = "^df_inc[1][0][0-7].*.feather|^df_inc[9][0-9].*.feather")
# path_list1 <- paste(getwd(), "/", l1, sep = "")
# df.list1 <- parLapply(cl, path_list1, read_feather)
# df.list1 <- parLapply(cl, df.list1, as.data.frame)
# 
# # 79-89
# l2 <- list.files(pattern = "^df_inc[7][9].*.feather|^df_inc[8][0-9].*.feather")
# path_list2 <- paste(getwd(), "/", l2, sep = "")
# df.list2 <- parLapply(cl, path_list2, read_feather)
# df.list2 <- parLapply(cl, df.list2, as.data.frame)
# 
# # save RData --------------------------------------------------------------
# 
# saveRDS(df.list1, file = "df.list(90-107).rds")
# saveRDS(df.list2, file = "df.list(79-89).rds")

# read rds
df.list1 <- readRDS("df.list(90-107).rds")
df.list2 <- readRDS("df.list(79-89).rds")

# filter ------------------------------------------------------------------

dt <- df.list1[[7]] 

# factor to numeric (bxx_)
lb101 <- grep("^b|itm101$", names(dt))
dt[ , lb101] <- lapply(dt[ , lb101], as.character) %>% lapply(., as.numeric)

setDT(dt)

# grep
lb1 <- grep("^b1_", names(dt))
# numbers of people in the household
dt[ , n.all := rowSums(!is.na(.SD)), .SDcols = lb1]

# numbers of children (< 18)
lb4 <- grep("^b4_", names(dt))
dt[ , n.children := rowSums(.SD < 18, na.rm = TRUE), .SDcols = lb4]
# numbers of the elderly (>= 65)
dt[ , n.elderly := rowSums(.SD >= 65, na.rm = TRUE), .SDcols = lb4]

# h.marital
# convert b2_ to numeric
lb2 <- grep("^b2_", names(dt))
m <- which(dt[ , ..lb2] == 1, arr.ind = TRUE)

# row and column
r <- m[ , 1]
c <- m[ , 2]
# recode h.select
dt[r , h.select := c]
# recode h.marital
dt[ , h.marital := .SD[[paste0("b16_", .BY$h.select)]], by = h.select]
tab1(dt[a18 %in% c(331, 332, 321, 322) & n.children >= 1, h.marital], bar.values = "percent")

# stop cluster ------------------------------------------------------------

# stopCluster(cl)
