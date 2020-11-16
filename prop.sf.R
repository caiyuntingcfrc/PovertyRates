
# prep and options --------------------------------------------------------
# rm
rm(list = ls()); cat("\14")
# source: function prop.sf.all
source("~/Github_CFRC/PovertyRates/func_prop.sf.all.R")
# source: functioni PovertyRates_single
source("~/Github_CFRC/PovertyRates/func_PovertyRates_single.R")
# source: function povertyThreshold
source("~/Github_CFRC/PovertyRates/func_povertyThreashold.R")
# devtools::source_url("https://raw.githubusercontent.com/caiyuntingcfrc/PovertyRates/master/func_prop.sf.all.R")
# load packages
ins.pack("tidyverse", "feather", "parallel", "data.table", "pbapply")
# setwd
setwd("d:/R_wd/tw_inc/R data files/")
# pboptions
pboptions("style" = 1, "use_lb" = TRUE)

# cluster -----------------------------------------------------------------

cpu.core <- detectCores() - 1L
cl <- makeCluster(cpu.core)
clusterEvalQ(cl, library(tidyverse))
clusterEvalQ(cl, library(magrittr))
clusterEvalQ(cl, library(data.table))
clusterEvalQ(cl, library(epiDisplay))

# # load Rdata --------------------------------------------------------------
# read rds
# df.list1 <- readRDS("df.list(90-107).rds")
# df.list2 <- readRDS("df.list(79-89).rds")

# 90 - 107
l1 <- list.files(pattern = "^df_inc[1][0][0-8].*.rds|^df_inc[9][0-9].*.rds")
path_list1 <- paste(getwd(), "/", l1, sep = "")
df.list1 <- parLapply(cl, path_list1, readRDS)

# 79-89
l2 <- list.files(pattern = "^df_inc[7][9].*.rds|^df_inc[8][0-9].*.rds")
path_list2 <- paste(getwd(), "/", l2, sep = "")
df.list2 <- parLapply(cl, path_list2, readRDS)
# 
# 
# # save RData --------------------------------------------------------------
# 
saveRDS(df.list1, file = "df.list(90-108).rds")
saveRDS(df.list2, file = "df.list(79-89).rds")


# poverty threshold -------------------------------------------------------

# threshold.list1 <- pblapply(df.list1, poverty_threshold, weight = "a20", cl = cl)
# threshold.list2 <- pblapply(df.list2, poverty_threshold, weight = "a21", cl = cl)
# 
# threshold.table1 <- rbindlist(threshold.list1)
# threshold.table2 <- rbindlist(threshold.list2)
# 
# threshold.table <- rbind(threshold.table1, threshold.table2) %>% .[order(.$year), ]
# 
# saveRDS(threshold.table, "povertyThreshold.rds")

# proportion of SF (all) --------------------------------------------------

prop.list1 <- pblapply(df.list1, prop.sf, weight = "a20", cl = cl)
prop.list2 <- pblapply(df.list2, prop.sf, weight = "a21", cl = cl)

prop.table1 <- Reduce(function(...) merge(..., all = TRUE), prop.list1)
prop.table2 <- Reduce(function(...) merge(..., all = TRUE), prop.list2)

prop.table <- merge(prop.table1, prop.table2, by = "type")
order <- sort(names(prop.table))
prop.table <- prop.table[ , ..order]


# set row and column order ------------------------------------------------

# column
nmc <- c("type", "1990")
setcolorder(prop.table, c(nmc, setdiff(names(prop.table), nmc)))

# row
nmr <- prop.table$type[c(12, 4, 9, 7, 5, 2, 13, 6, 11, 10, 3, 1, 8)]
prop.table <- prop.table[nmr, ]

# save file ---------------------------------------------------------------

# write rds
saveRDS(prop.table, "freq.sf.all.rds")
# write csv
write_excel_csv(prop.table, "freq.sf.all.csv")

# poverty rates: single-parent families (in narrow sense) -----------------

pr.list1 <- pblapply(df.list1, poverty.single, weight = "a20", cl = cl)
pr.list2 <- pblapply(df.list2, poverty.single, weight = "a21", cl = cl)

pr.table1 <- Reduce(function(...) merge(..., all = TRUE), pr.list1)
pr.table2 <- Reduce(function(...) merge(..., all = TRUE), pr.list2)


# set column order --------------------------------------------------------

pr.table <- merge(pr.table1, pr.table2, all = TRUE)
order <- sort(names(pr.table)); order
pr.table <- pr.table[ , ..order]

nmc <- c("type", "1990")
setcolorder(pr.table, c(nmc, setdiff(names(pr.table), nmc)))

# save file ---------------------------------------------------------------

# write rds
saveRDS(pr.table, "prRate.single.rds")
# write csv
write_excel_csv(pr.table, "prRate.single.csv")

# stop cluster ------------------------------------------------------------

stopCluster(cl)
