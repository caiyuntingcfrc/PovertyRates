
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

cpu.core <- detectCores() - 1L
cl <- makeCluster(cpu.core)
clusterEvalQ(cl, library(magrittr))
clusterEvalQ(cl, library(data.table))
clusterEvalQ(cl, library(epiDisplay))

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
# # 
# # save RData --------------------------------------------------------------
# 
# saveRDS(df.list1, file = "df.list(90-107).rds")
# saveRDS(df.list2, file = "df.list(79-89).rds")

# read rds
df.list1 <- readRDS("df.list(90-107).rds")
df.list90_103 <- df.list1[c(1:4, 9:18)]
df.list104_107 <- df.list1[5:8]
df.list79_89 <- readRDS("df.list(79-89).rds")
rm(df.list1)
gc()

# function ----------------------------------------------------------------

prop.marital <- function(df, weight) {
        
        # factor to numeric (bxx_)
        df <- as.data.frame(df)
        lb101 <- grep("^b|itm101$", names(df))
        df[ , lb101] <- lapply(df[ , lb101], as.character) %>% lapply(., as.numeric)  
        
        # check if the weight is numeric
        if(!is.numeric(df[[weight]])) {
                df[[weight]] <- as.numeric(df[[weight]])
                }
        
        dt <- setDT(df)
        
        # numbers of people in the householdweight
        lb1 <- grep("^b1_", names(dt))
        dt[ , n.all := rowSums(!is.na(.SD)), .SDcols = lb1]
        
        # numbers of children (< 18)
        lb4 <- grep("^b4_", names(dt))
        dt[ , n.children := rowSums(.SD < 18, na.rm = TRUE), .SDcols = lb4]
        
        # numbers of the elderly (>= 65)
        dt[ , n.elderly := rowSums(.SD >= 65, na.rm = TRUE), .SDcols = lb4]
        
        # h.marital
        lb2 <- grep("^b2_", names(dt))
        m <- which(dt[ , ..lb2] == 1, arr.ind = TRUE)
        # row and column
        r <- m[ , 1]
        c <- m[ , 2]
        # recode h.select
        dt[r , h.select := c]
        # recode h.marital
        dt[ , h.marital := .SD[[paste0("b16_", .BY$h.select)]], by = h.select]
        
        
        ##### prop of marital status #####
        dt1 <- dt[a18 %in% c(321, 322, 331, 332) & n.children >= 1,  ]
        # weigh
        s <- dt1$h.marital
        w <- dt1[[weight]]
        # xtab
        x <- round(xtabs(w ~ s), digits = 0); x
        n <- names(x)
        # weigh
        weighed <- mapply(rep, n, times = x)
        l <- unlist(weighed, use.names = FALSE)
        
        # out.table
        out.table <- tab1(l, 
                          graph = FALSE, 
                          decimal = 2) %>% 
                .[["output.table"]] %>% 
                as.data.table()
        
        # ifelse by year
        if(dt1[["year"]][1] %in% 79:103) {
                
                out.table <- out.table[-5, ]
                out.table[ , Percent := NULL][ , `Cum. percent` := NULL]
                out.table[ , marital := c("unmarried", 
                                          "divorced", 
                                          "separated", 
                                          "widowed")]
                # set column order
                setcolorder(out.table, c(2, 1))
                # set column names
                setnames(out.table, c("marital", df$year[1] + 1911L))
                
        } else if(dt1[["year"]][1] >= 104) {
                
                out.table <- out.table[-3, ]
                out.table[ , Percent := NULL][ , `Cum. percent` := NULL]
                out.table[ , marital := c("unmarried", 
                                          "divorced, separated or widowed")]
                # set column order
                setcolorder(out.table, c(2, 1))
                # set column names
                setnames(out.table, c("marital", df$year[1] + 1911L))
        }
        
        ##### prop of marital status by head's sex #####
        # male
        dtM <- dt[a18 %in% c(321, 331) & n.children >= 1, ]
        # weigh
        s <- dtM$h.marital
        w <- dtM[[weight]]
        # xtab
        x <- round(xtabs(w ~ s), digits = 0); x
        n <- names(x)
        # weigh
        weighed <- mapply(rep, n, times = x)
        l <- unlist(weighed, use.names = FALSE)
        
        # out.table
        out.tableM <- tab1(l, 
                          graph = TRUE, 
                          decimal = 2) %>% 
                .[["output.table"]] %>% 
                as.data.table()
        
        # ifelse by year
        if(dtM[["year"]][1] %in% 79:103) {
                
                out.tableM <- out.tableM[-5, ]
                out.tableM[ , Percent := NULL][ , `Cum. percent` := NULL]
                out.tableM[ , marital := c("unmarried", 
                                          "divorced", 
                                          "separated", 
                                          "widowed")]
                # set column order
                setcolorder(out.tableM, c(2, 1))
                # set column names
                setnames(out.tableM, c("marital", df$year[1] + 1911L))
                
        } else if(dtM[["year"]][1] >= 104) {
                
                out.tableM <- out.tableM[-3, ]
                out.tableM[ , Percent := NULL][ , `Cum. percent` := NULL]
                out.tableM[ , marital := c("unmarried", 
                                          "divorced, separated or widowed")]
                # set column order
                setcolorder(out.tableM, c(2, 1))
                # set column names
                setnames(out.tableM, c("marital", df$year[1] + 1911L))
        }
        
        # female
        dtF <- dt[a18 %in% c(322, 332) & n.children >= 1, ]
        # weigh
        s <- dtF$h.marital
        w <- dtF[[weight]]
        # xtab
        x <- round(xtabs(w ~ s), digits = 0); x
        n <- names(x)
        # weigh
        weighed <- mapply(rep, n, times = x)
        l <- unlist(weighed, use.names = FALSE)
        
        # out.table
        out.tableF <- tab1(l, 
                          graph = TRUE, 
                          decimal = 2) %>% 
                .[["output.table"]] %>% 
                as.data.table()
        
        # ifelse by year
        if(dtF[["year"]][1] %in% 79:103) {
                
                out.tableF <- out.tableF[-5, ]
                out.tableF[ , Percent := NULL][ , `Cum. percent` := NULL]
                out.tableF[ , marital := c("unmarried", 
                                          "divorced", 
                                          "separated", 
                                          "widowed")]
                # set column order
                setcolorder(out.tableF, c(2, 1))
                # set column names
                setnames(out.tableF, c("marital", df$year[1] + 1911L))
                
        } else if(dtF[["year"]][1] >= 104) {
                
                out.tableF <- out.tableF[-3, ]
                out.tableF[ , Percent := NULL][ , `Cum. percent` := NULL]
                out.tableF[ , marital := c("unmarried", 
                                          "divorced, separated or widowed")]
                # set column order
                setcolorder(out.tableF, c(2, 1))
                # set column names
                setnames(out.tableF, c("marital", df$year[1] + 1911L))
        }
        
        # return
        out.list <- list(all = out.table, m = out.tableM, f = out.tableF)
        return(out.list)
}

# test --------------------------------------------------------------------

prop.list79_89 <- pblapply(df.list79_89, prop.marital, weight = "a21", cl = cl)
prop.tbl79_89 <- Reduce(function(...) merge(..., all = TRUE), prop.list79_89)

# 79_89
l <- grep("^all", names(prop.tbl79_89), value = TRUE); l
p.all79_89 <- prop.tbl79_89[ , l]

l <- grep("^m", names(prop.tbl79_89), value = TRUE); l
p.m79_89 <- prop.tbl79_89[ , l]

l <- grep("^f", names(prop.tbl79_89), value = TRUE); l
p.f79_89 <- prop.tbl79_89[ , l]

# 90-103
prop.list90_103 <- pblapply(df.list90_103, prop.marital, weight = "a20", cl = cl)
prop.tbl90_103 <- Reduce(function(...) merge(..., all = TRUE), prop.list90_103)

l <- grep("^all", names(prop.tbl90_103), value = TRUE); l
p.all90_103 <- prop.tbl90_103[ , l]

l <- grep("^m", names(prop.tbl90_103), value = TRUE); l
p.m90_103 <- prop.tbl90_103[ , l]

l <- grep("^f", names(prop.tbl90_103), value = TRUE); l
p.f90_103 <- prop.tbl90_103[ , l]

# 104-107
prop.list104_107 <- pblapply(df.list104_107, prop.marital, weight = "a20", cl = cl)
prop.tbl104_107 <- Reduce(function(...) merge(..., all = TRUE), prop.list104_107)

l <- grep("^all", names(prop.tbl104_107), value = TRUE); l
p.all104_107 <- prop.tbl104_107[ , l]

l <- grep("^m", names(prop.tbl104_107), value = TRUE); l
p.m104_107 <- prop.tbl104_107[ , l]

l <- grep("^f", names(prop.tbl104_107), value = TRUE); l
p.f104_107 <- prop.tbl104_107[ , l]

# stop cluster ------------------------------------------------------------

stopCluster(cl)

# 79-103 ------------------------------------------------------------------

# p.all
prop.table79_103 <- merge(p.all79_89, p.all90_103, by = "all.marital")
colnames(prop.table79_103) <- names(prop.table79_103) %>% gsub("^all.", "", .)
# order
order <- sort(names(prop.table79_103))
setDT(prop.table79_103)
prop.table79_103 <- prop.table79_103[ , ..order]
# column
nmc <- c("marital", "1990")
setcolorder(prop.table79_103, c(nmc, setdiff(names(prop.table79_103), nmc)))
# row
prop.table79_103 <- prop.table79_103[c(3, 1, 2, 4), ]

# save
write_excel_csv(prop.table79_103, "f.marital_single_all(79-103).csv")

# p.m
prop.table79_103M <- merge(p.m79_89, p.m90_103, by = "m.marital")
colnames(prop.table79_103M) <- names(prop.table79_103M) %>% gsub("^m.", "", .)
# order
order <- sort(names(prop.table79_103M))
setDT(prop.table79_103M)
prop.table79_103M <- prop.table79_103M[ , ..order]
# column
nmc <- c("marital", "1990")
setcolorder(prop.table79_103M, c(nmc, setdiff(names(prop.table79_103M), nmc)))
# row
prop.table79_103M <- prop.table79_103M[c(3, 1, 2, 4), ]

# save
write_excel_csv(prop.table79_103M, "f.marital_single_M(79-103).csv")

# p.f
prop.table79_103F <- merge(p.f79_89, p.f90_103, by = "f.marital")
colnames(prop.table79_103F) <- names(prop.table79_103F) %>% gsub("^f.", "", .)
# order
order <- sort(names(prop.table79_103F))
setDT(prop.table79_103F)
prop.table79_103F <- prop.table79_103F[ , ..order]
# column
nmc <- c("marital", "1990")
setcolorder(prop.table79_103F, c(nmc, setdiff(names(prop.table79_103F), nmc)))
# row
prop.table79_103F <- prop.table79_103F[c(3, 1, 2, 4), ]

# save
write_excel_csv(prop.table79_103F, "f.marital_single_F(79-103).csv")

# 104-107 -----------------------------------------------------------------

prop.table104_107 <- p.all104_107
colnames(prop.table104_107) <- names(prop.table104_107) %>% gsub("^all.", "", .)
order <- sort(names(prop.table104_107))
setDT(prop.table104_107)
prop.table104_107 <- prop.table104_107[ , ..order]

# column
nmc <- c("marital", "2015")
setcolorder(prop.table104_107, c(nmc, setdiff(names(prop.table104_107), nmc)))

# row
prop.table104_107 <- prop.table104_107[c(2, 1), ]

# save
write_excel_csv(prop.table104_107, "f.marital_single_all(104-107).csv")

# p.m
prop.table104_107M <- p.m104_107
colnames(prop.table104_107M) <- names(prop.table104_107M) %>% gsub("^m.", "", .)
order <- sort(names(prop.table104_107M))
setDT(prop.table104_107M)
prop.table104_107M <- prop.table104_107M[ , ..order]

# column
nmc <- c("marital", "2015")
setcolorder(prop.table104_107M, c(nmc, setdiff(names(prop.table104_107M), nmc)))

# row
prop.table104_107M <- prop.table104_107M[c(2, 1), ]

# save
write_excel_csv(prop.table104_107M, "f.marital_single_M(104-107).csv")

# p.f
prop.table104_107F <- p.f104_107
colnames(prop.table104_107F) <- names(prop.table104_107F) %>% gsub("^f.", "", .)
order <- sort(names(prop.table104_107F))
setDT(prop.table104_107F)
prop.table104_107F <- prop.table104_107F[ , ..order]

# column
nmc <- c("marital", "2015")
setcolorder(prop.table104_107F, c(nmc, setdiff(names(prop.table104_107F), nmc)))

# row
prop.table104_107F <- prop.table104_107F[c(2, 1), ]

# save
write_excel_csv(prop.table104_107F, "f.marital_single_F(104-107).csv")


