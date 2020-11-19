# prep and options --------------------------------------------------------
# rm
rm(list = ls()); cat("\14")
# source
source("~/Documents/Github/PovertyRates/func_PovertyRates.R")
source("~/Documents/Github/PovertyRates/func_povertyThreashold.R")
source("~/Documents/Github/PovertyRates/func_PovertyRates_overall.R")
# ins.pack
ins.pack("tidyverse", "data.table", "parallel", "pbapply", "epiDisplay")
# setwd
setwd("~/R_wd/tw_inc/R data files/")
# pboptions
pboptions("style" = 1, "use_lb" = TRUE)

# read files --------------------------------------------------------------
# file
df.list1 <- readRDS("df.list(79-89).rds")
df.list2 <- readRDS("df.list(90-108).rds")
threshold.list <- readRDS("povertyThreshold.rds")

# setDT -------------------------------------------------------------------

df.list1 <- lapply(df.list1, setDT)
df.list2 <- lapply(df.list2, setDT)

# year
year1 <- sapply(df.list1, function(...){y <- ...[["year"]][1]}) + 1911L
year2 <- sapply(df.list2, function(...){y <- ...[["year"]][1]}) + 1911L

# threshold (79 - 89)
threshold1 <- threshold.list[match(year1, threshold.list$year), ]$threshold
# threshold (90 - 107)
threshold2 <- threshold.list[match(year2, threshold.list$year), ]$threshold

# calculate the child and the old -----------------------------------------
# n.children
for(i in 1:length(df.list1)){ 
        df.list1[[i]][, n.children := rowSums(.SD < 18, na.rm = TRUE), 
               .SDcols = grep("^b4_", names(df.list1[[i]]))]
        }
for(i in 1:length(df.list2)){ 
        df.list2[[i]][, n.children := rowSums(.SD < 18, na.rm = TRUE), 
               .SDcols = grep("^b4_", names(df.list2[[i]]))]
        }
# n.elder
for(i in 1:length(df.list1)){ 
        df.list1[[i]][, n.elder := rowSums(.SD >= 65, na.rm = TRUE), 
               .SDcols = grep("^b4_", names(df.list1[[i]]))]
        }
for(i in 1:length(df.list2)){ 
        df.list2[[i]][, n.elder := rowSums(.SD >= 65, na.rm = TRUE), 
               .SDcols = grep("^b4_", names(df.list2[[i]]))]
        }

# pRate: Overall households -----------------------------------------------

# df
df1 <- df.list1
df2 <- df.list2
# func_povertyRate
p.1 <- pbmapply(poverty_rate, df = df1, threshold = threshold1, 
                year = year1, weight= "a21") %>% 
        data.frame(`year` = year1, `poverty rate` = .)

p.2 <- pbmapply(poverty_rate, df = df2, threshold = threshold2, 
                year = year2, weight= "a20") %>% 
        data.frame(`year` = year2, `poverty rate` = .)
# combine and order
dt <- rbind(p.1, p.2)
dt <- dt[order(dt$year), ]
# spread
dt <- dt %>% spread(year, `poverty.rate`)
p.overall <- dt %>% mutate(type = "overall household")
# rm
rm(dt)

# pRate: single-person ----------------------------------------------------

# df
df1 <- lapply(df.list1, "[", a18 %in% c(101, 102))
df2 <- lapply(df.list2, "[", a18 %in% c(101, 102))
# func_povertyRate
p.1 <- pbmapply(poverty_rate, df = df1, threshold = threshold1, 
                year = year1, weight= "a21") %>% 
        data.frame(`year` = year1, `poverty rate` = .)

p.2 <- pbmapply(poverty_rate, df = df2, threshold = threshold2, 
                year = year2, weight= "a20") %>% 
        data.frame(`year` = year2, `poverty rate` = .)
# combine and order
dt <- rbind(p.1, p.2)
dt <- dt[order(dt$year), ]
# spread
dt <- dt %>% spread(year, `poverty.rate`)
p.single <- dt %>% mutate(type = "single-person")
# rm
rm(dt)

# pRate: married-couple ---------------------------------------------------
# df
df1 <- lapply(df.list1, "[", a18 %in% c(201, 202))
df2 <- lapply(df.list2, "[", a18 %in% c(201, 202))
# func_povertyRate
p.1 <- pbmapply(poverty_rate, df = df1, threshold = threshold1, 
                year = year1, weight= "a21") %>% 
        data.frame(`year` = year1, `poverty rate` = .)

p.2 <- pbmapply(poverty_rate, df = df2, threshold = threshold2, 
                year = year2, weight= "a20") %>% 
        data.frame(`year` = year2, `poverty rate` = .)
# combine and order
dt <- rbind(p.1, p.2)
dt <- dt[order(dt$year), ]
# spread
dt <- dt %>% spread(year, `poverty.rate`)
p.couple <- dt %>% mutate(type = "married-couple")
# rm
rm(dt)

# pRate: overall single-parent --------------------------------------------

# df
df1 <- lapply(df.list1, "[", a18 %in% c(321, 322, 331, 332) & n.children > 0)
df2 <- lapply(df.list2, "[", a18 %in% c(321, 322, 331, 332) & n.children > 0)
# func_povertyRate
p.1 <- pbmapply(poverty_rate, df = df1, threshold = threshold1, 
                year = year1, weight= "a21") %>% 
        data.frame(`year` = year1, `poverty rate` = .)

p.2 <- pbmapply(poverty_rate, df = df2, threshold = threshold2, 
                year = year2, weight= "a20") %>% 
        data.frame(`year` = year2, `poverty rate` = .)
# combine and order
dt <- rbind(p.1, p.2)
dt <- dt[order(dt$year), ]
# spread
dt <- dt %>% spread(year, `poverty.rate`)
p.single_parent <- dt %>% mutate(type = "overall single-parent")
# rm
rm(dt)

# pRate: single-parent (m-headed) -----------------------------------------

# df
df1 <- lapply(df.list1, "[", a18 %in% c(321, 322, 331, 332) & n.children > 0 & a7 == 1)
df2 <- lapply(df.list2, "[", a18 %in% c(321, 322, 331, 332) & n.children > 0 & a7 == 1)

# func_povertyRate
p.1 <- pbmapply(poverty_rate, df = df1, threshold = threshold1, 
                year = year1, weight= "a21") %>% 
        data.frame(`year` = year1, `poverty rate` = .)

p.2 <- pbmapply(poverty_rate, df = df2, threshold = threshold2, 
                year = year2, weight= "a20") %>% 
        data.frame(`year` = year2, `poverty rate` = .)
# combine and order
dt <- rbind(p.1, p.2)
dt <- dt[order(dt$year), ]
# spread
dt <- dt %>% spread(year, `poverty.rate`)
p.single_parent_m <- dt %>% mutate(type = "m-headed single-parent")
# rm
rm(dt)

# pRate: single-parent (f-headed) -----------------------------------------

# df
df1 <- lapply(df.list1, "[", a18 %in% c(321, 322, 331, 332) & n.children > 0 & a7 == 2)
df2 <- lapply(df.list2, "[", a18 %in% c(321, 322, 331, 332) & n.children > 0 & a7 == 2)

# func_povertyRate
p.1 <- pbmapply(poverty_rate, df = df1, threshold = threshold1, 
                year = year1, weight= "a21") %>% 
        data.frame(`year` = year1, `poverty rate` = .)

p.2 <- pbmapply(poverty_rate, df = df2, threshold = threshold2, 
                year = year2, weight= "a20") %>% 
        data.frame(`year` = year2, `poverty rate` = .)
# combine and order
dt <- rbind(p.1, p.2)
dt <- dt[order(dt$year), ]
# spread
dt <- dt %>% spread(year, `poverty.rate`)
p.single_parent_f <- dt %>% mutate(type = "f-headed single-parent")
# rm
rm(dt)

# pRate: nuclear ----------------------------------------------------------

# df
df1 <- lapply(df.list1, "[", a18 %in% c(421, 422, 431, 432)) 
df2 <- lapply(df.list2, "[", a18 %in% c(421, 422, 431, 432)) 
# func_povertyRate
p.1 <- pbmapply(poverty_rate, df = df1, threshold = threshold1, 
                year = year1, weight= "a21") %>% 
        data.frame(`year` = year1, `poverty rate` = .)

p.2 <- pbmapply(poverty_rate, df = df2, threshold = threshold2, 
                year = year2, weight= "a20") %>% 
        data.frame(`year` = year2, `poverty rate` = .)
# combine and order
dt <- rbind(p.1, p.2)
dt <- dt[order(dt$year), ]
# spread
dt <- dt %>% spread(year, `poverty.rate`)
p.nuclear <- dt %>% mutate(type = "nuclear")
# rm
rm(dt)

# pRate: grandparent ------------------------------------------------------

# df
df1 <- lapply(df.list1, "[", a18 %in% c(511, 512, 531, 532))
df2 <- lapply(df.list2, "[", a18 %in% c(511, 512, 531, 532)) 
# func_povertyRate
p.1 <- pbmapply(poverty_rate, df = df1, threshold = threshold1, 
                year = year1, weight= "a21") %>% 
        data.frame(`year` = year1, `poverty rate` = .)

p.2 <- pbmapply(poverty_rate, df = df2, threshold = threshold2, 
                year = year2, weight= "a20") %>% 
        data.frame(`year` = year2, `poverty rate` = .)
# combine and order
dt <- rbind(p.1, p.2)
dt <- dt[order(dt$year), ]
# spread
dt <- dt %>% spread(year, `poverty.rate`)
p.grandparent <- dt %>% mutate(type = "grand-parent")
# rm
rm(dt)

# pRate: Stem -------------------------------------------------------------

# df
df1 <- lapply(df.list1, "[", a18 %in% c(611, 612, 621, 622, 631, 632))
df2 <- lapply(df.list2, "[", a18 %in% c(611, 612, 621, 622, 631, 632)) 
# func_povertyRate
p.1 <- pbmapply(poverty_rate, df = df1, threshold = threshold1, 
                year = year1, weight= "a21") %>% 
        data.frame(`year` = year1, `poverty rate` = .)

p.2 <- pbmapply(poverty_rate, df = df2, threshold = threshold2, 
                year = year2, weight= "a20") %>% 
        data.frame(`year` = year2, `poverty rate` = .)
# combine and order
dt <- rbind(p.1, p.2)
dt <- dt[order(dt$year), ]
# spread
dt <- dt %>% spread(year, `poverty.rate`)
p.stem <- dt %>% mutate(type = "stem")
# rm
rm(dt)

# pRate: with children ----------------------------------------------------
# df
df1 <- lapply(df.list1, "[", n.children > 0)
df2 <- lapply(df.list2, "[", n.children > 0)
# func_povertyRate
p.1 <- pbmapply(poverty_rate, df = df1, threshold = threshold1, 
                year = year1, weight= "a21") %>% 
        data.frame(`year` = year1, `poverty rate` = .)

p.2 <- pbmapply(poverty_rate, df = df2, threshold = threshold2, 
                year = year2, weight= "a20") %>% 
        data.frame(`year` = year2, `poverty rate` = .)

# combine and order
dt <- rbind(p.1, p.2)
dt <- dt[order(dt$year), ]
# spread
dt <- dt %>% spread(year, `poverty.rate`)
p.withChildren <- dt %>% mutate(type = "with children")
# rm
rm(dt)

# pRate: without children ----------------------------------------------------
# df
df1 <- lapply(df.list1, "[", n.children == 0)
df2 <- lapply(df.list2, "[", n.children == 0)
# func_povertyRate
p.1 <- pbmapply(poverty_rate, df = df1, threshold = threshold1, 
                year = year1, weight= "a21") %>% 
        data.frame(`year` = year1, `poverty rate` = .)

p.2 <- pbmapply(poverty_rate, df = df2, threshold = threshold2, 
                year = year2, weight= "a20") %>% 
        data.frame(`year` = year2, `poverty rate` = .)

# combine and order
dt <- rbind(p.1, p.2)
dt <- dt[order(dt$year), ]
# spread
dt <- dt %>% spread(year, `poverty.rate`)
p.withoutChildren <- dt %>% mutate(type = "without children")
# rm
rm(dt)

# pRate: with elder ----------------------------------------------------
# df
df1 <- lapply(df.list1, "[", n.elder > 0)
df2 <- lapply(df.list2, "[", n.elder > 0)
# func_povertyRate
p.1 <- pbmapply(poverty_rate, df = df1, threshold = threshold1, 
                year = year1, weight= "a21") %>% 
        data.frame(`year` = year1, `poverty rate` = .)

p.2 <- pbmapply(poverty_rate, df = df2, threshold = threshold2, 
                year = year2, weight= "a20") %>% 
        data.frame(`year` = year2, `poverty rate` = .)

# combine and order
dt <- rbind(p.1, p.2)
dt <- dt[order(dt$year), ]
# spread
dt <- dt %>% spread(year, `poverty.rate`)
p.withElder <- dt %>% mutate(type = "with elder")
# rm
rm(dt)

# pRate: without elder ----------------------------------------------------
# df
df1 <- lapply(df.list1, "[", n.elder == 0)
df2 <- lapply(df.list2, "[", n.elder == 0)
# func_povertyRate
p.1 <- pbmapply(poverty_rate, df = df1, threshold = threshold1, 
                year = year1, weight= "a21") %>% 
        data.frame(`year` = year1, `poverty rate` = .)

p.2 <- pbmapply(poverty_rate, df = df2, threshold = threshold2, 
                year = year2, weight= "a20") %>% 
        data.frame(`year` = year2, `poverty rate` = .)

# combine and order
dt <- rbind(p.1, p.2)
dt <- dt[order(dt$year), ]
# spread
dt <- dt %>% spread(year, `poverty.rate`)
p.withoutElder <- dt %>% mutate(type = "without elder")
# rm
rm(dt)


# pRate: overall population-----------------------------------------------------

df1 <- df.list1
df2 <- df.list2

# func: pr.all
p.1 <- pbmapply(pr.all, df = df1, n.ppl = "a8", threshold = threshold1, 
                year = year1, weight= "a21") %>% 
        data.frame(`year` = year1, `poverty rate` = .)

p.2 <- pbmapply(pr.all, df = df2, n.ppl = "a8", threshold = threshold2, 
                year = year2, weight= "a20") %>% 
        data.frame(`year` = year2, `poverty rate` = .)

# combine and order
dt <- rbind(p.1, p.2)
dt <- dt[order(dt$year), ]
# spread
dt <- dt %>% spread(year, `poverty.rate`)
p.overallPop <- dt %>% mutate(type = "overall population")
# rm
rm(dt)

# pRate: overall children -----------------------------------------------------

df1 <- df.list1
df2 <- df.list2

# func: pr.all
p.1 <- pbmapply(pr.all, df = df1, n.ppl = "n.children", threshold = threshold1, 
                year = year1, weight= "a21") %>% 
        data.frame(`year` = year1, `poverty rate` = .)

p.2 <- pbmapply(pr.all, df = df2, n.ppl = "n.children", threshold = threshold2, 
                year = year2, weight= "a20") %>% 
        data.frame(`year` = year2, `poverty rate` = .)

# combine and order
dt <- rbind(p.1, p.2)
dt <- dt[order(dt$year), ]
# spread
dt <- dt %>% spread(year, `poverty.rate`)
p.overallChildren <- dt %>% mutate(type = "overall children")
# rm
rm(dt)

# pRate: sf ---------------------------------------------------------------

l <- grep("^p\\.", ls(), value = TRUE) %>% .[!(. %in% c("p.1", "p.2"))]; l
l <- mget(l)
pr.dt <- rbindlist(l)

# column order
nmc <- c("type", "1990")
setcolorder(pr.dt, c(nmc, setdiff(names(pr.dt), nmc)))

# row order
nmr <- pr.dt$type[c(5, 1, 6, 3, 2, 9, 4, 8, 7)]
pr.dt <- pr.dt[c(5, 1, 6, 3, 2, 9, 4, 8, 7), ]

write_excel_csv(pr.dt, "pRate.sf.csv")

# single-parent hidden in stem --------------------------------------------
# dt <- df.list1[[1]]
# # a18 %in% c(611, 612) 1st-gen
# dt <- dt[a18 %in% c(611, 612), ]
# 
# lb <- grep("^b2|^b4_|^b16", names(dt))
# lb2 <- grep("^b2_", names(dt))
# lb4 <- grep("^b4_", names(dt))
# lb16 <- grep("^b16_", names(dt))
# 
# # recode: n.kid and n.grandkid
# dt[ , n.kid := rowSums(.SD == 3, na.rm = TRUE), .SDcols = lb2]
# dt[ , n.grandkid := rowSums(.SD == 4, na.rm = TRUE), .SDcols = lb2]
# 
# # t: logical matrice
# t <- dt[ , .SD == 3, .SDcols = lb2]
# # b16_
# w <- dt[ , ..lb16]
# # kid's (2nd gen) marital statust and replace 0 with NA
# t <- w * t %>% na_if(., 0)
# 
# # dt <- as.data.frame(dt)
# # dt[ , lb16] <- t
# 
# # calculate numbers of single kids and numbers of all unmarried kids
# n.1 <- vector("integer", nrow(t))
# n.2 <- vector("integer", nrow(t))
# for(i in 1:nrow(t)) {
#         n.1[i] <- sum(t[i, ] %in% 91:96, na.rm = TRUE)
#         n.2[i] <- sum(t[i, ] == 91, na.rm = TRUE)
#         }
# dt$`n.single.kid` <- n.1
# dt$`n.all.unmarried` <- n.2
# 
# # filter all 2nd-gen are single and not all kids are unmarried
# setDT(dt)
# dt <- dt[ , hidden_single := ifelse(n.children >= 1 & 
#                                             n.single.kid == n.kid & 
#                                             !(n.single.kid == n.all.unmarried), 
#                                     1, 0)]


# read files --------------------------------------------------------------

# read rds
df.list1 <- readRDS("df.list(90-107).rds")
df.list90_103 <- df.list1[c(1:4, 9:18)]
df.list104_107 <- df.list1[5:8]
df.list79_89 <- readRDS("df.list(79-89).rds")
rm(df.list1)
gc()

# cluster -----------------------------------------------------------------

cpu.core <- detectCores() - 1L
cl <- makeCluster(cpu.core)
clusterEvalQ(cl, library(tidyverse))
clusterEvalQ(cl, library(data.table))
clusterEvalQ(cl, library(epiDisplay))

# function: prop.hiddensingle ---------------------------------------------

prop.hiddenSingle_79 <- function(df, weight) {
        
        # factor to numeric (bxx_)
        df <- as.data.frame(df)
        lb101 <- grep("^b|itm101$", names(df))
        df[ , lb101] <- lapply(df[ , lb101], as.character) %>% 
                lapply(., as.numeric) 

        dt <- setDT(df)
        
        # n.children
        lb4 <- grep("^b4_", names(dt))
        dt[ , n.children := rowSums(.SD < 18, na.rm = TRUE), .SDcols = lb4]
        
        # a18 %in% c(611, 612) 1st-gen
        dt <- dt[a18 %in% c(611, 612), ]
        
        lb <- grep("^b2|^b4_|^b16", names(dt))
        lb2 <- grep("^b2_", names(dt))
        lb4 <- grep("^b4_", names(dt))
        lb16 <- grep("^b16_", names(dt))
        
        # recode: n.kid and n.grandkid
        dt[ , n.kid := rowSums(.SD == 3, na.rm = TRUE), .SDcols = lb2]
        dt[ , n.grandkid := rowSums(.SD == 4, na.rm = TRUE), .SDcols = lb2]
        
        # t: logical matrice
        t <- dt[ , .SD == 3, .SDcols = lb2]
        # b16_
        w <- dt[ , ..lb16]
        # kid's (2nd gen) marital statust and replace 0 with NA
        t <- w * t %>% na_if(., 0)
        
        # dt <- as.data.frame(dt)
        # dt[ , lb16] <- t
        
        # calculate numbers of single kids and numbers of all unmarried kids
        n.1 <- vector("integer", nrow(t))
        n.2 <- vector("integer", nrow(t))
        for(i in 1:nrow(t)) {
                n.1[i] <- sum(t[i, ] %in% 91:96, na.rm = TRUE)
                n.2[i] <- sum(t[i, ] == 91, na.rm = TRUE)
        }
        dt$`n.single.kid` <- n.1
        dt$`n.all.unmarried` <- n.2
        
        # filter all 2nd-gen are single and not all kids are unmarried
        dt <- dt[ , hidden_single := ifelse(n.children >= 1 & 
                                                    n.grandkid >= 1 &
                                                    n.single.kid == n.kid, 
                                            1, NA)]
        df$h <- NA_integer_
        df$h[match(dt$x1, df$x1)] <- dt$hidden_single
        
        setDT(df)
        # fill in hidden single
        df[ , hidden_single := ifelse(a18 %in% c(611, 612, 621, 622, 631, 632), h, NA)]
        # fill in non-hidden single (stem-families)
        df[ , hidden_single := ifelse(a18 %in% c(611, 612, 621, 622, 631, 632) & is.na(hidden_single), 0, h)]
                
        ##### check if the weight is numeric #####
        if(!is.numeric(df[[weight]])) {
                df[[weight]] <- as.numeric(df[[weight]])
                }
        
        ##### weigh #####
        s <- df$hidden_single
        w <- df[[weight]]
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
        t <- t[-3, ]
        # add type
        t[ , `Cum. percent` := NULL]
        t[ , `Percent` := NULL]
        t[ , `type` := c("stem (without hidden-single-parent)", 
                          "stem (with hidden-single-parent)")]
        # set column order
        setcolorder(t, c(2, 1))
        # set column name
        setnames(t, c("type", df$year[1] + 1911L))
        
        return(t)
}
prop.hiddenSingle_104 <- function(df, weight) {
        
        # factor to numeric (bxx_)
        df <- as.data.frame(df)
        lb101 <- grep("^b|itm101$", names(df))
        df[ , lb101] <- lapply(df[ , lb101], as.character) %>% lapply(., as.numeric) 
        
        dt <- setDT(df)
        
        # n.children
        lb4 <- grep("^b4_", names(dt))
        dt[ , n.children := rowSums(.SD < 18, na.rm = TRUE), .SDcols = lb4]
        
        # a18 %in% c(611, 612) 1st-gen
        dt <- dt[a18 %in% c(611, 612), ]
        
        lb <- grep("^b2|^b4_|^b16", names(dt))
        lb2 <- grep("^b2_", names(dt))
        lb4 <- grep("^b4_", names(dt))
        lb16 <- grep("^b16_", names(dt))
        
        # recode: n.kid and n.grandkid
        dt[ , n.kid := rowSums(.SD == 3, na.rm = TRUE), .SDcols = lb2]
        dt[ , n.grandkid := rowSums(.SD == 4, na.rm = TRUE), .SDcols = lb2]
        
        # t: logical matrice
        t <- dt[ , .SD == 3, .SDcols = lb2]
        # b16_
        w <- dt[ , ..lb16]
        # kid's (2nd gen) marital statust and replace 0 with NA
        t <- w * t %>% na_if(., 0)
        
        # dt <- as.data.frame(dt)
        # dt[ , lb16] <- t
        
        # calculate numbers of single kids and numbers of all unmarried kids
        n.1 <- vector("integer", nrow(t))
        n.2 <- vector("integer", nrow(t))
        for(i in 1:nrow(t)) {
                n.1[i] <- sum(t[i, ] %in% 91:97, na.rm = TRUE)
                n.2[i] <- sum(t[i, ] == 91, na.rm = TRUE)
        }
        dt$`n.single.kid` <- n.1
        dt$`n.all.unmarried` <- n.2
        
        # filter all 2nd-gen are single and not all kids are unmarried
        dt <- dt[ , hidden_single := ifelse(n.children >= 1 & 
                                                    n.grandkid >= 1 &
                                                    n.single.kid == n.kid, 
                                            1, NA)]
        df$h <- NA_integer_
        df$h[match(dt$x1, df$x1)] <- dt$hidden_single
        
        setDT(df)
        
        df[ , hidden_single := ifelse(a18 %in% c(611, 612, 621, 622, 631, 632), h, NA)]
        df[ , hidden_single := ifelse(a18 %in% c(611, 612, 621, 622, 631, 632) & is.na(hidden_single), 0, h)]
                
        ##### check if the weight is numeric #####
        if(!is.numeric(df[[weight]])) {
                df[[weight]] <- as.numeric(df[[weight]])
                }
        
        ##### weigh #####
        s <- df$hidden_single
        w <- df[[weight]]
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
        t <- t[-3, ]
        # add type
        t[ , `Cum. percent` := NULL]
        t[ , `Percent` := NULL]
        t[ , `type` := c("stem (without hidden-single-parent)", 
                          "stem (with hidden-single-parent)")]
        # set column order
        setcolorder(t, c(2, 1))
        # set column name
        setnames(t, c("type", df$year[1] + 1911L))
        
        return(t)
}

# parapply ----------------------------------------------------------------

# 79_89
f.hidden79_89 <- pblapply(df.list79_89, prop.hiddenSingle_79, weight = "a21", cl = cl) %>% 
        Reduce(function(...) merge(..., all = TRUE), .)
# 90_103
f.hidden90_103 <- pblapply(df.list90_103, prop.hiddenSingle_79, weight = "a20", cl = cl) %>% 
        Reduce(function(...) merge(..., all = TRUE), .)
# 104_107
f.hidden104_107 <- pblapply(df.list104_107, prop.hiddenSingle_104, weight = "a20", cl = cl) %>% 
        Reduce(function(...) merge(..., all = TRUE), .)
# merge
f.all <- Reduce(function(...) merge(..., all = TRUE), mget(ls(pattern = "^f.hidden")))

# col order
setcolorder(f.all, sort(names(f.all)))
nmc <- c("type", "1990")
setcolorder(f.all, c(nmc, setdiff(names(f.all), nmc)))

# save to csv
write_excel_csv(f.all, "f.hidden_single.csv")

# pRate: hidden-single-parent ---------------------------------------------

recode_hidden_single <- function(df) {
        
        # factor to numeric (bxx_)
        df <- as.data.frame(df)
        lb101 <- grep("^b|itm101$", names(df))
        df[ , lb101] <- lapply(df[ , lb101], as.character) %>% lapply(., as.numeric) 
        
        dt <- setDT(df)
        
        # n.children
        lb4 <- grep("^b4_", names(dt))
        dt[ , n.children := rowSums(.SD < 18, na.rm = TRUE), .SDcols = lb4]
        
        # a18 %in% c(611, 612) 1st-gen
        dt <- dt[a18 %in% c(611, 612), ]
        
        lb <- grep("^b2|^b4_|^b16", names(dt))
        lb2 <- grep("^b2_", names(dt))
        lb4 <- grep("^b4_", names(dt))
        lb16 <- grep("^b16_", names(dt))
        
        # recode: n.kid and n.grandkid
        dt[ , n.kid := rowSums(.SD == 3, na.rm = TRUE), .SDcols = lb2]
        dt[ , n.grandkid := rowSums(.SD == 4, na.rm = TRUE), .SDcols = lb2]
        
        # t: logical matrice
        t <- dt[ , .SD == 3, .SDcols = lb2]
        # b16_
        w <- dt[ , ..lb16]
        # kid's (2nd gen) marital statust and replace 0 with NA
        t <- w * t %>% na_if(., 0)
        
        # dt <- as.data.frame(dt)
        # dt[ , lb16] <- t
        
        # calculate numbers of single kids and numbers of all unmarried kids
        n.1 <- vector("integer", nrow(t))
        n.2 <- vector("integer", nrow(t))
        for(i in 1:nrow(t)) {
                n.1[i] <- sum(t[i, ] %in% 91:97, na.rm = TRUE)
                n.2[i] <- sum(t[i, ] == 91, na.rm = TRUE)
        }
        dt$`n.single.kid` <- n.1
        dt$`n.all.unmarried` <- n.2
        
        # filter all 2nd-gen are single and not all kids are unmarried
        dt <- dt[ , hidden_single := ifelse(n.children >= 1 & 
                                                    n.grandkid >= 1 &
                                                    n.single.kid == n.kid, 
                                            1, NA)]
        df$h <- NA_integer_
        df$h[match(dt$x1, df$x1)] <- dt$hidden_single
        
        setDT(df)
        
        df[ , hidden_single := ifelse(a18 %in% c(611, 612, 621, 622, 631, 632), h, NA)]
        df[ , hidden_single := ifelse(a18 %in% c(611, 612, 621, 622, 631, 632) & is.na(hidden_single), 0, h)]
        
        return(df)
}

# df
df.list1 <- pbsapply(df.list1, recode_hidden_single, cl = cl)
df.list2 <- pbsapply(df.list2, recode_hidden_single, cl = cl)

df1 <- sapply(df.list1, filter, hidden_single == 1)
df2 <- sapply(df.list2, filter, hidden_single == 1)
# func_povertyRate
p.1 <- pbmapply(poverty_rate, df = df1, 
                threshold = threshold1, 
                year = year1, weight= "a21") %>% 
        data.frame(`year` = year1, 
                   `poverty rate` = .)

p.2 <- pbmapply(poverty_rate, df = df2, 
                threshold = threshold2, 
                year = year2, weight= "a20") %>% 
        data.frame(`year` = year2, 
                   `poverty rate` = .)
# combine and order
dt <- rbind(p.1, p.2)
dt <- dt[order(dt$year), ]
# spread
dt <- dt %>% spread(year, `poverty.rate`)
p.hidden_single <- dt %>% mutate(type = "hidden-single-parent")

# rm
rm(dt)

# column order
nmc <- c("type", "1990")
setcolorder(p.hidden_single, c(nmc, setdiff(names(p.hidden_single), nmc)))

# save to csv
write_excel_csv(p.hidden_single, "pRate.hiddenSingle.csv")

# stop cluster ------------------------------------------------------------

stopCluster(cl)
