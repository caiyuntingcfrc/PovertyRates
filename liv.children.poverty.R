# prep and options --------------------------------------------------------

# rm
rm(list = ls()); cat("\14")
# source
source("~/Github_CFRC/PovertyRates/func_PovertyRates_pop.R")
# ins.pack
ins.pack("tidyverse", "data.table", "parallel", "pbapply", "epiDisplay")
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

# read files --------------------------------------------------------------

# file
df.list1 <- readRDS("df.list(79-89).rds")
df.list2 <- readRDS("df.list(90-107).rds")
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


# df <- df.list1[[1]]
# weight <- "a21"
# n <- "two_parents"
# threshold <- threshold.list$threshold[1]

# funciton: poverty_pot ---------------------------------------------------

poverty_rate <- function(df, weight, 
                         n = "a8", 
                         threshold , 
                         year) {
        
        ##### equivalised income #####
        setDT(df)
        df[ , eq_inc := (itm400 - itm600) / sqrt(a8)]
        
        ##### check if the weight is numeric #####
        if(!is.numeric(df[[weight]])) {
                df[[weight]] <- as.numeric(df[[weight]])
                }
        
        ##### weigh #####
        i <- df[["eq_inc"]]
        n <- df[[n]]
        w <- df[[weight]]
        # weigh by numbers of people in the household
        w1 <- i[rep(1:length(i), times = n)] ; w1
        w2 <- w[rep(1:length(w), times = n)] ; w2
        # weight table
        wtab <- round(xtabs(w2 ~ w1))
        # replicate income by weight
        v <- names(wtab)
        weighed <- mapply(rep, x = v, times = wtab)
        weighed <- as.numeric(unlist(weighed, use.names = TRUE))
        # deprecated
        # weighed <- i[rep(1:length(i), times = w)]
        r <- weighed < threshold
        p <- length(r[r == TRUE]) / length(r) * 100
        return(round(p, 2))
}



# pRate: two_parents ------------------------------------------------------

p.1 <- pbmapply(poverty_rate, df = df.list1, 
                threshold = threshold1, 
                year = year1, weight= "a21", 
                n = "two_parents") %>% 
        data.frame(`year` = year1, 
                   `poverty rate` = .)

p.2 <- pbmapply(poverty_rate, df = df.list2, 
                threshold = threshold2, 
                year = year2, weight= "a20", 
                n = "two_parents") %>% 
        data.frame(`year` = year2, 
                   `poverty rate` = .)
# combine and order
dt <- rbind(p.1, p.2)
dt <- dt[order(dt$year), ]
# spread
dt <- dt %>% spread(year, `poverty.rate`)
p.two_parent <- dt %>% mutate(type = "living with two parents")
# rm
rm(dt)

# pRate: one_parent -------------------------------------------------------

p.1 <- pbmapply(poverty_rate, df = df.list1, 
                threshold = threshold1, 
                year = year1, weight= "a21", 
                n = "one_parent") %>% 
        data.frame(`year` = year1, 
                   `poverty rate` = .)

p.2 <- pbmapply(poverty_rate, df = df.list2, 
                threshold = threshold2, 
                year = year2, weight= "a20", 
                n = "one_parent") %>% 
        data.frame(`year` = year2, 
                   `poverty rate` = .)
# combine and order
dt <- rbind(p.1, p.2)
dt <- dt[order(dt$year), ]
# spread
dt <- dt %>% spread(year, `poverty.rate`)
p.one_parent <- dt %>% mutate(type = "living with one parent")
# rm
rm(dt)

# pRate: no parent --------------------------------------------------------

p.1 <- pbmapply(poverty_rate, df = df.list1, 
                threshold = threshold1, 
                year = year1, weight= "a21", 
                n = "no_parent") %>% 
        data.frame(`year` = year1, 
                   `poverty rate` = .)

p.2 <- pbmapply(poverty_rate, df = df.list2, 
                threshold = threshold2, 
                year = year2, weight= "a20", 
                n = "no_parent") %>% 
        data.frame(`year` = year2, 
                   `poverty rate` = .)
# combine and order
dt <- rbind(p.1, p.2)
dt <- dt[order(dt$year), ]
# spread
dt <- dt %>% spread(year, `poverty.rate`)
p.no_parent <- dt %>% mutate(type = "living with no parent")
# rm
rm(dt)

# stop cluster ------------------------------------------------------------

stopCluster(cl)

# pRate.dt ----------------------------------------------------------------

l <- grep("^p\\.", ls(), value = TRUE) %>% .[!(. %in% c("p.1", "p.2"))]; l
l <- mget(l)
pr.dt <- rbindlist(l)

# column order
nmc <- c("type", "1990")
setcolorder(pr.dt, c(nmc, setdiff(names(pr.dt), nmc)))
# row order
pr.dt <- pr.dt[c(3, 2, 1), ]

# save csv file -----------------------------------------------------------

write_excel_csv(pr.dt, "liv.children.pRate.csv")
