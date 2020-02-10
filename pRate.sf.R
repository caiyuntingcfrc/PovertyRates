# prep and options --------------------------------------------------------
# rm
rm(list = ls()); cat("\14")
# source
source("~/Github_CFRC/PovertyRates/func_PovertyRates.R")
# ins.pack
ins.pack("tidyverse", "data.table", "parallel", "pbapply")
# setwd
setwd("d:/R_wd/tw_inc/R data files/")
# pboptions
pboptions("style" = 1, "use_lb" = TRUE)

# read files --------------------------------------------------------------

# file
df.list1 <- readRDS("df.list(79-89).rds")
df.list2 <- readRDS("df.list(90-107).rds")
threshold.list <- readRDS("povertyThreshold.rds")

# year
year1 <- sapply(df.list1, function(...){y <- ...[["year"]][1]}) + 1911L
year2 <- sapply(df.list2, function(...){y <- ...[["year"]][1]}) + 1911L

# threshold (79 - 89)
threshold1 <- threshold.list[match(year1, threshold.list$year), ]$threshold
# threshold (90 - 107)
threshold2 <- threshold.list[match(year2, threshold.list$year), ]$threshold


# pRate: Overall households -----------------------------------------------

# df
df1 <- df.list1
df2 <- df.list2
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
p.overall <- dt %>% mutate(type = "overall household")
# rm
rm(dt)

# pRate: single-person ----------------------------------------------------

# df
df1 <- sapply(df.list1, filter, a18 %in% c(101, 102))
df2 <- sapply(df.list2, filter, a18 %in% c(101, 102))
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
p.single <- dt %>% mutate(type = "single-person")
# rm
rm(dt)

# pRate: married-couple ---------------------------------------------------
# df
df1 <- lapply(df.list1, filter, a18 %in% c(201, 202))
df2 <- lapply(df.list2, filter, a18 %in% c(201, 202))
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
p.couple <- dt %>% mutate(type = "married-couple")
# rm
rm(dt)

# pRate: overall single-parent --------------------------------------------

# df
df1 <- lapply(df.list1, filter, a18 %in% c(321, 322, 331, 332)) %>% 
        lapply(., filter_at, vars(matches("^b4_")), any_vars(. < 18))
df2 <- lapply(df.list2, filter, a18 %in% c(321, 322, 331, 332)) %>% 
        lapply(., filter_at, vars(matches("^b4_")), any_vars(. < 18))
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
p.single_parent <- dt %>% mutate(type = "overall single-parent")
# rm
rm(dt)


# pRate: single-parent (m-headed) -----------------------------------------

# df
df1 <- lapply(df.list1, filter, a18 %in% c(321, 322, 331, 332)) %>% 
        lapply(., filter_at, vars(matches("^b4_")), any_vars(. < 18)) %>% 
        lapply(., filter, a7 == 1)
df2 <- lapply(df.list2, filter, a18 %in% c(321, 322, 331, 332)) %>% 
        lapply(., filter_at, vars(matches("^b4_")), any_vars(. < 18)) %>% 
        lapply(., filter, a7 == 1)
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
p.single_parent_m <- dt %>% mutate(type = "m-headed single-parent")
# rm
rm(dt)

# pRate: single-parent (f-headed) -----------------------------------------

# df
df1 <- lapply(df.list1, filter, a18 %in% c(321, 322, 331, 332)) %>% 
        lapply(., filter_at, vars(matches("^b4_")), any_vars(. < 18)) %>% 
        lapply(., filter, a7 == 2)
df2 <- lapply(df.list2, filter, a18 %in% c(321, 322, 331, 332)) %>% 
        lapply(., filter_at, vars(matches("^b4_")), any_vars(. < 18)) %>% 
        lapply(., filter, a7 == 2)
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
p.single_parent_f <- dt %>% mutate(type = "f-headed single-parent")
# rm
rm(dt)

# pRate: nuclear ----------------------------------------------------------

# df
df1 <- lapply(df.list1, filter, a18 %in% c(421, 422, 431, 432)) 
df2 <- lapply(df.list2, filter, a18 %in% c(421, 422, 431, 432)) 
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
p.nuclear <- dt %>% mutate(type = "nuclear")
# rm
rm(dt)

# pRate: grandparent ------------------------------------------------------

# df
df1 <- lapply(df.list1, filter, a18 %in% c(511, 512, 531, 532))
df2 <- lapply(df.list2, filter, a18 %in% c(511, 512, 531, 532)) 
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
p.grandparent <- dt %>% mutate(type = "grand-parent")
# rm
rm(dt)

# pRate: Stem -------------------------------------------------------------

# df
df1 <- lapply(df.list1, filter, a18 %in% c(611, 612, 621, 622, 631, 632))
df2 <- lapply(df.list2, filter, a18 %in% c(611, 612, 621, 622, 631, 632)) 
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
p.stem <- dt %>% mutate(type = "stem")
# rm
rm(dt)

# single-parent hidden in stem --------------------------------------------

dt <- df.list1[[1]]
dt <- dt[a18 %in% c(611, 612), ]

lb2 <- grep("^b2_", names(dt))
dt[ , n.kid := rowSums(.SD == 3, na.rm = TRUE), .SDcols = lb2]
dt[ , n.grandkid := rowSums(.SD == 4, na.rm = TRUE), .SDcols = lb2]

# filter: children >=1 and only one of the parent
dt <- dt[n.kid == 1 & n.grandkid >= 1 & n.children >= 1, ]

# the single parent's sex
m <- which(dt[ , ..lb2] == 3, arr.ind = TRUE)
# row and column
r <- m[ , 1]
c <- m[ , 2]
# recode kid.select
dt[r , kid.select := c]
# recode kid.marital
dt[ , kid.marital := .SD[[paste0("b16_", .BY$kid.select)]], by = kid.select]
epiDisplay::tab1(dt$kid.marital)

dt <- dt[kid.marital %in% c(91, 92, 93, 94, 95), ]
epiDisplay::tab1(dt$kid.marital)

# df
df <- dt
poverty_rate(dt, threshold = threshold1[1], weight = "a21")
