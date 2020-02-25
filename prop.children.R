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


# prop --------------------------------------------------------------------

df <- df.list1[[1]]
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
dt[ , n.kidsSpouse := rowSums(.SD == 8, na.rm = TRUE), .SDcols = lb2]

d1 <- dt[
                n.grandkid >= 1 & 
                n.children >= 1 & 
                n.single.kid == n.kid, ..lb2]
d2 <- dt[
                n.grandkid >= 1 & 
                n.children >= 1 & 
                n.single.kid == n.kid, ..lb16]

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




n.hidden <- table(df$h)

setDT(df)
# fill in hidden single
# df[ , hidden_single := ifelse(a18 %in% c(611, 612, 621, 622, 631, 632), h, NA)]
# fill in non-hidden single (stem-families)
# df[ , hidden_single := ifelse(a18 %in% c(611, 612, 621, 622, 631, 632) & is.na(hidden_single), 0, h)]

