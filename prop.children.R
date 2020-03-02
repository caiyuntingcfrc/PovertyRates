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

# recode ------------------------------------------------------------------

dt <- df.list1[[1]]
# factor to numeric (bxx_)
dt <- as.data.frame(dt, stringsAsFactors = FALSE)
lb101 <- grep("^b|itm101$", names(dt))
dt[ , lb101] <- lapply(dt[ , lb101], as.character) %>% 
        lapply(., as.numeric) 

# dt ----------------------------------------------------------------------
# setDT
setDT(dt)

# # a18 %in% c(621, 622) 2nd-gen & at least one dependent children
# dt <- dt[a18 %in% c(621, 622), ][n.children >= 1, ]

lb1 <- grep("^b1_", names(dt))
lb2 <- grep("^b2_", names(dt))
lb4 <- grep("^b4_", names(dt))
lb16 <- grep("^b16_", names(dt))

# recode: n.all
dt[ , n.all := rowSums(!(is.na(.SD)), na.rm = TRUE), .SDcols = lb1]
# recode: n.children (dependent children)
dt[ , n.children := rowSums(.SD < 18, na.rm = TRUE), .SDcols = lb4]
# recode: n.elder (>= 65)
dt[ , n.elderly := rowSums(.SD >= 65, na.rm = TRUE), .SDcols = lb4]
# recode: n.spouse
dt[ , n.spouse := rowSums(.SD == 2, na.rm = TRUE), .SDcols = lb2]
# recode: n.kid (head's kid)
dt[ , n.kid := rowSums(.SD == 3, na.rm = TRUE), .SDcols = lb2]
# recode: n.sibling
dt[ , n.sibling := rowSums(.SD == 7, na.rm = TRUE), .SDcols = lb2]
# recode: n.sbspouse
dt[ , n.sbspouse := rowSums(.SD == 8, na.rm = TRUE), .SDcols = lb2]

# filter ------------------------------------------------------------------

# kids' age
b2_kid <- dt[ ,.SD == 3, .SDcols = lb2] * dt[ , ..lb4] 
b2_kid <- b2_kid %>% na_if(., 0)

# number of dependent kids
b2_kid[ , n.dependentKid := rowSums(.SD < 18, na.rm = TRUE)]

# merge to dt
dt[ , n.dependentKid := b2_kid$n.dependentKid]

# n.children - n.dependentKid
dt[ , n.diff := n.children - n.dependentKid]

# diff
diff <- dt[n.diff != 0, ]
d2 <- diff[ , ..lb2]
d4 <- diff[ , ..lb4]
d16 <- diff[ , ..lb16]
table(diff$a18)

d2 <- diff[a18 == 422, ..lb2]
d4 <- diff[a18 == 422, ..lb4]
d16 <- diff[a18 == 422, ..lb16]

d2 <- diff[a18 == 432, ..lb2]
d4 <- diff[a18 == 432, ..lb4]
d16 <- diff[a18 == 432, ..lb16]


# prop --------------------------------------------------------------------

weight <- "a21"
n.dependentKid <- "n.dependentKid"
n.children <- "n.children"

prop.children <- function(df, weight) {
        
        ##### setDT #####
        setDT(df)
        
        ##### check if the weight is numeric #####
        if(!is.numeric(df[[weight]])) {
                df[[weight]] <- as.numeric(df[[weight]])
                }
        
        ##### weigh #####
        n <- df[["n.dependentKid"]]
        w <- df[[weight]]
        # weigh by numbers of people in the household
        w1 <- i[rep(1:length(i), times = n)]
        w2 <- w[rep(1:length(n), times = n)]
        # weight table
        wtab <- round(xtabs(df[[weight]] ~ df[["eq_inc"]]))
        # replicate income by weight
        v <- names(wtab)
        weighed <- mapply(rep, x = v, times = wtab)
        weighed <- as.numeric(unlist(weighed, use.names = TRUE))
}
