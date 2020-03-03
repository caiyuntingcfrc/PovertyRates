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

# recode sums -------------------------------------------------------------

lb1 <- grep("^b1_", names(dt))
lb2 <- grep("^b2_", names(dt))
lb4 <- grep("^b4_", names(dt))
lb16 <- grep("^b16_", names(dt))

# recode: n.all
dt[ , n.all := rowSums(!(is.na(.SD)), na.rm = TRUE), .SDcols = lb1]
# recode: n.children (dependent children)
dt[ , n.allChildren := rowSums(.SD < 18, na.rm = TRUE), .SDcols = lb4]
# recode: n.elder (>= 65)
dt[ , n.elderly := rowSums(.SD >= 65, na.rm = TRUE), .SDcols = lb4]

# recode: n.spouse
dt[ , n.spouse := rowSums(.SD == 2, na.rm = TRUE), .SDcols = lb2]
# recode: n.kid (head's kid)
dt[ , n.kid := rowSums(.SD == 3, na.rm = TRUE), .SDcols = lb2]
# recode: n.grandkid (head's grandkid)
dt[ , n.grandkid := rowSums(.SD == 4, na.rm = TRUE), .SDcols = lb2]
# recode: n.parents
dt[ , n.parent := rowSums(.SD == 5, na.rm = TRUE), .SDcols = lb2]
# recode: n.grandparents
dt[ , n.grandparent := rowSums(.SD == 6, na.rm = TRUE), .SDcols = lb2]
# recode: n.sibling
dt[ , n.sibling := rowSums(.SD == 7, na.rm = TRUE), .SDcols = lb2]
# recode: n.kidSpouse
dt[ , n.kidSpouse := rowSums(.SD == 8, na.rm = TRUE), .SDcols = lb2]

# filter: 2nd-gen head ----------------------------------------------------

# the head has dependent children
# kids' age
b2_2nd <- dt[ ,.SD == 3, .SDcols = lb2] * dt[ , ..lb4] 
b2_2nd <- b2_2nd %>% na_if(., 0)

# number of dependent kids
b2_2nd[ , n.children.children := rowSums(.SD < 18, na.rm = TRUE)]
# add x1 for merging and subsetting
cols <- c("x1", "n.children.children")
b2_2nd <- b2_2nd[ , x1 := dt$x1][ , ..cols]
# number of dependent children
s2 <- sum(b2_2nd$n.children.children)

# filter: 3rd-gen head ----------------------------------------------------

# the head has children siglings
# siblings's age (b2_x == 7)
b2_3rd <- dt[ ,.SD == 7, .SDcols = lb2] * dt[ , ..lb4] 
b2_3rd <- b2_3rd %>% na_if(., 0)

# number of dependent kids
b2_3rd[ , n.children.sibling := rowSums(.SD < 18, na.rm = TRUE)]
# add x1 for merging and subsetting
cols <- c("x1", "n.children.sibling")
b2_3rd <- b2_3rd[ , x1 := dt$x1][ , ..cols]
# numberof children sibling
s3 <- sum(b2_3rd$n.children.sibling)

# filter: 1st-gen head ----------------------------------------------------

# the head has children grandkids
# grandkid's age (b2_x == 4)
b2_1st <- dt[ ,.SD == 4, .SDcols = lb2] * dt[ , ..lb4] 
b2_1st <- b2_1st %>% na_if(., 0)

# number of dependent kids
b2_1st[ , n.children.grandchildren := rowSums(.SD < 18, na.rm = TRUE)]
# add x1 for merging and subsetting
cols <- c("x1", "n.children.grandchildren")
b2_1st <- b2_1st[ , x1 := dt$x1][ , ..cols]
# number of children grandchildren
s1 <- sum(b2_1st$n.children.grandchildren)

# merge -------------------------------------------------------------------

l <- list(b2_1st, b2_2nd, b2_3rd)
tb <- Reduce(function(...) merge(..., all = TRUE), l)
# sum
cols <- grep("^n.children.", names(tb), value = TRUE)
tb[ , n.dependentKids := rowSums(.SD, na.rm = TRUE), .SDcols = cols]
# merge to dt
dt <- merge(dt, tb, by = "x1")

# sum of dependent kids ---------------------------------------------------

s.allKids <- sum(dt$n.dependentKids)
s.allChildren <- sum(dt$n.allChildren)

# filter ------------------------------------------------------------------

cols <- c("x1", "n.children.children", 
          "n.children.sibling", 
          "n.children.grandchildren",
          "n.spouse", 
          "n.dependentKids", 
          "n.allChildren", 
          "n.kid", 
          "n.kidSpouse", 
          "a18")

# liv of grandchildren ----------------------------------------------------

# two-parents
# at least one grandchildren
dt[n.children.grandchildren > 0 & 
           # head has at least one kid and kid's spouse
           n.kid > 0 & 
           n.kidSpouse > 0, 
   # recode: grandchildren with two parents
   two_parents_grandchildren := n.children.grandchildren]

# one-parent
dt[n.children.grandchildren > 0 & 
           # head has no kid or no kid's spouse
           (n.kid == 0 | n.kidSpouse == 0) & 
           # kid and kid's spouse cannot be both absent
           !(n.kid == 0 & n.kidSpouse == 0), 
   # recode: grandchildren with one parent
   one_parent_grandchildren := n.children.grandchildren]

# no-parents
dt[n.children.grandchildren > 0 & 
           # kid and kid's spouse are both absent
           (n.kid == 0 & n.kidSpouse == 0), 
   # recode: grandchildren with no parents
   no_parent_grandchildren := n.children.grandchildren]

# liv of children ---------------------------------------------------------

# two-parents
# at least one children
dt[n.children.children > 0 & 
           # have spouse
           n.spouse > 0, 
   # recode: children with two parents
   two_parents_children := n.children.children]
sum(dt$two_parents_children, na.rm = TRUE)

# one-parent
dt[n.children.children > 0 & 
           # have no spouse
           n.spouse == 0, 
   # recode: children with one parent
   one_parent_children := n.children.children]
sum(dt$one_parent_children, na.rm = TRUE)

# liv of sibling ----------------------------------------------------------

# two-parents
# at least one children sibling
dt[n.children.sibling > 0 & 
           # have two parents
           n.parent == 2, 
   # recode: sibling with two parents
   two_parents_sibling := n.children.sibling]
sum(dt$two_parents_sibling, na.rm = TRUE)

# one parent
dt[n.children.sibling > 0 & 
           # have two parents
           n.parent == 1, 
   # recode: sibling with two parents
   one_parent_sibling := n.children.sibling]
sum(dt$one_parent_sibling, na.rm = TRUE)

# no parents
dt[n.children.sibling > 0 & 
           # have two parents
           n.parent == 0, 
   # recode: sibling with two parents
   no_parent_sibling := n.children.sibling]
sum(dt$no_parent_sibling, na.rm = TRUE)

# sum ---------------------------------------------------------------------

# sum two parents
cols <- grep("^two_parents_", names(dt))
dt[ , two_parents := rowSums(.SD, na.rm = TRUE), .SDcols = cols]
# sum one parent
cols <- grep("^one_parent_", names(dt))
dt[ , one_parent := rowSums(.SD, na.rm = TRUE), .SDcols = cols]
# sum no parent
cols <- grep("^no_parent_", names(dt))
dt[ , no_parent := rowSums(.SD, na.rm = TRUE), .SDcols = cols]

# check
cols <- grep("^two_parents|^one_parent|^no_parent", names(dt), value = TRUE)
utils::View(dt[ , ..cols])

with(dt, sum(two_parents, na.rm = TRUE))
with(dt, sum(one_parent, na.rm = TRUE))
with(dt, sum(no_parent, na.rm = TRUE))


# special cases -----------------------------------------------------------

# cols
cols <- grep("^n.children.|n.dependentKids|n.allChildren", names(dt))

# stem-families
d_stem <- dt[n.children.children > 0 &
                     n.children.grandchildren > 0, ][ , ..cols]
# other
d_other <- dt[n.children.children > 0 & 
                      n.children.sibling > 0, ][ , ..cols]
# other
d_o <- dt[n.children.children > 0 & 
                  n.children.grandchildren > 0 & 
                  n.children.sibling == 0, ][ , ..cols]

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
