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

# prop --------------------------------------------------------------------

liv.children <- function(dt, weight) {
        
        ##### recode #####
        # factor to numeric (bxx_)
        dt <- as.data.frame(dt, stringsAsFactors = FALSE)
        lb101 <- grep("^b|itm101$", names(dt))
        dt[ , lb101] <- lapply(dt[ , lb101], as.character) %>% 
                lapply(., as.numeric)
        
        ##### setDT #####
        setDT(dt)
        
        ##### recode sums #####
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
        
        ##### filter: children (<18) #####
        # the head has dependent children
        # children's age
        b2_2nd <- dt[ ,.SD == 3, .SDcols = lb2] * dt[ , ..lb4] 
        b2_2nd <- b2_2nd %>% na_if(., 0)
        # number of dependent kids
        b2_2nd[ , n.children.children := rowSums(.SD < 18, na.rm = TRUE)]
        # add x1 for merging and subsetting
        cols <- c("x1", "n.children.children")
        b2_2nd <- b2_2nd[ , x1 := dt$x1][ , ..cols]
        
        ##### filter: grandchildren (<18) #####
        # the head has children grandkids
        # grandkid's age (b2_x == 4)
        b2_1st <- dt[ ,.SD == 4, .SDcols = lb2] * dt[ , ..lb4] 
        b2_1st <- b2_1st %>% na_if(., 0)
        # number of dependent kids
        b2_1st[ , n.children.grandchildren := rowSums(.SD < 18, na.rm = TRUE)]
        # add x1 for merging and subsetting
        cols <- c("x1", "n.children.grandchildren")
        b2_1st <- b2_1st[ , x1 := dt$x1][ , ..cols]
        
        ##### filter: siblings #####
        # the head has children siblings
        # siblings's age (b2_x == 7)
        b2_3rd <- dt[ ,.SD == 7, .SDcols = lb2] * dt[ , ..lb4] 
        b2_3rd <- b2_3rd %>% na_if(., 0)
        # number of dependent kids
        b2_3rd[ , n.children.sibling := rowSums(.SD < 18, na.rm = TRUE)]
        # add x1 for merging and subsetting
        cols <- c("x1", "n.children.sibling")
        b2_3rd <- b2_3rd[ , x1 := dt$x1][ , ..cols]
        
        ##### merge #####
        l <- list(b2_1st, b2_2nd, b2_3rd)
        tb <- Reduce(function(...) merge(..., all = TRUE), l)
        # sum
        cols <- grep("^n.children.", names(tb), value = TRUE)
        tb[ , n.dependentKids := rowSums(.SD, na.rm = TRUE), .SDcols = cols]
        # merge to dt
        dt <- merge(dt, tb, by = "x1")
        
        ##### liv of grandchildren #####
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
        
        ##### liv of children #####
        # two-parents
        # at least one children
        dt[n.children.children > 0 & 
                   # have spouse
                   n.spouse > 0, 
           # recode: children with two parents
           two_parents_children := n.children.children]
        
        # one-parent
        dt[n.children.children > 0 & 
                   # have no spouse
                   n.spouse == 0, 
           # recode: children with one parent
           one_parent_children := n.children.children]
        
        ##### liv of siblings #####
        # two-parents
        # at least one children sibling
        dt[n.children.sibling > 0 & 
                   # have two parents
                   n.parent == 2, 
           # recode: sibling with two parents
           two_parents_sibling := n.children.sibling]
        
        # one parent
        dt[n.children.sibling > 0 & 
                   # have two parents
                   n.parent == 1, 
           # recode: sibling with two parents
           one_parent_sibling := n.children.sibling]
        
        # no parents
        dt[n.children.sibling > 0 & 
                   # have two parents
                   n.parent == 0, 
           # recode: sibling with two parents
           no_parent_sibling := n.children.sibling]
        
        ##### sum #####
        # sum two parents
        cols <- grep("^two_parents_", names(dt))
        dt[ , two_parents := rowSums(.SD, na.rm = TRUE), .SDcols = cols]
        # sum one parent
        cols <- grep("^one_parent_", names(dt))
        dt[ , one_parent := rowSums(.SD, na.rm = TRUE), .SDcols = cols]
        # sum no parent
        cols <- grep("^no_parent_", names(dt))
        dt[ , no_parent := rowSums(.SD, na.rm = TRUE), .SDcols = cols]
        
        ##### check if the weight is numeric #####
        if(!is.numeric(dt[[weight]])) {
                dt[[weight]] <- as.numeric(dt[[weight]])
                }
        
        
        ##### filter: have dependent kids #####
        dt <- dt[!(two_parents == 0 & 
                           one_parent == 0 & 
                           no_parent == 0), ]
        
        ##### weigh #####
        # weight table: two-parents
        wtab <- round(xtabs(dt[[weight]] ~ dt[["two_parents"]]))
        # replicate income by weight
        v <- names(wtab)
        weighed <- mapply(rep, x = v, times = wtab)
        weighed <- as.numeric(unlist(weighed, use.names = TRUE))
        s_two_parents <- sum(weighed, na.rm = TRUE)
        
        # weight table: one-parent
        wtab <- round(xtabs(dt[[weight]] ~ dt[["one_parent"]]))
        # replicate income by weight
        v <- names(wtab)
        weighed <- mapply(rep, x = v, times = wtab)
        weighed <- as.numeric(unlist(weighed, use.names = TRUE))
        s_one_parent <- sum(weighed, na.rm = TRUE)
        
        # weight table: no-parent
        wtab <- round(xtabs(dt[[weight]] ~ dt[["no_parent"]]))
        # replicate income by weight
        v <- names(wtab)
        weighed <- mapply(rep, x = v, times = wtab)
        weighed <- as.numeric(unlist(weighed, use.names = TRUE))
        s_no_parent <- sum(weighed, na.rm = TRUE)
        
        # out.table
        out.table <- data.table(type = c("two-parents", 
                                         "one-parent", 
                                         "no-parent"), 
                                frequency = c(s_two_parents, 
                                              s_one_parent, 
                                              s_no_parent))
        # add percentage
        # out.table[ , Percent := sum(frequency)][ , Percent := frequency / Percent]
        # setnames
        setnames(out.table, c("type", dt$year[1] + 1911L))
        
        ##### return #####
        return(out.table)
}


# freq (1900 - 2018) ------------------------------------------------------

freq.list1 <- pblapply(df.list1, liv.children, weight = "a21", cl = cl)
freq.list2 <- pblapply(df.list2, liv.children, weight = "a20", cl = cl)

# stop cluster ------------------------------------------------------------

stopCluster(cl)

# merge -------------------------------------------------------------------

freq.tb1 <- Reduce(function(...) merge(..., all = TRUE), freq.list1)
freq.tb2 <- Reduce(function(...) merge(..., all = TRUE), freq.list2)
freq.tb <- merge(freq.tb1, freq.tb2, all = TRUE)

# col order
order <- sort(names(freq.tb)); order
freq.tb <- freq.tb[ , ..order]
nmc <- c("type", "1990")
setcolorder(freq.tb, c(nmc, setdiff(names(freq.tb), nmc)))

# row order
freq.tb <- freq.tb[c(3, 2, 1), ]

# save file ---------------------------------------------------------------

# rds
saveRDS(freq.tb, "liv.children.rds")
# csv
write_excel_csv(freq.tb, "liv.children.csv")

# 20200325 write df.list --------------------------------------------------

write.f <- function(dt) {
        ##### recode #####
        # factor to numeric (bxx_)
        dt <- as.data.frame(dt, stringsAsFactors = FALSE)
        lb101 <- grep("^b|itm101$", names(dt))
        dt[ , lb101] <- lapply(dt[ , lb101], as.character) %>% 
                lapply(., as.numeric)
        
        ##### setDT #####
        setDT(dt)
        
        ##### recode sums #####
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
        
        ##### filter: children (<18) #####
        # the head has dependent children
        # children's age
        b2_2nd <- dt[ ,.SD == 3, .SDcols = lb2] * dt[ , ..lb4] 
        b2_2nd <- b2_2nd %>% na_if(., 0)
        # number of dependent kids
        b2_2nd[ , n.children.children := rowSums(.SD < 18, na.rm = TRUE)]
        # add x1 for merging and subsetting
        cols <- c("x1", "n.children.children")
        b2_2nd <- b2_2nd[ , x1 := dt$x1][ , ..cols]
        
        ##### filter: grandchildren (<18) #####
        # the head has children grandkids
        # grandkid's age (b2_x == 4)
        b2_1st <- dt[ ,.SD == 4, .SDcols = lb2] * dt[ , ..lb4] 
        b2_1st <- b2_1st %>% na_if(., 0)
        # number of dependent kids
        b2_1st[ , n.children.grandchildren := rowSums(.SD < 18, na.rm = TRUE)]
        # add x1 for merging and subsetting
        cols <- c("x1", "n.children.grandchildren")
        b2_1st <- b2_1st[ , x1 := dt$x1][ , ..cols]
        
        ##### filter: siblings #####
        # the head has children siblings
        # siblings's age (b2_x == 7)
        b2_3rd <- dt[ ,.SD == 7, .SDcols = lb2] * dt[ , ..lb4] 
        b2_3rd <- b2_3rd %>% na_if(., 0)
        # number of dependent kids
        b2_3rd[ , n.children.sibling := rowSums(.SD < 18, na.rm = TRUE)]
        # add x1 for merging and subsetting
        cols <- c("x1", "n.children.sibling")
        b2_3rd <- b2_3rd[ , x1 := dt$x1][ , ..cols]
        
        ##### merge #####
        l <- list(b2_1st, b2_2nd, b2_3rd)
        tb <- Reduce(function(...) merge(..., all = TRUE), l)
        # sum
        cols <- grep("^n.children.", names(tb), value = TRUE)
        tb[ , n.dependentKids := rowSums(.SD, na.rm = TRUE), .SDcols = cols]
        # merge to dt
        dt <- merge(dt, tb, by = "x1")
        
        ##### liv of grandchildren #####
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
        
        ##### liv of children #####
        # two-parents
        # at least one children
        dt[n.children.children > 0 & 
                   # have spouse
                   n.spouse > 0, 
           # recode: children with two parents
           two_parents_children := n.children.children]
        
        # one-parent
        dt[n.children.children > 0 & 
                   # have no spouse
                   n.spouse == 0, 
           # recode: children with one parent
           one_parent_children := n.children.children]
        
        ##### liv of siblings #####
        # two-parents
        # at least one children sibling
        dt[n.children.sibling > 0 & 
                   # have two parents
                   n.parent == 2, 
           # recode: sibling with two parents
           two_parents_sibling := n.children.sibling]
        
        # one parent
        dt[n.children.sibling > 0 & 
                   # have two parents
                   n.parent == 1, 
           # recode: sibling with two parents
           one_parent_sibling := n.children.sibling]
        
        # no parents
        dt[n.children.sibling > 0 & 
                   # have two parents
                   n.parent == 0, 
           # recode: sibling with two parents
           no_parent_sibling := n.children.sibling]
        
        ##### sum #####
        # sum two parents
        cols <- grep("^two_parents_", names(dt))
        dt[ , two_parents := rowSums(.SD, na.rm = TRUE), .SDcols = cols]
        # sum one parent
        cols <- grep("^one_parent_", names(dt))
        dt[ , one_parent := rowSums(.SD, na.rm = TRUE), .SDcols = cols]
        # sum no parent
        cols <- grep("^no_parent_", names(dt))
        dt[ , no_parent := rowSums(.SD, na.rm = TRUE), .SDcols = cols]
        
        return(dt)
        }

# cluster -----------------------------------------------------------------

cpu.core <- detectCores() - 1L
cl <- makeCluster(cpu.core)
clusterEvalQ(cl, library(tidyverse))
clusterEvalQ(cl, library(magrittr))
clusterEvalQ(cl, library(data.table))

# lapply ------------------------------------------------------------------

df.list1 <- pblapply(df.list1, write.f, cl = cl)
df.list2 <- pblapply(df.list2, write.f, cl = cl)

# stop cluster ------------------------------------------------------------

stopCluster(cl)

# save files --------------------------------------------------------------

saveRDS(df.list1, "df.list(79-89).rds")
saveRDS(df.list2, "df.list(90-107).rds")
