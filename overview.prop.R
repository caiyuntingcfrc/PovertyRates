d <- read_feather("df_inc98.feather")
setDT(d)
d <- d[a18 %in% c(321, 322, 331, 332) , ]
l <- grep("^b16_", names(d))
# convert b2_ to numeric
d[ , (l) := lapply(.SD, as.character), .SDcols = l]
d[ , (l) := lapply(.SD, as.numeric), .SDcols = l]

# convert b2_ to numeric
lb2 <- grep("^b2_", names(d))
d[ , (lb2) := lapply(.SD, as.character), .SDcols = lb2]
d[ , (lb2) := lapply(.SD, as.numeric), .SDcols = lb2]
m <- unique(which(d[ , ..lb2] == 1, arr.ind = TRUE))
# row and column
r <- m[ , 1]
c <- m[ , 2]
# recode h.select
d[r , h.select := c]
# recode h.marital
d[ , h.marital := .SD[[paste0("b16_", .BY$h.select)]], by = h.select]
tab1(d$h.marital)
