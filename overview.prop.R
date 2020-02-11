library(feather)
rm(list = ls()); cat("\14")
dt <- read_feather("df_inc107.feather")
setDT(dt)
dt <- dt[a18 %in% c(611, 612) , ]

# convert b2_ to numeric
lb <- grep("^b|itm", names(dt))
lb2 <- grep("^b2_", names(dt))
lb4 <- grep("^b4_", names(dt))
lb16 <- grep("^b16_", names(dt))
dt[ , (lb) := lapply(.SD, as.character), .SDcols = lb]
dt[ , (lb) := lapply(.SD, as.numeric), .SDcols = lb]

lb <- grep("^b2_|^b4_|^b16_", names(dt))
dtlb2 <- dt[ , ..lb2]
dtlb4 <- dt[ , ..lb4]
dtlb16 <- dt[ , ..lb16]
dt <- dt[ , which(unlist(lapply(dt, function(x)!all(is.na(x))))), with = FALSE]


m <- unique(which(d[ , ..lb2] == 1, arr.ind = TRUE))
# row and column
r <- m[ , 1]
c <- m[ , 2]
# recode h.select
d[r , h.select := c]
# recode h.marital
d[ , h.marital := .SD[[paste0("b16_", .BY$h.select)]], by = h.select]
epiDisplay::tab1(d$h.marital)

dd <- d[ , ..lb2]
dd2 <- d[ , ..lb16]
