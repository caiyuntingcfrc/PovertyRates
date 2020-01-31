# prep and options --------------------------------------------------------

# clearing env and console
rm(list = ls()); cat("\14")
# source function: ins.pack
devtools::source_url("https://raw.githubusercontent.com/caiyuntingcfrc/PovertyRates/master/func_ins.pack.R")
# load packages
ins.pack("tidyverse", "magrittr", "feather", "data.table", "epiDisplay")
# setwd
setwd("D:/R_wd/tw_inc")

# read file ---------------------------------------------------------------

df <- read_feather("R data files/df_inc107.feather")

# data manipulation -------------------------------------------------------

setDT(df)
# grep
l <- grep("^b4_", names(df), value = TRUE)
# numbers of people in the household
df[ , n.all := rowSums(!is.na(.SD)), .SDcols = l]
# numbers of children (< 18)
df[ , n.children := rowSums(.SD < 18, na.rm = TRUE), .SDcols = l]
# numbers of the elderly (>= 65)
df[ , n.elderly := rowSums(.SD >= 65, na.rm = TRUE), .SDcols = l]


# recode:sf ---------------------------------------------------------------

df[a18 %in% c(101, 102), sf := 1L]
df[a18 %in% c(201, 202), sf := 2L]
df[a18 %in% c(321, 322, 331, 332), sf := 3L]
df[a18 %in% c(421, 422, 431, 432), sf := 4L]
df[a18 %in% c(511, 512, 531, 532), sf := 5L]
df[a18 %in% c(611, 612, 621, 622, 631, 632), sf := 6L]
df[a18 %in% c(701, 702), sf := 7L]

# recode sf: single parent families
df[ , sf_narrow := ifelse(a18 %in% c(321, 322) & n.children >= 1, 3.1, sf)]
# tab1(df$sf_narrow, graph = TRUE, bar.values = "percent", decimal = 2)

# prop:sf -----------------------------------------------------------------

# weigh: sf
s <- df$sf_narrow
w <- df$a20
# xtab
x <- round(xtabs(w ~ s), digits = 0); x
n <- names(x)
# weigh
weighed <- mapply(rep, n, times = x)
l <- unlist(weighed, use.names = FALSE)
t <- tab1(l, decimal = 2, graph = TRUE, bar.values = "percent") %>% 
        .[["output.table"]] %>% 
        as.data.table()

# remove cum.percentage
t <- t[-9, ]
# add type
t[ , `Cum. percent` := NULL]
t[ , `Frequency` := NULL]
t[ , type := c("single-person", 
               "married-couple", 
               "single-parent (broad)", 
               "single-parent (narrow)", 
               "nuclear", 
               "grandparent", 
               "stem", 
               "others")]
# set column order
setcolorder(t, c(2, 1))
# set column names
setnames(t, c("type", df$year[1] + 1911L)); t

# prop.sf:  by head's sex -------------------------------------------------

df[sf_narrow == 3.1 & a18 == 321, single_hsex := 1]
df[sf_narrow == 3.1 & a18 == 322, single_hsex := 2]
df[ , single_hsex := ifelse(single_hsex %in% c(1, 2), single_hsex, 3)]

# weigh: singele-parent households by head's sex
s <- df$single_hsex
w <- df$a20
# xtab
x <- round(xtabs(w ~ s), digits = 0); x
n <- names(x)
# weigh
weighed <- mapply(rep, n, times = x)
l <- unlist(weighed, use.names = FALSE)
t2 <- tab1(l, decimal = 2, graph = TRUE, bar.values = "percent") %>% 
        .[["output.table"]] %>% 
        as.data.table()

# remove cum.percentage
t2 <- t2[-c(3, 4), ]
# add type
t2[ , `Cum. percent` := NULL]
t2[ , `Frequency` := NULL]
t2[ , type := c("single-parent(narrow m headed)",
                "single-parent(narrow f headed)")]
# set column order
setcolorder(t2, c(2, 1))
# set column names
setnames(t2, c("type", df$year[1] + 1911L))
out.table <- merge(t, t2, all = TRUE); out.table

# poverty rates -----------------------------------------------------------
# equivalized income ------------------------------------------------------
n <- df[["n.all"]]
weight <- "a20"
w <- df[[weight]]
df[ , eq_inc := (itm400 - itm600) / sqrt(n.all)]

##### check if the weight is numeric #####
if(!is.numeric(df[[weight]])) {
        df[[weight]] <- as.numeric(df[[weight]])
        }
##### poverty threshold #####
# weight table
wtab <- round(xtabs(df[[weight]] ~ df[["eq_inc"]]))
# replicate income by weight
i <- names(wtab)
weighed <- mapply(rep, x = i, times = wtab)
weighed <- as.numeric(unlist(weighed, use.names = TRUE))
t <- median(weighed, na.rm = TRUE) * 0.5

# df: single parent -------------------------------------------------------

##### function p.prop #####
p.prop <- function(df, w) {
        # weight table
        wtab <- round(xtabs(df[[w]] ~ df[["eq_inc"]]))
        # replicate income by weight
        i <- names(wtab)
        weighed <- mapply(rep, x = i, times = wtab)
        weighed <- as.numeric(unlist(weighed, use.names = TRUE))
        # deprecated
        # weighed <- i[rep(1:length(i), times = w)]
        r <- weighed < t
        p <- length(r[r == TRUE]) / length(r) * 100
        return(p)
        }

##### poverty rate: single-parent families #####
dt <- df[sf == 3 & n.children >= 1, ]
p.single_broad <- p.prop(df = dt, w = weight)
names(p.single_broad) <- "single-parent (broad)"

##### poverty rate: single-parent families #####
dt <- df[sf_narrow == 3.1, ]
p.single <- p.prop(df = dt, w = weight)
names(p.single) <- "single-parent (narrow)"

##### poverty rate: single-parent families (m-headed) #####
dt <- df[single_hsex == 1, ]
p.single_m <- p.prop(df = dt, w = weight)
names(p.single_m) <- "single-parent (narrow: m-headed)"

##### poverty rate: single-parent families (f-headed) #####
dt <- df[single_hsex == 2, ]
p.single_f <- p.prop(df = dt, w = weight)
names(p.single_f) <- "single-parent (narrow: f-headed)"

##### merge #####
l <- mget(grep("^p.single", ls(), value = TRUE))
out.table <- do.call(rbind, l) %>% 
        data.table()
out.table[ , type := sapply(l, names)]
setnames(out.table, c(df$year[1] + 1911L, "type"))
setcolorder(out.table, c(2, 1))
