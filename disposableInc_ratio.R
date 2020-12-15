# prep and options --------------------------------------------------------
# rm
rm(list = ls()); cat("\14")
# source
# soure func: ins.pack
devtools::source_url("https://raw.githubusercontent.com/caiyuntingcfrc/misc/function_poverty/func_ins.pack.R")
# ins.pack
ins.pack("data.table", "epiDisplay", "pastecs")
# setwd
setwd("D:/R_wd/tw_inc/R data files/")
# options
options(scipen = 999)

# readfile ----------------------------------------------------------------

df <- readRDS("df_inc108.rds")

# setDT
setDT(df)


# function ----------------------------------------------------------------

eq_inc <- function(d, weight){
        # mean disposable income
        d[ , eq_inc := (itm400 - itm600) / sqrt(a8)]
        
        # check weight
        if(!is.numeric(d[[weight]])) {
                d[[weight]] <- as.numeric(d[[weight]])
        }
        
        # weight table
        wtab <- round(xtabs(d[[weight]] ~ d[["eq_inc"]]))
        # replicate income by weight
        i <- names(wtab)
        weighed <- mapply(rep, x = i, times = wtab)
        weighed <- as.numeric(unlist(weighed, use.names = TRUE))
        return(weighed)
}

# calc: number of people --------------------------------------------------

# calc: number of adults
df[ , n.adults := rowSums(.SD >= 18, na.rm = TRUE), .SDcols = grep("^b4_", names(df))]

# calc: number of children
df[ , n.children := rowSums(.SD < 18, na.rm = TRUE), .SDcols = grep("^b4_", names(df))]

# calc: number of workers
df[ , n.workers := rowSums(.SD == 1, na.rm = TRUE), .SDcols = grep("^b13_", names(df))]

# filter: working age head (a6)
df <- df[18 <= a6 & a6 <= 64, ]

# calc: base household ----------------------------------------------------

# filter: two or more adults
d <- df[n.adults >= 2, ]

# filter: no children
d <- d[n.children == 0, ]

# filter: one worker
d <- d[n.workers > 0, ]

baseHousehold <- mean(eq_inc(d, "a20"), na.rm = TRUE)

# calc: single adult ------------------------------------------------------

# filter: single adult
d <- df[n.adults == 1, ]

# filter: at least one child
d <- d[n.children >= 1, ]

# filter: one worker
d <- d[n.workers == 1, ]

singleAdult <- mean(eq_inc(d, "a20"), na.rm = TRUE)

# calc: two or more adults with one worker --------------------------------

# filter: two or more adults
d <- df[n.adults >= 2, ]

# filter: at least one child
d <- d[n.children >= 1, ]

# filter: one worker
d <- d[n.workers == 1, ]

a2w1 <- mean(eq_inc(d, "a20"), na.rm = TRUE)


# calc: two or more adults with two or more workers ----------------------------

# filter: two or more adults
d <- df[n.adults >= 2, ]

# filter: at least one child
d <- d[n.children >= 1, ]

# filter: one worker
d <- d[n.workers >= 2, ]

a2w2 <- mean(eq_inc(d, "a20"), na.rm = TRUE)


# ratio -------------------------------------------------------------------


