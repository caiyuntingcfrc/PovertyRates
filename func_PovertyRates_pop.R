##### Author: Cai, Yun-Ting #####
##### Date: 2020/01/22 #####

# prep and options --------------------------------------------------------

devtools::source_url("https://raw.githubusercontent.com/caiyuntingcfrc/PovertyRates/master/func_ins.pack.R")
ins.pack("tidyverse", "data.table")

# funciton ----------------------------------------------------------------

poverty_rate <- function(df, weight, 
                         n.all = "a8", 
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
        n <- df[[n.all]]
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
        # deprecated
        # weighed <- i[rep(1:length(i), times = w)]
        r <- weighed < threshold
        p <- length(r[r == TRUE]) / length(r) * 100
        return(round(p, 2))
}