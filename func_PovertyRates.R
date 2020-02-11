##### Author: Cai, Yun-Ting #####
##### Date: 2020/01/22 #####

# prep and options --------------------------------------------------------

devtools::source_url("https://raw.githubusercontent.com/caiyuntingcfrc/PovertyRates/master/func_ins.pack.R")
ins.pack("tidyverse", "data.table")

# funciton ----------------------------------------------------------------

poverty_rate <- function(df, weight, 
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
        # weight table
        wtab <- round(xtabs(df[[weight]] ~ df[["eq_inc"]]))
        # replicate income by weight
        i <- names(wtab)
        weighed <- mapply(rep, x = i, times = wtab)
        weighed <- as.numeric(unlist(weighed, use.names = TRUE))
        # deprecated
        # weighed <- i[rep(1:length(i), times = w)]
        r <- weighed < threshold
        p <- length(r[r == TRUE]) / length(r) * 100
        return(round(p, 2))
}

# test load files
