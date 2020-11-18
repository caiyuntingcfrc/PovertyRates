##### Author: Cai, Yun-Ting #####
##### Date: 2020/01/22 #####

# prep and options --------------------------------------------------------

devtools::source_url("https://raw.githubusercontent.com/caiyuntingcfrc/PovertyRates/master/func_ins.pack.R")
ins.pack("tidyverse", "data.table")

# func: pr.all ------------------------------------------------------------

pr.all <- function(df, weight, 
                   threshold , 
                   n.ppl, 
                   year) {
        ##### equivalised income #####
        setDT(df)
        # df[ , itm400 := ifelse(is.na(itm400), 0, itm400)]
        # df[ , itm600 := ifelse(is.na(itm600), 0, itm600)]
        df[ , eq_inc := (itm400 - itm600) / sqrt(a8)]
        
        ##### check if the weight is numeric #####
        if(!is.numeric(df[[weight]])) {
                df[[weight]] <- as.numeric(df[[weight]])
        }
        i <- df[["eq_inc"]]
        n <- df[[n.ppl]]
        w <- df[[weight]]
        # weigh by numbers of people in the household
        w1 <- i[rep(1:length(i), times = n)]
        w2 <- w[rep(1:length(n), times = n)]
        ##### weigh #####
        # weight table
        wtab <- round(xtabs(w2 ~ w1))
        v <- names(wtab)
        weighed <- mapply(rep, times = wtab, x = v, SIMPLIFY = TRUE)
        weighed <- as.numeric(unlist(weighed, use.names = FALSE))
        # below threshold
        r <- weighed < threshold
        p <- length(r[r == TRUE]) / length(r) * 100
        return(round(p, 2))
}