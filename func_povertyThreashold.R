##### author: CAI YUN-TING ######
##### prep and options #####
##### source function: ins.pak #####
devtools::source_url("https://raw.githubusercontent.com/caiyuntingcfrc/misc/function_poverty/func_ins.pack.R")
ins.pack("tidyverse")
##### function -- poverty threshold #####
poverty_threshold <- function(df, weight, 
                         n.all = "a8", sex = "a7", aged = "a19", 
                         type = "a18", n.adult = "a12") {
        
        ##### equivalised income #####
        n <- df[[n.all]]
        df <- df %>% 
                mutate(sqrt_scale = sqrt(n),
                       eq_inc = (itm400 - itm600) / sqrt_scale)
        
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
        # deprecated
        # calculate the median and poverty threshold
        t <- median(weighed, na.rm = TRUE) * 0.5
        
        out <- data.frame(year = 1911 + df$year[1], 
                          threshold = t)
        return(out)
        
        }