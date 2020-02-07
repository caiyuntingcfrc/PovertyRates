
# prep and options --------------------------------------------------------
# source function: ins.pack
devtools::source_url("https://raw.githubusercontent.com/caiyuntingcfrc/PovertyRates/master/func_ins.pack.R")
# load packages
ins.pack("magrittr", "data.table", "epiDisplay")


# function: prop.sf -------------------------------------------------------

prop.sf <- function(df, weight) {

        #####  data.table #####                
        setDT(df)
        # grep
        lb1 <- grep("^b1_", names(df), value = TRUE)
                # numbers of people in the household
        df[ , n.all := rowSums(!is.na(.SD)), .SDcols = lb1]
        
        # grep
        lb4 <- grep("^b4_", names(df), value = TRUE)
        # numbers of children (< 18)
        df[ , n.children := rowSums(.SD < 18, na.rm = TRUE), .SDcols = lb4]
        # numbers of the elderly (>= 65)
        df[ , n.elderly := rowSums(.SD >= 65, na.rm = TRUE), .SDcols = lb4]
        
        ##### recode sf #####
        df[a18 %in% c(101, 102), sf := 1L]
        df[a18 %in% c(201, 202), sf := 2L]
        df[a18 %in% c(321, 322, 331, 332), sf := 3L]
        df[a18 %in% c(421, 422, 431, 432), sf := 4L]
        df[a18 %in% c(511, 512, 531, 532), sf := 5L]
        df[a18 %in% c(611, 612, 621, 622, 631, 632), sf := 6L]
        df[a18 %in% c(701, 702), sf := 7L]
        
        ##### recode sf: single parent families #####
        df[ , sf_narrow := ifelse(sf == 3 & n.children >= 1, 3.1, sf)]
        
        ##### check if the weight is numeric #####
        if(!is.numeric(df[[weight]])) {
                df[[weight]] <- as.numeric(df[[weight]])
                }
        
        ##### prop.sf: all #####
        # weigh: sf
        s <- df$sf_narrow
        w <- df[[weight]]
        # xtab
        x <- round(xtabs(w ~ s), digits = 0); x
        n <- names(x)
        # weigh
        weighed <- mapply(rep, n, times = x)
        l <- unlist(weighed, use.names = FALSE)
        dt <- tab1(l, decimal = 2, graph = FALSE) %>% 
                .[["output.table"]] %>% 
                as.data.table()
        # remove cum.percentage
        dt <- dt[-9, ]
        # add type
        dt[ , `Cum. percent` := NULL]
        dt[ , `Percent` := NULL]
        dt[ , type := c("single-person", 
                        "married-couple", 
                        "single-parent (broad - narrow)", 
                        "single-parent (narrow)", 
                        "nuclear",
                        "grandparent", 
                        "stem", 
                        "others")]
        # set column order
        setcolorder(dt, c(2, 1))
        # set column name
        setnames(dt, c("type", df$year[1] + 1911L))
        
        ### prop.sf: single-parent households (broad)
        # weigh: sf
        s <- df$sf
        w <- df[[weight]]
        # xtab
        x <- round(xtabs(w ~ s), digits = 0); x
        n <- names(x)
        # weigh
        weighed <- mapply(rep, n, times = x)
        l <- unlist(weighed, use.names = FALSE)
        dt3 <- tab1(l, decimal = 2, graph = FALSE) %>% 
                .[["output.table"]] %>% 
                as.data.table()
        # select sf == 3 (braod)
        dt3 <- dt3[3, ]
        # add type
        dt3[ , `Cum. percent` := NULL]
        dt3[ , `Percent` := NULL]
        dt3[ , type := "single-parent (broad)"]
        # set column order
        setcolorder(dt3, c(2, 1))
        # set column name
        setnames(dt3, c("type", df$year[1] + 1911L))
        
        ##### prop.sf: single-parent households by head's sex #####
        df[sf_narrow == 3.1 & a18 %in% c(321, 331), single_hsex := 1]
        df[sf_narrow == 3.1 & a18 %in% c(322, 332), single_hsex := 2]
        df[ , single_hsex := ifelse(single_hsex %in% c(1, 2), single_hsex, 3)]
        
        # weigh: singele-parent households by head's sex
        s <- df$single_hsex
        w <- df[[weight]]
        # xtab
        x <- round(xtabs(w ~ s), digits = 0); x
        n <- names(x)
        # weigh
        weighed <- mapply(rep, n, times = x)
        l <- unlist(weighed, use.names = FALSE)
        dt2 <- tab1(l, decimal = 2, graph = FALSE) %>% 
                .[["output.table"]] %>% 
                as.data.table()
        
        # remove cum.percentage
        dt2 <- dt2[-c(3, 4), ]
        # add type
        dt2[ , `Cum. percent` := NULL]
        dt2[ , `Percent` := NULL]
        dt2[ , `type` := c("single-parent(narrow: m-headed)",
                           "single-parent(narrow: f-headed)")]
        # set column order
        setcolorder(dt2, c(2, 1))
        # set column names
        setnames(dt2, c("type", df$year[1] + 1911L))
        
        ##### prop.sf: single by sex of total single
        dt4 <- df[single_hsex %in% c(1, 2), ]
        # weigh: singele-parent households by head's sex
        s <- dt4$single_hsex
        w <- dt4[[weight]]
        # xtab
        x <- round(xtabs(w ~ s), digits = 0); x
        n <- names(x)
        # weigh
        weighed <- mapply(rep, n, times = x)
        l <- unlist(weighed, use.names = FALSE)
        dt4 <- tab1(l, decimal = 2, graph = FALSE) %>% 
                .[["output.table"]] %>% 
                as.data.table()
        
        # remove cum.percentage
        dt4 <- dt4[-3, ]
        # add type
        dt4[ , `Cum. percent` := NULL]
        dt4[ , `Percent` := NULL]
        dt4[ , `type` := c("m-headed of single-parent (narrow)",
                           "f-headed of single-parent (narrow)")]
        # set column order
        setcolorder(dt4, c(2, 1))
        # set column names
        setnames(dt4, c("type", df$year[1] + 1911L))
        
        ##### merge #####
        out.table <- rbindlist(list(dt, dt2, dt3, dt4))
        
        ##### return #####
        return(out.table)
}
