
# prep and options --------------------------------------------------------
# source function: ins.pack
devtools::source_url("https://raw.githubusercontent.com/caiyuntingcfrc/PovertyRates/master/func_ins.pack.R")
# load packages
ins.pack("magrittr", "data.table", "epiDisplay", "rlist")


# function: prop.sf -------------------------------------------------------

prop.sf <- function(df, weight) {

        dfList <- list()
        #####  data.table #####   
        setDT(df)
        # grep
        lb1 <- grep("^b1_", names(df))
        # numbers of people in the household
        df[ , n.all := rowSums(!is.na(.SD), na.rm = TRUE), .SDcols = lb1]
        # grep
        lb4 <- grep("^b4_", names(df))
        # numbers of children (< 18)
        df[ , n.children := rowSums(.SD < 18, na.rm = TRUE), .SDcols = lb4]
        # numbers of the elderly (>= 65)
        df[ , n.elderly := rowSums(.SD >= 65, na.rm = TRUE), .SDcols = lb4]
        
        ##### recode sf #####
        # single person
        df[a18 %in% c(101, 102), sf := 1L]
        # married couple
        df[a18 %in% c(201, 202), sf := 2L]
        # single-parent
        df[a18 %in% c(321, 322, 331, 332), sf := 3L]
        # nuclear
        df[a18 %in% c(421, 422, 431, 432), sf := 4L]
        # grandparent
        df[a18 %in% c(511, 512, 531, 532), sf := 5L]
        # stem
        df[a18 %in% c(611, 612, 621, 622, 631, 632), sf := 6L]
        # others
        df[a18 %in% c(701, 702), sf := 7L]
        
        ##### recode sf: single parent families #####
        df[ , sf_narrow := ifelse(sf == 3 & n.children >= 1, 3.1, sf)]
        
        ##### check if the weight is numeric #####
        if(!is.numeric(df[[weight]])) {
                df[[weight]] <- as.numeric(df[[weight]])
                }
        
        ##### 1. prop.sf: all #####
        # weigh: sf
        s <- df$sf_narrow
        w <- df[[weight]]
        # xtab
        x <- round(xtabs(w ~ s), digits = 0)
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
        dfList <- list.append(dfList, dt)
        
        ##### 2. prop.sf: single-parent households by head's sex #####
        df[sf_narrow == 3.1 & a18 %in% c(321, 331), single_hsex := 1]
        df[sf_narrow == 3.1 & a18 %in% c(322, 332), single_hsex := 2]
        df[ , single_hsex := ifelse(single_hsex %in% c(1, 2), single_hsex, 3)]
        
        # weigh: singele-parent households by head's sex
        s <- df$single_hsex
        w <- df[[weight]]
        # xtab
        x <- round(xtabs(w ~ s), digits = 0)
        n <- names(x)
        # weigh
        weighed <- mapply(rep, n, times = x)
        l <- unlist(weighed, use.names = FALSE)
        dt <- tab1(l, decimal = 2, graph = FALSE) %>% 
                .[["output.table"]] %>% 
                as.data.table()
        
        # remove cum.percentage
        dt <- dt[-c(3, 4), ]
        # add type
        dt[ , `Cum. percent` := NULL]
        dt[ , `Percent` := NULL]
        dt[ , `type` := c("single-parent(narrow: m-headed)",
                           "single-parent(narrow: f-headed)")]
        # set column order
        setcolorder(dt, c(2, 1))
        # set column names
        setnames(dt, c("type", df$year[1] + 1911L))
        # append
        dfList <- list.append(dfList, dt)
        
        ##### 3. prop.sf: overall by head's sex #####
        # filter out head in the house, returns logical array
        
        a <- df[ , .SD == "01" | .SD == 1, .SDcols = grep("^b2_", names(df))]
        # b3_ (sex) from factor to numeric
        b <- df[ , lapply(.SD, as.character), .SDcols = grep("^b3_", names(df))]
        b <- b[ , lapply(.SD, as.numeric)]
        c <- (a * b != 0) * b
        c[ , s := rowSums(.SD, na.rm = TRUE)]
        # assign headSex
        df[ , headSex := c$s]
        
        # weigh
        s <- df$headSex
        w <- df[[weight]]
        # xtab
        x <- round(xtabs(w ~ s), digits = 0)
        n <- names(x)
        # weigh
        weighed <- mapply(rep, n, times = x)
        l <- unlist(weighed, use.names = FALSE)
        dt <- tab1(l, decimal = 2, graph = FALSE) %>% 
                .[["output.table"]] %>% 
                as.data.table()
        
        # remove cum.percentage
        dt <- dt[-3, ]
        # add type
        dt[ , `Cum. percent` := NULL]
        dt[ , `Percent` := NULL]
        dt[ , `type` := c("male-headed",
                          "female-headed")]
        # set column order
        setcolorder(dt, c(2, 1))
        # set column names
        setnames(dt, c("type", df$year[1] + 1911L))
        # append
        dfList <- list.append(dfList, dt)
        
        ##### 4. prop.sf: with/without aged #####
        df[ , withAged := ifelse(n.elderly > 0, 1, 0)]
        # weigh
        s <- df$withAged
        w <- df[[weight]]
        # xtab
        x <- round(xtabs(w ~ s), digits = 0)
        n <- names(x)
        # weigh
        weighed <- mapply(rep, n, times = x)
        l <- unlist(weighed, use.names = FALSE)
        dt <- tab1(l, decimal = 2, graph = FALSE) %>% 
                .[["output.table"]] %>% 
                as.data.table()
        
        # remove cum.percentage
        dt <- dt[-3, ]
        # add type
        dt[ , `Cum. percent` := NULL]
        dt[ , `Percent` := NULL]
        dt[ , `type` := c("without-aged",
                          "with-aged")]
        # set column order
        setcolorder(dt, c(2, 1))
        # set column names
        setnames(dt, c("type", df$year[1] + 1911L))
        # append
        dfList <- list.append(dfList, dt)
        
        ##### 5. prop.sf: with/without children #####
        
        df[ , withChildren := ifelse(n.children > 0, 1, 0)]
        # weigh
        s <- df$withChildren
        w <- df[[weight]]
        # xtab
        x <- round(xtabs(w ~ s), digits = 0)
        n <- names(x)
        # weigh
        weighed <- mapply(rep, n, times = x)
        l <- unlist(weighed, use.names = FALSE)
        dt <- tab1(l, decimal = 2, graph = FALSE) %>% 
                .[["output.table"]] %>% 
                as.data.table()
        
        # remove cum.percentage
        dt <- dt[-3, ]
        # add type
        dt[ , `Cum. percent` := NULL]
        dt[ , `Percent` := NULL]
        dt[ , `type` := c("without-children",
                          "with-children")]
        # set column order
        setcolorder(dt, c(2, 1))
        # set column names
        setnames(dt, c("type", df$year[1] + 1911L))
        # append
        dfList <- list.append(dfList, dt)
        
        ###### 6. prop.sf: children population #####
        s <- df$n.children
        w <- df[[weight]]
        # xtab
        x <- round(xtabs(w ~ s), digits = 0)
        n <- as.numeric(names(x))
        # weigh
        weighed <- mapply(rep, n, times = x)
        l <- unlist(weighed, use.names = FALSE)
        dt <- tab1(l, decimal = 2, graph = FALSE) %>% 
                .[["output.table"]] %>% 
                as.data.table()
        dt[ , `Cum. percent` := NULL]
        dt[ , `Percent` := NULL]
        dt <- dt[-8, ]
        dt <- dt[ , `n` := n * Frequency]
        dt <- dt[ , Frequency := NULL]
        overallChildren <- sum(dt$n, na.rm = TRUE)
        # add type
        dt <- data.table(overallChildren = overallChildren, 
                         type = c("Overall children"))
        # set column order
        setcolorder(dt, c(2, 1))
        # set column names
        setnames(dt, c("type", df$year[1] + 1911L))
        # append
        dfList <- list.append(dfList, dt)
        
        ###### 7. prop.sf: elderly population #####
        s <- df$n.elderly
        w <- df[[weight]]
        # xtab
        x <- round(xtabs(w ~ s), digits = 0)
        n <- as.numeric(names(x))
        # weigh
        weighed <- mapply(rep, n, times = x)
        l <- unlist(weighed, use.names = FALSE)
        dt <- tab1(l, decimal = 2, graph = FALSE) %>% 
                .[["output.table"]] %>% 
                as.data.table()
        dt[ , `Cum. percent` := NULL]
        dt[ , `Percent` := NULL]
        dt <- dt[-7, ]
        dt <- dt[ , `n` := n * Frequency]
        dt[ , Frequency := NULL]
        overallElderly <- sum(dt$n, na.rm = TRUE)
        # add type
        dt <- data.table(overallElderly = overallElderly, 
                         type = c("Overall elderly"))
        # set column order
        setcolorder(dt, c(2, 1))
        # set column names
        setnames(dt, c("type", df$year[1] + 1911L))
        # append
        dfList <- list.append(dfList, dt)
        
        ##### 8. prop.sf: overall population #####
        s <- df$n.all
        w <- df[[weight]]
        # xtab
        x <- round(xtabs(w ~ s), digits = 0)
        n <- as.numeric(names(x))
        # weigh
        weighed <- mapply(rep, n, times = x)
        l <- unlist(weighed, use.names = FALSE)
        dt <- tab1(l, decimal = 2, graph = FALSE) %>% 
                .[["output.table"]] %>% 
                as.data.table()
        dt[ , `Cum. percent` := NULL]
        dt[ , `Percent` := NULL]
        dt <- dt[1:nrow(dt) - 1, ]
        dt <- dt[ , `n` := n * Frequency]
        dt[ , Frequency := NULL]
        overallPop <- sum(dt$n, na.rm = TRUE)
        # add type
        dt <- data.table(overallPop = overallPop, 
                         type = c("Overall population"))
        # set column order
        setcolorder(dt, c(2, 1))
        # set column names
        setnames(dt, c("type", df$year[1] + 1911L))
        # append
        dfList <- list.append(dfList, dt)
        
        ##### merge #####
        out.table <- rbindlist(dfList)
        
        ##### return #####
        return(out.table)
}

