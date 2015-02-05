rankall <- function(outcome, num = "best") {
        ## Read outcome data
        r <- read.csv("outcome-of-care-measures.csv",
                      colClasses = "character")
        ## Check that state and outcome are valid
        if( ! outcome %in% c('heart attack','heart failure','pneumonia') )
                stop('invalid outcome')
        
        i <- switch(outcome,
                    'heart attack' = i <- 11,
                    'heart failure' = i <- 17,
                    'pneumonia' = i <- 23)
        
        #Make NAs
        r[, i] <- as.numeric(r[, i])
        
        #Trim unesessary content
        df <- r[c(2,7,i)]
        
        #Remove NAs and not needed states
        df <- df [complete.cases(df),]
        
       
        
        ## For each state, find the hospital of the given rank
        results <- lapply(
                split(df, df['State']), function(x) {
                        #Interperet best/worst
                        if ( num == 'best') 
                                n <- 1
                        else if ( num == 'worst') 
                                n <- nrow(x)
                        else
                                n <- num
                        x[with(x,order(x[,3], x[,1])),][n,1]
                })
        
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        a <- as.data.frame(do.call(rbind, results))
        names(a)[names(a)=='V1'] <- 'hospital'
        cbind(state = rownames(a), a)
}