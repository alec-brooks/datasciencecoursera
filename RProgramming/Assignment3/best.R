best <- function(state, outcome) {
        ## Read outcome data
        df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        ## Check that state and outcome are valid
        if(!state %in% df[['State']])
                stop('invalid state')
        
        if(!outcome %in% c("heart attack", "heart failure", "pneumonia"))
                stop('invalid condition')
        
        switch(outcome,
               "heart attack" = i <- 11,
               "heart failure" = i <- 17,
               "pneumonia" = i <- 23)
        options(warn=-1)
        #Remove NAs
        df[ ,i] <- as.numeric(df[ ,i])
        df <- df[complete.cases(df[, i]), ]
        options(warn=1)
        df <- df[, c(2,7,i)]
        
        #Remove states
        df <- df[df[2]==state,]
        
        ans <- which(df[3]==min(df[3]))
        sort(df[ans,'Hospital.Name'])[1]
}