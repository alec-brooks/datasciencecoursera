rankhospital <- function(state, outcome, num = "best") {
	## Read outcome data
	r <- read.csv("outcome-of-care-measures.csv",
		     colClasses = "character")

	## Check that state and outcome are valid
	if( ! state %in% r[['State']] )
	       stop('invalid state')
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
	df <- df[df[,'State']==state,]

	#Interperet best/worst
	if ( num == 'best') 
		n <- 1
	else if ( num == 'worst') 
		n <- nrow(df)
	else
		n <- num
        
	df[with(df,order(df[,3], df[,1])),][n,1]

}
