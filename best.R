best <- function(state, outcome) {    
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

	## [11] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"  
	## [17] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
	## [23] "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" 
    o <- c(11, 17, 23)
	names(o) = c("heart attack","heart failure","pneumonia")
    
	## Check that state and outcome are valid
	if(!state %in% data$State) stop("invalid state")
	if(!outcome %in% names(o)) stop("invalid outcome")
		
	## coerce columns to numeric
    suppressWarnings(data[, o] <- as.numeric(unlist(data[, o])))
    
    ## split the tables by state
    s <- split(data, data$State)

    ## select state and omit na, is this the right moment?
    tx <- na.omit(s[[state]])
    
	## Return hospital name in that state with lowest 30-day death	## rate
    ## tx$Hospital.Name[tx[11] == min(tx[11])]
	
	## get and sort the result and return the first value
	## I assume sort is fine as it only has one field
	## are there examples with multiple values?
	index <- o[outcome]
	sort(tx$Hospital.Name[tx[index] == min(tx[index])])[1]	
}