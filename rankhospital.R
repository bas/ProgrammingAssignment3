rankhospital <- function(state, outcome, num = "best") {
	## Read outcome data
	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

	## [11] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"  
	## [17] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
	## [23] "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" 
	o <- c(11, 17, 23)
	names(o) = c("heart attack", "heart failure", "pneumonia")

	## Check that state and outcome are valid
	if (!state %in% data$State) 
		stop("invalid state")
	if (!outcome %in% names(o)) 
		stop("invalid outcome")

	## coerce columns to numeric
	suppressWarnings(data[, o] <- as.numeric(unlist(data[, o])))

	## ordering all data and removing NA
	## s <- data[order(data[, 11], data[,2], na.last = NA),]

	## split the tables by state and get the data for the given state
	s <- split(data, data$State)[[state]]
		
	## order the hospitals by outcome and name
	index <- o[outcome] 
	s <- s[order(s[index], s[2], na.last = NA),]

	## Return hospital name in that state with the given rank
	## 30-day death rate
	if (num == "best") {
		s[1, 2]
	} else if (num == "worst") {
		s[nrow(s), 2]
	} else {
		s[num, 2]
	}
}