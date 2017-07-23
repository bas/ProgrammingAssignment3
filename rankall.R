## Examples:
## > r <- rankall("heart attack", 4)
## > as.character(subset(r, state == "HI")$hospital)
## [1] "CASTLE MEDICAL CENTER"
## > r <- rankall("pneumonia", "worst")
## > as.character(subset(r, state == "NJ")$hospital)
## [1] "BERGEN REGIONAL MEDICAL CENTER"
## > r <- rankall("heart failure", 10)
## > as.character(subset(r, state == "NV")$hospital)
## [1] "RENOWN SOUTH MEADOWS MEDICAL CENTER"
rankall <- function(outcome, num = "best") {
	## Read outcome data
	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

	## [11] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"  
	## [17] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
	## [23] "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" 

	## Check that state and outcome are valid
	o <- c(11, 17, 23)
	names(o) = c("heart attack", "heart failure", "pneumonia")

	if (!outcome %in% names(o)) 
		stop("invalid outcome")
	
	suppressWarnings(data[, o] <- as.numeric(unlist(data[, o])))
	
	## split the tables by state
	s <- split(data, data$State)
	index <- o[outcome] 
	
	df = data.frame()
	
	## For each state, find the hospital of the given rank
	## Return a data frame with the hospital names and the
	## (abbreviated) state name
	for (i in s) {
		ranked <- i[order(i[index], i[2], na.last = NA),]
		
		if (num == "best") {
			df <- rbind(df, ranked[1, c(7, 2)])
		} else if (num == "worst") {
			df <- rbind(df, ranked[nrow(ranked), c(7, 2)])
		} else {
			df <- rbind(df, ranked[num, c(7, 2)])
		}
	}

	colnames(df) <- c("state", "hospital")
	
	df
	
}