rankall <- function(outcome, num = "best") {
	## Read outcome data
	file <- "data/rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv"
	data <- read.csv(file, na.strings = "Not Available", colClasses = "character")

	## Check that state and outcome are valid
	if (outcome == "heart attack")
		outcomePos <- 11
	else if (outcome == "heart failure")
		outcomePos <- 17
	else if (outcome == "pneumonia")
		outcomePos <- 23
	else
		stop("invalid outcome")

	## For each state, find the hospital of the given rank
	
	## Create data frame of just the data I want
	measures <- data[,c(2,7,outcomePos)]

	## Rename columns and outcomes to numeric
	names(measures) <- c("Hospital", "State", "Outcome")
	measures[,3] <- as.numeric(measures[,3])
	measures <- measures[complete.cases(measures),]
	
	if(num == "worst")
		measures <- measures[order(measures$Outcome, measures$Hospital, decreasing = "TRUE"),]
	else
		measures <- measures[order(measures$Outcome, measures$Hospital),]
		
	## Return a data frame with the hospital names and the
	## (abbreviated) state name
	data.split <- split(measures, measures$State)

	if (num == "best" || num == "worst")
		hospital <- sapply(data.split, function(x) x[1,1])
	else
		hospital <- sapply(data.split, function(x) x[num,1])
	state <- sapply(data.split, function(x) x[1,2])

	data.frame(hospital, state)
}