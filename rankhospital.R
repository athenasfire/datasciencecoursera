rankhospital <- function(state, outcome, num = "best") {

	##Read outcome data
	file <- "data/rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv"
	data <- read.csv(file, na.strings = "Not Available", colClasses = "character")
	
	##Check that state and outcome are valid
	library(datasets)
	states <- c(state.abb, "DC", "GU", "MP", "PR", "VI")

	validState <- state %in% states
	validOutcome <- outcome %in% c("heart attack", "heart failure", "pneumonia")

	if (!validState)
		stop("invalid state")

	if (outcome == "heart attack")
		outcomePos <- 11
	else if (outcome == "heart failure")
		outcomePos <- 17
	else if (outcome == "pneumonia")
		outcomePos <- 23
	else
		stop("invalid outcome")

	## Return hospital name in that state with the given rank
	## 30-day death rate

	## Create data frame of just the data I want
	measures <- data[,c(2,7,outcomePos)]

	## Rename columns and outcomes to numeric
	names(measures) <- c("Hospital", "State", "Outcome")
	measures[,3] <- as.numeric(measures[,3])
	measures <- measures[complete.cases(measures),]
	measures <- measures[order(measures$Outcome),]

	m.state <- measures[measures$State == state,]

	numRows <- nrow(m.state)

	
	if (num == "best")
		answer <- m.state[1,1]
	else if (num == "worst")
		answer <- m.state[numRows,1]
	else if (num > numRows)
		answer <- NA
	else
		answer <- m.state[num,1]

	answer
}