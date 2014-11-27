rankhospital <- function(state, outcome, num = "best") {

	##Read outcome data
	data <- read.csv("data/rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv")
	
	##Check that state and outcome are valid
	library(datasets)
	states <- c(state.abb, "DC", "GU", "MP", "PR", "VI")

	validState <- state %in% states
	validOutcome <- outcome %in% c("heart attack", "heart failure", "pneumonia")

	if (!validState)
		stop("invalid state")

	if (!validOutcome)
		stop("invalid outcome")

	## Return hospital name in that state with the given rank
	## 30-day death rate
}