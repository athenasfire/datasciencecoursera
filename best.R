best <- function(state, outcome) {
	##Read outcome data
	data <- read.csv("data/rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv")
	
	##Check that state and outcome are valid
	library(datasets)
	##states <- state.abb
	
	##outcomes <- c("heart attack", "heart failure", "pneumonia")

	validState <- state %in% state.abb
	validOutcome <- outcome %in% c("heart attack", "heart failure", "pneumonia")

	if (!validState)
		return("Invalid state")

	if (!validOutcome)
		return("Invalid outcome")


	##Return the hospital name in that state with the lowest 30-day death 
	## rate
	outcomesInState <- data[data$State == state,]

	if (outcome == "heart attack")
		outcomePos <- 11
	else if (outcome == "heart failure")
		outcomePos <- 17
	else if (outcome == "pneumonia")
		outcomePos <- 23
	else
		return("Invalid outcome")

	variableName <- outcomesInState[,c(2,outcomePos)]
	complete <- variableName[variableName[2] != "Not Available",]
	ordered <- complete[order(complete[,2], decreasing = TRUE),]

	ordered[1,1]
}