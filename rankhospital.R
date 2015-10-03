rankhospital <- function(state, outcome, num = "best"){
	## Read Outcome data
	readFile <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	head(readFile)
	vOutcomes <- c("heart attack", "heart failure", "pneumonia")
	outcomeIndex <-c(11, 17, 23)

	## Check the state and outcome are valid
	if(!state %in% readFile$State) {
		stop("invalid state")
	}
	else { 
		if(!outcome %in% vOutcomes) {
			stop("invalid outcome")
		}
	}

	optionIndex <- outcomeIndex[match(outcome, vOutcomes)]
	readFile[,optionIndex] <- as.numeric(readFile[, optionIndex])
	data_state <- readFile[readFile$State == state,]
	data_state <- na.omit(data_state)

	if(num == "best") {
		num = 1
	}else if(num == "worst") {
		num = nrow(data_state)
	}else if(is.numeric(x=num)) {
		##if(num < 1 || num > nrow(data_state)) {
			##return(NA)
		##}
	}
	else {
		stop("invalid num")
	}
	
	data_state <- data_state[order(data_state[,optionIndex], data_state[,2]),]

	return(data_state[num, 2])
	
	## Return hospital nam in that state with the given rank
	## 30-day death rate
}
