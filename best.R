best <- function(state, outcome) {
	## Read outcome data
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
		else {
			optionIndex <- outcomeIndex[match(outcome, vOutcomes)]			
			readFile[,optionIndex] <- as.numeric(readFile[,optionIndex])
			data_state <- readFile[readFile$State == state, ]
			data_mortality <- data_state[, optionIndex]
			min_mortality <- min(data_mortality, na.rm=T)
			min_index <- which(data_mortality == min_mortality)
			hosp_name <- data_state[min_index, 2]
			return(hosp_name)
		}
	}
	

	## Return hospital name in the state with lowest 30-day death
	## rate
}
