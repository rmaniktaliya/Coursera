rankall <- function(outcome, num = "best") {

	readFile <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	head(readFile)
	vOutcomes <- c("heart attack", "heart failure", "pneumonia")
	##outcomeIndex <-c(11, 17, 23)

	## Check the state and outcome are valid
	if(!outcome %in% vOutcomes) {
		stop("invalid outcome")
	}
	
	names(readFile)[c(11, 17, 23)] <- vOutcomes
	##optionIndex <- outcomeIndex[match(outcome, vOutcomes)]
	readFile <- readFile[, c("State", "Hospital.Name", outcome)]
	readFile[,outcome] <- suppressWarnings(as.numeric(readFile[, outcome]))
	readFile <- readFile[!is.na(readFile[outcome]),]
	readFile <- readFile[order(readFile$State, readFile[outcome], readFile$Hospital.Name),]
	##state_split = split(readFile, readFile$State)

	rankByState <- aggregate(readFile, by=list(readFile$State), function(x) {

		## For each state, find the hospital of the given rank
		if(!is.numeric(num)) {
			if(num == "best") {
				num <- 1
			}else if(num == "worst") {
				num <- length(x)
			}
			else {
				stop("invalid num")
			}
		}
		x[num]
		})		
	
	out <- rankByState[,c(3,1)]
	names(out) <- c("hospital", "state")

	## Return a data frame with the hospital names and the (abbreviated) state name
	return(out)
}
