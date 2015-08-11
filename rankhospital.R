rankhospital <- function(state, outcome, num = "best") {
	## Read outcome data - the same as best.R
	data <- read.csv ("D:/user/Desktop/Programming Assignment 3/outcome-of-care-measures.csv")
	colnames(data)[11] <- "heart attack"  # blank space might cause problems
	colnames(data)[17] <- "heart failure" # the same problem
	colnames(data)[23] <- "pneumonia"
	## Check that state and outcome are valid - the same as best.R
	if (!is.element(state, data$State)){
	 stop("invalid state")
	}
	condition <- c("heart attack","heart failure","pneumonia")
	if (max(as.numeric(condition %in% outcome)) == 0){
	 stop("invalid outcome")
	}
	## Return hospital name in that state with the given rank
	## 30-day death rate
	subset.state <- subset(data, State==state, c(outcome, "Hospital.Name"))
	numeric.data <- suppressWarnings( data.frame(
 	outcome = as.numeric(as.character(subset.state[ ,1])), 
 	Hospital.Name = subset.state$Hospital.Name ) )
	## this function return the data frame with hospital name, rate of mortality
	## and rank of the mortality rate. I have troubles when I want to exclude NA values.
	## Number of rows differs.
	ranked.data <- data.frame(Hospital.Name = numeric.data$Hospital.Name, 
	rate = numeric.data[ ,1], rank = rank(numeric.data[ ,1], ties.method= "first", na.last = NA))
	## This function order the ranked.data. I need to order data in alphabetical
	## order based on hospital name.	
	ordered <- ranked.data[order(ranked.data$rank) , ]
}



