## Úkolem je napsat funkci, která vybere nejlepší nemocnici v zadaném státì 
## podle nejnižší úmrtnosti pacientù na zadanou komplikaci.
best <- function(state, outcome) {
	## Read outcome data
# data <- read.csv ("D:/user/Desktop/Programming Assignment 3/outcome-of-care-measures.csv")
data <- read.csv ("outcome-of-care-measures.csv")
	## Pøejmenovala jsem potøebné sloupce, aby se s nimi jednodušeji pracovalo..
	colnames(data)[11] <- "heart attack"  # blank space might cause problems
	colnames(data)[17] <- "heart failure" # the same problem
	colnames(data)[23] <- "pneumonia"
	## Vytvoøila jsem subset, obsahuje data zadaného státu

	if (!is.element(state, data$State)){
	 stop("invalid state")
	}

	condition <- c("heart attack","heart failure","pneumonia")
	if (max(as.numeric(condition %in% outcome)) == 0){
	 stop("invalid outcome")
	}

	subset.state <- subset(data, State==state, c(outcome, "Hospital.Name") ) ## I can subset certain column but I need to replace it with "outcome"
	## Tady mi to pøestává fungovat... Využila jsem rady z diskuzního fóra a zkusila pøevést data 
	## na numerická a následnì je seøadit, ale nefunguje to...
 numeric.data <- suppressWarnings( data.frame(
 outcome = as.numeric(as.character(subset.state[ ,1])), 
 Hospital.Name = subset.state$Hospital.Name ) )

 foo <- as.character(numeric.data[which.min(numeric.data[ ,1]), 2])
 foo
}







