## Úkolem je napsat funkci, která vybere nejlepší nemocnici v zadaném státě 
## podle nejnižší úmrtnosti pacientů na zadanou komplikaci.
best <- function(state, outcome) {
	## Read outcome data
	data <- read.csv ("D:/user/Desktop/Programming Assignment 3/outcome-of-care-measures.csv")
	## Přejmenovala jsem potřebné sloupce, aby se s nimi jednodušeji pracovalo..
	colnames(data)[11] <- "heart.attack"  # blank space might cause problems
	colnames(data)[17] <- "heart.failure" #  the same
	colnames(data)[23] <- "pneumonia"
	## Vytvořila jsem subset, obsahuje data zadaného státu
	subset.state <- subset(data, State==state)
	## Tady mi to přestává fungovat... Využila jsem rady z diskuzního fóra a zkusila převést data 
	## na numerická a následně je seřadit, ale nefunguje to...
	numeric.data <- as.numeric(as.character(subset.state))
	ordered <- numeric.data[order("heart attack"),]
	## Check that state and outcome are valid
	if (!is.element(state, data$State)){
	 stop("invalid state")
	}
	## Podle všeho, by předchozí funkce měla fungovat
	## ale při kontrole outcome mi pořád vyskakuje "invalid outcome",
	## i když je zadán správně. Zkusila jsem to zadat třemi způsoby:
	if (outcome != "heart attack" | outcome != "heart failure" | outcome != "pneumonia") {stop("invalide outcome")}
	##if (outcome != "heart attack") {
	##	stop("invalid outcome")
	##}else if (outcome != "heart failure") {
	##	stop("invalid outcome")
	##}else if (outcome != "pneumonia"){
	##	stop("invalid outcome")
	##}
	##condition <- c("heart attack","heart failure","pneumonia")
	##condition <- levels(condition)
	##if(!outcome %in% condition){stop("invalid outcome")}
	## Return hospital name in that state with lowest 30-day death
	## rate
}