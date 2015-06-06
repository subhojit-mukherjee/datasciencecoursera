best <- function(state, outcome) {
  ## Validate the outcome string with possible values
  possibleDiseases = c("heart attack", "heart failure", "pneumonia")
  if(!is.element(outcome,possibleDiseases)){
    stop("invalid outcome")
  }
  
  ## Reading the csv
  myTable <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Filtering the required columns and simplify the column names
  myTable <- myTable[c(2, 7, 11, 17, 23)]
  names(myTable)[1] <- "name"
  names(myTable)[2] <- "state"
  names(myTable)[3] <- "heart attack"
  names(myTable)[4] <- "heart failure"
  names(myTable)[5] <- "pneumonia"
  
  ##Validating the state
  possibleStates<-myTable[,2]
  possibleStates<-unique(possibleStates)
  if(!is.element(state,possibleStates)){
    stop("invalid state")
  }
  
  ## Filtering the given state value and not null outcome	
  myTable <- myTable[myTable$state==state & myTable[outcome] != 'Not Available', ]
  vals <- myTable[, outcome]
  rowNum <- which.min(vals)
  ## Return hospital name in that state with lowest 30-day death rate
  myTable[rowNum, ]$name
} 