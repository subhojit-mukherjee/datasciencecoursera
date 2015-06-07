rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  possibleDiseases = c("heart attack", "heart failure", "pneumonia")
  if(!is.element(outcome,possibleDiseases)){
    stop("invalid outcome")
  }
  myTable <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  myTable <- myTable[c(2, 7, 11, 17, 23)]
  names(myTable)[1] <- "name"
  names(myTable)[2] <- "state"
  names(myTable)[3] <- "heart attack"
  names(myTable)[4] <- "heart failure"
  names(myTable)[5] <- "pneumonia"
  
  possibleStates<-myTable[,2]
  possibleStates<-unique(possibleStates)
  if(!is.element(state,possibleStates)){
    stop("invalid state")
  }
  if (num=="best")
  {
    ## Filtering the given state value and not null outcome  
    myTable <- myTable[myTable$state==state & myTable[outcome] != 'Not Available', ]
    vals <- myTable[, outcome]
    rowNum <- which.min(vals)
    ## Return hospital name in that state with lowest 30-day death rate
    myTable[rowNum, ]$name
  }
  else if(num=="worst")
  {
    myTable <- myTable[myTable$state==state & myTable[outcome] != 'Not Available', ]
    vals <- myTable[, outcome]
    rowNum <- which.max(vals)
    ## Return hospital name in that state with lowest 30-day death rate
    myTable[rowNum, ]$name
  }
  else if(!is.numeric(num)) stop("invalid num")
  else
  {
    myTable <- myTable[myTable$state==state & myTable[outcome] != 'Not Available', ]
    myTable[outcome] <- as.data.frame(sapply(myTable[outcome], as.numeric))
    print(myTable)
    myTable<-myTable[order(myTable$name, decreasing = FALSE),]
    myTable<-myTable[order(myTable[,outcome], decreasing = FALSE),]
    myTable[num, ]$name
    
  }
}