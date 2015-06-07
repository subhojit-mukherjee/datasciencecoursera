rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  possibleDiseases = c("heart attack", "heart failure", "pneumonia")
  if(!is.element(outcome,possibleDiseases)){
    stop("invalid outcome")
  }
  
  if( num != "best" && num != "worst" && !is.numeric(num) ) stop("invalid num")
  
  myTable <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  myTable <- myTable[c(2, 7, 11, 17, 23)]
  names(myTable)[1] <- "name"
  names(myTable)[2] <- "state"
  names(myTable)[3] <- "heart attack"
  names(myTable)[4] <- "heart failure"
  names(myTable)[5] <- "pneumonia"
  
  myTable<-myTable[myTable[outcome] != 'Mot Availiable',]
  myTable[outcome] <- as.data.frame(sapply(myTable[outcome], as.numeric))
  myTable <- myTable[order(myTable$name, decreasing = FALSE), ]
  myTable <- myTable[order(myTable[outcome], decreasing = FALSE), ]
  
  
  getHospitalRankEach<-function(subsetframe,state,num){
    subsetframe<-subsetframe[subsetframe$state==state,]
    requiredValues<-subsetframe[,outcome]
    if(num=="best")
      rqrdRow<-which.min(requiredValues)
    else if(num=="worst")
      rqrdRow<-which.max(requiredValues)
    else
      rqrdRow<-num
    
    subsetframe[rqrdRow,]$name
    
  }
  
  possibleStates<-myTable[,2]
  possibleStates<-sort(unique(possibleStates))
  outputData<-data.frame("hospital"=character(), "state"=character())
  for(i in possibleStates)
  {
    h<-getHospitalRankEach(myTable,i,num)
    outputData <- rbind(outputData, data.frame(hospital=h, state=i))
  }
  outputData
  
  
  
}