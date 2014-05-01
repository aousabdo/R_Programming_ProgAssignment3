best <- function(state, outcome) {
  ## List of outcomes
  outcomelist <- c("heart attack", "heart failure", "pneumonia")
  ## Read outcome data
  outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  if(!(state %in% outcomedata$State)){
    stop("invalid state")
  }
  #   Min          <- min(outcomedata[,outcome], na.rm=TRUE)
  #   HospitalName <- outcomedata[Min,]$Hospital.Name
  #   cat(HospitalName)
  
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
}


