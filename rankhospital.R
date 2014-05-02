library(plyr)
## Disable comments
options(warn=-1)

rankhospital <- function(state, outcome, num= "best") {
  ## List of outcomes
  outcomelist <- c("heart attack", "heart failure", "pneumonia")
  
  ## Read outcome data
  outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if(!(state %in% outcomedata$State)){
    stop("invalid state")
  }
  if(!(outcome %in% outcomelist)){
    stop("invalid outcome")
  }
  
  RankHospitals <- function(dataframe, state, outcome){
    if(outcome=="heart attack"){
      condition <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    } else if(outcome=="heart failure"){
      condition <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    } else if(outcome=="pneumonia"){
      condition <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    }
    ## subset on the state
    data               <- subset(dataframe,subset =(State==state))
    data[, condition]  <- as.numeric(data[, condition])
    ## subset on the three relevant columns
    data <- data[, c("Hospital.Name", "State", condition)]
    data <- data[ !is.na(data[,condition]),]
    HospsOrder         <- data$Hospital.Name[order(data[,condition], na.last=TRUE, decreasing=FALSE)]
    HospsOrder         <- arrange(data, data[,condition], data$Hospital.Name)$Hospital.Name
    if(num=="best") {
      num = 1
    } else if(num=="worst"){
      num = length(HospsOrder)
    }
    
    print(HospsOrder[num])
  }
  
  ## Return hospital name in that state with lowest 30-day death
  RankHospitals(outcomedata, state=state, outcome)
}
