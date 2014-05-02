## Disable warnings 
options(warn=-1)

## function to get best hospitals given a state and and an outcome
best <- function(state, outcome) {
  ## List of possible outcomes
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

  BestHospitals <- function(dataframe, state, outcome){
    ## match outcome name with column name from df
    if(outcome=="heart attack"){
      condition <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    } else if(outcome=="heart failure"){
      condition <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    } else if(outcome=="pneumonia"){
      condition <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    }
    ## subset on the given state
    data               <- subset(dataframe,subset =(State==state))
    ## convert to numeric
    data[, condition]  <- as.numeric(data[, condition])
    ## get best hospital
    Min                <- min(data[, condition], na.rm=TRUE)
    BestHospsCond      <- data[, condition]==Min
    BestHosps          <- data[BestHospsCond,]$Hospital.Name
    BestHosps          <- BestHosps[!is.na(BestHosps)]
    if(length(BestHosps) > 1){
      BestHosps <- sort(BestHosps)
      BestHosps <- BestHosps[1]
    }
    print(BestHosps)
  }
  
  ## Return hospital name in that state with lowest 30-day death
  BestHospitals(outcomedata, state=state, outcome)
}
