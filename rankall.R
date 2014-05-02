library(plyr)
options(warn=-1)

rankall <- function(outcome, num= "best") {
  ## List of outcomes
  outcomelist <- c("heart attack", "heart failure", "pneumonia")
  
  ## Read outcome data
  outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that outcome entries are valid
  if(!(outcome %in% outcomelist)){
    stop("invalid outcome")
  }
  
  RankHospitals <- function(dataframe, outcome){
    if(outcome=="heart attack"){
      condition <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    } else if(outcome=="heart failure"){
      condition <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    } else if(outcome=="pneumonia"){
      condition <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    }
    ## subset on the three relevant columns
    data               <- dataframe[, c("Hospital.Name", "State", condition)]
    data[, condition]  <- as.numeric(data[, condition])
    data               <- data[ !is.na(data[,condition]),]
    
    ## split data frame by state
    bystate    <- split(data, data$State)
    statename  <- vector(length=length(bystate))
    hospname   <- vector(length=length(bystate))
    
    for (i in 1:length(bystate)){
      statename[i] <- names(bystate[i])
      df           <- as.data.frame(bystate[i])
      foo          <- statename[i]
      names(df)    <- gsub(paste(foo,".", sep="") ,"", names(df))
      HospsOrder   <- df$Hospital.Name[order(df[,condition], na.last=TRUE, decreasing=FALSE)]
      HospsOrder   <- arrange(df, df[,condition], df$Hospital.Name)$Hospital.Name
      if(num=="best") {
        hospname[i]  <- head(HospsOrder, n=1)
      } else if(num=="worst"){
        hospname[i]  <- tail(HospsOrder, n=1)
      }
      else{
        hospname[i]  <- HospsOrder[num]
      }
    }
    all <- data.frame(hospital=hospname, state=statename)
    return(all)
  }
  RankHospitals(outcomedata, outcome)
}

