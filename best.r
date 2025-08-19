best <- function(state, outcome) {
  library(tidyverse)
  ## Read outcome data
  Data <- read.csv("outcome-of-care-measures.csv")
  ## Check that state and outcome are valid
  StateCol <- as.character(Data$State)
 
  if (state %in% StateCol == FALSE) {
    stop("invalid state")
  }
  
  ## Return hospital name in that state with lowest 30-day death rate
  #convert outcome name to outcome column
  if (outcome == "heart attack") {
    outcome <- colnames(Data)[11]
  } else {
    if (outcome == "heart failure") {
      outcome <- colnames(Data)[17]
    } else {
      if (outcome == "pneumonia") {
        outcome <- colnames(Data)[23]
      } else {stop("invalid outcome")}
    }
  }
  #isolating the rows with only the specified state
  SingleState <<- Data[Data$State == state,]
  #sorting those by outcome value; 
  SingleState[,outcome] <- as.numeric(SingleState[,outcome])
  SingleStateOrdered <- arrange(SingleState, SingleState[,outcome])
  return(SingleStateOrdered[1,2])
}
