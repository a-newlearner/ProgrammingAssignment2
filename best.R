##Finding the best Hospital in a State
  best <- function(state, outcome){
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  states <- data[, 7]
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  ##Checking Validity of Argument
  if((state %in% states)== FALSE)
    stop(print("Invalid State"))
  else if ((outcome %in% outcomes) == FALSE) 
    stop(print("Invalid Outcome"))
  
  ##Subsetting data with the entered State
  new1 <- subset(data, states == state)
  
  ##Extracting the required column
  if(outcome == "heart attack") {
    outcome_column <- 11
  }
  else if(outcome == "heart failure") {
    outcome_column <- 17
  }
  else (outcome == "pneumonia")
  outcome_column <- 23
  
  ##Removing NAs from the required column
  required_column <- as.numeric(new1[,outcome_column])
  to_be_removed <- is.na(required_column)
  desired_data <- new1[!to_be_removed,]
  
  ##Finding the minimum value
  columns_considered <- as.numeric(desired_data[, outcome_column])
  desired_rows <- which(columns_considered == min(columns_considered))
  desired_hospitals <- desired_data[desired_rows, 2]
  
  ##When there are multiple hospitals with minimum value
  if (length(desired_hospitals) > 1) {
    hospitals_sorted <- sort(desired_hospitals)
    hospitals_sorted[1]
  }
  else
    desired_hospitals
}