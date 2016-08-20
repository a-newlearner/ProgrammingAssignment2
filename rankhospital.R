rankhospital <- function(state, outcome, num="best"){
##Read Outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  states <- data[,7]
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  ##Checking Validity of Argument
  if((state %in% states)== FALSE){
    stop(print("Invalid State"))
  }
  else if ((outcome %in% outcomes) == FALSE) {
    stop(print("Invalid Outcome"))
  }
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

  ##Ranking the hospitals
  outcome_column_name <- names(desired_data)[outcome_column]
  hospital_column_name <- names(desired_data)[2]
  index <- with(desired_data, order(desired_data[outcome_column_name], desired_data[hospital_column_name]))
  ordered_desired_data <- desired_data[index, ]
  if (is.character(num) == TRUE) {
    if (num == "best") {
      num = 1
    }
    else if (num == "worst") {
      num = length(ordered_desired_data[, outcome_column])
    }
  }
  #return the hospital name with the outcome ranking of num
  ordered_desired_data[num, 2]
  
  }
  