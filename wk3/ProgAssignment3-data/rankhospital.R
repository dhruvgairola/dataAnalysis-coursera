rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  if(!(state %in% data$State)) {
    stop("Invalid state")
  }
  if(!(outcome == "heart attack" || outcome == "heart failure" || outcome == "pneumonia")) {
    stop("Invalid outcome")
  }
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  if(outcome == "heart attack") {
    data_for_state <- data[data$State == state, c("Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")]
    
    data_for_state[,2] <- as.numeric(data_for_state[,2])  
    ordered_data_for_state <- order(data_for_state["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"], data_for_state$Hospital.Name)
    inv_ordered_data_for_state <- order(data_for_state["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"], data_for_state$Hospital.Name, decreasing = TRUE)
    
    as.character(data_for_state$Hospital.Name[ordered_data_for_state[1]])
    
  } else if(outcome == "heart failure") {
    data_for_state <- data[data$State == state, c("Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")]
    
    data_for_state[,2] <- as.numeric(data_for_state[,2])	
    ordered_data_for_state <- order(data_for_state["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"], data_for_state$Hospital.Name)
    inv_ordered_data_for_state <- order(data_for_state["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"], data_for_state$Hospital.Name, decreasing = TRUE)
    
    as.character(data_for_state$Hospital.Name[ordered_data_for_state[1]])
    
  } else if(outcome == "pneumonia") {
    data_for_state <- data[data$State == state, c("Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
    
    data_for_state[,2] <- as.numeric(data_for_state[,2])	
    ordered_data_for_state <- order(data_for_state["Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"], data_for_state$Hospital.Name)
    inv_ordered_data_for_state <- order(data_for_state["Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"], data_for_state$Hospital.Name, decreasing = TRUE)
    
    as.character(data_for_state$Hospital.Name[ordered_data_for_state[1]])
    
  } else {
    stop("Invalid something else")
  }
  
  if (num == "best") {
    as.character(data_for_state$Hospital.Name[ordered_data_for_state[1]])
  } else if (num == "worst") {
    as.character(data_for_state$Hospital.Name[inv_ordered_data_for_state[1]])
  } else if (is.numeric(num)) {
    as.character(data_for_state$Hospital.Name[ordered_data_for_state[num]])
  } else {
    stop("invalid num")
  }
}