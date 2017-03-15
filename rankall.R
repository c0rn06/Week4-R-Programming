rankall <- function(outcome, num = "best") {
  
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  outcomepossibilities <- c("heart attack", "heart failure", "pneumonia")
  
  ## Read outcome datas
  my_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that outcome is valid
  if(!outcome %in% outcomepossibilities){
    stop("invalid outcome")
  }
  
  my_frame <- data.frame(Hospital = character(0), State = character(0))
  
  append(state.abb, "DC")
  
  for(s in state.abb){
    hospital <- NA
    hospital <- rankhospital(s, outcome, num)
    
    swinner <- data.frame(hospital, s)
    
    my_frame <- rbind(my_frame, swinner)
  }
  
  colnames(my_frame) <- c("hospital", "state")
  my_frame <- my_frame[order(my_frame$state),]
  
  return(my_frame)
}