rankhospital <- function(state, outcome, num = "best") {
  
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  outcomepossibilities <- c("heart attack", "heart failure", "pneumonia")
  
  ## Read outcome datas
  my_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if(!state %in% my_data$State){
    stop("invalid state")
  }
  if(!outcome %in% outcomepossibilities){
    stop("invalid outcome")
  }else if(outcome == "heart attack"){
    outcomecol <- 11
  }else if(outcome == "heart failure"){
    outcomecol <- 17
  }else if(outcome == "pneumonia"){
    outcomecol <- 23
  }
  
  ##subset out the state
  my_data <- subset(my_data, my_data[,7] == state)
  ##subset out only the Hospital.Name and the outcome column
  my_data_sub <- my_data[, c(2, outcomecol)]
  
  suppressWarnings( my_data_sub[,2]<-as.numeric(my_data_sub[,2]) )
  
  ##subset out the NAs from the outcome column
  my_data_sub <- subset(my_data_sub, !is.na(my_data_sub[,2]))
  
  ##order by outcome, then Hospital.Name
  names <- my_data_sub[order(my_data_sub[,2], my_data_sub$Hospital.Name),]
  
  if(num == "best"){
    i <- 1
  }else if(num == "worst"){
    i <- nrow(names)
  }else {
    i <- num
  }
  
  return(names[i,1])
  
}