best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  data<-read.csv(file.path("rprog_data_ProgAssignment3-data","outcome-of-care-measures.csv"))
  #get the possible states
  states<-unique(data$State)
  #check if the parameter state is a valid state
  if(! (state %in% as.character(states))){
    stop("invalid state")
  }
  valid_outcomes<-list("heart attack"="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                       "heart failure"="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                       "pneumonia"="Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" )
  #check if the parameter outcome is valid
  if(!(outcome %in% names(valid_outcomes))){
    stop("invalid outcome")
  }
  #filter data on the state
  filt_data<-data[data$State==state,]
  #get the right name of the column depending on the outcome
  outcome_name<-valid_outcomes[[outcome]]
  #filter the column
  f<-filt_data[,outcome_name]
  #convert Not Available to NA to avoid warning in conversion with numeric
  f[f=="Not Available"]<-NA
  #convert the outcome column to numeric
  f<-as.numeric(levels(f)[f])
  #get the name of the best ospital
  as.character(filt_data[which.min(f),"Hospital.Name"])
}


rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  data<-read.csv(file.path("rprog_data_ProgAssignment3-data","outcome-of-care-measures.csv"))
  #get the possible states
  states<-unique(data$State)
  #check if the parameter state is a valid state
  if(! (state %in% as.character(states))){
    stop("invalid state")
  }
  valid_outcomes<-list("heart attack"="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                       "heart failure"="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                       "pneumonia"="Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" )
  #check if the parameter outcome is valid
  if(!(outcome %in% names(valid_outcomes))){
    stop("invalid outcome")
  }
  #filter data on the state
  filt_data<-data[data$State==state,]
  #get the right name of the column depending on the outcome
  outcome_name<-valid_outcomes[[outcome]]
  #filter the column
  f<-filt_data[,outcome_name]
  #convert Not Available to NA to avoid warning in conversion with numeric
  f[f=="Not Available"]<-NA
  #convert the outcome column to numeric
  f<-as.numeric(levels(f)[f])
  filt_data[[outcome_name]]<-f
  #consider only complete cases
  filt_data<-filt_data[complete.cases(filt_data[,outcome_name]),]
  if(num=="best"){
    num<-1
  }
  else if(num=="worst"){
    num<-nrow(filt_data)
  }
  #order the data based on outcome and name of the hospital
  ordered_data<-filt_data[ order(filt_data[,outcome_name], filt_data[,"Hospital.Name"]), ]
  as.character(ordered_data[num,"Hospital.Name"])
  
}

rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  data<-read.csv(file.path("rprog_data_ProgAssignment3-data","outcome-of-care-measures.csv"), na.strings = "Not Available")
  valid_outcomes<-list("heart attack"="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                       "heart failure"="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                       "pneumonia"="Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" )
  #check if the parameter outcome is valid
  if(!(outcome %in% names(valid_outcomes))){
    stop("invalid outcome")
  }
  #get the right name of the column depending on the outcome
  outcome_name<-valid_outcomes[[outcome]]
  #consider only complete cases
  data<-data[complete.cases(data[,outcome_name]),]
  
  my_split <- split(data, data$State)
  my_output <- lapply(my_split, function (my_df){
    my_df <- my_df[order(my_df["State"], my_df[outcome_name], my_df["Hospital.Name"]) , ]
    
    # Determine the row that we want
    if (num == "best") {
      index <- 1
    } else if (num == "worst")
      index <- nrow(my_df)     # this specifies last row
    else {
      index <- num                # Assume rank is an integer
    } 
    
    # Extract the desired data by row and column and build dataframe output
    hospital <- my_df[index, c("Hospital.Name")]
    state <- my_df[1, c("State")]
    data.frame(hospital, state)
    
  }  ) # close lapply
  do.call(rbind.data.frame, my_output)
}
