rankhospital <- function(state, outcome, num = "best"){
     mydata <- read.csv("outcome-of-care-measures.csv", colClasses = "character",
                        na.strings="Not Available")
     mydata[,11] <- as.numeric(mydata[,11])
     mydata[,17] <- as.numeric(mydata[,17])
     mydata[,23] <- as.numeric(mydata[,23])
     stateSplit <- split (mydata, mydata$State)
     
     #check that state and outcome are valid
     s <- c("AK","AL","AR","AZ","CA","CO","CT","DC","DE","FL","GA","GU",
            "HI","IA","ID","IL","IN","KS","KY","LA","MA","MD","ME","MI",
            "MN","MO","MS","MT","NC","ND","NE","NH","NJ","NM","NV","NY",
            "OH","OK","OR","PA","PR","RI", "SC","SD","TN","TX","UT","VA",
            "VI","VT","WA","WI","WV","WY")
     state_check <- is.element(state, s)
     if (state_check == FALSE) stop("invalid state") 
     o <- c("heart attack", "heart failure", "pneumonia")
     outcome_check <- is.element(outcome, o)
     if (outcome_check == FALSE) stop("invalid outcome")
     
     
     if (outcome == "heart attack") {
          Z <- stateSplit[[state]]
          good <- complete.cases(Z[ ,11])
          #return only cases with heart attack data and 2 relevant columns
          X <<- Z[good, c(2,11)]
     
     } else if (outcome == "heart failure") {
          Z <- stateSplit[[state]]
          good <- complete.cases(Z[ ,17])
          X <<- Z[good, c(2,17)]

     } else if (outcome == "pneumonia") {
          Z <- stateSplit[[state]]
          good <- complete.cases(Z[ ,23])
          X <<- Z[good, c(2,23)]
     }
     
     #re-order rows based first on heart attack value, then alphabetically
     #by hospital name
     X_reordered <<- X[order(X[ ,2], X[ ,1]), ]
     #return length of data frame
     df_length <<- as.numeric(length(X_reordered$Hospital.Name))
     #create new column 'Rank' from 1 to length of data frame 
     Rank <- 1:df_length
     X_reordered$Rank <- Rank
     #return hospital name corresponding to Rank "num"
     if (num == "best") {
          print(X_reordered$Hospital.Name[X_reordered$Rank == 1])
     } else if (num == "worst"){
          print(X_reordered$Hospital.Name[X_reordered$Rank == df_length])
     } else if (num > df_length) {
          print (NA)
     } else {
          print(X_reordered$Hospital.Name[X_reordered$Rank == num])
     }
}



     