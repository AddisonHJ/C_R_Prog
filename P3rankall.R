rankall <- function(outcome, num = "best"){
     mydata <- read.csv("outcome-of-care-measures.csv", colClasses = "character",
                        na.strings="Not Available")
     mydata[,11] <- as.numeric(mydata[,11])
     mydata[,17] <- as.numeric(mydata[,17])
     mydata[,23] <- as.numeric(mydata[,23])
     
     o <- c("heart attack", "heart failure", "pneumonia")
     outcome_check <- is.element(outcome, o)
     if (outcome_check == FALSE) stop("invalid outcome")
     
     if (outcome == "heart attack") {
          #return only cases with heart attack data and 3 relevant columns
          good <- complete.cases(mydata[ ,11])
          X <- mydata[good, c(2,7,11)]
     } else if (outcome == "heart failure") {
          good <- complete.cases(mydata[ ,17])
          X <- mydata[good, c(2,7,17)]
     } else if (outcome == "pneumonia") {
          good <- complete.cases(mydata[ ,23])
          X <- mydata[good, c(2,7,23)]
     }
     #re-order rows based 1st on state, 2nd on condition value, 3rd alpha by hospital name
     X_reordered <- X[order(X[ ,2], X[ ,3], X[ ,1]), ]
     
     #split data frame by state
     stateSplit <<- split (X_reordered, X_reordered$State)
     
     for (i in 1:54){
          # calculate length of each list element
          len <- as.numeric(length(stateSplit[[i]][,2]))
          #create new column 'Rank'(actually 'V4') from 1 to length of 
          # data frame. And add to i state's data frame in stateSplit. 
          Rank <- 1:len
          stateSplit[[i]][,4] <<- Rank
          #Add another column replacing the lowest-ranked hospital for each
          #state with "worst" in a new ranking column
          Rank2 <- replace(Rank, Rank == len, "worst")
          #Append Rank2 to data frame
          stateSplit[[i]][,5] <<- Rank2
          
          #Replace hospital names with <NA> if num > number of hospitals in a state
          #Set first row's ranking equal to num 
          if (num != "worst" & num != "best") {
              if (num > len ){
                    stateSplit[[i]][,1] <<- "<NA>"
                    stateSplit[[i]][1,4] <<- num
               }
          }
     }
     X_entire <<- unsplit(stateSplit, f = X_reordered$State)
     colnames(X_entire) <<- c("hospital", "state", "rate", "rank", "rank2")
     
     if (num == "best") {
          y <- X_entire[which(X_entire[ , "rank"] == 1), ]
     } else if (num == "worst"){
          y <- X_entire[which(X_entire[ ,"rank2"] == "worst"), ]
     } else {
          y <- X_entire[which(X_entire[ ,"rank"] == num), ]
     }
     (y[,1:2])
}
