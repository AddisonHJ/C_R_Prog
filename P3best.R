best <- function (state, outcome){
     mydata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
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
          #extract data frame for selected state
          Z <- stateSplit[[state]]
          #remove rows with missing values on "heart attack"
          good <- complete.cases(Z[ ,11])
          X <- Z[good, ]
          #find minimum value of heart attack column
          m <- min(X[ ,11])
          #subset rows from X that have that minimum value
          L <- X[which(X[ , 11] == m), ]
         
     } else if (outcome == "heart failure") {
          Z <- stateSplit[[state]]
          good <- complete.cases(Z[ ,17])
          X <- Z[good, ]
          m <- min(X[ ,17])
          L <- X[which(X[ , 17] == m), ]
          
     } else if (outcome == "pneumonia"){
          Z <- stateSplit[[state]]
          good <- complete.cases(Z[ ,23])
          X <- Z[good, ]
          m <- min(X[ ,23])
          L <- X[which(X[ , 23] == m), ]
     } 
     #re-order rows alphabetically based on hospital name
     Alpha <- L[order(L$Hospital.Name), ]
     #return first row's hospital name
     print(Alpha[1,2])
}
