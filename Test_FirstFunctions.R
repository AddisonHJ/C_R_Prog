add2 <- function(x, y) {
     x+y
}

above10 <- function(x) {
     use <- x > 10 #Will return a logical vector with T and F indicating which element is greater than 10
     x[use] #Subset vector x with logical vector
}

above <- function(x, n = 10){
     #User can decide the numeric limit for subsetting the vector
     #But default value, if user specifies nothing, is 10
     use <- x > n
     x[use]
}

#A function to calculate the mean of each column of a matrix or dataframe
#Has an argument to remove NAs, which will default to TRUE
columnmean <- function(y, removeNA = TRUE) { 
          #y will be a dataframe or matrix
     nc <- ncol(y) 
          #Determine how many columns the matrix has and store as object nc
     means <- numeric(nc) 
          #Initialise an empty vector that will store the means for each col.
          #Length of vector has to equal the number of columns
     for(i in 1:nc) { 
          #Create 'for' loop that will loop through the columns
          #Creates also an integer vector that starts at 1 and ends at nc
          means[i] <- mean(y[, i], na.rm = removeNA) 
               #Assign to means vector the mean of the ith column of y (subsetting)
               #'mean' is a function
               #pass removeNA argument to the na.rm argument of the mean function
     }
     means #return the vector of means (as last expression in the function, this will be returned)
}
