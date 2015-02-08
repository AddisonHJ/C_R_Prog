complete <- function(directory, id = 1:322){
     files_list <- list.files(directory, full.names=TRUE) 
     total_nobs <- numeric()
     for (i in id) {
          id_data <- read.csv(files_list[i])
          id_nobs <- c(sum(complete.cases(id_data)))
          total_nobs <- c(total_nobs, id_nobs)
     }
     m <- matrix(data = c(id,total_nobs), ncol = 2, dimnames= list(NULL, c("id","nobs")))
     final_data <- data.frame(m)
     print(final_data)
}
       
complete <- function(directory, id = 1:322){
     files_list <- list.files(directory, full.names=TRUE) 
     ## Create a character vector containing all files in the directory.
     total_nobs <- numeric()
     ## Initialise an empty vector
     for (i in id) {
          id_data <- read.csv(files_list[i])
          ## Read the data frame from i
          id_nobs <- c(sum(complete.cases(id_data)))
          ## Sum of complete cases in the id_data data frame
          total_nobs <- c(total_nobs, id_nobs)
          ## Concatenate total_nobs and id_nobs
     }
     m <- matrix(data = c(id,total_nobs), ncol = 2, dimnames= list(NULL, c("id","nobs")))
     final_data <- data.frame(m)
     print(final_data)
}    
