corr <- function(directory, threshold = 0){
     options (digits=4)
     files_list <- list.files(directory, full.names=TRUE) 
     correlations <- numeric()
          
     for (i in 1:332) {
          data <- read.csv(files_list[i])
          good <- complete.cases(data)
          data_complete <- data[good, ]
          nobs <- sum(good)
     
          if(nobs > threshold){
               nitrate <- data_complete[ , "nitrate"]
               sulfate <- data_complete[ , "sulfate"]
               file_corr <- cor(nitrate, sulfate)
               correlations <- c(correlations, file_corr)
          }
     }
     output <<- correlations
}
