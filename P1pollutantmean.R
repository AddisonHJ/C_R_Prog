#build a function that will return the mean value of a pollutant for given 
#monitoring stations (id)
pollutantmean <- function(directory, pollutant, id = 1:332){
     files_list <- list.files(directory, full.names=TRUE) 
          #Create a character vector containing all files in the directory.
     filesall <- data.frame()
          #create an empty data frame to use the in the rbind argument
     for (i in id) {
          filesall <- rbind(filesall, read.csv(files_list[i]))
     }
          #combine all csv files into one
     
     files_subset <- filesall[filesall$ID %in% id, ]
          #take subset of 'filesall' consisting of files with specified id
     
     mean(files_subset[,pollutant], na.rm = TRUE)
          #calculate mean of the specified pollutant for subset of files 
}

#clean
pollutantmean <- function(directory, pollutant, id = 1:332){
     files_list <- list.files(directory, full.names=TRUE) 
     filesall <- data.frame()
     for (i in id) {
          filesall <- rbind(filesall, read.csv(files_list[i]))
     }
     files_subset <- filesall[filesall$ID %in% id, ]
     mean(files_subset[,pollutant], na.rm = TRUE)
}