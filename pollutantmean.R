pollutantmean <- function(directory, pollutant, id = 1:332) {
  filenames <- list.files(path=directory, pattern="*.csv")
  vals <- vector()
  for(i in id) {
                ## Pad the i to create a filename
                  filename = sprintf("%03d.csv", i)
                filepath <- paste(directory, filename, sep="/")
                ## Load the data
        		data <- read.csv(filepath)
                ## Select our column
                  d <- data[,pollutant]
                ## Ignore NAs
      			d <- d[!is.na(d)]
                ## append to our vector
                  vals = c(vals, d)
    		}
  
  round(mean(vals), 3)
}