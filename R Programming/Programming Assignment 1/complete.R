complete <- function(directory, id = 1:332) {
  filenames <- list.files(path=directory, pattern="*.csv")
  ##df<-data.frame(id = numeric(length(id)), nobs = numeric(length(id)), stringsAsFactors = FALSE)
  x <- numeric(length(id))
  y <- numeric(length(id))
  #print(length(x))
  #print(length(y))
  c=1
  for(i in id) {
    ## Pad the i to create a filename
    filename = sprintf("%03d.csv", i)
    filepath <- paste(directory, filename, sep="/")
    ## Load the data
    data <- read.csv(filepath)
    sub=subset(data,!is.na(sulfate) & !is.na(nitrate))
    x[c] <- i
    y[c] <- nrow(sub)
    c=c+1
  }
  
  
  #print(x)
  #print(y)
  data.frame(id=x, nobs=y, stringsAsFactors=FALSE)
  
}