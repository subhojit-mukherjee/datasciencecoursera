source('complete.R')
corr <- function(directory, threshold = 0) {
df<-complete('specdata')
df1<-subset(df,nobs>=threshold)
p<-numeric()
for(i in df1$id){
  filename = sprintf("%03d.csv", i)
  filepath <- paste(directory, filename, sep="/")
  data <- read.csv(filepath)
  d=subset(data,!is.na(sulfate) & !is.na(nitrate))
  p<-c(p,cor(d[,'sulfate'],d[,'nitrate']))
}
#print(nrow(df)) 
p
}
