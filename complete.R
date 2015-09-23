complete <- function(directory, id = 1:332) {
  finalFilename <- c()

  fileList = list.files(directory)
  
  filenames = as.numeric(sub("\\.csv$","",fileList))
  selectedfiles = fileList[match(id,filenames)]
  selectedfiles = paste(directory , selectedfiles ,sep = "/")

  nobs <-c()

  for(i in seq_along(selectedfiles))
  {
    df <- read.csv(selectedfiles[i])
    countR <-nrow( subset(df,complete.cases(df)))
    nobs <- c(nobs,countR)
  }
  
  data.frame(id,nobs)
}
