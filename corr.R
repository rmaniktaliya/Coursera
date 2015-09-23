corr <- function(directory, threshold = 0) {
  finalFilename <- c()

  fileList = list.files(directory)
  

  selectedfiles <- paste(directory,fileList,sep = "/")

  vectorCorelation <-c()

  for(i in seq_along(selectedfiles))
  {
    df <- read.csv(selectedfiles[i])
    completeDf <- subset(df , complete.cases(df))
   
    finalDf <- subset(completeDf,nrow(completeDf) > threshold)
   
    if(nrow(finalDf) > 0)
    {
      corelation <- cor(finalDf$nitrate , finalDf$sulfate)
      vectorCorelation <- c(vectorCorelation , corelation)
    }
    
  }
  
  vectorCorelation
}
