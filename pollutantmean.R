pollutantmean <- function(directory, pollutant, id=1:332 ) {
  finalFilename <- c()
  fileList = list.files(directory)
  
  filenames = as.numeric(sub("\\.csv$","",fileList))
  
  selectedfiles = fileList[match(id,filenames)]
  selectedfiles = paste(directory , selectedfiles ,sep = "/")
 
  dataFile <- lapply(selectedfiles,read.csv)
  
  combinedFile = do.call(rbind,dataFile)
  
  meanValue <- mean(combinedFile[[pollutant]], na.rm = TRUE)
  
  round(meanValue , digits = 3)
}
