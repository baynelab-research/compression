library(tuneR)
library(soundecology)
library(seewave)
#library(R.utils)
FileLoc="E:/Compression_ACI/32000_wav"

setwd(FileLoc)
#Set working directory, I put the recordings on a hard drive to run this loop
#but it may be possible to set the server location as the working directory

#Read all wave files as object in directory

l <- list.files(FileLoc, full.names= TRUE, recursive=T, pattern="\\.wav")
unique(substr(l, start=nchar(l)-2, nchar(l)))
#make an empty data frame
m <- data.frame()

#ACI loop for first 3 minutes of each recording
?assignInNamespace
i=1
for(i in 1:length(l)){
  file <- paste0(l[i])
  data<-readWave(filename=file,units="seconds") #this part specifies the length, so shouldn't need to clip the files first
  a <- ACI(data, wl=512, flim = c(0,12))
  #a <- acoustic_complexity(data, max_freq = 12000, j = 60)
  #b <- a$AciTotAll_left
  #c <- a$AciTotAll_right
  #d <- (b+c)/2
  #m <- rbind(m, d)
  m <- rbind(m, a)
  message(file)
}

#Vector of file names
r <- list.files(FileLoc, pattern="\\.wav")
p <- c(r)

#Change the row and column names in the dataframe
rownames(m) <- as.character(p)
colnames(m) <- as.character("ACI")

write.csv(m, file = "ACI.csv")

