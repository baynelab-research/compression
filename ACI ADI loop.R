library(tuneR)
library(soundecology)
library(seewave)
#library(R.utils)
FileLoc="C:/Users/Richard Hedley/Desktop/44100_320_mp3"

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

for(i in 1:length(l)){
  file <- paste0(l[i])
  data<-readWave(filename=file,units="seconds") #this part specifies the length, so shouldn't need to clip the files first
  a <- acoustic_complexity(data, max_freq = 12000, j = 60)
  b <- a$AciTotAll_left
  c <- a$AciTotAll_right
  d <- (b+c)/2
  m <- rbind(m, d)
  message(file)
}
?acoustic_complexity
#Vector of file names
r <- list.files(FileLoc, pattern="\\.wav")
p <- c(r)

#Change the row and column names in the dataframe
rownames(m) <- as.character(p)
colnames(m) <- as.character("ACI")

write.csv(m, file = "ACI.csv")

#Calculating ADI
#Read all wave files as object in directory
l2 <- list.files(FileLoc, full.names= TRUE, pattern="wav")

#make an empty data frame
m2 <- data.frame()

#Gives average of left and right channel ADI for the first 3 minutes of each recording
for(i in 1:length(l2)){
  file <- paste0(l2[i])
  data<-readWave(filename=file, units="seconds") #this part specifies the length, so shouldn't need to clip the files first
  a2 <- acoustic_diversity(data, max_freq = 12000, db_threshold = -50, freq_step = 1000, shannon = TRUE)
  b2 <- a2$adi_left
  c2 <- a2$adi_right
  d2 <- (b2+c2)/2
  m2 <- rbind(m2, d2) 
  message(file)
}
?acoustic_diversity
#Vector of file names
r2 <- list.files(FileLoc, pattern="wav")
p2 <- c(r2)

#Change the row and column names in the dataframe
rownames(m2) <- as.character(p2)
colnames(m2) <- as.character("ADI")

write.csv(m2, file = "ADI.csv")
