#Set working directory to desktop.
setwd("C:/Users/Richard Hedley/Desktop")


Folders=c('22050_96_mp3', '22050_320_mp3', '22050_wav',
          '32000_96_mp3', '32000_320_mp3', '32000_wav',
          '44100_96_mp3', '44100_320_mp3', '44100_wav')

for(i in 1:length(Folders)) {
  Fold=Folders[i]
  x=read.csv(paste('./', Fold, '/ACI.csv', sep=""), header=T)
  colnames(x)=c('File', Fold)
  y=read.csv(paste('./', Fold, '/ADI.csv', sep=""), header=T)
  colnames(y)=c('File', Fold)
  if(i==1) {
    ACI=x
    ADI=y
  } else {
    #loop to make sure correct file is chosen
    ACI$name=NA
    colnames(ACI)[ncol(ACI)]=Fold    
    ADI$name=NA
    colnames(ADI)[ncol(ADI)]=Fold
    for(j in 1:nrow(ACI)) {
      file=ACI$File[j]
      select=x[which(x$File==file),2]
      ACI[j,Fold]=select
      
      file=ADI$File[j]
      select=y[which(y$File==file),2]
      ADI[j,Fold]=select

    }
  }
}

All=list(ACI, ADI)
#save(All, file='ACI_ADI_Results.RData')
#load(file='ACI_ADI_Results.RData')
ACI=All[[1]]
ADI=All[[2]]

sum(is.na(ACI[,2:8]))
splom(ACI[,2:10])
splom(ADI[,2:10])


