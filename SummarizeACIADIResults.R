setwd("G:/My Drive/Compression ACI")

library(lattice)

load(file='ACI_ADI_Results.RData')
ACI=All[[1]]
ADI=All[[2]]

splom(ACI[,2:10])
splom(ADI[,2:10])

Matrix=matrix(NA, nrow=9, ncol=9)
colnames(Matrix)=colnames(ACI[,2:10])
rownames(Matrix)=colnames(Matrix)

Resids.ACI=Matrix
R.Squared.ACI=Matrix
Same.ACI=Matrix
Slope.ACI=Matrix
Intercept.ACI=Matrix

Resids.ADI=Matrix
R.Squared.ADI=Matrix
Slope.ADI=Matrix
Intercept.ADI=Matrix


i=4
j=9
for(i in 1:9) {
  for(j in 1:9) {
    if(i<j) {
      x=ACI[,i+1]
      y=ACI[,j+1]
      Resid=abs(x-y)/((x+y)/2)
      Resids.ACI[i,j]=mean(Resid)
      model=lm(y ~ x)
      R.Squared.ACI[i,j]=summary(model)$r.squared
      Same.ACI[i,j]=sum(x==y)
      Intercept.ACI[i,j]=coef(model)[1]
      Slope.ACI[i,j]=coef(model)[2]
      
      
      x=ADI[,i+1]
      y=ADI[,j+1]
      Resid=abs(x-y)/((x+y)/2)
      Resid[is.na(Resid)]=0
      Resids.ADI[i,j]=mean(Resid)
      model=lm(y ~ x)
      R.Squared.ADI[i,j]=summary(model)$r.squared
      Intercept.ADI[i,j]=coef(model)[1]
      Slope.ADI[i,j]=coef(model)[2]
      
    }
  }
}

Slope.ACI[i,j]
Intercept.ACI[i,j]
R.Squared.ACI[i,j]

jpeg('ShiftedSlopeACI.jpeg', units='in', width=7, height=7, res=500)
plot(Slope.ACI[,'44100_wav'], Intercept.ACI[,'44100_wav'], pch=20, cex=3, ylab='Intercept',
     xlab='Slope', las=1)
for(i in 2:8) {
  points(Slope.ACI[i,'44100_wav'], Intercept.ACI[i,'44100_wav'], pch=20, col=i, cex=3)
}
abline(h=0, lty=2)
abline(v=1, lty=2)
legend('topright', legend=rownames(Slope.ACI)[1:8],pch=rep(20,8), col=c(1:8))
dev.off()

jpeg('ShiftedSlopeADI.jpeg', units='in', width=7, height=7, res=500)
plot(Slope.ADI[,'44100_wav'], Intercept.ADI[,'44100_wav'], pch=20, cex=3, ylab='Intercept',
     xlab='Slope', las=1)
for(i in 2:8) {
  points(Slope.ADI[i,'44100_wav'], Intercept.ADI[i,'44100_wav'], pch=20, col=i, cex=3)
}
abline(h=0, lty=2)
abline(v=1, lty=2)
legend('topright', legend=rownames(Slope.ADI)[1:8],pch=rep(20,8), col=c(1:8))
dev.off()








