
setwd("G:/My Drive/Compression ACI")

library(lattice)

load(file='ACI_ADI_Results.RData')
ACI=All[[1]]
ADI=All[[2]]


RelACI=ACI[,2:9]
RelADI=ADI[,2:9]
for(i in 1:8) {
  RelACI[,i]=(RelACI[,i]-ACI[,10])/sd(ACI[,10])
  RelADI[,i]=(RelADI[,i]-ADI[,10])/sd(ADI[,10])
  #RelACI[,i]=(RelACI[,i]/ACI[,10])
  #RelADI[,i]=(RelADI[,i]/ADI[,10])
}
RelACI=stack(RelACI)
RelADI=stack(RelADI)

#Remove NA values if doing it via ratios.
#RelADI=RelADI[!is.na(RelADI$value) & RelADI$value != Inf,]

Labels=colnames(ACI[,2:9])
Labels=unlist(strsplit(Labels,'_'))
Labels[1:3]=paste(Labels[1:3], collapse='\n')
Labels[4:6]=paste(Labels[4:6], collapse='\n')
Labels[7:8]=paste(Labels[7:8], collapse='\n')
Labels[9:11]=paste(Labels[9:11], collapse='\n')
Labels[12:14]=paste(Labels[12:14], collapse='\n')
Labels[15:16]=paste(Labels[15:16], collapse='\n')
Labels[17:19]=paste(Labels[17:19], collapse='\n')
Labels[20:22]=paste(Labels[20:22], collapse='\n')
Labels=unique(Labels)

jpeg('ACIADI_Relative.jpeg', width=7, height=4, units='in', res=500)
par(mar=c(3.2,2.5,0.5,0))
par(oma=c(0,0.5,0,0.8))
par(mfrow=c(1,2))
stripchart(values~ind, data=RelACI, method='jitter', vertical=T, pch=21,
           bg='white', las=1,
           ylab=NA, xlab=NA, axes=F, jitter=0.1)
axis(side = 1, tck = -.015, at=c(1:8), labels = NA)
axis(side = 2, tck = -.015, labels = NA)
axis(side = 1, at=c(1:8), labels=Labels, line=0,
     lwd=0, cex.axis=0.5)
axis(side = 2, lwd = 0, line = -.4, las = 1)
abline(h=0, lty=2)
mtext(side = 1, "Compression scheme", line = 2)
mtext(side = 2, "Standardized change in Index Value (z-score)", line = 2)
box()
stripchart(values~ind, data=RelADI, method='jitter', vertical=T, 
           pch=21, bg='darkgray', col='black', jitter=0.1,ylab=NA, xlab=NA, axes=F)
axis(side = 1, tck = -.015, at=c(1:8), labels = NA)
axis(side = 2, tck = -.015, labels = NA)
axis(side = 1, at=c(1:8), labels=Labels, line=0,
     lwd=0, cex.axis=0.5)
axis(side = 2, lwd = 0, line = -.4, las = 1)
abline(h=0, lty=2)
mtext(side = 1, "Compression scheme", line = 2)
box()

dev.off()


