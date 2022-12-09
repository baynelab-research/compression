lmod=lm(ACI[,10] ~ ACI[,7])
plot(ACI[,7], ACI[,10], xlab=colnames(ACI)[10], ylab=colnames(ACI)[7])
abline(lmod)
?predict
Pred=predict.lm(lmod)

Output=data.frame(ACI[,1],ACI[,7], ACI[,10], Pred)
Output$Test=Output[,3]>Output[,4]


jpeg('SM3 Influence.jpeg', units='in', width=7, height=7, res=500)
plot(Output[Output$Test==T,2], Output[Output$Test==T,3], ylim=c(130,240),pch=20, col='red',
     xlab='32000_wav', ylab='44100_wav')
points(Output[Output$Test==F,2], Output[Output$Test==F,3], pch=20)

legend('topleft', legend=c('mostly SM3', 'Other'),pch=rep(20,2), col=c('black','red'))
dev.off()

#Seems to be related to recorder type. SM3 gives different values. However, not sure why it is
#only on 44100 frequencies. Doesn't seem to be an issue in 32000 Hz and 22050 Hz.