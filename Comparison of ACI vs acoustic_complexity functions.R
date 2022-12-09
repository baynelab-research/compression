
data(tico)
x=ACI(tico, flim=c(0,10))
y=acoustic_complexity(tico, min_freq=0, max_freq=10000, j=tico@samp.rate/length(tico@left))


wave=tico
f=tico@samp.rate
wn='hamming'
ovlp=0
wlen=512
#One difference is ACI uses 
z=sspectro(wave@left, f, wn = wn, ovlp = ovlp)
#While acoustic_complexity uses
w=spectro(wave@left, f = wave@samp.rate, wl = wlen, 
        plot = FALSE, norm = TRUE, dB = NULL, scale = FALSE, 
        wn = wn)$amp

?spectro
?sspectro
plot(w,z)
#Clearly, the sspectro and spectro functions differ.
