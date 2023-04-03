library(tidyverse)
library(tuneR)
library(soundgen)

files <- list.files("figures/wav", full.names = TRUE, pattern="*.wav")

for(i in 1:length(files)){
  
  wav.i <- readWave(files[i], from=53, to=56, units="seconds")
  
  if(i %in% c(1,4)){
    spectrogram(wav.i, cores=6,
                colorTheme='bw',
                ylab="",
                xlab="",
                contrast=0.1,
                osc = "none",
                width = 6,
                height = 4,
                res = 300,
                units = "in",
                xaxt = "n",
                bg = NA,
                savePlots = paste0("figures/wav/", i))
  }
  if(i %in% c(2,3,5,6)){
    spectrogram(wav.i, cores=6,
                colorTheme='bw',
                ylab="",
                xlab="",
                contrast=0.1,
                osc = "none",
                width = 6,
                height = 4,
                res = 300,
                units = "in",
                xaxt = "n",
                yaxt = "n",
                bg = NA,
                savePlots = paste0("figures/wav/", i))
  }
  if(i==7){
    spectrogram(wav.i, cores=6,
                colorTheme='bw',
                ylab="",
                xlab="",
                contrast=0.1,
                osc = "none",
                width = 6,
                height = 4,
                res = 300,
                units = "in",
                bg = NA,
                savePlots = paste0("figures/wav/", i))

  }
  if(i %in% c(8,9)){
    spectrogram(wav.i, cores=6,
                colorTheme='bw',
                ylab="",
                xlab="",
                contrast=0.1,
                osc = "none",
                width = 6,
                height = 4,
                res = 300,
                units = "in",
                yaxt = "n",
                bg = 'transparent',
                savePlots = paste0("figures/wav/", i))
  }

  
}


