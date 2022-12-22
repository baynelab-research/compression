library(tidyverse)
library(tuneR)
library(seewave)
library(gridExtra)

my.theme <- theme_classic() +
  theme(text=element_text(size=12, family="Arial"),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(margin=margin(10,0,0,0)),
        axis.title.y=element_text(margin=margin(0,10,0,0)),
        axis.line.x=element_line(linetype=1),
        axis.line.y=element_line(linetype=1),
        legend.text=element_text(size=12),
        legend.title=element_text(size=12),
        plot.title=element_text(size=12, hjust = 0.5))

sr <- c("wav", "mp3_320", "mp3_96", "", "", "", "", "", "")
cmp <- c("", "", "44100 Hz", "", "", "32000 Hz", "", "", "22050 Hz")

files <- list.files("figures/wav", full.names = TRUE)

i <- 1
specs <- list()
for(i in 1:length(files)){
  
  wav.i <- readWave(files[i], from=53, to=56, units="seconds")
  
  specs[[i]] <- ggspectro(wav.i) +
    stat_contour(geom="polygon", aes(fill=..level..), bins=30, show.legend=FALSE) +
    scale_fill_continuous(name="Amplitude\n(dB)\n", limits=c(-40,0),
                          na.value="transparent", low="white", high="black") + 
    xlab(sr[i]) + 
    ylab(cmp[i]) +
    my.theme +
    theme(axis.title.y.right = element_text(margin = unit(c(0, 0, 0, 20), 'mm')),
          axis.title.x.top = element_text(margin = unit(c(0, 0, 0, 20), 'mm')))
  
}

ggsave(grid.arrange(specs[[1]], specs[[2]], specs[[3]],
                    specs[[4]], specs[[5]], specs[[6]],
                    specs[[7]], specs[[8]], specs[[9]],
                    heights=c(2,1.6,1.2),
                    widths=c(2,2,2),
                    layout_matrix=(rbind(c(1,2,3), c(4,5,6), c(7,8,9))),
                    left = "Frequency (kHz)",
                    bottom = "Time (s)",
                    right = "Sample rate (Hz) treatment",
                    top = "Compression type (file type_bitrate)"),
       filename="figures/spectrogram.jpeg", width=12, height = 6)
