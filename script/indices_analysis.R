library(tidyverse)
library(lme4)
library(MuMIn)
library(gridExtra)

#1. Read in data----
aci <- read.csv("data/acoustic indices/ACI.csv") %>% 
  pivot_longer(cols=X22050_320_mp3:X44100_or_48000_wav, names_to="treatment", values_to="aci")
adi <- read.csv("data/acoustic indices/ADI.csv") %>% 
  pivot_longer(cols=X22050_320_mp3:X44100_or_48000_wav, names_to="treatment", values_to="adi")

#2. Put together and wrangle----
dat <- full_join(aci, adi) %>% 
  rename(recording=X) %>% 
  mutate(samplerate = as.numeric(str_sub(treatment, 2, 6)),
         compressiontype = ifelse(samplerate==44100,
                                  str_sub(treatment, 17, 100),
                                  str_sub(treatment, 8, 100)),
         samplerate.s = (samplerate-22050)/22050)

#3. Visualize----
ggplot(dat) +
  geom_point(aes(x=samplerate.s, y=aci, colour=compressiontype)) +
  geom_smooth(aes(x=samplerate.s, y=aci, colour=compressiontype))

ggplot(dat) +
  geom_point(aes(x=samplerate.s, y=adi, colour=compressiontype)) +
  geom_smooth(aes(x=samplerate.s, y=adi, colour=compressiontype))

#4. Model----

#4a. ACI----
aci0 <- lm(aci ~ samplerate.s*compressiontype, data=dat, na.action="na.fail")
dredge(aci0)

aci1 <- lm(aci ~ samplerate.s + compressiontype, data=dat, na.action="na.fail")
summary(aci1)

#4b. ADI----
adi0 <- lm(adi ~ samplerate.s*compressiontype, data=dat, na.action="na.fail")
dredge(adi0)

adi1 <- adi0
summary(adi1)

#5. Plot----
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

newdat <- expand.grid(compressiontype=unique(dat$compressiontype),
                      samplerate.s = seq(0, 1, 0.01))

#8a. Effect of sample rate on p(true positive)----
pred <- data.frame(pred = predict(aci1, newdat, se=TRUE)) %>% 
  cbind(newdat) %>% 
  dplyr::select(pred.fit, pred.se.fit, samplerate.s, compressiontype) %>% 
  unique() %>% 
  mutate(samplerate = samplerate.s*22050 + 22050,
         upper = pred.fit + 1.96*pred.se.fit,
         lower = pred.fit - 1.96*pred.se.fit)

h.plot <- ggplot() +
  geom_point(data=dat, aes(x=samplerate, y=aci, colour=compressiontype), pch=21, colour="grey50", fill="grey70", size=2) +
  geom_ribbon(data=pred, aes(x=samplerate, ymin = lower, ymax=upper, group=compressiontype), alpha = 0.3) +
  scale_colour_viridis_d(name="Compression treatment\n(File type_Bit rate)") +
  geom_line(data=pred, aes(x=samplerate, y=pred.fit, colour=compressiontype)) +
  my.theme +
  theme(legend.position="bottom") +
  xlab("Sample rate") +
  ylab("Acoustic complexity index (ACI)") 
h.plot

ggsave(h.plot, filename="figures/Indices.jpeg", width=6, height=5)
