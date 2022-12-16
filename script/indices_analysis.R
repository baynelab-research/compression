library(tidyverse)
library(lme4)
library(MuMIn)
library(gridExtra)
library(merTools)
library(performance)

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
         samplerate.s = (samplerate-22050)/22050) %>% 
  separate(compressiontype, into=c("bitrate", "filetype")) %>% 
  mutate(compressiontype = ifelse(bitrate=="wav", "wav", paste0(filetype, "_", bitrate)),
         aci.s = (aci-min(aci))/(max(aci)-min(aci)),
         adi.s = (adi-min(adi))/max(adi)-min(adi))

#make compression type a factor
dat$compressiontype <- factor(dat$compressiontype, levels=c("wav", "mp3_320", "mp3_96"))

#3. Visualize----
ggplot(dat) +
  geom_point(aes(x=samplerate.s, y=aci, colour=compressiontype)) +
  geom_smooth(aes(x=samplerate.s, y=aci, colour=compressiontype))

ggplot(dat) +
#  geom_point(aes(x=samplerate.s, y=adi, colour=compressiontype)) +
  geom_smooth(aes(x=samplerate.s, y=adi, colour=compressiontype))

#4. Model----

#4a. ACI----
aci0 <- lmer(aci.s ~ samplerate.s + compressiontype + (1|recording), data=dat, na.action="na.fail", REML=FALSE)
icc(aci0)
dredge(aci0)

aci1 <- lmer(aci.s ~ samplerate.s*compressiontype + (1|recording), data=dat, na.action="na.fail")
aci2 <- lmer(aci.s ~ samplerate.s+compressiontype + (1|recording), data=dat, na.action="na.fail")
aci3 <- lmer(aci.s ~ samplerate.s + (1|recording), data=dat, na.action="na.fail")
aci4 <- lmer(aci.s ~ compressiontype + (1|recording), data=dat, na.action="na.fail")
aci5 <- lmer(aci.s ~ 1 + (1|recording), data=dat, na.action="na.fail")
summary(aci1)

#4b. ADI----
adi0 <- lmer(adi.s ~ samplerate.s*compressiontype + (1|recording), data=dat, na.action="na.fail", REML=FALSE)
icc(adi0)
dredge(adi0)
#Compression type

adi1 <- lmer(adi.s ~ samplerate.s*compressiontype + (1|recording), data=dat, na.action="na.fail")
adi2 <- lmer(adi.s ~ samplerate.s+compressiontype + (1|recording), data=dat, na.action="na.fail")
adi3 <- lmer(adi.s ~ samplerate.s + (1|recording), data=dat, na.action="na.fail")
adi4 <- lmer(adi.s ~ compressiontype + (1|recording), data=dat, na.action="na.fail")
adi5 <- lmer(adi.s ~ 1 + (1|recording), data=dat, na.action="na.fail")
lrtest(adi1, adi2, adi3, adi4, adi5)
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
                      samplerate.s = seq(0, 1, 0.01),
                      recording = unique(dat$recording))

#8a. Effect of sample rate on p(true positive)----
ci.aci <- data.frame(confint(aci1))[3:6,] %>% 
  cbind(mean = aci1@beta[1:4])
colnames(ci.aci) <- c("lwr", "upr", "mean")
ci.aci$beta <- row.names(ci.aci)

ci.adi <- data.frame(confint(adi1))[3:6,] %>% 
  cbind(mean = adi1@beta[1:4])
colnames(ci.adi) <- c("lwr", "upr", "mean")
ci.aci$beta <- row.names(ci.aci)

ci.cmp <- data.frame(compressiontype=unique(dat$compressiontype),
                     mean = c(ci.aci$mean[1], sum(ci.aci$mean[c(1,3)]), sum(ci.aci$mean[c(1,4)])),
                     upr = c(ci.aci$upr[1], sum(ci.aci$upr[c(1,3)]), sum(ci.aci$upr[c(1,4)])),
                     lwr = c(ci.aci$lwr[1], sum(ci.aci$lwr[c(1,3)]), sum(ci.aci$lwr[c(1,4)]))) %>% 
  mutate(index = "ACI") %>% 
  rbind(data.frame(compressiontype=unique(dat$compressiontype),
                   mean = c(ci.adi$mean[1], sum(ci.adi$mean[c(1,3)]), sum(ci.adi$mean[c(1,4)])),
                   upr = c(ci.adi$upr[1], sum(ci.adi$upr[c(1,3)]), sum(ci.adi$upr[c(1,4)])),
                   lwr = c(ci.adi$lwr[1], sum(ci.adi$lwr[c(1,3)]), sum(ci.adi$lwr[c(1,4)]))) %>% 
          mutate(index = "ADI"))

plot.indices <- ggplot(ci.cmp) +
  geom_point(aes(x=index, y=mean, colour=compressiontype), position=position_dodge(width = 1)) +
  geom_errorbar(aes(x=index, ymin=lwr, ymax=upr, colour=compressiontype), position = position_dodge(width=1)) +
  scale_colour_manual(values=c("orange", "darkblue", "lightblue"), name="Compression treatment\n(File type_bit rate)") +
  my.theme +
  theme(legend.position = "bottom")

ggsave(plot.indices, filename="figures/Indices.jpeg", width=6, height=5)
