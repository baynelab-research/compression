library(tidyverse)
library(lme4)
library(MuMIn)
library(gridExtra)

#NOTE: 100 recordings with confirmed detections were scanned for each species and for each treatment combination

#TO DO: NEED TO INVESTIGATE NUMBER OF FILES RUN FOR 96 X 44100

#1. Read in data----
oven <- read.csv("data/recognizer/CompiledCNNResults_OVEN_Validated.csv") %>%
  mutate(species="OVEN")
coni <- read.csv("data/recognizer/CompiledCNNResults2_CONI_Validated.csv") %>% 
  mutate(species="CONI")

#2. Bind together and wrangle----
#sample rates 32000 & 22050 were flipped in file creation
hit <- rbind(oven, coni) %>% 
  mutate(detection = ifelse(validation !="n", 1, 0),
         samplerate = case_when(samplerate==22050 ~ 32000,
                                samplerate==32000 ~ 22050,
                                samplerate==44100 ~ 44100),
         compressiontype = ifelse(filetype=="wav", "wav", paste0("mp3_", compressionrate)),
         compressiontype = factor(compressiontype, levels=c("wav", "mp3_320", "mp3_96")),
         samplerate.s = (samplerate-22050)/22050) %>% 
  dplyr::filter(!is.na(detection))

#3. Summarize at recording level----
rec <- hit %>% 
  group_by(recording, species, samplerate, samplerate.s, compressiontype) %>% 
  summarize(n=n(),
            hits = sum(detection)) %>% 
  ungroup() %>% 
  mutate(precision = hits/n)

#4. Summarize at species level----
spp <- rec %>% 
  group_by(species, samplerate.s, compressiontype) %>% 
  summarize(precision = mean(precision),
            hits = n()) %>% 
  ungroup() %>% 
  mutate(recall=hits/100)
spp

#5. Make a dataframe for recall analysis----
recall <- data.frame()
for(i in 1:nrow(spp)){
  recall.i <- data.frame(species = rep(spp$species[i],100),
                        samplerate.s = rep(spp$samplerate.s[i],100),
                        compressiontype = rep(spp$compressiontype[i]), 100,
                        hit = c(rep(1, spp$hits[i]), rep(0, 100-spp$hits[i])))
  recall <- rbind(recall.i, recall)
}

#6. Visualize----

#6a. Precision----
ggplot(rec) + 
  geom_smooth(aes(x=samplerate.s, y=precision, colour=species)) +
  facet_grid(species~compressiontype, scales="free")

#6b. Recall----
ggplot(recall) +
  geom_smooth(aes(x=samplerate.s, y=hit, colour=species)) +
  facet_grid(species~ compressiontype)

#7. Model----

#7a. Probability of detection being true----
#CONI
rec.coni <- dplyr::filter(rec, species=="CONI")
h0.coni <- glm(precision ~ samplerate.s*compressiontype,
            data=rec.coni, na.action = "na.fail")
dredge(h0.coni)

h1.coni <- lm(precision ~ 1, data=rec.coni)
summary(h1.coni)
#no effect

#OVEN
rec.oven <- dplyr::filter(rec, species=="OVEN")
h0.oven <- lm(precision ~ samplerate.s*compressiontype,
                 data=rec.oven, na.action = "na.fail")
dredge(h0.oven)

h1.oven <- lm(precision ~ samplerate.s,
              data=rec.oven, na.action = "na.fail")
summary(h1.oven)
#Strong effect of sample rate

#7b. Recording level recall----
#CONI
recall.coni <- dplyr::filter(recall, species=="CONI")
r0.coni <- glm(hit ~ samplerate.s*compressiontype, data=recall.coni, na.action="na.fail")
dredge(r0.coni)

r1.coni <- glm(hit ~ samplerate.s + compressiontype, data=recall.coni, na.action="na.fail")
summary(r1.coni)

#OVEN
recall.oven <- dplyr::filter(recall, species=="OVEN")
r0.oven <- glm(hit ~ samplerate.s*compressiontype, data=recall.oven, na.action="na.fail")
dredge(r0.oven)

r1.oven <- r0.oven
summary(r1.oven)

#8. Plot----
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

newdat <- expand.grid(compressiontype=unique(hit$compressiontype),
                      samplerate.s = seq(0, 1, 0.01))

#8a. Effect of sample rate on p(true positive)----
h.pred <- data.frame(pred = predict(h1.oven, newdat, se=TRUE)) %>% 
  cbind(newdat) %>% 
  mutate(species="OVEN") %>% 
  rbind(data.frame(pred = predict(h1.coni, newdat, se=TRUE)) %>% 
          cbind(newdat) %>% 
          mutate(species="CONI")) %>% 
  dplyr::select(pred.fit, pred.se.fit, samplerate.s, species) %>% 
  unique() %>% 
  mutate(samplerate = samplerate.s*22050 + 22050,
         upper = pred.fit + 1.96*pred.se.fit,
         lower = pred.fit - 1.96*pred.se.fit)

h.pred$species <- factor(h.pred$species, labels=c("Common Nighthawk", "Ovenbird"))
rec$species <- factor(rec$species, labels=c("Common Nighthawk", "Ovenbird"))

h.plot <- ggplot() +
  geom_point(data=rec, aes(x=samplerate, y=precision), pch=21, colour="grey50", fill="grey70", size=2) +
  geom_ribbon(data=h.pred, aes(x=samplerate, ymin = lower, ymax=upper,), alpha = 0.3) +
  geom_line(data=h.pred, aes(x=samplerate, y=pred.fit)) +
  facet_wrap(~species) + 
  my.theme +
  theme(legend.position="none") +
  xlab("Sample rate") +
  ylab("Precision of detections") 
h.plot

#Sample rate*compression on p(recall)

#8b. Compression and sample rate effect on recall----
r.pred <- data.frame(pred = predict(r1.oven, newdat, se=TRUE, type="response")) %>% 
  cbind(newdat) %>% 
  mutate(species="OVEN") %>% 
  rbind(data.frame(pred = predict(r1.coni, newdat, se=TRUE)) %>% 
          cbind(newdat) %>% 
          mutate(species="CONI")) %>% 
  dplyr::select(pred.fit, pred.se.fit, samplerate.s, compressiontype, species) %>% 
  unique() %>% 
  mutate(samplerate = samplerate.s*22050 + 22050,
         upper = pred.fit + 1.96*pred.se.fit,
         lower = pred.fit - 1.96*pred.se.fit)

r.pred$species <- factor(r.pred$species, labels=c("Common Nighthawk", "Ovenbird"))

r.plot <- ggplot() +
  geom_ribbon(data=r.pred, aes(x=samplerate, ymin = lower, ymax=upper, group=compressiontype), alpha = 0.3) +
  geom_line(data=r.pred, aes(x=samplerate, y=pred.fit, colour=compressiontype)) +
  scale_colour_viridis_d(name="Compression treatment\n(File type_Compression rate)") +
  facet_wrap(~species) + 
  my.theme +
  xlab("Sample rate") +
  ylab("Recall of recordings")  +
  theme(legend.position="bottom")
r.plot

#8c. Put together----
rh.plot <- grid.arrange(h.plot, r.plot,
                        widths = c(1),
                        heights = c(4,5),
                        layout_matrix=rbind(c(1), c(2)))

#8d. Save----
ggsave(rh.plot, filename="figures/Recognizers.jpeg", width=6, height=8)
