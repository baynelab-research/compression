library(tidyverse)
library(lme4)
library(MuMIn)

#NOTE: 100 recordings with confirmed detections were scanned for each species and for each treatment combination----

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
         compressiontype = ifelse(filetype=="wav", "wav", paste0("mp3-", compressionrate)),
         compressiontype = factor(compressiontype, levels=c("wav", "mp3-96", "mp3-320")),
         samplerate.s = as.numeric(scale(samplerate))) %>% 
  dplyr::filter(!is.na(detection))

#3. Summarize at recording level----
rec <- hit %>% 
  group_by(recording, species, samplerate.s, compressiontype) %>% 
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

#6b. Score----
ggplot(hit) +
  geom_boxplot(aes(x=factor(samplerate.s), y=score, colour=species)) +
  facet_grid(species~compressiontype)

#6c. Recall----
ggplot(recall) +
  geom_smooth(aes(x=samplerate.s, y=hit, colour=species)) +
  facet_grid(species~ compressiontype)

#7. Model----

#7a. Probability of detection being true----
#CONI
hit.coni <- dplyr::filter(hit, species=="CONI")
h0.coni <- glmer(detection ~ samplerate.s*compressiontype + (1|recording),
            data=hit.coni, na.action = "na.fail", family="binomial")
dredge(h0.coni)

h1.coni <- glmer(detection ~ samplerate.s + (1|recording),
            data=hit.coni, na.action = "na.fail", family="binomial")
summary(h1.coni)
#Small effect of sample rate

#OVEN
hit.oven <- dplyr::filter(hit, species=="OVEN")
h0.oven <- glmer(detection ~ samplerate.s*compressiontype + (1|recording),
                 data=hit.oven, na.action = "na.fail", family="binomial")
dredge(h0.oven)

h1.oven <- glmer(detection ~ samplerate.s + (1|recording),
                 data=hit.oven, na.action = "na.fail", family="binomial")
summary(h1.oven)
#Strong effect of sample rate

#7b. Score value----
#CONI
s0.coni <- glmer(score ~ samplerate.s*compressiontype + (1|recording),
            data=hit.coni, na.action = "na.fail", family="Gamma")
dredge(s0.coni)

s1.coni <- glmer(score ~ compressiontype + (1|recording),
           data=hit.coni, na.action = "na.fail", family="Gamma")
summary(s1.coni)
plot(s1.coni)

#OVEN
s0.oven <- glmer(score ~ samplerate.s*compressiontype + (1|recording),
                 data=hit.oven, na.action = "na.fail", family="Gamma")
dredge(s0.oven)

s1.oven <- glmer(score ~ compressiontype + (1|recording),
                 data=hit.oven, na.action = "na.fail", family="Gamma")
summary(s1.oven)
plot(s1.oven)

#7c. Recording level recall----
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
summary(r0.oven)

#8. Plot----
#Sample rate on p(true positive)