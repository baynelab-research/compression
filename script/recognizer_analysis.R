library(tidyverse)
library(brms)
library(gridExtra)

#NOTE: 100 recordings with confirmed detections were scanned for each species and for each treatment combination

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

#3. Calculate precision----
rec <- hit %>% 
  group_by(recording, species, samplerate, samplerate.s, compressiontype) %>% 
  summarize(n=n(),
            hits = sum(detection)) %>% 
  ungroup() %>% 
  mutate(precision = hits/n)

#4. Get list of all files----
#Get list of files processed for combo that doesn't have 1000
# files <- data.frame(file = list.files("/Volumes/ECK004/AudioFiles/Compression/Recognizer Files/CONI/Treatments", recursive=TRUE, pattern="*.wav"),
#                          species="CONI") %>% 
#   rbind(data.frame(file=list.files("/Volumes/ECK004/AudioFiles/Compression/Recognizer Files/OVEN/Treatments", recursive = TRUE, pattern="*.wav"),
#                    species="OVEN")) %>% 
#   mutate(recording = str_sub(file, -36, -1)) %>% 
#   separate(file, into=c("treatment", "file"), sep="/") %>% 
#   separate(treatment, into=c("samplerate", "compressionrate", "filetype")) %>% 
#   dplyr::filter(!samplerate %in% c("results", "results1")) %>% 
#   mutate(compressiontype = ifelse(compressionrate %in% c("320", "96"), paste0("mp3_", compressionrate), "wav"),
#          samplerate = as.numeric(samplerate)) %>% 
#   dplyr::select(recording, species, samplerate, compressiontype)
# 
# write.csv(files, "data/recognizerfilelist.csv", row.names = FALSE)

files <- read.csv("data/recognizerfilelist.csv")

#5. Add recall of each file----
dat <- files %>% 
  left_join(rec) %>% 
  mutate(recall = ifelse(is.na(precision), 0, 1),
         samplerate.s = (samplerate-22050)/22050)

#6. Visualize----

#6a. Precision----
ggplot(dat) + 
  geom_smooth(aes(x=samplerate, y=precision, colour=species)) +
  facet_grid(species~compressiontype, scales="free")

#6b. Recall----
ggplot(dat) +
  geom_smooth(aes(x=samplerate, y=recall, colour=species)) +
  facet_grid(species~ compressiontype)

#7. Model----
dat.coni <- dplyr::filter(dat, species=="CONI")
dat.oven <- dplyr::filter(dat, species=="OVEN")

#7a. Probability of detection being true----
#CONI
p.coni.b <- brm(precision ~ samplerate.s*compressiontype + (1|recording),
                data = dat.coni, 
                warmup = 1000, 
                iter   = 5000, 
                chains = 3, 
                inits  = "random",
                cores  = 6)
summary(p.coni.b)
#no effect

#OVEN
p.oven.b <- brm(precision ~ samplerate.s*compressiontype + (1|recording),
                data = dat.oven, 
                warmup = 1000, 
                iter   = 5000, 
                chains = 3, 
                inits  = "random",
                cores  = 6)
summary(p.oven.b)
#effect of samplerate, so rerun

p.oven.b <- brm(precision ~ samplerate.s + (1|recording),
                data = dat.oven, 
                warmup = 1000, 
                iter   = 5000, 
                chains = 3, 
                inits  = "random",
                cores  = 6)
summary(p.oven.b)

#7b. Recording level recall----
#CONI
r.coni.b <- brm(recall ~ samplerate.s*compressiontype + (1|recording),
                family="bernoulli",
                data = dat.coni, 
                warmup = 1000, 
                iter   = 5000, 
                chains = 3, 
                inits  = "random",
                cores  = 6)
summary(r.coni.b)
#marginal effect of 96 compression rate

#OVEN
r.oven.b <- brm(recall ~ samplerate.s*compressiontype + (1|recording),
                family="bernoulli",
                data = dat.oven, 
                warmup = 1000, 
                iter   = 5000, 
                chains = 3, 
                inits  = "random",
                cores  = 6)
summary(r.oven.b)

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

#8d. Save----
ggsave(h.plot, filename="figures/Recognizers.jpeg", width=6, height=4)
