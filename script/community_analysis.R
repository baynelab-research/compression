library(tidyverse)
library(readxl)
library(brms)
library(bayesplot)
library(tidybayes)
library(ggridges)

#1. Read in data----
raw <- readxl::read_excel("data/community/community_listening_data.xlsx")
wav <- read.csv("data/community/wavdata_1sthalf.csv") %>% 
  unique()
lu <- read_excel("data/community/compression_lookup.xlsx")

#2. Put listening files together and join to lookup----
colnms <- c("recording", "observer", "species", "id", "abundance", "samplerate", "compressiontype", "TMTC")

all.w <- wav %>% 
  dplyr::select(-observer) %>% 
  rename(recording=se, species=species_code, id=individual_appearance_order, observer=observer_id) %>% 
  mutate(samplerate=44100,
         compressiontype="wav",
         TMTC=1) %>% 
  dplyr::select(all_of(colnms)) %>% 
  unique()

all <- lu %>% 
  rename(FileName = Alias, samplerate = Sample_rate) %>% 
  mutate(Observer = as.character(Observer)) %>% 
  inner_join(raw) %>% 
  rename(t0 = '0min', t1 = '1min', t2 = '2min', species = SPECIES, id = INDIV_ID, observer=Observer) %>% 
  mutate(recording = gsub(Recording, pattern="-000", replacement = "-"),
         recording = gsub(recording, pattern="-00", replacement = "-"),
         recording = gsub(recording, pattern="-0", replacement = "-"),
         abundance=ifelse(TMTC==2, "TMTT", 1),
         compressiontype= case_when(Bitrate==96 ~ "mp3_96",
                                    Bitrate %in% c(192, 32192) ~ "mp3_320"),
         observer = case_when(observer=="1" ~ 15,
                              observer=="118" ~ 531,
                              observer=="23" ~ 18,
                              observer=="37" ~ 41,
                              observer=="7" ~ 11)) %>% 
  dplyr::select(all_of(colnms)) %>% 
  rbind(all.w)
  
#3. Filter---- 
#Take out amphibians
#Take out mammals
#Take out farm animals
#Take out noise codes
#Take out SM3s
use <- all %>% 
  dplyr::filter(TMTC %in% c(-9, 1, 2),
                !str_sub(species, 1, 2) %in% c("UN"),
                !species %in% c("CATO", "RESQ", "BEAV", "BCFR", "CHIK", "COYT", "DOGG", "WOFR", "WETO"),
                !species %in% c("HEAI", "HEBA", "HEDT", "HENO", "HERA", "HETR", "HEWI", "LIAI", "LIBA", "LINO", "LIRA", "LITR", "LIWI", "MOAI", "MOBA", "MONO", "MORA", "MOTR", "MOWI"),
                samplerate==44100)

#4. Filter to just recordings with all 3 compression treatments----
rec <- use %>% 
  dplyr::select(recording, samplerate, compressiontype) %>% 
  unique() %>% 
  group_by(recording) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  dplyr::filter(n==3)

use.rec <- use %>% 
  dplyr::filter(recording %in% rec$recording)

#5. Replace tmtts----

#5a. Read in lookup----
tmtt <- read.csv("tmtt_predictions.csv")

#5b. Replace with values from lookup and bind back to table----
use.tmtt <- use.rec %>%
  dplyr::filter(abundance=="TMTT") %>% 
  mutate(species_code = ifelse(species%in% tmtt$species_code, species, "species"),
         user_id=as.numeric(ifelse(observer %in% tmtt$user_id, observer, 0))) %>%
  data.frame() %>%
  left_join(tmtt, by=c("species_code", "user_id")) %>% 
  mutate(abundance = round(pred)) %>% 
  dplyr::select(c(colnames(use.rec), abundance, user_id)) %>% 
  rbind(use.rec %>% 
          dplyr::filter(abundance!="TMTT") %>% 
          mutate(user_id=observer)) %>% 
  mutate(abundance = as.numeric(abundance))

#6. Summarize----

#6a. Abundance----
dat.abun <- use.tmtt %>% 
  group_by(samplerate, compressiontype, recording, user_id) %>% 
  summarize(abundance = sum(abundance)) %>%
  ungroup()

#6b. Richness----
dat.rich <- use.tmtt %>% 
  dplyr::select(samplerate, compressiontype, recording, user_id, species) %>% 
  unique() %>% 
  group_by(samplerate, compressiontype, recording, user_id) %>% 
  summarize(richness=n()) %>% 
  ungroup()

#6c. Put together----
#Take out 1 recording that was processed twice
dat <- full_join(dat.abun, dat.rich) %>% 
  dplyr::filter(!(user_id==531 & recording=="Y-5-217-CT_20160625_020000" & compressiontype=="wav"))

#6d. Make factors----
dat$compressiontype <- factor(dat$compressiontype, levels=c("wav", "mp3_320", "mp3_96"))

#7. Visualize----

#7a. Abundance----
ggplot(dat) +
  geom_boxplot(aes(x=factor(samplerate), y=abundance, colour=compressiontype))

#7b. Richness----
ggplot(dat) +
  geom_boxplot(aes(x=factor(samplerate), y=richness, colour=compressiontype))

#8. Model----
priors <- c(prior(normal(0,10), class = "Intercept"),
                prior(normal(0,10), class = "b", coef ="compressiontypemp3_96"),
                prior(normal(0,10), class = "b", coef = "compressiontypemp3_320"))

#8a. Abundance----
abun.b <- brm(abundance ~ compressiontype + (1|recording) + (1|user_id),
             data = dat, 
             warmup = 1000, 
             iter   = 20000, 
             chains = 3, 
             inits  = "random",
             cores  = 6,
             prior=priors)

summary(abun.b)

#8b. Richness
rich.b <- brm(richness ~ compressiontype + (1|recording) + (1|user_id),
              data = dat, 
              warmup = 1000, 
              iter   = 20000, 
              chains = 3, 
              inits  = "random",
              cores  = 6,
              prior=priors)

summary(rich.b)

#9. Predict----
newdat <- data.frame(compressiontype = unique(dat$compressiontype),
                     samplerate=44100)

abun.pred <- newdat %>% 
  add_epred_draws(abun.b,
                  re_formula = NA,
                  ndraws = 1000) %>% 
  mutate(metric="Total abundance")

rich.pred <- newdat %>% 
  add_epred_draws(rich.b,
                  re_formula = NA,
                  ndraws = 1000) %>% 
  mutate(metric="Species richness")

pred <- rbind(abun.pred, rich.pred)

#10. Plot----
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

community.plot <- ggplot(pred) +
  geom_density_ridges(aes(x=.epred, y=samplerate, fill=compressiontype), colour="grey30", alpha = 0.5) +
  scale_fill_viridis_d(name="Compression type\n(file type_bitrate)") +
 ylab("") +
 xlab("") +
  my.theme +
  theme(legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank()) +
  facet_wrap(~metric, scales="free_x")
community.plot

ggsave(community.plot, filename="figures/Community.jpeg", width=8, height=5)

