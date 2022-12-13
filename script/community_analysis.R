library(tidyverse)
library(readxl)
library(dplyr)
library(MuMIn)

#1. Read in data----
raw <- readxl::read_excel("data/community/community_listening_data.xlsx")

#2. Wrangle---- 
#Take out amphibians
#Take out mammals
#Take out farm animals
#Take out noise codes
#Select only relevant columns
#Rename some stuff
#Flag TMTTs properly
dat <- raw %>% 
  dplyr::filter(TMTC %in% c(-9, 1, 2),
                !str_sub(SPECIES, 1, 2) %in% c("UN"),
                !SPECIES %in% c("CATO", "RESQ", "BEAV", "BCFR", "CHIK", "COYT", "DOGG"),
                !SPECIES %in% c("HEAI", "HEBA", "HEDT", "HENO", "HERA", "HETR", "HEWI", "LIAI", "LIBA", "LINO", "LIRA", "LITR", "LIWI", "MOAI", "MOBA", "MONO", "MORA", "MOTR", "MOWI")) %>% 
  rename(t0 = '0min', t1 = '1min', t2 = '2min', species = SPECIES, id = INDIV_ID) %>% 
  dplyr::select(FileName, Observer, Rain, Wind, Industry, Noise, Microphone, ProsTime, species, id, t0:t2, TMTC, VT, ToBeChecked)

#3. Replace tmtts----

#3a. Read in lookup----
tmtt <- read.csv("data/tmtt_predictions.csv")

#3b. Replace with values from lookup and bind back to table----
dat.tmtt <- dat %>%
  dplyr::filter(TMTC==2) %>% 
  mutate(species_code = ifelse(species%in% tmtt$species_code, species, "species"),
         user_id = as.integer(ifelse(Observer %in% tmtt$user_id, Observer, 0))) %>%
  data.frame() %>%
  left_join(tmtt, by=c("species_code", "user_id")) %>% 
  mutate(abundance = round(pred)) %>% 
  dplyr::select(c(colnames(dat), abundance)) %>% 
  rbind(dat %>% 
          dplyr::filter(TMTC!=2) %>% 
          mutate(abundance=1))

#4. Join to treatment data----
treat <- read_excel("data/community/compression_lookup.xlsx") %>% 
  rename(FileName = Alias, samplerate = Sample_rate) %>% 
  mutate(Observer = as.character(Observer))

#NEED TO FIGURE OUT WTF IS GOING ON HERE. Best guess is below, but this is different from elsewhere
table(treat$samplerate, treat$Bitrate)

dat.treat <- dat.tmtt %>% 
  left_join(treat) %>% 
  mutate(compressiontype = case_when(Bitrate==96 ~ "mp3_96",
                                     Bitrate==192 ~ "mp3_192",
                                     Bitrate==32192 ~ "wav")) %>% 
  dplyr::filter(!is.na(Bitrate))
#7 FILES MISSING FROM LOOKUP

#5. Summarize----

#5a. Abundance----
dat.abun <- dat.treat %>% 
  group_by(samplerate, compressiontype, FileName, species) %>% 
  summarize(abundance = sum(abundance)) %>% 
  ungroup()

#5b. Richness----
dat.rich <- dat.abun %>% 
  group_by(samplerate, compressiontype, FileName) %>% 
  summarize(n=n()) %>% 
  ungroup()

#6. Visualize----

#6a. Abundance----
ggplot(dat.abun) +
  geom_violin(aes(x=factor(samplerate), y=abundance, colour=compressiontype))

#6b. Richness----
ggplot(dat.rich) +
  geom_boxplot(aes(x=factor(samplerate), y=n, colour=compressiontype))

#7. Analysis----

#7a. Richness---
sr.list <- unique(dat.rich$samplerate)
ci.rich <- data.frame()
for(i in 1:length(sr.list)){
  x <- dat.rich %>%
    subset(samplerate == sr.list[i])
  ct.list <- unique(x$compressiontype)
  for(j in 1:length(ct.list)){
    y <- x %>%
      subset(compressiontype == ct.list[j])
    z <- as.data.frame(quantile(y$n, c(0.085,0.915), na.rm = TRUE))
    z$ci <- rownames(z)
    z$samplerate <- sr.list[i]
    z$compressiontype <- ct.list[j]
    z$mean.rich <- mean(y$n)
    ci.rich <- rbind(ci.rich,z)
  }
}
m1 <- glm(n ~ 1, data = dat.rich)
m2 <- glm(n ~ samplerate, data = dat.rich)
m3 <- glm(n ~ compressiontype, data = dat.rich)
m4 <- glm(n ~ compressiontype + samplerate, data = dat.rich)
m5 <- glm(n ~ compressiontype*samplerate, data = dat.rich)
model.sel(m1,m2,m3,m4,m5)

#7b. Abundance
file.list <- unique(dat.abun$FileName)
dat.ind <- data.frame()
y <- data.frame(samplerate=NA,compressiontype=NA,FileName=NA,sum=NA)
for(i in 1:length(file.list)){
  x <- dat.abun %>%
    subset(FileName == file.list[i])
  y$samplerate <- x$samplerate[1]
  y$compressiontype <- x$compressiontype[1]
  y$FileName <- x$FileName[1]
  y$sum <- sum(x$abundance)
  dat.ind <- rbind(dat.ind, y)
}

sr.list <- unique(dat.ind$samplerate)
ci.ind <- data.frame()
for(i in 1:length(sr.list)){
  x <- dat.ind %>%
    subset(samplerate == sr.list[i])
  ct.list <- unique(x$compressiontype)
  for(j in 1:length(ct.list)){
    y <- x %>%
      subset(compressiontype == ct.list[j])
    z <- as.data.frame(quantile(y$sum, c(0.085,0.915), na.rm = TRUE))
    z$ci <- rownames(z)
    z$samplerate <- sr.list[i]
    z$compressiontype <- ct.list[j]
    z$mean.ind <- mean(y$sum)
    ci.ind <- rbind(ci.ind,z)
  }
}

n1 <- glm(sum ~ 1, data = dat.ind)
n2 <- glm(sum ~ samplerate, data = dat.ind)
n3 <- glm(sum ~ compressiontype, data = dat.ind)
n4 <- glm(sum ~ compressiontype + samplerate, data = dat.ind)
n5 <- glm(sum ~ compressiontype*samplerate, data = dat.ind)
model.sel(n1,n2,n3,n4,n5)