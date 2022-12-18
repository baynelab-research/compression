library(tidyverse)
library(readxl)

#A. INDICE FILES####

#1. Read in data----
aci <- read.csv("data/acoustic indices/ACI.csv") %>% 
  pivot_longer(cols=X22050_320_mp3:X44100_or_48000_wav, names_to="treatment", values_to="aci")
adi <- read.csv("data/acoustic indices/ADI.csv") %>% 
  pivot_longer(cols=X22050_320_mp3:X44100_or_48000_wav, names_to="treatment", values_to="adi")

#2. Put together and wrangle----
rec.i <- full_join(aci, adi) %>% 
  rename(recording=X) %>% 
  rowwise() %>% 
  mutate(samplerate = as.numeric(str_sub(treatment, 2, 6)),
         compressiontype = ifelse(samplerate==44100,
                                  str_sub(treatment, 17, 100),
                                  str_sub(treatment, 8, 100)),
         sm3 = str_detect(recording, pattern='\\+')) %>% 
  separate(compressiontype, into=c("bitrate", "filetype")) %>% 
  mutate(compressiontype = ifelse(bitrate=="wav", "wav", paste0(filetype, "_", bitrate))) 

#3. Get list of recordings----
rec.i <- dat.i %>% 
  dplyr::select(recording, sm3) %>% 
  unique() %>% 
  mutate(indices=1)

#B. COMMUNITY FILES####

#1. Read in compression data----
raw <- readxl::read_excel("data/community/community_listening_data.xlsx") %>% 
  rename(t0 = '0min', t1 = '1min', t2 = '2min', species = SPECIES, id = INDIV_ID) %>% 
  dplyr::select(FileName) %>% 
  unique()

#2. Join to treatment data----
#make sure we have the 44100 Hz
rec.c <- read_excel("data/community/compression_lookup.xlsx") %>% 
  rename(FileName = Alias, samplerate = Sample_rate) %>% 
  mutate(Observer = as.character(Observer)) %>% 
  inner_join(raw) %>% 
  mutate(recording = paste0(Recording, ".wav")) %>% 
  dplyr::filter(samplerate==44100) %>% 
  dplyr::select(Unit_Type, recording) %>% 
  mutate(community=1) %>% 
  unique()

#3. Filter by what we have wav data for----
#wav <- read.csv("data/community/wavdata_1sthalf.csv")

#C. PUT TOGETHER####
rec <- full_join(rec.i, rec.c, by="recording")

use <- rec %>% 
  dplyr::filter(sm3==FALSE,
                Unit_Type!="SM3",
                indices==1,
                community==1)

write.csv(use, "data/community_indices_filestouse.csv", row.names = FALSE)
