library(tidyverse)
library(readxl)

#A. INDICE FILES####

#1. Read in data----
aci <- read.csv("data/acoustic indices/ACI.csv") %>% 
  pivot_longer(cols=X22050_320_mp3:X44100_or_48000_wav, names_to="treatment", values_to="aci")
adi <- read.csv("data/acoustic indices/ADI.csv") %>% 
  pivot_longer(cols=X22050_320_mp3:X44100_or_48000_wav, names_to="treatment", values_to="adi")

#2. Put together and wrangle----
#Take out the leading zeros
dat.i <- full_join(aci, adi) %>% 
  rename(recording=X) %>% 
  rowwise() %>% 
  mutate(samplerate = as.numeric(str_sub(treatment, 2, 6)),
         compressiontype = ifelse(samplerate==44100,
                                  str_sub(treatment, 17, 100),
                                  str_sub(treatment, 8, 100)),
         sm3 = str_detect(recording, pattern='\\+')) %>% 
  separate(compressiontype, into=c("bitrate", "filetype")) %>% 
  mutate(compressiontype = ifelse(bitrate=="wav", "wav", paste0(filetype, "_", bitrate)),
         recording0 = gsub(recording, pattern="-000", replacement = "-"),
         recording0 = gsub(recording0, pattern="-00", replacement = "-"),
         recording0 = gsub(recording0, pattern="-0", replacement = "-")) 

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

#2. See we have wav data for----
wav <- read.csv("data/community/wavdata_1sthalf.csv")

rec.w <- wav %>% 
  dplyr::select(se) %>% 
  unique() %>% 
  rename(recording0=se) %>% 
  mutate(recording0 = paste0(recording0, ".wav"))

#2. Join to treatment data and filter by wav availability----
#make sure we have the 44100 Hz
rec.c <- read_excel("data/community/compression_lookup.xlsx") %>% 
  rename(FileName = Alias, samplerate = Sample_rate) %>% 
  mutate(Observer = as.character(Observer)) %>% 
  inner_join(raw) %>% 
  mutate(recording = paste0(Recording, ".wav")) %>% 
  dplyr::filter(samplerate==44100) %>% 
  dplyr::select(Unit_Type, recording) %>% 
  mutate(community=1) %>% 
  unique() %>% 
  mutate(recording0 = gsub(recording, pattern="-000", replacement = "-"),
         recording0 = gsub(recording0, pattern="-00", replacement = "-"),
         recording0 = gsub(recording0, pattern="-0", replacement = "-")) %>% 
  inner_join(rec.w)

#C. PUT TOGETHER####
rec <- full_join(rec.i, rec.c, by="recording")

use <- rec %>% 
  dplyr::filter(sm3==FALSE,
                Unit_Type!="SM3",
                indices==1,
                community==1)

write.csv(use, "data/community_indices_filestouse.csv", row.names = FALSE)





%>% 
  mutate(recording = case_when(str_sub(se, 1, 5) %in% c("BG-10", "BG-11", "BG-12", "BG-13") ~ gsub(se, pattern="BG-", replacement="BG-00"),
                               str_sub(se, 1, 2)=="BG" ~ gsub(se, pattern="BG-", replacement="BG-000"),
                               str_sub(se, 1, 7)=="FMX-6-4" ~ gsub(se, pattern="FMX-6-4", replacement="FMX-06-04"),
                               str_sub(se, 1, 7)=="FMX-7-8" ~ gsub(se, pattern="FMX-7-8", replacement="FMX-07-08"),
                               str_sub(se, 1, 4)=="HL-2" ~ gsub(se, pattern="HL-2-", replacement="HL-2-0"),
                               str_sub(se, 1, 4)=="HL-3" ~ gsub(se, pattern="HL-3-", replacement="HL-3-0"),
                               str_sub(se, 1, 4)=="HL-6" ~ gsub(se, pattern="HL-6-", replacement="HL-6-0")))s