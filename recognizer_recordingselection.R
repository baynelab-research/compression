library(tidyverse)

#1. Read in files----
#These are recognizer results files from Elly's Statistical Validation paper in Ecological Applications
coni <- read.csv("data/recognizer/UP_CONIpeent0mv2_20_20_results_validated.csv")
oven <- read.csv("data/recognizer/UP2015_OVEN0mv1_20_50_results_validated.csv")

#2. Select files with confirmed detections----

#2a. List of available files----
setwd("/Volumes/ECK001/AudioFiles/UP/2015/3MIN")
files <- data.frame(HDpath = list.files(recursive = TRUE)) %>% 
  separate(HDpath, into=c("f1", "recording"), sep="/", remove=FALSE)

#2b. CONI files with confirmed detections----
coni.1 <- coni %>% 
  dplyr::filter(score > 60,
                CONI==1) %>% 
  group_by(path, recording) %>% 
  summarize(hits = sum(CONI)) %>% 
  ungroup() %>% 
  mutate(species="CONI")

#2c. Pick 100 files----
coni.files <- coni.1 %>% 
  mutate(recording.coni = str_replace_all(recording, "[$]", "_")) %>% 
  separate(recording.coni, into=c("station", "datename", "fileend"), sep="_", remove=FALSE) %>% 
  separate(fileend, into=c("timename", "filetype"), remove=TRUE) %>% 
  mutate(recording=paste0(station,"_",datename,"_",timename,"_000.wav")) %>% 
  left_join(files) %>% 
  dplyr::filter(!is.na(f1)) %>% 
  dplyr::filter(!str_sub(recording, 1, 5)=="UP-08") %>% 
  arrange(-hits) %>% 
  head(100) %>% 
  dplyr::select(species, HDpath, recording, hits)

#2d. Pick 100 oven files----
oven.1 <- oven %>% 
  dplyr::filter(oven==1) %>% 
  group_by(filepath, filename) %>% 
  summarize(hits = sum(oven)) %>% 
  ungroup() %>% 
  arrange(-hits) %>% 
  head(100) %>% 
  rename(path=filepath, recording=filename) %>% 
  mutate(species="OVEN")

oven.files <- oven.1 %>% 
  left_join(files) %>% 
  dplyr::select(species, HDpath, recording, hits)

#3. Make list of files to copy----
selected <- rbind(coni.files, oven.files) %>% 
  mutate(from = paste0("/Volumes/ECK001/AudioFiles/UP/2015/3MIN/",HDpath),
         to = paste0("/Volumes/baynelabworkspace/ListeningCrew/FileCompression/Recognizer Files/",species,"/Original/",recording))

#4. Copy files----
for(i in 179:200){
  file.copy(from = selected$from[i], to = selected$to[i])
  print(paste0("Copied file ", selected$recording[i], " - ", i, " of ", nrow(selected), " files"))
}

