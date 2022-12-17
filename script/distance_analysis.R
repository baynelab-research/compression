library(readxl)
library(lme4)
library(MuMIn)
library(car)
library(tidyverse)
library(ggridges)

#1. Read in data----
u16 <- read_excel("data/detection distance/qry_detections_comp16.xlsx")
u37 <- read_excel("data/detection distance/qry_detections_comp37.xlsx")
u38 <- read_excel("data/detection distance/qry_detections_comp38.xlsx")
u39 <- read_excel("data/detection distance/qry_detections_comp39.xlsx")

#2. Bind together----
detect <- rbind(u16,u37,u38,u39) %>% 
  rename(dist = 'Playback Distance (m)')

#3. Remove blank lines of data----
detect <- subset(detect, Site == 5)
as.data.frame(table(unlist(detect$Observer)))

#4. Remove Hz from tag name----
sp <- detect[!grepl("Hz", detect$Species), ]
tn <- detect[grep("Hz", detect$Species), ]

#5. Determine if detected----
tn$detect <- ifelse(tn$Identification1 == "TONE", 1, 0)
sp$detect <- ifelse(sp$Species==sp$Identification1, 1, 0)
detect <- rbind(sp, tn)

#6. Assigning method----
detect$method <- ifelse(detect$Observer == "16" | detect$Observer == "38",
                        "wav", "mp3")

#7. Square for EDR----
detect$x <- -detect$dist^2

#8. Tidy observer IDs----
detect$Observer[detect$Observer == 37] <- 16
detect$Observer[detect$Observer == 39] <- 38
detect$Observer <- as.factor(detect$Observer)

#9. Split into file type----
mp3 <- detect[detect$method == "mp3", ]
wav <- detect[detect$method == "wav", ]

#10. Make some models----
m1 <- glmer(detect ~ dist + (1|Observer) + (1|Species), data = detect, family = binomial)
m2 <- glmer(detect ~ method + (1|Observer) + (1|Species), data = detect, family = binomial)
m3 <- glmer(detect ~ dist + method + (1|Observer) + (1|Species), data = detect, family = binomial)
null <- glmer(detect ~ 1 + (1|Observer) + (1|Species), data = detect, family = binomial)
model.sel(m1, m2, m3, null)
summary(m1)

#11. Try without species and observer RE----
lr <- glm(detect ~ dist + method, data = detect, family = "binomial")
lr1 <- glm(detect ~ dist, data = detect, family = "binomial")
lr2 <- glm(detect ~ method, data = detect, family = "binomial")
model.sel(lr, lr1, lr2)

#12. EDR calculations----
sp.list <- unique(detect$Species)
method.list <- unique(detect$method)
conf <- list()
edr.dat <- data.frame()
edr_mat <- list()
for(i in 1:length(method.list)){
  method1 <- detect[detect$method == method.list[i], ]
  for(j in 1:length(sp.list)){
    data <- method1[method1$Species == sp.list[j], ]
    data <- data[!is.na(data$detect),]
    m <- glm(detect ~ x -1, data = data, family = binomial("cloglog"))
    f <- fitted(m)
    edr <- list()
    for(k in 1:1000) {
      y_star <- rbinom(length(f), 1, f)
      df_star <- data.frame(y = y_star, x = data$x)
      m_star <- glm(y ~ x -1, data = df_star, family = binomial("cloglog"))
      edr[[k]] <- sqrt(1/coef(m_star))
    }
    
    edr_mat[[j]] <- data.frame(do.call(rbind, edr)) %>% 
      rename(edr=x) %>% 
      mutate(species=sp.list[j],
             compression=method.list[i])
    conf[[paste(as.character(sp.list[j]), as.character(method.list[i]), sep = ".")]] <- quantile(edr_mat[[j]]$edr,c(0.085, 0.915), na.rm = TRUE)
    conf[[paste(as.character(sp.list[j]), as.character(method.list[i]), sep = ".")]]
    
  }
  
  edr.dat <- do.call(rbind, edr_mat) %>% 
    rbind(edr.dat)

}

CI83 <- do.call(rbind.data.frame, conf)
CI83$index <- rownames(CI83)
CI83 <- separate(data=CI83, col=index, into=c("Species","method","limit","misc"))
CI_wide <- CI83 %>%
  pivot_wider(names_from = limit, values_from = x)
CI_wide <- CI_wide[-c(3)]
colnames(CI_wide) <- c("Species","method","lci","uci")

CI_wide$mean <- ((CI_wide$uci - CI_wide$lci)/2) + CI_wide$lci
CI_wide$ci <- ((CI_wide$uci - CI_wide$lci)/2)
CI_wide <- filter(CI_wide, !(Species %in% c("1000Hz","1414Hz","2000Hz","11313Hz","BOOW","GGOW","BAOW")))

#13. Plot results----
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

edr.dat2 <- edr.dat %>% 
  group_by(species, compression) %>% 
  dplyr::filter(edr >= quantile(edr, 0.085, na.rm=TRUE),
                edr <= quantile(edr, 0.915, na.rm=TRUE)) %>% 
  dplyr::filter(!species %in% c("1000Hz","1414Hz","2000Hz","11313Hz","BOOW","GGOW","BAOW")) %>% 
  mutate(species = ifelse(species=="BLWA", "CMWA", species))

edr.dat2$compression <- factor(edr.dat2$compression, levels=c("wav", "mp3"), labels=c("wav", "mp3_320"))
edr.dat2$species <- factor(edr.dat2$species, labels=c("White-throated sparrow",
                                                      "Warbling vireo",
                                                      "Tenessee warbler",
                                                      "Red-breasted nuthatch",
                                                      "Rose-breasted grosbeak",
                                                      "Pine siskin",
                                                      "Ovenbird",
                                                      "Olive-sided flycatcher",
                                                      "Northern saw-what owl",
                                                      "Lincoln's sparrow",
                                                      "Long-eared owl",
                                                      "Dark-eyed junco",
                                                      "Common raven",
                                                      "Clay-coloured sparrow",
                                                      "Cape may warbler",
                                                      "Brown-headed cowbird",
                                                      "Belted kingfisher",
                                                      "Bay-breasted warbler",
                                                      "Black-and-white warbler",
                                                      "8000 Hz tone",
                                                      "5656 Hz tone",
                                                      "4000 Hz tone",
                                                      "28282 Hz tone"))

clrs <- viridis::viridis(3)

edr.plot <- ggplot(edr.dat2) +
  geom_density_ridges(aes(x=edr, y=species, fill=compression), colour="grey30", alpha = 0.5) +
  scale_fill_manual(name="Compression type\n(file type_bit rate)", values=clrs[c(1,2)]) +
  scale_y_discrete(limits=rev) +
  xlab("Effective detection radius (m)") +
  my.theme +
  theme(legend.position = "bottom",
        axis.title.y = element_blank())
edr.plot

ggsave(edr.plot, filename="figures/Distance.jpeg", width=8, height=10)

