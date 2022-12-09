library(readxl)
library(lme4)
library(MuMIn)
library(car)
library(tidyverse)

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
for(i in 1:length(method.list)){
  method1 <- detect[detect$method == method.list[i], ]
  for(j in 1:length(sp.list)){
    data <- method1[method1$Species == sp.list[j], ]
    m <- glm(detect ~ x -1, data = data, family = binomial("cloglog"))
    f <- fitted(m)
    edr <- list()
    for(k in 1:1000) {
      y_star <- rbinom(length(f)+1, 1, f)
      df_star <- data.frame(y = y_star, x = data$x)
      m_star <- glm(y ~ x -1, data = df_star, family = binomial("cloglog"))
      edr[[k]] <- sqrt(1/coef(m_star))
    }
    
    edr_mat <- do.call(rbind, edr)
    conf[[paste(as.character(sp.list[j]), as.character(method.list[i]), sep = ".")]] <- apply(
      edr_mat, 2, quantile, c(0.085, 0.915), na.rm = TRUE)
    conf[[paste(as.character(sp.list[j]), as.character(method.list[i]), sep = ".")]]
    
    
  }

  
}