library(readxl)
library(lme4)
library(MuMIn)
library(car)

setwd("C:/Users/yipd/Documents/compression_analysis/")

u16 <- read_excel("qry_detections_comp16.xlsx")
u37 <- read_excel("qry_detections_comp37.xlsx")
u38 <- read_excel("qry_detections_comp38.xlsx")
u39 <- read_excel("qry_detections_comp39.xlsx")

detect <- rbind(u16,u37,u38,u39)
detect <- subset(detect, Site == 5)
as.data.frame(table(unlist(detect$Observer)))

#write.csv(detect, file = "detect.csv")
detect <- read.csv("detect.csv")
sp <- detect[!grepl("Hz", detect$spp), ]
tn <- detect[grep("Hz", detect$spp), ]
tn$detect <- ifelse(tn$spp.id == "TONE", "1", "0")
detect <- rbind(sp, tn)
detect$method <- ifelse(detect$obs == "16" | detect$obs == "38",
                        "wav", "mp3")
detect$detect <- as.numeric(detect$detect)
detect$x <- -detect$dist^2
detect$obs[detect$obs == 37] <- 16
detect$obs[detect$obs == 39] <- 38
detect$obs <- as.factor(detect$obs)
mp3 <- detect[detect$method == "mp3", ]
wav <- detect[detect$method == "wav", ]

null <- glmer(detect ~ dist + (1|obs), data = detect, family = binomial)
m1 <- glmer(detect ~ method + (1|obs), data = detect, family = binomial)
m2 <- glmer(detect ~ spp + (1|obs), data = detect, family = binomial)
m3 <- glmer(detect ~ dist + method + (1|obs), data = detect, family = binomial)
m4 <- glmer(detect ~ spp + method + (1|obs), data = detect, family = binomial)
m5 <- glmer(detect ~ dist + spp + (1|obs), data = detect, family = binomial)
global <- glmer(detect ~ dist + method + spp + (1|obs), data = detect, family = binomial)
model.sel(null, m1, m2, m3, m4, m5, global)

lr <- glm(detect ~ dist + method, data = detect, family = "binomial")
lr1 <- glm(detect ~ dist, data = detect, family = "binomial")
lr2 <- glm(detect ~ method, data = detect, family = "binomial")
model.sel(lr, lr1, lr2)

sp.list <- unique(detect$spp)
method.list <- unique(detect$method)
conf <- list()
for(i in 1:length(method.list)){
  method1 <- detect[detect$method == method.list[i], ]
  for(j in 1:length(sp.list)){
    data <- method1[method1$spp == sp.list[j], ]
    m <- glm(detect ~ x -1, data = data, family = binomial("cloglog"))
    f <- fitted(m)
    edr <- list()
    for(k in 1:1000) {
      y_star <- rbinom(length(f), 1, f)
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


for(i in 1:length(method.list)){
  method1 <- detect[detect$method == method.list[i], ]
  m <- glm(detect ~ x -1, data = method1, family = binomial("cloglog"))
  f <- fitted(m)
  edr <- list()
  for(k in 1:1000) {
    y_star <- rbinom(length(f), 1, f)
    df_star <- data.frame(y = y_star, x = method1$x)
    m_star <- glm(y ~ x -1, data = df_star, family = binomial("cloglog"))
    edr[[k]] <- sqrt(1/coef(m_star))
  }
    
  edr_mat <- do.call(rbind, edr)
  conf[[paste(as.character(method.list[i]), sep = "")]] <- apply(
    edr_mat, 2, quantile, c(0.085, 0.915), na.rm = TRUE)
    
    
}
  
  
