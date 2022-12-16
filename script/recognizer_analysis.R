library(tidyverse)
library(brms)
library(bayesplot)
library(tidybayes)
library(ggridges)

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
         samplerate = factor(samplerate, levels=c("22050", "32000", "44100"))) %>% 
  dplyr::filter(!is.na(detection))

#3. Calculate precision----
rec <- hit %>% 
  group_by(recording, species, samplerate, compressiontype) %>% 
  summarize(n=n(),
            hits = sum(detection)) %>% 
  ungroup() %>% 
  mutate(precision = hits/n,
         precision = ifelse(precision==1, 0.99999999, precision),
         precision = ifelse(precision==0, 0.00000001, precision))

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

files <- read.csv("data/recognizerfilelist.csv") %>% 
  mutate(samplerate = factor(samplerate, levels=c("22050", "32000", "44100")),
         compressiontype = factor(compressiontype, levels=c("wav", "mp3_320", "mp3_96")))

#5. Add recall of each file----
dat <- files %>% 
  left_join(rec) %>% 
  mutate(recall = ifelse(is.na(precision), 0, 1))

#6. Visualize----

#6a. Precision----
ggplot(dat) + 
  geom_point(aes(x=samplerate, y=precision, colour=species)) +
  facet_grid(species~compressiontype, scales="free")

#6b. Recall----
ggplot(dat) +
  geom_jitter(aes(x=samplerate, y=recall, colour=species)) +
  facet_grid(species~ compressiontype)

#7. Model----

#https://biol609.github.io/lectures/23c_brms_prediction.html#3_prediction_with_brms_and_mixed_models

dat.coni <- dplyr::filter(dat, species=="CONI")
dat.oven <- dplyr::filter(dat, species=="OVEN")

priors <- c(prior(normal(100, 10000), class = "Intercept"),
              prior(normal(0,10), class = "b", coef ="compressiontypemp3_96"),
              prior(normal(0,10), class = "b", coef = "compressiontypemp3_320"),
              prior(normal(0,10), class = "b", coef = "samplerate32000"),
            prior(normal(0,10), class = "b", coef = "samplerate44100"),
              prior(normal(0,10), class = "b", coef = "samplerate32000:compressiontypemp3_96"),
              prior(normal(0,10), class = "b", coef = "samplerate32000:compressiontypemp3_320"),
            prior(normal(0,10), class = "b", coef = "samplerate44100:compressiontypemp3_96"),
            prior(normal(0,10), class = "b", coef = "samplerate44100:compressiontypemp3_320"))

#7a. CONI
p.coni.b <- brm(precision ~ samplerate*compressiontype + (1|recording),
                family=Beta(),
                data = dat.coni, 
                warmup = 1000, 
                iter   = 20000, 
                chains = 3, 
                inits  = "random",
                cores  = 6,
                prior=priors)

summary(p.coni.b)

# #Inspect mixing
# plot(p.coni.b)
# #Inspect rhat
# mcmc_rhat(rhat(p.coni.b))
# #Inspect neff
# mcmc_neff(neff_ratio(p.coni.b))
# #Inspect residuals
# preds <- dat.coni %>%
#   dplyr::filter(!is.na(precision)) %>% 
#   add_predicted_draws(p.coni.b)
# qres <- preds %>% 
#   summarise(quant_residual = mean(.prediction < precision)) 
# ggplot(qres, aes(sample = quant_residual)) +
#   geom_qq(distribution = stats::qunif) +
#   geom_qq_line(distribution = stats::qunif)
# #Inspect fit
# ggplot(preds) + 
#   stat_pointinterval(aes(x = .row, y = .prediction)) +
#   geom_point(aes(x = .row, y = precision), color = "red")
# #check distribution
# pp_check(p.coni.b, ndraws = 500)

#7b. OVEN
p.oven.b <- brm(precision ~ samplerate*compressiontype + (1|recording),
                family=Beta(),
                data = dplyr::filter(dat.oven, !is.na(precision)), 
                warmup = 1000, 
                iter   = 20000, 
                chains = 3, 
                inits  = "random",
                cores  = 6,
                prior = priors)

summary(p.oven.b)

#8. Predict----
newdat <- expand.grid(samplerate = unique(dat$samplerate),
                      compressiontype = unique(dat$compressiontype))

p.coni.pred <- newdat %>% 
  add_epred_draws(p.coni.b,
                   re_formula = NA,
                   ndraws = 1000) %>% 
  mutate(species="CONI")

p.oven.pred <- newdat %>% 
  add_epred_draws(p.oven.b,
                   re_formula = NA,
                   ndraws = 1000) %>% 
  mutate(species="OVEN")

pred <- rbind(p.coni.pred, p.oven.pred)
pred$species <- factor(pred$species, labels=c("Common nighthawk (CONI)", "Ovenbird (OVEN)"))

#9. Plot----
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

recognizer.plot <- ggplot(pred) +
  geom_density_ridges(aes(x=.epred, y=samplerate, fill=compressiontype), colour="grey30", alpha = 0.5) +
  scale_fill_viridis_d(name="Compression type\n(file type_bit rate") +
  ylab("Sample rate (Hz)") +
  xlab("Precision") +
  my.theme +
  theme(legend.position = "bottom") +
  facet_wrap(~species, scales="free_x")

ggsave(recognizer.plot, filename="figures/Recognizers.jpeg", width=8, height=6)

#10. Summary stats----
mean(dat.coni$precision, na.rm=TRUE)
mean(dat.oven$precision, na.rm=TRUE)
mean(dat.coni$recall, na.rm=TRUE)
mean(dat.oven$recall, na.rm=TRUE)

dat %>% 
  group_by(species, samplerate, compressiontype) %>% 
  summarize(n=n(),
            hits = sum(recall)) %>% 
  ungroup() %>% 
  mutate(recall=hits/n)
