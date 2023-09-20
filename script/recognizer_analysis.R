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
         samplerate = factor(samplerate, levels=c("22050", "32000", "44100")),
         score = ifelse(score==1, 0.99999999, score)) %>% 
  dplyr::filter(!is.na(detection))

#3. Create gold standard set-----
gold <- hit %>% 
  dplyr::filter(detection==1) %>% 
  dplyr::select(species, recording, time) %>% 
  merge(expand.grid(samplerate = unique(hit$samplerate),
                    compressiontype = unique(hit$compressiontype))) %>% 
  mutate(gold=1)

#3. Join gold standards back to hits----
hit.gold <- hit %>% 
  full_join(gold) %>% 
  mutate(gold = ifelse(is.na(gold), 0, 1),
         fn = ifelse(gold==1 & is.na(detection), 1, 0),
         hit = ifelse(!is.na(detection), 1, 0),
         detection = ifelse(is.na(detection), 0, detection)) 

#5. Calculate precision & recall----
rec <- hit.gold %>% 
  group_by(recording, species, samplerate, compressiontype) %>% 
  summarize(hits=sum(hit),
            detections = sum(detection),
            misses = sum(fn),
            score = mean(score, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(precision = detections/hits,
         recall = detections/(detections+misses),
         precision = ifelse(precision==1, 0.99999999, precision),
         precision = ifelse(precision==0, NA, precision),
         recall = ifelse(recall==1, 0.99999999, recall),
         recall = ifelse(recall==0, 0.00000001, recall))

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

#manually add in the file that is missing from the list
files <- read.csv("data/recognizerfilelist.csv") %>% 
  mutate(samplerate = factor(samplerate, levels=c("22050", "32000", "44100")),
         compressiontype = factor(compressiontype, levels=c("wav", "mp3_320", "mp3_96"))) %>% 
  rbind(data.frame(recording="UP-09-057-OG_20150701_050000_000.wav", species="OVEN", samplerate="44100", compressiontype="mp3_96"))

#5. Add recall of each file and recording treatment type----
dat <- files %>% 
  full_join(rec) %>% 
  mutate(recall.r = ifelse(is.na(precision), 0, 1)) %>% 
  separate(recording, into=c("project", "site", "station", "treatment", "date", "time", "seconds", "filetype"), remove=FALSE)
  
table(dat$samplerate, dat$compressiontype, dat$species)

#6. Visualize----

#6a. Precision----
ggplot(dat) + 
  geom_point(aes(x=samplerate, y=precision, colour=species)) +
  facet_grid(species~compressiontype, scales="free")

#6b. Recall----
ggplot(dat) +
  geom_jitter(aes(x=samplerate, y=recall, colour=species)) +
  facet_grid(species~ compressiontype)

#6c. Score----
ggplot(hit) +
  geom_boxplot(aes(x=samplerate, y=score, colour=species)) +
  facet_grid(species~ compressiontype)

#7. Model----

#https://biol609.github.io/lectures/23c_brms_prediction.html#3_prediction_with_brms_and_mixed_models

dat.coni <- dplyr::filter(dat, species=="CONI")
hit.coni <- dplyr::filter(hit, species=="CONI")
dat.oven <- dplyr::filter(dat, species=="OVEN")
hit.oven <- dplyr::filter(hit, species=="OVEN")

priors <- c(prior(normal(100, 10000), class = "Intercept"),
              prior(normal(0,10), class = "b", coef ="compressiontypemp3_96"),
              prior(normal(0,10), class = "b", coef = "compressiontypemp3_320"),
              prior(normal(0,10), class = "b", coef = "samplerate32000"),
            prior(normal(0,10), class = "b", coef = "samplerate44100"),
              prior(normal(0,10), class = "b", coef = "samplerate32000:compressiontypemp3_96"),
              prior(normal(0,10), class = "b", coef = "samplerate32000:compressiontypemp3_320"),
            prior(normal(0,10), class = "b", coef = "samplerate44100:compressiontypemp3_96"),
            prior(normal(0,10), class = "b", coef = "samplerate44100:compressiontypemp3_320"))

#7a. CONI precision
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

p.coni.ranef <- ranef(p.coni.b)$recording %>% 
  data.frame() %>% 
  mutate(recording=rownames(ranef(p.coni.b)$recording)) %>% 
  separate(recording, into=c("project", "site", "station", "treatment", "date", "time", "seconds", "filetype"), remove=FALSE) %>% 
  group_by(treatment) %>% 
  summarize(est = mean(Estimate.Intercept),
            low = mean(Q2.5.Intercept),
            high = mean(Q97.5.Intercept)) %>% 
  ungroup()

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

#7b. CONI recall
r.coni.b <- brm(recall ~ samplerate*compressiontype + (1|recording),
                family=Beta(),
                data = dat.coni, 
                warmup = 1000, 
                iter   = 20000, 
                chains = 3, 
                inits  = "random",
                cores  = 6,
                prior=priors)

summary(r.coni.b)

r.coni.ranef <- ranef(r.coni.b)$recording %>% 
  data.frame() %>% 
  mutate(recording=rownames(ranef(r.coni.b)$recording)) %>% 
  separate(recording, into=c("project", "site", "station", "treatment", "date", "time", "seconds", "filetype"), remove=FALSE) %>% 
  group_by(treatment) %>% 
  summarize(est = mean(Estimate.Intercept),
            low = mean(Q2.5.Intercept),
            high = mean(Q97.5.Intercept)) %>% 
  ungroup()

#7c. CONI score
s.coni.b <- brm(score ~ samplerate*compressiontype + (1|recording),
                family=Beta(),
                data = dat.coni, 
                warmup = 1000, 
                iter   = 20000, 
                chains = 3, 
                inits  = "random",
                cores  = 6,
                prior=priors)

summary(s.coni.b)

s.coni.ranef <- ranef(s.coni.b)$recording %>% 
  data.frame() %>% 
  mutate(recording=rownames(ranef(s.coni.b)$recording)) %>% 
  separate(recording, into=c("project", "site", "station", "treatment", "date", "time", "seconds", "filetype"), remove=FALSE) %>% 
  group_by(treatment) %>% 
  summarize(est = mean(Estimate.Intercept),
            low = mean(Q2.5.Intercept),
            high = mean(Q97.5.Intercept)) %>% 
  ungroup()

#7d. OVEN precision
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

p.oven.ranef <- ranef(p.oven.b)$recording %>% 
  data.frame() %>% 
  mutate(recording=rownames(ranef(p.oven.b)$recording)) %>% 
  separate(recording, into=c("project", "site", "station", "treatment", "date", "time", "seconds", "filetype"), remove=FALSE) %>% 
  group_by(treatment) %>% 
  summarize(est = mean(Estimate.Intercept),
            low = mean(Q2.5.Intercept),
            high = mean(Q97.5.Intercept)) %>% 
  ungroup()

#7e. OVEN recall
r.oven.b <- brm(recall ~ samplerate*compressiontype + (1|recording),
                family=Beta(),
                data = dplyr::filter(dat.oven, !is.na(precision)), 
                warmup = 1000, 
                iter   = 20000, 
                chains = 3, 
                inits  = "random",
                cores  = 6,
                prior = priors)

summary(r.oven.b)

r.oven.ranef <- ranef(r.oven.b)$recording %>% 
  data.frame() %>% 
  mutate(recording=rownames(ranef(r.oven.b)$recording)) %>% 
  separate(recording, into=c("project", "site", "station", "treatment", "date", "time", "seconds", "filetype"), remove=FALSE) %>% 
  group_by(treatment) %>% 
  summarize(est = mean(Estimate.Intercept),
            low = mean(Q2.5.Intercept),
            high = mean(Q97.5.Intercept)) %>% 
  ungroup()

#7f. CONI score
s.oven.b <- brm(score ~ samplerate*compressiontype + (1|recording),
                family=Beta(),
                data = dat.oven, 
                warmup = 1000, 
                iter   = 20000, 
                chains = 3, 
                inits  = "random",
                cores  = 6,
                prior=priors)

summary(s.oven.b)

s.oven.ranef <- ranef(s.oven.b)$recording %>% 
  data.frame() %>% 
  mutate(recording=rownames(ranef(s.oven.b)$recording)) %>% 
  separate(recording, into=c("project", "site", "station", "treatment", "date", "time", "seconds", "filetype"), remove=FALSE) %>% 
  group_by(treatment) %>% 
  summarize(est = mean(Estimate.Intercept),
            low = mean(Q2.5.Intercept),
            high = mean(Q97.5.Intercept)) %>% 
  ungroup()

#8. Predict----
newdat <- expand.grid(samplerate = unique(dat$samplerate),
                      compressiontype = unique(dat$compressiontype))

p.coni.pred <- newdat %>% 
  add_epred_draws(p.coni.b,
                   re_formula = NA,
                   ndraws = 1000) %>% 
  mutate(species="CONI",
         response="Precision")

p.oven.pred <- newdat %>% 
  add_epred_draws(p.oven.b,
                   re_formula = NA,
                   ndraws = 1000) %>% 
  mutate(species="OVEN",
         response="Precision")

r.coni.pred <- newdat %>% 
  add_epred_draws(r.coni.b,
                  re_formula = NA,
                  ndraws = 1000) %>% 
  mutate(species="CONI",
         response="Recall")

r.oven.pred <- newdat %>% 
  add_epred_draws(r.oven.b,
                  re_formula = NA,
                  ndraws = 1000) %>% 
  mutate(species="OVEN",
         response="Recall")

s.coni.pred <- newdat %>% 
  add_epred_draws(s.coni.b,
                  re_formula = NA,
                  ndraws = 1000) %>% 
  mutate(species="CONI",
         response="Mean score")

s.oven.pred <- newdat %>% 
  add_epred_draws(s.oven.b,
                  re_formula = NA,
                  ndraws = 1000) %>% 
  mutate(species="OVEN",
         response="Mean score")

pred <- rbind(p.coni.pred, p.oven.pred, r.coni.pred, r.oven.pred, s.coni.pred, s.oven.pred)
pred$species <- factor(pred$species, labels=c("Common nighthawk (CONI)", "Ovenbird (OVEN)"))
pred$response <- factor(pred$response, levels=c("Precision", "Recall", "Mean score"))

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
  scale_fill_viridis_d(name="Compression type\n(file type_bitrate)") +
  ylab("Sample rate (Hz)") +
  my.theme +
  theme(legend.position = "bottom",
        axis.title.x = element_blank()) +
  facet_grid(species~response, scales="free_x")
recognizer.plot

ggsave(recognizer.plot, filename="figures/Recognizers.jpeg", width=12, height=8)

#10. Summary stats----
mean(dat.coni$precision, na.rm=TRUE)
sd(dat.coni$precision, na.rm=TRUE)
mean(dat.oven$precision, na.rm=TRUE)
sd(dat.oven$precision, na.rm=TRUE)
mean(dat.coni$recall, na.rm=TRUE)
sd(dat.coni$recall, na.rm=TRUE)
mean(dat.oven$recall, na.rm=TRUE)
sd(dat.oven$recall, na.rm=TRUE)
mean(dat.coni$recall.r, na.rm=TRUE)
sd(dat.coni$recall.r, na.rm=TRUE)
mean(dat.oven$recall.r, na.rm=TRUE)
sd(dat.oven$recall.r, na.rm=TRUE)

dat %>% 
  group_by(species, samplerate, compressiontype) %>% 
  summarize(n=n(),
            hits = sum(recall)) %>% 
  ungroup() %>% 
  mutate(recall=hits/n)
