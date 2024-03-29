library(tidyverse)
library(brms)
library(bayesplot)
library(tidybayes)
library(ggridges)

#1. Read in data----
aci <- read.csv("data/acoustic indices/ACI.csv") %>% 
  pivot_longer(cols=X22050_320_mp3:X44100_or_48000_wav, names_to="treatment", values_to="aci")
adi <- read.csv("data/acoustic indices/ADI.csv") %>% 
  pivot_longer(cols=X22050_320_mp3:X44100_or_48000_wav, names_to="treatment", values_to="adi")

#2. Put together and wrangle----
dat <- full_join(aci, adi) %>% 
  rename(recording=X) %>% 
  rowwise() %>% 
  mutate(samplerate = as.numeric(str_sub(treatment, 2, 6)),
         compressiontype = ifelse(samplerate==44100,
                                  str_sub(treatment, 17, 100),
                                  str_sub(treatment, 8, 100)),
         sm3 = str_detect(recording, pattern='\\+')) %>% 
  dplyr::filter(sm3==FALSE) %>% 
  separate(compressiontype, into=c("bitrate", "filetype")) %>% 
  mutate(compressiontype = ifelse(bitrate=="wav", "wav", paste0(filetype, "_", bitrate))) 

#make compression type a factor
dat$compressiontype <- factor(dat$compressiontype, levels=c("wav", "mp3_320", "mp3_96"))
dat$samplerate <- factor(dat$samplerate, levels=c("22050", "32000", "44100"))

#3. Visualize----
ggplot(dat) +
  geom_point(aes(x=samplerate, y=aci, colour=compressiontype)) +
  facet_wrap(~compressiontype)

ggplot(dat) +
  geom_point(aes(x=samplerate, y=adi, colour=compressiontype))

#4. Model----
priors.aci <- c(prior(normal(4000, 10000), class = "Intercept"),
                prior(normal(0,1000), class = "b", coef ="compressiontypemp3_96"),
                prior(normal(0,1000), class = "b", coef = "compressiontypemp3_320"),
                prior(normal(0,1000), class = "b", coef = "samplerate32000"),
                prior(normal(0,1000), class = "b", coef = "samplerate44100"),
                prior(normal(0,1000), class = "b", coef = "samplerate32000:compressiontypemp3_96"),
                prior(normal(0,1000), class = "b", coef = "samplerate32000:compressiontypemp3_320"),
                prior(normal(0,1000), class = "b", coef = "samplerate44100:compressiontypemp3_96"),
                prior(normal(0,1000), class = "b", coef = "samplerate44100:compressiontypemp3_320"))

priors.adi <- c(prior(normal(0,10), class = "Intercept"),
                prior(normal(0,10), class = "b", coef ="compressiontypemp3_96"),
                prior(normal(0,10), class = "b", coef = "compressiontypemp3_320"),
                prior(normal(0,10), class = "b", coef = "samplerate32000"),
                prior(normal(0,10), class = "b", coef = "samplerate44100"),
                prior(normal(0,10), class = "b", coef = "samplerate32000:compressiontypemp3_96"),
                prior(normal(0,10), class = "b", coef = "samplerate32000:compressiontypemp3_320"),
                prior(normal(0,10), class = "b", coef = "samplerate44100:compressiontypemp3_96"),
                prior(normal(0,10), class = "b", coef = "samplerate44100:compressiontypemp3_320"))

#4a. ACI----
aci.b <- brm(aci ~ samplerate*compressiontype + (1|recording),
                data = dat, 
                warmup = 1000, 
                iter   = 20000, 
                chains = 3, 
                inits  = "random",
                cores  = 6,
                prior=priors.aci)

summary(aci.b)

#Inspect mixing
plot(aci.b)
#Inspect rhat
mcmc_rhat(rhat(aci.b))
#Inspect neff
mcmc_neff(neff_ratio(aci.b))
#Inspect residuals
preds <- dat %>%
  add_predicted_draws(aci.b,
                      ndraws = 100)
qres <- preds %>%
  summarise(quant_residual = mean(.prediction < aci))
ggplot(qres, aes(sample = quant_residual)) +
  geom_qq(distribution = stats::qunif) +
  geom_qq_line(distribution = stats::qunif)
#Inspect fit
ggplot(preds) +
  stat_pointinterval(aes(x = .row, y = .prediction)) +
  geom_point(aes(x = .row, y = aci), color = "red")
#check distribution
pp_check(aci.b, ndraws = 500)

#4b. ADI----
adi.b <- brm(adi ~ samplerate*compressiontype + (1|recording),
             data = dat, 
             warmup = 1000, 
             iter   = 20000, 
             chains = 3, 
             inits  = "random",
             cores  = 6,
             prior=priors.adi)

summary(adi.b)

#5. Predict----
newdat <- expand.grid(samplerate = unique(dat$samplerate),
                      compressiontype = unique(dat$compressiontype))

aci.pred <- newdat %>% 
  add_epred_draws(aci.b,
                  re_formula = NA,
                  ndraws = 1000) %>% 
  mutate(index="ACI")

adi.pred <- newdat %>% 
  add_epred_draws(adi.b,
                  re_formula = NA,
                  ndraws = 1000) %>% 
  mutate(index="ADI")

pred <- rbind(aci.pred, adi.pred)
pred$index <- factor(pred$index, labels=c("Acoustic complexity index (ACI)", "Acoustic diversity index (ADI)"))

#6. Plot----
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

index.plot <- ggplot(pred) +
  geom_density_ridges(aes(x=.epred, y=samplerate, fill=compressiontype), colour="grey30", alpha = 0.5) +
  scale_fill_viridis_d(name="Compression type\n(file type_bitrate)") +
  ylab("Sample rate (Hz)") +
  xlab("Index value") +
  my.theme +
  theme(legend.position = "bottom") +
  facet_wrap(~index, scales="free_x")
index.plot

ggsave(index.plot, filename="figures/Indices.jpeg", width=8, height=5)

#7. Summary stats----
mean(dat$aci, na.rm=TRUE)
sd(dat$aci, na.rm=TRUE)
mean(dat$adi, na.rm=TRUE)
sd(dat$adi, na.rm=TRUE)
