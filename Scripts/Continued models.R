library(dplyr)
library(lme4)
library(tidyr)
library(stringr)
library(ggplot2)
library(lmerTest)
library(sjPlot)
library(MuMIn)
library(ggeffects)
library(ggpubr)
library(effects)
library(rr2)

#Script to run continued models

cm <- read.csv("data/communitymatrixSite.csv")

cm_long <- cm %>% 
    pivot_longer(!X, names_to = "scientific_name", values_to = "abundance") %>% 
    rename(Site = X) %>% 
    mutate(scientific_name = str_replace(scientific_name,
                                         pattern = "\\.", replacement = " "))

#combine with urbanization data
urb <- read.csv("data/impervious_surface.csv")
cm_long_urbanization <- left_join(cm_long, urb)

## combine with trait data
traits <- read.csv("data/TraitData.csv")
cm_long_urbanization <- left_join(cm_long_urbanization, traits, 
                                  by = c("scientific_name" = "Species")) # normally distributed

#combine with light data
light <- read.csv("data/lightData.csv")
cm_long_urbanization <- left_join(cm_long_urbanization, light)

#Can't Join this and niche because of how we named the species in those
range <- read.csv("data/geographicdata.csv") %>% 
    mutate(scientific_name = word(scientificName, start = 1, end = 2)) %>% 
    select(-scientificName)
cm_long_urbanization <- left_join(cm_long_urbanization, range)

niche <- read.csv("data/nichedata.csv") %>% 
    mutate(scientific_name = word(scientificName, start = 1, end = 2)) %>% 
    select(-scientificName)
cm_long_urbanization <- left_join(cm_long_urbanization, niche)

cm_long_urbanization <- cm_long_urbanization %>% 
    select(-X, -max_lat, -min_lat, -med_lat)

nSites <- cm_long_urbanization %>% 
    filter(abundance > 0) %>%
    group_by(scientific_name) %>% 
    summarise(nSites = length(unique(Site)))

cm_long_urbanization <- left_join(cm_long_urbanization, nSites) %>% 
    filter(nSites > 2)

mdf <- cm_long_urbanization %>% 
    na.omit

write.csv(mdf, file = "data/modelData.csv")

abun <- mdf$abundance

mdf <- mdf %>% 
    mutate_if(is.numeric, scale)

mdf <- mdf %>% 
    mutate(abundance = abun)

model <- lmer(formula = abundance ~ Dev_1 + 
                  LHSCategory:Dev_1 + VoltinismCategory:Dev_1 + 
                  LarvalHabitatCategory:Dev_1 + BodySize:Dev_1 +
                  max_lat_dif:Dev_1 + min_lat_dif:Dev_1 +
                  med_lat_dif:Dev_1 + temp_niche:Dev_1 +
                  precip_niche:Dev_1 +
                  (1 | scientific_name),
              na.action = "na.fail", REML = F,
              data = mdf)

summary(model)

dd <- MuMIn::dredge(model)
top_model <- get.models(dd, subset = 2)[[1]]
car::vif(top_model)
summary(top_model)
r.squaredGLMM(top_model)

#Checking Assumptions
resids <- residuals(top_model)
hist(resids)
qqnorm(resids)
qqline(resids)
shapiro.test(resids)

## model this at a 10 km scale for imprevious surface
model <- glmer(formula = abundance ~ Dev_10 + 
                  LHSCategory:Dev_10 + VoltinismCategory:Dev_10 + 
                  LarvalHabitatCategory:Dev_10 + BodySize:Dev_10 +
                  max_lat_dif:Dev_10 + min_lat_dif:Dev_10 +
                  med_lat_dif:Dev_10 + temp_niche:Dev_10 +
                  precip_niche:Dev_10 + (1 | scientific_name),
              na.action = "na.fail", family = poisson,
              data = mdf)

summary(model)

dd <- MuMIn::dredge(model)
top_model <- get.models(dd, subset = 1)[[1]]
car::vif(top_model)
summary(top_model)

model <- glmer(formula = abundance ~ Dev_10 + 
                   LHSCategory:Dev_10 + VoltinismCategory:Dev_10 + 
                   LarvalHabitatCategory:Dev_10 + BodySize:Dev_10 +
                   max_lat_dif:Dev_10 + (1 | scientific_name),
               na.action = "na.fail", family = poisson,
               control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
               data = mdf)

dd <- MuMIn::dredge(model)
top_model <- get.models(dd, subset = 1)[[1]]
car::vif(top_model)
summary(top_model)

#Checking Assumptions
resids <- residuals(top_model)
hist(resids)
qqnorm(resids)
qqline(resids)
shapiro.test(resids)

#Light model
model <- lmer(formula = abundance ~ meanLight + 
                  VoltinismCategory:meanLight +
                  (1 | scientific_name),
              na.action = "na.fail",
              data = mdf)

summary(model)

dd <- MuMIn::dredge(model)
top_model <- get.models(dd, subset = 2)[[1]]
car::vif(top_model)
summary(top_model)
r.squaredGLMM(model)

## look into AICs of 1km and 10km
AICc(top_model, top_model2) # our top top model is top model 2
Weights(AICc(top_model, top_model2))

top_model <- top_model2
summary(top_model2)
r.squaredGLMM(top_model2)