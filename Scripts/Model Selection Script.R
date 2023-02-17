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
library(cowplot)

#Script to run continued models
setwd()

#### Creating Model Dataframe ####
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

tempData <- read.csv("data/temp_gradient.csv")

cm_long_urbanization <- left_join(cm_long_urbanization, tempData)

mdf <- cm_long_urbanization %>% 
    na.omit

write.csv(mdf, file = "data/modelData.csv")

abun <- mdf$abundance

mdf <- mdf %>% 
    mutate(mean_temp = mean_temp * -1) %>% 
    mutate_if(is.numeric, scale)

mdf <- mdf %>% 
    mutate(abundance = abun)

# Abundance Modeling #
model1 <- glmer(formula = abundance ~ Dev_1 + meanLight + mean_temp +
                    LHSCategory:Dev_1 + VoltinismCategory:Dev_1 + 
                    LarvalHabitatCategory:Dev_1 + BodySize:Dev_1 +
                    temp_niche:Dev_1 + 
                    (1 | scientific_name),
                na.action = "na.fail", family = poisson,
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                data = mdf)

summary(model1)


model1reduced <- glmer(formula = abundance ~ Dev_1 + meanLight + mean_temp +
                    LHSCategory:Dev_1 +  
                    LarvalHabitatCategory:Dev_1 + 
                    (1 | scientific_name),
                na.action = "na.fail", family = poisson,
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                data = mdf)

summary(model1reduced)


Weights(AICc(model1, model1reduced)) #model1 reduced is our first competing model
car::vif(model1)



model2 <- glmer(formula = abundance ~ Dev_1 + meanLight + mean_temp +
                    LHSCategory:meanLight + VoltinismCategory:meanLight + 
                    LarvalHabitatCategory:meanLight + BodySize:meanLight +
                    temp_niche:meanLight + 
                    (1 | scientific_name),
                na.action = "na.fail", family = poisson,
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                data = mdf)

summary(model2)

model2reduced <- glmer(formula = abundance ~ Dev_1 + mean_temp +
                    LHSCategory:meanLight + VoltinismCategory:meanLight + 
                    LarvalHabitatCategory:meanLight + 
                    (1 | scientific_name),
                na.action = "na.fail", family = poisson,
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                data = mdf)
summary(model2reduced)

Weights(AICc(model2, model2reduced)) #reduced model selected
car::vif(model2reduced)


model3 <- glmer(formula = abundance ~ Dev_1 + meanLight + mean_temp +
                    LHSCategory:mean_temp + VoltinismCategory:mean_temp + 
                    LarvalHabitatCategory:mean_temp + BodySize:mean_temp +
                    temp_niche:mean_temp + 
                    (1 | scientific_name),
                na.action = "na.fail", family = poisson,
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                data = mdf)

summary(model3)

model3reduced <- glmer(formula = abundance ~ Dev_1 + meanLight +
                    LHSCategory:mean_temp + 
                    LarvalHabitatCategory:mean_temp + BodySize:mean_temp +
                    temp_niche:mean_temp + 
                    (1 | scientific_name),
                na.action = "na.fail", family = poisson,
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                data = mdf)
summary(model3reduced)

Weights(AICc(model3, model3reduced)) #Reduced model selected
car::vif(model3reduced)


model10 <- glmer(formula = abundance ~ Dev_10 + meanLight + mean_temp +
                    LHSCategory:Dev_10 + VoltinismCategory:Dev_10 + 
                    LarvalHabitatCategory:Dev_10 + BodySize:Dev_10 +
                    temp_niche:Dev_10 + 
                    (1 | scientific_name),
                na.action = "na.fail", family = poisson,
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                data = mdf)

summary(model10)

model10reduced <- glmer(formula = abundance ~ Dev_10 + meanLight + mean_temp +
                    LHSCategory:Dev_10 + VoltinismCategory:Dev_10 + 
                    LarvalHabitatCategory:Dev_10 + 
                    (1 | scientific_name),
                na.action = "na.fail", family = poisson,
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                data = mdf)

summary(model10reduced)

Weights(AICc(model10, model10reduced)) #Reduced model selected
car::vif(model10reduced)

model20 <- glmer(formula = abundance ~ Dev_10 + meanLight + mean_temp +
                    LHSCategory:meanLight + VoltinismCategory:meanLight + 
                    LarvalHabitatCategory:meanLight + BodySize:meanLight +
                    temp_niche:meanLight + 
                    (1 | scientific_name),
                na.action = "na.fail", family = poisson,
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                data = mdf)

summary(model2)

model20reduced <- glmer(formula = abundance ~ Dev_10 + mean_temp +
                           LHSCategory:meanLight + VoltinismCategory:meanLight + 
                           LarvalHabitatCategory:meanLight + 
                           (1 | scientific_name),
                       na.action = "na.fail", family = poisson,
                       control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                       data = mdf)
summary(model20reduced)

Weights(AICc(model20, model20reduced)) #reduced model selected
car::vif(model20reduced)

model30 <- glmer(formula = abundance ~ Dev_1 + meanLight + mean_temp +
                    LHSCategory:mean_temp + VoltinismCategory:mean_temp + 
                    LarvalHabitatCategory:mean_temp + BodySize:mean_temp +
                    temp_niche:mean_temp + 
                    (1 | scientific_name),
                na.action = "na.fail", family = poisson,
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                data = mdf)

summary(model30)

model30reduced <- glmer(formula = abundance ~ Dev_1 + meanLight +
                           LHSCategory:mean_temp + 
                           LarvalHabitatCategory:mean_temp + BodySize:mean_temp +
                           temp_niche:mean_temp + 
                           (1 | scientific_name),
                       na.action = "na.fail", family = poisson,
                       control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                       data = mdf)
summary(model30reduced)

Weights(AICc(model30, model30reduced)) #Reduced model selected
car::vif(model30reduced)

Weights(AICc(model1reduced, model2reduced, model3reduced, model10reduced, model20reduced, model30reduced))

table <- MuMIn::model.sel(model1reduced, model2reduced, model3reduced, model10reduced, model20reduced, model30reduced)
# model selection table

write.csv(table,"model_selection_table.csv")

