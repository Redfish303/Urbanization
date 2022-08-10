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

tempData <- read.csv("data/temp_gradient.csv")

cm_long_urbanization <- left_join(cm_long_urbanization, tempData)

### test for richness
rich <- cm_long_urbanization %>% 
    filter(abundance != 0) %>% 
    group_by(Site) %>% 
    summarise(richness = length(unique(scientific_name))) %>% 
    left_join(urb) %>% 
    left_join(tempData) %>% 
    left_join(light) %>% 
    mutate(mean_temp = mean_temp * -1)

rich <- rich %>% 
    mutate(Dev_10 = scale(Dev_10),
           Dev_1 = scale(Dev_1),
           mean_temp = scale(mean_temp),
           meanLight = scale(meanLight))

#richness on 1 and 10km scale
modelRich <- lm(formula = richness ~ Dev_1 + mean_temp + meanLight,
                na.action = "na.fail",
                data = rich)

modelRich_dd <- dredge(modelRich)

top_model_Rich1km <- get.models(modelRich_dd, subset = 1)[[1]]

# do this for 10km scale
modelRich2 <- lm(formula = richness ~ Dev_10 + mean_temp + meanLight,
                 na.action = "na.fail",
                 data = rich)

modelRich_dd2 <- dredge(modelRich2)

top_model_Rich10km <- get.models(modelRich_dd2, subset = 1)[[1]]

AICc(top_model_Rich1km, top_model_Rich10km)
Weights(AICc(top_model_Rich1km, top_model_Rich10km))

# these sj plots are not working. not going to trouble shoot why. table of results should be good
a <- sjPlot::plot_model(top_model_Rich1km, terms = "meanLight", type = "pred", title = "")

a +
    geom_point(rich, mapping = aes(x = Dev_1, y = richness)) +
    labs(x = "Proportion urbanization (1-km)", y = "Species richness") +
    theme_classic()

ggsave("richness.png")

## community weighted mean
cwm <- cm_long_urbanization %>% 
    group_by(Site) %>% 
    summarise(
        BodySize_cwm =weighted.mean(BodySize, abundance, na.rm=T)
    )

cwm_urb <- left_join(cwm, urb)

head(cwm_urb)

# CWM model
modelCWM <- lm(formula = BodySize_cwm ~ Dev_1,
               na.action = "na.fail",
               data = cwm_urb)

modelCWM2 <- lm(formula = BodySize_cwm ~ Dev_10,
                na.action = "na.fail",
                data = cwm_urb)

AICc(modelCWM, modelCWM2)
Weights(AICc(modelCWM, modelCWM2))

summary(modelCWM2)

b <- sjPlot::plot_model(modelCWM2, terms = "Dev_10", type = "pred", title = "")

b + 
    geom_point(cwm_urb, mapping = aes(x = Dev_10, y = BodySize_cwm)) +
    labs(x = "Proportion urbanization (10-km)", y = "Body Size CWM") +
    theme_classic()

ggsave("CMW.png")



## none of the temperature cwms are significant, so to avoid complexity of writing,
## i think we should just stick to the body size analysis and the richness 

### temperature niche
## community weighted mean
cwm <- cm_long_urbanization %>% 
    group_by(Site) %>% 
    summarise(
        tempNiche_cwm =weighted.mean(temp_niche, abundance, na.rm=T)
    )

cwm_urb <- left_join(cwm, urb)

head(cwm_urb)

# CWM model
modelCWM <- lm(formula = tempNiche_cwm ~ Dev_1,
               na.action = "na.fail",
               data = cwm_urb)

modelCWM2 <- lm(formula = tempNiche_cwm ~ Dev_10,
                na.action = "na.fail",
                data = cwm_urb)

AICc(modelCWM, modelCWM2)
Weights(AICc(modelCWM, modelCWM2))

summary(modelCWM2)

b <- sjPlot::plot_model(modelCWM2, terms = "Dev_10", type = "pred", title = "")

b + 
    geom_point(cwm_urb, mapping = aes(x = Dev_10, y = tempNiche_cwm)) +
    labs(x = "Proportion urbanization (1-km)", y = "Temperature Niche") +
    theme_classic()

ggsave("CMW.png")



voltanismSize <- na.omit(cm_long_urbanization) %>% 
    group_by(VoltinismCategory) %>% 
    summarise(avg = mean(BodySize))

dietSize <- na.omit(cm_long_urbanization) %>% 
    group_by(LHSCategory) %>% 
    summarise(avg = mean(BodySize))



### max latitude difference
## community weighted mean
cwm <- cm_long_urbanization %>% 
    group_by(Site) %>% 
    summarise(
        maxLat_cwm =weighted.mean(max_lat_dif, abundance, na.rm=T)
    )

cwm_urb <- left_join(cwm, urb)

head(cwm_urb)

# CWM model
modelCWM <- lm(formula = maxLat_cwm ~ Dev_1,
               na.action = "na.fail",
               data = cwm_urb)

modelCWM2 <- lm(formula = maxLat_cwm ~ Dev_10,
                na.action = "na.fail",
                data = cwm_urb)

AICc(modelCWM, modelCWM2)
Weights(AICc(modelCWM, modelCWM2))

summary(modelCWM)

b <- sjPlot::plot_model(modelCWM, terms = "Dev_1", type = "pred", title = "")

b + 
    geom_point(cwm_urb, mapping = aes(x = Dev_10, y = maxLat_cwm)) +
    labs(x = "Proportion urbanization (1-km)", y = "Max Latitude Difference") +
    theme_classic()

ggsave("CMW.png")



voltanismSize <- na.omit(cm_long_urbanization) %>% 
    group_by(VoltinismCategory) %>% 
    summarise(avg = mean(BodySize))

dietSize <- na.omit(cm_long_urbanization) %>% 
    group_by(LHSCategory) %>% 
    summarise(avg = mean(BodySize))