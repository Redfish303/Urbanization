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

tempData <- read.csv("data/temp_gradient.csv")

cm_long_urbanization <- left_join(cm_long_urbanization, tempData)



rich <- cm_long_urbanization %>% 
    filter(abundance != 0) %>% 
    group_by(Site) %>% 
    summarise(richness = length(unique(scientific_name))) %>% 
    left_join(urb) %>% 
    left_join(tempData) %>% 
    left_join(light) %>% 
    mutate(mean_temp = mean_temp * -1)


scalecolumns <- c("Dev_10","Dev_1", "mean_temp", "meanLight")
rich$Dev_10 <-  scale(rich$Dev_10)
rich$Dev_1 <-  scale(rich$Dev_1)
rich$mean_temp <-  scale(rich$mean_temp)
rich$meanLight <-  scale(rich$meanLight)
             
#### Richness Modeling ####
modelRich <- lm(formula = richness ~ Dev_1 + mean_temp + meanLight,
                na.action = "na.fail",
                data = rich)

modelRich_dd <- dredge(modelRich)

top_model_rich1km = get.models(modelRich_dd,subset = 1)[[1]]

modelRich2 <- lm(formula = richness ~ Dev_10,
                 na.action = "na.fail",
                 data = rich)

modelRich_dd2 <-dredge(modelRich2)

top_model_Rich10km <- get.models(modelRich_dd2, subset = 1)[[1]]
summary(top_model_Rich10km)
r.squaredGLMM(top_model_Rich10km)

AICc(top_model_rich1km, top_model_Rich10km)
Weights(AICc(top_model_rich1km, top_model_Rich10km))






#### Community Weighted Mean Body Size Dataframe ####
cwm <- cm_long_urbanization %>% 
    group_by(Site) %>% 
    summarise(
        BodySize_cwm =weighted.mean(BodySize, abundance, na.rm=T)
    )

cwm_urb <- left_join(cwm, urb)

head(cwm_urb)


## CWM Body Size Modeling##
modelCWM <- lm(formula = BodySize_cwm ~ Dev_1,
               na.action = "na.fail",
               data = cwm_urb)

modelCWM2 <- lm(formula = BodySize_cwm ~ Dev_10,
                na.action = "na.fail",
                data = cwm_urb)

b <- sjPlot::plot_model(modelCWM2, terms = "Dev_10", type = "pred", title = "")

b + 
    geom_point(cwm_urb, mapping = aes(x = Dev_10, y = BodySize_cwm)) +
    labs(x = "Proportion urbanization (1-km)", y = "Body Size CWM") +
    theme_classic()

Weights(AICc(modelCWM, modelCWM2))
summary(modelCWM2)
r.squaredGLMM(modelCWM2)

## Community Weight Mean Temperature Niche Dataframe ##
cwm <- cm_long_urbanization %>% 
    group_by(Site) %>% 
    summarise(
        tempNiche_cwm =weighted.mean(temp_niche, abundance, na.rm=T)
    )

cwm_urb <- left_join(cwm, urb)

head(cwm_urb)

## CWM Temperature Niche Modeling ##
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

ggsave("CWMniche.png", width = 7, height = 7)

## Community Weighted Mean Geographic Range Dataframe ##

cwm <- cm_long_urbanization %>% 
    group_by(Site) %>% 
    summarise(
        maxlat_cwm =weighted.mean(max_lat_dif, abundance, na.rm=T)
    )

cwm_urb <- left_join(cwm, urb)
modelCWM <- lm(formula = maxlat_cwm ~ Dev_1,
               na.action = "na.fail",
               data = cwm_urb)

modelCWM2 <- lm(formula = maxlat_cwm ~ Dev_10,
                na.action = "na.fail",
                data = cwm_urb)

AICc(modelCWM, modelCWM2)
Weights(AICc(modelCWM, modelCWM2))

summary(modelCWM)

b <- sjPlot::plot_model(modelCWM, terms = "Dev_1", type = "pred", title = "")

b + 
    geom_point(cwm_urb, mapping = aes(x = Dev_10, y = maxlat_cwm)) +
    labs(x = "Proportion urbanization (1-km)", y = "Max Latitude Difference") +
    theme_classic()

ggsave("CWMrange.png", width = 7, height = 7)
