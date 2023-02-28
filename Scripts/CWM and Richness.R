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
    mutate(meanLight = log(meanLight + 1),
           mean_temp = (mean_temp - 1.02) * -1)

#### Richness Modeling ####
modelRich <- lm(formula = richness ~ Dev_1,
                na.action = "na.fail",
                data = rich)

modelRich2 <- lm(formula = richness ~ Dev_10,
                 na.action = "na.fail",
                 data = rich)

modelRich3 <- lm(formula = richness ~ mean_temp,
                 na.action = "na.fail",
                 data = rich)

modelRich4 <- lm(formula = richness ~ meanLight,
                 na.action = "na.fail",
                 data = rich)

Weights(AICc(modelRich, modelRich2, modelRich3, modelRich4))
table <- MuMIn::model.sel(modelRich, modelRich2, modelRich3, modelRich4)
table
summary(modelRich)


a <- sjPlot::plot_model(modelRich4, terms = "meanLight", type = "pred", title = "") 

a <- a + 
    geom_point(rich, mapping = aes(x = meanLight, y = richness)) +
    labs(x = "Artificial light at night", y = "Species Richness") +
    font_size(title  = 10, axis_title.x = 10, axis_title.y = 10, labels.x = 8, labels.y = 8) + theme_classic()
a


#### Community Weighted Mean Body Size Dataframe ####
cwm <- cm_long_urbanization %>% 
    group_by(Site) %>% 
    summarise(
        BodySize_cwm =weighted.mean(BodySize, abundance, na.rm=T)
    )

cwm_urb <- left_join(cwm, urb) %>% 
    left_join(light) %>% 
    left_join(tempData) %>% 
    mutate(meanLight = log(meanLight + 1),
           mean_temp = (mean_temp - 1.02) * -1)

head(cwm_urb)


## CWM Body Size Modeling##
modelCWM <- lm(formula = BodySize_cwm ~ Dev_1,
               na.action = "na.fail",
               data = cwm_urb)

modelCWM2 <- lm(formula = BodySize_cwm ~ Dev_10,
                na.action = "na.fail",
                data = cwm_urb)

modelCWM3 <- lm(formula = BodySize_cwm ~ mean_temp,
                na.action = "na.fail",
                data = cwm_urb)

modelCWM4 <- lm(formula = BodySize_cwm ~ meanLight,
                na.action = "na.fail",
                data = cwm_urb)

Weights(AICc(modelCWM, modelCWM2, modelCWM3, modelCWM4))
table <- MuMIn::model.sel(modelCWM, modelCWM2, modelCWM3, modelCWM4)
table

b <- sjPlot::plot_model(modelCWM2, terms = "Dev_10", type = "pred", title = "")

b <- b + 
    geom_point(cwm_urb, mapping = aes(x = Dev_10, y = BodySize_cwm)) +
    labs(x = "Proportion urbanization (10-km)", y = "Body Size CWM") +
    font_size(title  = 10, axis_title.x = 10, axis_title.y = 10, labels.x = 8, labels.y = 8) + theme_classic()

b

## Community Weight Mean Temperature Niche Dataframe ##
cwm <- cm_long_urbanization %>% 
    group_by(Site) %>% 
    summarise(
        tempNiche_cwm =weighted.mean(temp_niche, abundance, na.rm=T)
    )

cwm_urb <- left_join(cwm, urb) %>% 
    left_join(light) %>% 
    left_join(tempData) %>% 
    mutate(meanLight = log(meanLight + 1),
           mean_temp = (mean_temp - 1.02) * -1)

head(cwm_urb)

## CWM Temperature Niche Modeling ##
modelCWM <- lm(formula = tempNiche_cwm ~ Dev_1,
               na.action = "na.fail",
               data = cwm_urb)

modelCWM2 <- lm(formula = tempNiche_cwm ~ Dev_10,
                na.action = "na.fail",
                data = cwm_urb)

modelCWM3 <- lm(formula = tempNiche_cwm ~ mean_temp,
                na.action = "na.fail",
                data = cwm_urb)

modelCWM4 <- lm(formula = tempNiche_cwm ~ meanLight,
                na.action = "na.fail",
                data = cwm_urb)

Weights(AICc(modelCWM, modelCWM2, modelCWM3, modelCWM4))
table <- MuMIn::model.sel(modelCWM, modelCWM2, modelCWM3, modelCWM4)
table

summary(modelCWM)

c <- sjPlot::plot_model(modelCWM, terms = "Dev_1", type = "pred", title = "")

c <- c + 
    geom_point(cwm_urb, mapping = aes(x = Dev_1, y = tempNiche_cwm)) +
    labs(x = "Proportion urbanization (1-km)", y = "Temperature Niche") +
    font_size(title  = 10, axis_title.x = 10, axis_title.y = 10, labels.x = 8, labels.y = 8) + theme_classic()

c

ggsave("CWMniche.png", width = 7, height = 7)


Figure4 <- cowplot::plot_grid(a,b,c, labels = c("A", "B", "C"), label_size = 12)
Figure4

ggsave("New Figues/Figure4_CWM.png", Figure4,
       width = 5.5, height = 4)






