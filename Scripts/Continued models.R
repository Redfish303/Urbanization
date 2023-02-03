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

#### Abundance Modeling ####

model1 <- glmer(formula = abundance ~ 
                  LHSCategory:Dev_1 + VoltinismCategory:Dev_1 + 
                  LarvalHabitatCategory:Dev_1 + BodySize:Dev_1 +
                  max_lat_dif:Dev_1 + min_lat_dif:Dev_1 +
                  (1 | scientific_name),
              na.action = "na.fail", family = poisson,
              control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
              data = mdf)

model1.5 <- glmer(formula = abundance ~ 
                    LHSCategory:Dev_1 + VoltinismCategory:Dev_1 + 
                    LarvalHabitatCategory:Dev_1 + BodySize:Dev_1 +
                    med_lat_dif:Dev_1 + 
                    (1 | scientific_name),
                na.action = "na.fail", family = poisson,
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                data = mdf)

## Selected Model for 1km spatial scale ###
model1.5.1 <- glmer(formula = abundance ~ 
                    VoltinismCategory:Dev_1 + 
                    LarvalHabitatCategory:Dev_1 + BodySize:Dev_1 +
                    med_lat_dif:Dev_1 +
                    (1 | scientific_name),
                na.action = "na.fail", family = poisson,
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                data = mdf)

summary(model1.5.1)

model1.5.2 <- glmer(formula = abundance ~ 
                      LHSCategory:Dev_1 + VoltinismCategory:Dev_1 + 
                      LarvalHabitatCategory:Dev_1 +
                      med_lat_dif:Dev_1 + 
                      (1 | scientific_name),
                  na.action = "na.fail", family = poisson,
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                  data = mdf)

model2 <- glmer(formula = abundance ~ Dev_1 + 
                    LHSCategory:Dev_1 + VoltinismCategory:Dev_1 + 
                    LarvalHabitatCategory:Dev_1 + BodySize:Dev_1 +
                    temp_niche:Dev_1 +
                    precip_niche:Dev_1 +
                    (1 | scientific_name),
                na.action = "na.fail", family = poisson,
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                data = mdf)

#Chosen model 2 based on VIF
model2.5 <- glmer(formula = abundance ~ Dev_1 + 
                    VoltinismCategory:Dev_1 + 
                    LarvalHabitatCategory:Dev_1 +
                    temp_niche:Dev_1 +
                    (1 | scientific_name),
                na.action = "na.fail", family = poisson,
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                data = mdf)

summary(model2.5)

s <- stats::step(model)
top_model_Dev1<- get.models(dd, subset = 2)[[1]]
car::vif(model1)# Vif needs to be below 5, we remove the variables that were above 5 to improve the model
summary(top_model_Dev1)
r.squaredGLMM(top_model_Dev1)

#Check AICc of all the models we have made
AICc(model1, model1.5, model1.5.1, model1.5.2, model2, model2.5)
Weights(AICc(model1, model1.5, model1.5.1, model1.5.2, model2, model2.5))

#Vif needs to be below 5.  
car::vif(model1.5.1)


#Checking Assumptions
resids <- residuals(model1.5.1)
hist(resids)
qqnorm(resids)
qqline(resids)
shapiro.test(resids)

summary(model1.5.1)
#SJplot of 1.5.1 correlation
plot_model(model1.5.1, type = "eff", terms = c("Dev_1", "VoltinismCategory"))

plot_model(model1.5.1, type = "eff", terms = c("Dev_1", "LarvalHabitatCategory"))

plot_model(model1.5.1, type = "eff", terms = c("Dev_1", "BodySize"))

plot_model(model1.5.1, type = "eff", terms = c("Dev_1", "med_lat_dif"))#Would have expected more northern species to be more impacted

## model this at a 10 km scale for imprevious surface 

model <- glmer(formula = abundance ~ Dev_10 + 
                  LHSCategory:Dev_10 + VoltinismCategory:Dev_10 + 
                  LarvalHabitatCategory:Dev_10 + BodySize:Dev_10 +
                  max_lat_dif:Dev_10 + min_lat_dif:Dev_10 +
                  med_lat_dif:Dev_10 + temp_niche:Dev_10 +
                  precip_niche:Dev_10 + (1 | scientific_name),
              na.action = "na.fail", family = poisson,
              control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
              data = mdf)

summary(model)

dd <- MuMIn::dredge(model)
top_model_Dev10 <- get.models(dd, subset = 1)[[1]]
car::vif(top_model_Dev10)
summary(top_model_Dev10)

model <- glmer(formula = abundance ~ Dev_10 + 
                   LHSCategory:Dev_10 + VoltinismCategory:Dev_10 + 
                   LarvalHabitatCategory:Dev_10 + BodySize:Dev_10 +
                   max_lat_dif:Dev_10 + (1 | scientific_name),
               na.action = "na.fail", family = poisson,
               control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
               data = mdf)

dd <- MuMIn::dredge(model)
top_model_Dev10.2 <- get.models(dd, subset = 1)[[1]]
car::vif(top_model_Dev10.2)
summary(top_model_Dev10.2)
r.squaredGLMM(top_model_Dev10.2)

#Checking Assumptions
resids <- residuals(top_model_Dev10.2)
hist(resids)
qqnorm(resids)
qqline(resids)
shapiro.test(resids)

#SJplots of 10km Scale Results

c <- plot_model(top_model_Dev10.2, type = "eff", terms = c("Dev_10", "VoltinismCategory"),
                legend.title = 'Voltinism', title = '',
                    axis.title = c('Proportion urbanization (10-km)','Predicted Abundance'))+
    font_size(title  = 10, axis_title.x = 10, axis_title.y = 10, labels.x = 8, labels.y = 8)+ theme_classic()+
    theme(legend.position = c(.95, .95),
          legend.justification = c("right", "top"),
          legend.box.just = "right")
c<- c + 
    scale_color_discrete()

a <- plot_model(top_model_Dev10.2, type = "eff", terms = c("Dev_10", "LarvalHabitatCategory"),
                legend.title = 'Larval Habitat', title = '',
                axis.title = c('Proportion urbanization (10-km)','Predicted Abundance'))+
    font_size(title  = 10, axis_title.x = 10, axis_title.y = 10, labels.x = 8, labels.y = 8)+ theme_classic()+
    theme(legend.position = c(.95, .95),
          legend.justification = c("right", "top"),
          legend.box.just = "right")
a <- a + 
    scale_color_discrete()

d <- plot_model(top_model_Dev10.2, type = "eff", terms = c("Dev_10", "max_lat_dif"),
                legend.title = 'Species Geographic Range', title = '',
                axis.title = c('Proportion urbanization (10-km)','Predicted Abundance'))+
    font_size(title  = 10, axis_title.x = 10, axis_title.y = 10, labels.x = 8, labels.y = 8)+ theme_classic()+
    theme(legend.position = c(.95, .95),
          legend.justification = c("right", "top"),
          legend.box.just = "right")
d <- d + 
    scale_color_discrete(labels = c("At Northern Extent", "At Mean Extent", "At Southern Extent"))



b <- plot_model(top_model_Dev10.2, type = "eff", terms = c("Dev_10", "LHSCategory"),
                legend.title = 'Larval Diet', title = '',
                axis.title = c('Proportion urbanization (10-km)','Predicted Abundance'))+
    font_size(title  = 10, axis_title.x = 10, axis_title.y = 10, labels.x = 8, labels.y = 8)+ theme_classic()+
    theme(legend.position = c(.95, .95),
          legend.justification = c("right", "top"),
          legend.box.just = "right")
b <- b + 
    scale_color_discrete()

Figure1 <- cowplot::plot_grid(b,a,c,d, labels = c("A","B","C","D"), label_size = 12, nrow = 2, ncol = 2)

Figure1 

#Compare 1km and 10km Scales
AICc(model1.5.1, top_model_Dev10.2) ###10Km scale explains more variation
Weights(model1.5.1, top_model_Dev10.2)

#### Light modeling ####

lightModel <- glmer(formula = abundance ~ meanLight + 
                  VoltinismCategory:meanLight + LHSCategory:meanLight +
                  BodySize:meanLight + max_lat_dif:meanLight + min_lat_dif:meanLight +
                  med_lat_dif:meanLight + temp_niche:meanLight + LarvalHabitatCategory:meanLight +
                  precip_niche:meanLight + (1 | scientific_name),
               na.action = "na.fail", family = poisson,
               control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
               data = mdf)

summary(lightModel)

dd <- MuMIn::dredge(lightModel)
top_model_light <- get.models(dd, subset = 1)[[1]]
car::vif(top_model_light)
summary(top_model_light)
r.squaredGLMM(model)

## Selected Light Model ##
lightModel.1 <- glmer(formula = abundance ~  
                          LHSCategory:meanLight + ## LHSCategory is not showing up
                          BodySize:meanLight + max_lat_dif:meanLight +
                          LarvalHabitatCategory:meanLight + (1 | scientific_name),
                      na.action = "na.fail", family = poisson,
                      control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                      data = mdf)

summary(lightModel.1)

b <- plot_model(lightModel.1, type = "eff", terms = c("meanLight", "LHSCategory"),
                legend.title = 'Larval Diet', title = '',
                axis.title = c('Mean Light at Site','Predicted Abundance'))+
    font_size(title  = 10, axis_title.x = 10, axis_title.y = 10, labels.x = 8, labels.y = 8)+ theme_classic()+
    theme(legend.position = c(.95, .95),
          legend.justification = c("right", "top"),
          legend.box.just = "right")
b <- b + 
    scale_color_discrete()

a <- plot_model(lightModel.1, type = "eff", terms = c("meanLight", "LarvalHabitatCategory"),
                legend.title = 'Larval Habitat', title = '',
                axis.title = c('Mean Light at Site','Predicted Abundance'))+
    font_size(title  = 10, axis_title.x = 10, axis_title.y = 10, labels.x = 8, labels.y = 8)+ theme_classic()+
    theme(legend.position = c(.95, .95),
          legend.justification = c("right", "top"),
          legend.box.just = "right")
a <- a + 
    scale_color_discrete()

c <- plot_model(lightModel.1, type = "eff", terms = c("meanLight", "max_lat_dif"),
                legend.title = 'Species Geographic Range', title = '',
                axis.title = c('Mean Light at Site','Predicted Abundance')) + 
    font_size(title  = 10, axis_title.x = 10, axis_title.y = 10, labels.x = 8, labels.y = 8)+ theme_classic()+
    theme(legend.position = c(.95, .95),
          legend.justification = c("right", "top"),
          legend.box.just = "right")
c <- c + 
    scale_color_discrete(labels = c("At Northern Extent", "At Mean Extent", "At Southern Extent"))

top_row <- cowplot::plot_grid(b,a, labels = c("A", "B"), nrow = 1, ncol = 2)

Figure2 <- cowplot::plot_grid(top_row,c, labels = c('',"C"), label_size = 12, nrow = 2, ncol = 1)
Figure2


dd <- MuMIn::dredge(lightModel.1)
top_model_light <- get.models(dd, subset = 1)[[1]]
car::vif(lightModel.1)
summary(top_model_light)
r.squaredGLMM(lightModel.1)

resids <- residuals(top_model_light)
hist(resids)
qqnorm(resids)
qqline(resids)
shapiro.test(resids)

#### Temperature Modeling ####
modelTemp <- glmer(formula = abundance ~ mean_temp +
                       VoltinismCategory:mean_temp + LHSCategory:mean_temp +
                       BodySize:mean_temp + max_lat_dif:mean_temp + min_lat_dif:mean_temp +
                       med_lat_dif:mean_temp + LarvalHabitatCategory:mean_temp +
                       precip_niche:mean_temp + (1 | scientific_name),
                   na.action = "na.fail", family = poisson,
                   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                   data = mdf)

summary(modelTemp)

dd <- MuMIn::dredge(modelTemp)
top_model_temp <- get.models(dd, subset = 1)[[1]]
car::vif(top_model_temp)
summary(top_model_temp)
r.squaredGLMM(modelTemp)

### Selected Model ###
modelTemp.1 <- glmer(formula = abundance ~
                       LHSCategory:mean_temp +
                       max_lat_dif:mean_temp + LarvalHabitatCategory:mean_temp +
                       (1 | scientific_name),
                   na.action = "na.fail", family = poisson,
                   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                   data = mdf)

summary(modelTemp.1)
car::vif(modelTemp.1)
r.squaredGLMM(modelTemp.1)

resids <- residuals(top_model_temp)
hist(resids)
qqnorm(resids)
qqline(resids)
shapiro.test(resids)

b <- plot_model(modelTemp.1, type = "eff", terms = c("mean_temp", "LHSCategory"),
                legend.title = 'Larval Diet', title = '',
                axis.title = c('Mean Temperature at Site','Predicted Abundance'))+
    font_size(title  = 10, axis_title.x = 10, axis_title.y = 10, labels.x = 8, labels.y = 8)+ theme_classic()+
    theme(legend.position = c(.95, .95),
          legend.justification = c("right", "top"),
          legend.box.just = "right")
b <- b + 
    scale_color_discrete()



a <- plot_model(modelTemp.1, type = "eff", terms = c("mean_temp", "LarvalHabitatCategory"),
                legend.title = 'Larval Habitat', title = '',
                axis.title = c('Mean Temperature at Site','Predicted Abundance'))+
    font_size(title  = 10, axis_title.x = 10, axis_title.y = 10, labels.x = 8, labels.y = 8)+ theme_classic()+
    theme(legend.position = c(.95, .95),
          legend.justification = c("right", "top"),
          legend.box.just = "right")
a <- a +
    scale_color_discrete()



c <- plot_model(modelTemp.1, type = "eff", terms = c("mean_temp", "max_lat_dif"),
                legend.title = 'Species Geographic Range', title = '',
                axis.title = c('Mean Temperature at Site','Predicted Abundance'))+
    font_size(title  = 10, axis_title.x = 10, axis_title.y = 10, labels.x = 8, labels.y = 8)+ theme_classic()+
    theme(legend.position = c(.95, .95),
          legend.justification = c("right", "top"),
          legend.box.just = "right")
c <- c + 
    scale_color_discrete(labels = c("At Northern Extent", "mean", "At Southern Extent"))



top_row <- cowplot::plot_grid(b,a, labels = c("A", "B"), nrow = 1, ncol = 2)

Figure3 <- cowplot::plot_grid(top_row,c,labels = c("","C"), label_size = 12, nrow = 2, ncol = 1, rel_widths = 2)
Figure3


setwd()
ggsave("New Figues/Figure1_Dev_1.png", Figure1, width = 7, height = 7)
ggsave("New Figues/Figure2_light.png", Figure2, width = 7, height = 7)
ggsave("New Figues/Figure3_temp.png", Figure3, width = 7, height = 7)
