library(dplyr)
library(lme4)
library(tidyr)
library(stringr)
library(ggplot2)
library(lmerTest)
library(sjPlot)
### script to analyze effect of urbanization on overall abundace by species

#read in abundance data

cm <- read.csv("data/communitymatrixSite.csv")
cm[1:2]

#pivot data into long form
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
                                  by = c("scientific_name" = "Species"))

#plot all species abundance by urbanization amount with colors and slopes per spp.
ggplot(cm_long_urbanization, mapping = aes(x = Dev_1, y = abundance, color = scientific_name)) +
    geom_point(show.legend = FALSE) +
    geom_smooth(method = "lm", se = F,  show.legend = FALSE) +
    theme_classic() 

#facet the gg plot to see individual sp.
ggplot(cm_long_urbanization, mapping = aes(x = Dev_1, y = abundance, color = scientific_name)) +
    geom_point(show.legend = FALSE) +
    geom_smooth(method = "lm", se = F,  show.legend = FALSE) +
    theme_classic() +
    facet_wrap(~scientific_name)

ggplot(cm_long_urbanization, mapping = aes(x = Dev_10, y = abundance, color = scientific_name)) +
    geom_point(show.legend = FALSE) +
    geom_smooth(method = "lm", se = F,  show.legend = FALSE) +
    theme_classic() +
    facet_wrap(~scientific_name)

mdf <- cm_long_urbanization %>% 
    na.omit()

## quick test to see if abundance of selected species is impacted by urbanization
model <- lmer(formula = abundance ~ Dev_1 + 
                  LHSCategory + VoltinismCategory + LarvalHabitatCategory + BodySize +
                  LHSCategory:Dev_1 + VoltinismCategory:Dev_1 + 
                  LarvalHabitatCategory:Dev_1 + BodySize:Dev_1 +
                  (1 | scientific_name), na.action = "na.fail",
              data = mdf)

summary(model)

dd <- MuMIn::dredge(model)
s <- step(model)

top_model <- get.models(dd, subset = 1)[[1]]
car::vif(top_model)
summary(top_model)

sjPlot::plot_model(top_model, type = "pred")
sjPlot::plot_model(top_model, terms = "BodySize", type = "pred")
sjPlot::plot_model(top_model, terms = "LarvalHabitatCategory", type = "pred")
sjPlot::plot_model(top_model, terms = c("Dev_1","LarvalHabitatCategory"), type = "pred")
sjPlot::plot_model(top_model, terms = c("Dev_1","VoltinismCategory"), type = "pred")

## model this at a 10 km scale for imprevious surface
model2 <- lmer(formula = abundance ~ Dev_10 + 
                  LHSCategory + VoltinismCategory + LarvalHabitatCategory + BodySize +
                  LHSCategory:Dev_10 + VoltinismCategory:Dev_10 + 
                  LarvalHabitatCategory:Dev_10 + BodySize:Dev_10 +
                  (1 | scientific_name), na.action = "na.fail",
              data = mdf)

summary(model2)

dd2 <- MuMIn::dredge(model2)

top_model2 <- get.models(dd2, subset = 1)[[1]]
car::vif(top_model2)
summary(top_model2)

## look into AICs of 1km and 10km
AICc(top_model, top_model2) # our top top model is top model 2

top_model <- top_model2
summary(top_model2)

sjPlot::plot_model(top_model, terms = "Dev_10", type = "pred")
sjPlot::plot_model(top_model, terms = "BodySize", type = "pred")
sjPlot::plot_model(top_model, terms = "LarvalHabitatCategory", type = "pred")
sjPlot::plot_model(top_model, terms = c("Dev_10","LarvalHabitatCategory"), type = "pred")
sjPlot::plot_model(top_model, terms = c("Dev_10","VoltinismCategory"), type = "pred")

### test for richness
rich <- cm_long_urbanization %>% 
    filter(abundance != 0) %>% 
    group_by(Site) %>% 
    summarise(richness = length(unique(scientific_name))) %>% 
    left_join(urb)

ggplot(rich, mapping = aes(x = Dev_1, y = richness)) +
    geom_point(show.legend = FALSE) +
    geom_smooth(method = "lm",  show.legend = FALSE) +
    theme_classic() 

#richness on 1 and 10km scale
modelRich <- lm(formula = richness ~ Dev_1,
                  na.action = "na.fail",
              data = rich)

modelRich2 <- lm(formula = richness ~ Dev_10,
            na.action = "na.fail",
            data = rich)

AICc(modelRich, modelRich2)

sjPlot::plot_model(modelRich, terms = "Dev_1", type = "pred")
sjPlot::plot_model(modelRich2, terms = "Dev_10", type = "pred")

## community weighted mean
cwm <- cm_long_urbanization %>% 
    group_by(Site) %>% 
    summarise(
        BodySize_cwm =weighted.mean(BodySize, abundance, na.rm=T)
    )

cwm_urb <- left_join(cwm, urb)

head(cwm_urb)

ggplot(cwm_urb, mapping = aes(x = BodySize_cwm, y = Dev_10)) +
    geom_point() +
    geom_smooth(method = "lm")

# CWM model
