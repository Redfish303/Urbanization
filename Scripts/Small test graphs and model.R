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

## quick test to see if abundance of selected species is impacted by urbanization

model <- lmer(formula = abundance ~ Dev_1 + (1 | scientific_name),
              data = cm_long_urbanization)

summary(model)
sjPlot::plot_model(model, type = "pred")

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


