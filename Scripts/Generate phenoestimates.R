library(dplyr)
library(lme4)
library(tidyr)
library(stringr)
library(ggplot2)
library(lmerTest)
library(sjPlot)
### script to analyze effect of urbanization on overall abundace by species

#read in abundance data
cm <- read.csv("data/communitymatrixDate.csv")

#pivot data into long form
cm_long <- cm %>% 
    pivot_longer(-c(Plot,Year), names_to = "scientific_name", values_to = "abundance") %>% 
    mutate(scientific_name = str_replace(scientific_name,
                                         pattern = "\\.", replacement = " ")) %>% 
    rename(Site = Plot, Date = Year)

## estimate phenophases for species with at least 3 days with observations per site
unique_days <- cm_long %>% 
    filter(abundance > 0) %>% 
    filter(scientific_name != "none" & scientific_name != "None") %>% 
    group_by(Site, scientific_name) %>% 
    summarise(unique_days = length(unique(Date)))

## join with cm_long to see which site x species combinations don't have enough data
mdf <- left_join(cm_long, unique_days) %>% 
    filter(unique_days >= 3)

# make generalized additive models (GAMs) for each species

