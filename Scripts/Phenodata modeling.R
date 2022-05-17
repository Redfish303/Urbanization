library(dplyr)
library(ggplot2)
library(mgcv)
library(lubridate)
library(lme4)
library(lmerTest)

df <- read.csv('data/cleanData.csv') %>% 
    mutate(year = year(Date),
           doy = yday(Date)) %>% 
    mutate(doy2 = if_else(year == 2020,
                          true = doy + 365,
                          false = doy))

head(df)

peakCount <- df %>%
    group_by(scientificName, Site) %>% 
    summarise(peakDoy = doy2[which.max(Count)])

urb <- read.csv("data/impervious_surface.csv")



peakCount <- left_join(peakCount, urb)

ggplot(peakCount, mapping = aes(x = Dev_10, y = peakDoy, color = scientificName)) +
    geom_point() +
    geom_smooth(, method = "lm", se = F) + 
    theme_classic() +
    theme(legend.position = "none")

### add in trait data
traits <- read.csv("data/TraitData.csv")
peakCount <- left_join(peakCount, traits, by = c("scientificName" = "Species"))
### test linear model

lm <- lmer(peakDoy ~ Dev_10 +
               Dev_10:LHSCategory +
               Dev_10:BodySize +
               Dev_10:LarvalHabitatCategory +
               Dev_10:VoltinismCategory
           + (1|scientificName), data = peakCount)

sjPlot::plot_model(lm, type = "eff", terms = c("Dev_10", "LHSCategory"))
    