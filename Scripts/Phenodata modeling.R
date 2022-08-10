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

#Need to only estimate peakdoy if a species was observed at a site on three unique days

uniqueDays <- df %>% 
    group_by(scientificName, Site) %>% 
    summarise(uniqueDaysObs = length(unique(doy2))) #Makes a variable of the amount of days it was found

peakCount <- left_join(df, uniqueDays) %>%
    filter(uniqueDaysObs >= 3) %>% 
    group_by(scientificName, Site) %>% 
    summarise(peakDoy = doy2[which.max(Count)]) #Here we have joined this new variable to the df and filters for more than 3

#Find the species that have enough uniqueDaysObs at 3 or more sites
enoughObs <- peakCount %>% 
    group_by(scientificName) %>% 
    summarise(nSites = length(unique(Site))) %>% 
    filter(nSites >= 3)

#Filter peak count to only include species with enoughObs >= 3
peakCount <- peakCount %>% 
    filter(scientificName %in% enoughObs$scientificName) %>% 
    filter(scientificName != "None")

#Join with other data
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

lm.1 <- lmer(peakDoy ~ Dev_1 +
               Dev_1:LHSCategory +
               Dev_1:BodySize +
               Dev_1:LarvalHabitatCategory +
               Dev_1:VoltinismCategory
           + (1|scientificName), data = peakCount)

summary(lm.1)

dd <- MuMIn::dredge(model)
s <- step(lm)
top_model <- get.models(dd, subset = 1)[[1]]
car::vif(top_model)
summary(top_model)
r.squaredGLMM(model)

resids <- residuals(lm)
hist(resids)
qqnorm(resids)
qqline(resids)
shapiro.test(resids)

sjPlot::plot_model(lm, type = "eff", terms = c("Dev_10", "LHSCategory"))

sjPlot::plot_model(lm, type = "eff", terms = c("Dev_10", "BodySize"))

sjPlot::plot_model(lm, type = "eff", terms = c("Dev_10", "LarvalHabitatCategory"))

sjPlot::plot_model(lm, type = "eff", terms = c("Dev_10", "VoltinismCategory"))
#Standing water species vs running water species and how they are impacted by differences in ambient temp
#Lentic - Running Lotic - standing
    