library(dplyr)
library(ggplot2)
library(mgcv)
library(lubridate)

df_geo <- read.csv('data/cleanData.csv') %>% 
    mutate(year = year(Date),
           doy = yday(Date)) %>% 
    mutate(doy2 = if_else(year == 2020,
                          true = doy + 365,
                          false = doy))

tdf <- filter(df_geo, scientificName == 'Hydaticus bimarginatus')
              
rist_test <- filter(tdf, Site == "Rist")

gam_m <- gam(Count ~ s(doy2, k = 15), bs = "cr", data = tdf)
gam_p <- predict.gam(gam_m, newdata = data.frame(doy2 = 69:425),
                     type = "response", se = F)

gam_p_df <- data.frame(doy2 = 69:425, Count = gam_p)

ggplot(gam_p_df, aes(x = doy2, y = Count)) +
    geom_smooth()

ggplot(tdf, aes(x = doy2, y = Count)) + 
    geom_point()

gam.check(gam_m)

##Result: Most species do not have enough data for us to do a GAM with.