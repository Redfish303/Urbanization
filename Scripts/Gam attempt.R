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

tdf <- filter(df_geo, scientificName == 'Anomala innuba')
              
rist_test <- filter(tdf, Site == "Rist")

gam_m <- gam(Count ~ s(doy2, k = 5), bs = "cr", data = rist_test)
gam_p <- predict.gam(gam_m, newdata = data.frame(doy2 = 69:425),
                     type = "response", se = F)

gam_p_df <- data.frame(doy2 = 69:425, Count = gam_p)

ggplot(gam_p_df, aes(x = doy2, y = Count)) +
    geom_smooth()


