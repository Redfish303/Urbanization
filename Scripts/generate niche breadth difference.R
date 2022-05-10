library(dplyr)
library(terra)
library(sf)
library(rgbif)
library(data.table)
library(rnaturalearth)
library(ggplot2)

df_geo <- read.csv('data/geographicdata')

# let's look where these occurrences are
# first grab a basemap of North and South america
americas <- rnaturalearth::ne_countries(continent = c("North America", "South America"),
                                        returnclass = "sf")

ggplot() +
    geom_sf(americas, mapping = aes(), fill = NA) +
    geom_point(df, mapping = aes(x = decimalLongitude, y = decimalLatitude)) +
    theme_classic()+
    facet_wrap(~scientificName)

#800m resolution
# read in temperature raster file
t <- rast("data/bio1.tif")

#precipitation raster file
p <- rast('data/bio12.tif')

#extract the temp/precip values for each unique data point per species
#first pull out species from out filtered data frame and retain only 

df_coords <- df_geo[,2:3]
#extracts temperature for each data point
temp_e <- extract(t, df_coords)
precip_e <- extract(p,df_coords)

#combine this with species df
df_climate <- df_geo %>% 
    mutate(temp = temp_e$bio1,
            precip = precip_e$bio12) %>% 
    filter(!is.na(temp), !is.na(precip))

ggplot() +
    geom_point(df_climate, mapping = aes(x = temp, y = precip, color = scientificName),
               alpha = 0.5) +
    theme_bw()

#calculate niche breadth

niche_df <- df_climate %>% 
    group_by(scientificName) %>% 
    summarise(temp_niche = sd(temp),
              precip_niche = sd(precip))
