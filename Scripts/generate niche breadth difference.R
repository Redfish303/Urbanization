library(dplyr)
library(terra)
library(sf)
library(rgbif)
library(data.table)
library(rnaturalearth)
library(ggplot2)

# download occurrence records from gbif using the api, 
# when you do this for all species, you can just read in your gbif download
df <- fread("", 
            select = c("scientificName", 
                       "decimalLongitude", "decimalLatitude", 
                       "year", "eventDate")) # use this function to read in gbif download

# for our example we will use Prionus pocularis
# Prionus pocularis
pp <- occ_search(scientificName = "Prionus pocularis",
                 fields = c("scientificName", 
                            "decimalLongitude", "decimalLatitude", 
                            "year", "eventDate"), limit = 100000)$data

pi <- occ_search(scientificName = "Prionus imbricornis",
                 fields = c("scientificName", 
                            "decimalLongitude", "decimalLatitude", 
                            "year", "eventDate"), limit = 100000)$data

cr <- occ_search(scientificName = "Chauliodes rastricornis",
                 fields = c("scientificName", 
                            "decimalLongitude", "decimalLatitude", 
                            "year", "eventDate"), limit = 100000)$data
#Seems to be a misidentification in chauliodes- California point

cr <- cr %>% 
    filter(decimalLongitude > -115)

df <- rbind(pp, pi, cr)



# let's look where these occurrences are
# first grab a basemap of North and South america
americas <- rnaturalearth::ne_countries(continent = c("North America", "South America"),
                                        returnclass = "sf")

ggplot() +
    geom_sf(americas, mapping = aes(), fill = NA) +
    geom_point(df, mapping = aes(x = decimalLongitude, y = decimalLatitude)) +
    theme_classic()+
    facet_wrap(~scientificName)


df <- df %>%  
    filter(!is.na(decimalLatitude),
           !is.na(decimalLongitude))

#calculate high, low, and median latitudes for species.
#First, remove NA for lat and long

#Combine synonyms and drop BOLD samples

df <- df %>% 
    mutate(scientificName = if_else(
        condition = scientificName == "Prionus pocularius Haldeman, 1847",
        true = "Prionus pocularis Dalman, 1817",
        false = scientificName
    )) %>%  
    filter(scientificName != "BOLD:AAH3594")


#800m resolution
# read in temperature raster file
t <- rast("data/bio1.tif")

#precipitation raster file
p <- rast('data/bio12.tif')

#extract the temp/precip values for each unique data point per species
#first pull out species from out filtered data frame and retain only 


df_coords <- df[,2:3]
#extracts temperature for each data point
temp_e <- extract(t, df_coords)
precip_e <- extract(p,df_coords)

#combine this with species df
df_climate <- df %>% 
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
