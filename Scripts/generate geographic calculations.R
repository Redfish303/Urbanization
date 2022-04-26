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
# let's look where these occurrences are
# first grab a basemap of North and South america
americas <- rnaturalearth::ne_countries(continent = c("North America", "South America"),
                                        returnclass = "sf")

ggplot() +
    geom_sf(americas, mapping = aes(), fill = NA) +
    geom_point(df, mapping = aes(x = decimalLongitude, y = decimalLatitude)) +
    theme_classic()


