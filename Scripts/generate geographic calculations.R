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
    filter(scientificName != "BOLD:AAH3594") %>% 
    mutate(decimalLongitude = round(decimalLongitude, digits = 4),
             decimalLatitude = round(decimalLatitude, digits = 4)) %>% 
    distinct(decimalLongitude, decimalLatitude, .keep_all = T)


df_sum <- df %>% 
    group_by(scientificName) %>% 
    summarise(max_lat = quantile(decimalLatitude, probs = 0.9),
              min_lat = quantile(decimalLatitude, probs = 0.1),
              med_lat = median(decimalLatitude))

#Then Calculate the difference of these  values to the latitude of BACA, which is 29.6429

df_sum <- df_sum %>% 
    mutate(max_lat_dif = max_lat - 29.6429,
           min_lat_dif = min_lat - 29.6429,
           med_lat_dif = med_lat - 29.6429)

#save lat dif dataframe
