library(dplyr)
library(terra)
library(sf)
library(rgbif)
library(data.table)
library(rnaturalearth)
library(ggplot2)

# download occurrence records from gbif using the api, 
# when you do this for all species, you can just read in your gbif download
df <- fread("data/Occurencedata/occurrence.txt", 
            select = c("scientificName", 
                           "decimalLongitude", "decimalLatitude"))
    
df <- df %>% 
    mutate(scientificName = if_else(
        condition = scientificName == "Acanalonia latifrons (Walker, 1851)",
        true = "Acanalonia servillei Spinola, 1839",
        false = scientificName
    )) %>%  
    mutate(scientificName = if_else(
        condition = scientificName == "Anomala medorensis Casey, 1915",
        true = "Anomala innuba (Fabricius, 1787)",
        false = scientificName
    )) %>% 
    mutate(scientificName = if_else(
        condition = scientificName == "BOLD:AAB5640",
        true = "Harmonia axyridis (Pallas, 1773)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "BOLD:AAB3246",
        true = "Odontotaenius disjunctus (Illiger, 1800)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "BOLD:AAB8495",
        true = "Periplaneta americana (Linnaeus, 1758)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "BOLD:AAD4429",
        true = "Camponotus castaneus (Latreille, 1802)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "BOLD:AAE4042",
        true = "Corydalus cornutus (Linnaeus, 1758)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "BOLD:AAG2066",
        true = "Necrophila americana (Linnaeus, 1758)",
        false = scientificName
    )) %>% 
    mutate(scientificName = if_else(
        condition = scientificName == "BOLD:AAG2882",
        true = "Tylozygus bifidus (Say, 1830)",
        false = scientificName
    )) %>% 
    mutate(scientificName = if_else(
        condition = scientificName == "BOLD:AAH3594",
        true = "Chauliodes rastricornis Rambur, 1842",
        false = scientificName
    )) %>% 
    mutate(scientificName = if_else(
        condition = scientificName == "BOLD:AAH3836",
        true = "Cybister fimbriolatus (Say, 1825)",
        false = scientificName
    )) %>% 
    mutate(scientificName = if_else(
        condition = scientificName == "BOLD:AAH6809",
        true = "Pelidnota punctata (Linnaeus, 1758)",
        false = scientificName
    )) %>% 
    mutate(scientificName = if_else(
        condition = scientificName == "BOLD:AAH6814",
        true = "Euphoria sepulcralis (Fabricius, 1801)",
        false = scientificName
    )) %>% 
    mutate(scientificName = if_else(
        condition = scientificName == "BOLD:AAH6841",
        true = "Anomala innuba (Fabricius, 1787)",
        false = scientificName
    )) %>% 
    mutate(scientificName = if_else(
        condition = scientificName == "BOLD:AAH7229",
        true = "Dicromantispa interrupta (Say, 1825)",
        false = scientificName
    )) %>% 
    mutate(scientificName = if_else(
        condition = scientificName == "BOLD:AAJ4157",
        true = "Euphoria sepulcralis (Fabricius, 1801)",
        false = scientificName
    )) %>% 
    mutate(scientificName = if_else(
        condition = scientificName == "BOLD:AAK1384",
        true = "Tabanus atratus Fabricius, 1775",
        false = scientificName
    )) %>% 
    mutate(scientificName = if_else(
        condition = scientificName == "BOLD:AAL6939",
        true = "Tylozygus geometricus (Signoret, 1854)",
        false = scientificName
    )) %>% 
    mutate(scientificName = if_else(
        condition = scientificName == "BOLD:AAM7544",
        true = "Melanolestes picipes (Herrich-Schaeffer, 1848)",
        false = scientificName
    )) %>% 
    mutate(scientificName = if_else(
        condition = scientificName == "BOLD:AAN8345",
        true = "Draeculacephala inscripta Van Duzee, 1915",
        false = scientificName
    )) %>% 
    mutate(scientificName = if_else(
        condition = scientificName == "BOLD:AAQ0749",
        true = "Phileurus valgus (Olivier, 1789)",
        false = scientificName
    )) %>% 
    mutate(scientificName = if_else(
        condition = scientificName == "BOLD:AAV0188",
        true = "Tylozygus geometricus (Signoret, 1854)",
        false = scientificName
    )) %>% 
    mutate(scientificName = if_else(
        condition = scientificName == "BOLD:AAX0700",
        true = "Plecia nearctica Hardy, 1940",
        false = scientificName
    )) %>% 
    mutate(scientificName = if_else(
        condition = scientificName == "BOLD:AAY8739",
        true = "Periplaneta americana (Linnaeus, 1758)",
        false = scientificName
    )) %>% 
    mutate(scientificName = if_else(
        condition = scientificName == "BOLD:ABA2459",
        true = "Phileurus valgus (Olivier, 1789)",
        false = scientificName
    )) %>% 
    mutate(scientificName = if_else(
        condition = scientificName == "BOLD:ABA2833",
        true = "Phileurus truncatus (Palisot de Beauvois, 1807)",
        false = scientificName
    )) %>% 
    mutate(scientificName = if_else(
        condition = scientificName == "BOLD:ABW6208",
        true = "Odontotaenius disjunctus (Illiger, 1800)",
        false = scientificName
    )) %>% 
    mutate(scientificName = if_else(
        condition = scientificName == "BOLD:ABW8775",
        true = "Hydaticus bimarginatus (Say, 1830)",
        false = scientificName
    )) %>% 
    mutate(scientificName = if_else(
        condition = scientificName == "BOLD:ABX0304",
        true = "Neoscapteriscus borellii (Giglio-Tos, 1894)",
        false = scientificName
    )) %>% 
    mutate(scientificName = if_else(
        condition = scientificName == "BOLD:ABX2110",
        true = "Phileurus valgus (Olivier, 1789)",
        false = scientificName
    )) %>% 
    mutate(scientificName = if_else(
        condition = scientificName == "BOLD:ABX6270",
        true = "Periplaneta americana (Linnaeus, 1758)",
        false = scientificName
    )) %>% 
    mutate(scientificName = if_else(
        condition = scientificName == "BOLD:ABX6755",
        true = "Euphoria sepulcralis (Fabricius, 1801)",
        false = scientificName
    )) %>% 
    mutate(scientificName = if_else(
        condition = scientificName == "BOLD:ABZ5061",
        true = "Euphoria sepulcralis (Fabricius, 1801)",
        false = scientificName
    )) %>% 
    mutate(scientificName = if_else(
        condition = scientificName == "BOLD:ACA3550",
        true = "Climaciella brunnea (Say, 1824)",
        false = scientificName
    )) %>% 
    mutate(scientificName = if_else(
        condition = scientificName == "BOLD:ACE9105",
        true = "Periplaneta americana (Linnaeus, 1758)",
        false = scientificName
    )) %>% 
    mutate(scientificName = if_else(
        condition = scientificName == "BOLD:ADB4771",
        true = "Neoconocephalus triops (Linnaeus, 1758)",
        false = scientificName
    )) %>% 
    mutate(scientificName = if_else(
        condition = scientificName == "BOLD:ADJ6678",
        true = "Periplaneta americana (Linnaeus, 1758)",
        false = scientificName
    )) %>% 
    mutate(scientificName = if_else(
        condition = scientificName == "Anomala marginata (Fabricius, 1793)",
        true = "Callistethus marginatus (Fabricius, 1792)",
        false = scientificName
    )) %>% 
    mutate(scientificName = if_else(
        condition = scientificName == "BOLD:AAQ0749",
        true = "Phileurus valgus (Olivier, 1789)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Carabus scrutator Fabricius, 1775",
        true = "Calosoma scrutator (Fabricius, 1775)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Climaciella brunnea occidentis (Banks, 1911)",
        true = "Climaciella brunnea (Say, 1824)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Corydalus cognatus Hagen, 1861",
        true = "Corydalus cornutus (Linnaeus, 1758)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Cybister ellipticus LeConte, 1852",
        true = "Climaciella brunnea (Say, 1824)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Cybister fimbriolatus crotchi Wilke, 1920",
        true = "Climaciella brunnea (Say, 1824)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Cybister flavocinctus AubÃ©, 1838",
        true = "Climaciella brunnea (Say, 1824)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Dibolocelus ovatus (Gemminger & Harold, 1868)",
        true = "Hydrophilus ovatus Gemminger & Harold, 1868",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Dicromantispa floridana (Banks, 1897)",
        true = "Dicromantispa interrupta (Say, 1825)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Erythroneura tricincta var. noncincta Johnson, 1934",
        true = "	Erythroneura calycula McAtee, 1920",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Euphoria appalachia Casey, 1915",
        true = "Euphoria sepulcralis (Fabricius, 1801)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Euphoria scolopacea Casey, 1915",
        true = "Euphoria sepulcralis (Fabricius, 1801)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Euphoria nitens Casey, 1915",
        true = "Euphoria sepulcralis (Fabricius, 1801)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Euphoria sepulcharis floridana Casey, 1915",
        true = "Euphoria sepulcralis (Fabricius, 1801)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Euphoria limatula Janson, 1881",
        true = "Euphoria sepulcralis (Fabricius, 1801)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Harmonia axyridis f. axyridis",
        true = "Harmonia axyridis (Pallas, 1773)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Harmonia axyridis f. conspicua",
        true = "Harmonia axyridis (Pallas, 1773)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Harmonia axyridis f. spectabilis",
        true = "Harmonia axyridis (Pallas, 1773)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Harmonia axyridis f. succinea",
        true = "Harmonia axyridis (Pallas, 1773)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Mallodon baiulus Erichson, 1847",
        true = "Mallodon dasystomum (Say, 1824)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Mallodon chevrolatii chevrolatii",
        true = "Mallodon dasystomum (Say, 1824)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Mallodon chevrolatii galapagoensis (Mutchler, 1938)",
        true = "Mallodon dasystomum (Say, 1824)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Mallodon chevrolatii glabratus (Linsley & Chemsak, 1966)",
        true = "Mallodon dasystomum (Say, 1824)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Mallodon chevrolatii Thomson, 1867",
        true = "Mallodon dasystomum (Say, 1824)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Mallodon dasystomus dasystomus",
        true = "Mallodon dasystomum (Say, 1824)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Mallodon dasystomus MonnÃ©, 2002",
        true = "Mallodon dasystomum (Say, 1824)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Mallodon downesi Quentin & Villiers, 1981",
        true = "Mallodon dasystomum (Say, 1824)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Mallodon downesii Hope, 1843",
        true = "Mallodon dasystomum (Say, 1824)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Mallodon molarius molarius",
        true = "Mallodon dasystomum (Say, 1824)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Mallodon molarius Fragoso & MonnÃ©, 1995",
        true = "Mallodon dasystomum (Say, 1824)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Mallodon spinibarbe Haldeman, 1847",
        true = "Mallodon dasystomum (Say, 1824)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Mallodon spinibarbis Bodkin, 1919",
        true = "Mallodon dasystomum (Say, 1824)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Mantispa brunnea var. occidentis Banks, 1911",
        true = "Dicromantispa interrupta (Say, 1825)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Mantispa cincticornis Banks, 1911",
        true = "Dicromantispa interrupta (Say, 1825)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Mantispa varia Erichson, 1839",
        true = "Dicromantispa interrupta (Say, 1825)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Melanolestes abdominalis (Herrich-Schaeffer, 1848)",
        true = "Melanolestes picipes (Herrich-Schaeffer, 1848)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Pelidnota brevicollis Casey, 1915",
        true = "Pelidnota punctata (Linnaeus, 1758)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Pelidnota brevicollis Casey, 1915",
        true = "Pelidnota punctata (Linnaeus, 1758)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Pelidnota brevis Casey, 1915",
        true = "Pelidnota punctata (Linnaeus, 1758)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Pelidnota debiliceps Casey, 1915",
        true = "Pelidnota punctata (Linnaeus, 1758)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Pelidnota pallidipes Casey, 1915",
        true = "Pelidnota punctata (Linnaeus, 1758)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Pelidnota tarsalis Casey, 1915",
        true = "Pelidnota punctata (Linnaeus, 1758)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Mallodon Lepeletier & Audinet-Serville, 1830",
        true = "Mallodon dasystomum (Say, 1824)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Prionus pocularius Haldeman, 1847",
        true = "Prionus pocularis Dalman, 1817",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Scapteriscus acletus Rehn & Hebard, 1916",
        true = "Scapteriscus borellii Giglio-Tos, 1894",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Stenodontes dasytomus dasytomus",
        true = "Mallodon dasystomum (Say, 1824)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Stenodontes dasytomus Frost, 1975",
        true = "Mallodon dasystomum (Say, 1824)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Stenodontes dasytomus Linsley, 1962",
        true = "Mallodon dasystomum (Say, 1824)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Stenodontes dasytomus socorrensis Noguera & Chemsak, 1996",
        true = "Mallodon dasystomum (Say, 1824)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Stenodontes downesi Burgeon, 1928",
        true = "Mallodon dasystomum (Say, 1824)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Stenodontes downesi Hintz, 1911",
        true = "Mallodon dasystomum (Say, 1824)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Stenodontes downesi Villiers, 1959",
        true = "Mallodon dasystomum (Say, 1824)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Stenodontes masticator Linsley, 1934",
        true = "Mallodon dasystomum (Say, 1824)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Stenodontes molaria Blackwelder, 1946",
        true = "Mallodon dasystomum (Say, 1824)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Stenodontes molarius glabratus Linsley & Chemsak, 1966",
        true = "Mallodon dasystomum (Say, 1824)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Stenodontes molarius vandykei Linsley & Chemsak, 1966",
        true = "Mallodon dasystomum (Say, 1824)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Stenodontes spinibarbis Aurivillius, 1904",
        true = "Mallodon dasystomum (Say, 1824)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Stenodontes spinibarbis Marinoni & Napp, 1984",
        true = "Mallodon dasystomum (Say, 1824)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Tabanus nantuckensis Hine, 1917",
        true = "Tabanus atratus Fabricius, 1775",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Passalus cornutus Fabricius, 1801",
        true = "Odontotaenius disjunctus (Illiger, 1800)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Passalus distinctus Weber, 1801",
        true = "Odontotaenius disjunctus (Illiger, 1800)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Phileurus castaneus Haldeman, 1843",
        true = "Phileurus valgus (Olivier, 1789)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Phileurus floridanus Casey, 1915",
        true = "Phileurus valgus (Olivier, 1789)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Phileurus valgus antillarum Prell, 1912",
        true = "Phileurus valgus (Olivier, 1789)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Popilius disjunctus (Illiger, 1800)",
        true = "Odontotaenius disjunctus (Illiger, 1800)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Erythroneura calycula McAtee, 1920",
        true = "Erythroneura calycula McAtee, 1920",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Scapteriscus borellii Giglio-Tos, 1894",
        true = "Neoscapteriscus borellii (Giglio-Tos, 1894)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Silpha americana Linnaeus, 1758",
        true = "Necrophila americana (Linnaeus, 1758)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Tettigonia psittacella Fowler, 1900",
        true = "Tylozygus geometricus (Signoret, 1854)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "Mantispa interrupta Say, 1825",
        true = "Dicromantispa interrupta (Say, 1825)",
        false = scientificName
    )) %>%
    mutate(scientificName = if_else(
        condition = scientificName == "\tErythroneura calycula McAtee, 1920",
        true = "Erythroneura calycula McAtee, 1920",
        false = scientificName
    )) %>%
    filter(scientificName != "BOLD:ACA8839") %>% 
    mutate(decimalLongitude = round(decimalLongitude, digits = 4),
           decimalLatitude = round(decimalLatitude, digits = 4)) %>% 
    distinct(decimalLongitude, decimalLatitude, .keep_all = T)

dflist <- df[!duplicated(df$scientificName)]

# let's look where these occurrences are
# first grab a basemap of North and South america
americas <- rnaturalearth::ne_countries(continent = c("North America", "South America"),
                                        returnclass = "sf")

ggplot() +
    geom_sf(americas, mapping = aes(), fill = NA) +
    geom_point(df, mapping = aes(x = decimalLongitude, y = decimalLatitude)) +
    theme_classic()+
    facet_wrap(~scientificName)

ggsave('Figures/outliers.pdf', width = 50, height = 36)

plot_sppOccs <- function(x){
    
    print(x)
    
    pdf <- df %>% 
        filter(scientificName == x)
    
    ggplot() +
        geom_sf(americas, mapping = aes(), fill = NA) +
        geom_point(pdf, mapping = aes(x = decimalLongitude, y = decimalLatitude)) +
        theme_classic()
    
    ggsave(paste('Figures/outliers', x, '.png', sep = '_'),
           width = 5, height = 5)
}

sf_use_s2(FALSE)
lapply(unique(df$scientificName), plot_sppOccs)

#removing outliers on a species specific basis
df <- df %>% 
    mutate(id = 1:nrow(df))

pointsToRemove1 <- df %>% 
    filter(scientificName == 'Acanalonia servillei Spinola, 1839' &
               decimalLatitude < 20)
pointsToRemove2 <- df %>% 
    filter(scientificName == 'Anomala innuba (Fabricius, 1787)' & 
               decimalLongitude < -100)
pointsToRemove3 <- df %>% 
    filter(scientificName == 'Camponotus castaneus (Latreille, 1802)' & 
               decimalLatitude < 20)
pointsToRemove4 <- df %>% 
    filter(scientificName == 'Camponotus floridanus (Buckley, 1866)' & 
               decimalLongitude < -100)
pointsToRemove5 <- df %>% 
    filter(scientificName == 'Chauliodes rastricornis Rambur, 1842' & 
               decimalLongitude < -100)
pointsToRemove6 <- df %>% 
    filter(scientificName == 'Corydalus cornutus (Linnaeus, 1758)' & 
               decimalLongitude > 60)
pointsToRemove7 <- df %>% 
    filter(scientificName == 'Euphoria sepulcralis (Fabricius, 1801)' & 
               decimalLongitude > -24)
pointsToRemove8 <- df %>% 
    filter(scientificName == 'Euphoria sepulcralis (Fabricius, 1801)' & 
               decimalLatitude < 0)
pointsToRemove9 <- df %>% 
    filter(scientificName == 'Hydrophilus ovatus Gemminger & Harold, 1868' & 
               decimalLatitude > 50)
pointsToRemove10 <- df %>% 
    filter(scientificName == 'Lethocerus uhleri (Montandon, 1896)' & 
               decimalLatitude > 45)
pointsToRemove11 <- df %>% 
    filter(scientificName == 'Lytta polita Say, 1824' & 
               decimalLongitude < -100)
pointsToRemove12 <- df %>% 
    filter(scientificName == 'Mallodon dasystomum (Say, 1824)' & 
               decimalLongitude < -115)
pointsToRemove13 <- df %>% 
    filter(scientificName == 'Necrophila americana (Linnaeus, 1758)' & 
               decimalLongitude > -50)
pointsToRemove14 <- df %>% 
    filter(scientificName == 'Lethocerus uhleri (Montandon, 1896)' & 
               decimalLatitude > 45)
pointsToRemove15 <- df %>% 
    filter(scientificName == 'Neoconocephalus triops (Linnaeus, 1758)' & 
               decimalLatitude > 45)
pointsToRemove16 <- df %>% 
    filter(scientificName == 'Neoscapteriscus borellii (Giglio-Tos, 1894)' & 
               decimalLongitude > -65)
pointsToRemove17 <- df %>% 
    filter(scientificName == 'Odontotaenius disjunctus (Illiger, 1800)' & 
               decimalLongitude > -65)
pointsToRemove18 <- df %>% 
    filter(scientificName == 'Neoscapteriscus borellii (Giglio-Tos, 1894)' & 
               decimalLatitude < 20)

ptr <- rbind(pointsToRemove1, pointsToRemove2, pointsToRemove3, pointsToRemove4, pointsToRemove5,
             pointsToRemove6, pointsToRemove7, pointsToRemove8, pointsToRemove9, pointsToRemove10,
             pointsToRemove11, pointsToRemove12, pointsToRemove13, pointsToRemove14, pointsToRemove15,
             pointsToRemove16, pointsToRemove17, pointsToRemove18)

df <- df %>% 
    filter(!df$id %in% ptr$id)

write.csv(df, file = 'data/cleanindOccurences.csv', row.names = F)
#calculate high, low, and median latitudes for species.
#First, remove NA for lat and long

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

write.csv(df_sum, file = "data/geographicdata", row.names = F)

