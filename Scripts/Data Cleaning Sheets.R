library(googlesheets4)
library(dplyr)

## Read in google sheet with by-catch data

d <- read_sheet("https://docs.google.com/spreadsheets/d/1OsDSQ6iZFz38Z6nv939W9ePDAQkYKneuIEe4qCPop1s/edit#gid=0")

head(d)

# are there naming inconsistencies with our site?
unique(d$Site) #yes


#fix naming inconsistencies in the sites
d2 <- d %>%
    dplyr::mutate(Site = case_when(
        Site == "prcr" ~ "Prcr",
        Site == "PRCR" ~ "Prcr",
        Site == "Biar" ~ "Biva",
        TRUE ~ Site
    ))


unique(d2$Site) %>% length()

#rename columns to get rid of back ticks

d2 <- d2 %>% 
    dplyr::rename(Tube = 3, 
                  scientificName = 4) %>% 
    dplyr::mutate(scientificNameL = tolower(scientificName))

head(d2)

## look into naming inconsistencies with scientificName
sort(unique(d2$scientificNameL))

#' anomala innuba, anomala inuba
#' blatella asahinai, blattella asahinai
#' chauliodes rastricornins, chauliodes rastricornis, chauliodes rastricornus
#' cibister fimbriolatus, cybister fimbriolatus
#' dichotomius corlinus, dichotomius carolinus
#' magaladocne fasciata, megalodacne fasciata
#' neoscapteriscus borelli, neoscapteriscus borellii
#' odontotaenuius disjuctus, odontotaenuius disjunctus
#' plecia nearctica, plecia neartica
#' prionus imbricornis, prionus imbricornus, prionius imbricornus
#' prionus pocularis, pronius pocularis,
#' prosapia bicincta, procapia bicincta

d3 <- d2 %>% 
    dplyr::mutate(scientificNameL = case_when(
        scientificNameL == "anomala inuba" ~ "anomala innuba",
        scientificNameL == "annomala innuba" ~ "anomala innuba",
        scientificNameL == "blatella asahinai" ~ "blattella asahinai",
        scientificNameL == "chauliodes rastricornins" ~ "chauliodes rastricornis",
        scientificNameL == "chauliodes rastricornus" ~ "chauliodes rastricornis",
        scientificNameL == "cibister fimbriolatus" ~ "cybister fimbriolatus",
        scientificNameL == "dichotomius corlinus" ~ "dichotomius carolinus",
        scientificNameL == "dichotromius carolinus" ~ "dichotomius carolinus",
        scientificNameL == "megaladocne fasciata" ~ "megalodacne fasciata",
        scientificNameL == "neoscapteriscus borelli" ~ "neoscapteriscus borellii",
        scientificNameL == "odontotaenuius disjuctus" ~ "odontotaenuius disjunctus",
        scientificNameL == "plecia neartica" ~ "plecia nearctica",
        scientificNameL == "prionus imbricornus" ~ "prionus imbricornis",
        scientificNameL == "prionius imbricornus" ~ "prionus imbricornis",
        scientificNameL == "pionus imbricornis" ~ "prionus imbricornis",
        scientificNameL == "pronius imbricornus" ~ "prionus imbricornis",
        scientificNameL == "prionius pocularis" ~ "prionus pocularis",
        scientificNameL == "pronius pocularis" ~ "prionus pocularis",
        scientificNameL == "procapia bicincta" ~ "prosapia bicincta",
        scientificNameL == "stilpnochloria couliana" ~ "stilpnochlora couliana",
        TRUE ~ scientificNameL
    ))

sort(unique(d3$scientificNameL))

#' function to capitalize first letter in string
firstup <- function(x) {
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
}

d3 <- d3 %>% 
    dplyr::mutate(scientificName = firstup(scientificNameL)) %>% 
    dplyr::select(-scientificNameL)

write.csv(d3, file = "data/cleanData.csv", row.names = F)

