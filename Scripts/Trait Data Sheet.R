library(googlesheets4)
library(dplyr)

d <- read_sheet("https://docs.google.com/spreadsheets/d/14xPyfvsydOw95YC7SWKi5UrCimHIFETZOw4c7qB1Ek0/edit#gid=0")

d2 <- d %>% 
    select(Species, BodySize, LHSCategory, VoltinismCategory, LarvalHabitatCategory)

write.csv(d2, file = "data/TraitData.csv", row.names = F)
