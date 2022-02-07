library(googlesheets4)
library(dplyr)

d <- read_sheet("https://docs.google.com/spreadsheets/d/199ZGN6cTT6H0izlWo0rJ77ZL4YkYQDgwkgnQUxfX4YA/edit#gid=0")

d2 <- d %>% 
    filter(E != "NA") %>% 
    rowwise() %>% 
    mutate(siteMean = mean(c(N, W, S, E), na.rm = TRUE)) %>% 
    group_by(Site) %>% 
    summarise(meanLight = mean(siteMean, na.rm = TRUE))
    
write.csv(d2, "data/lightData.csv", row.names = FALSE)
