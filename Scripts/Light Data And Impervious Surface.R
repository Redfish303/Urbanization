library(dplyr)
library(ggplot2)

ld <- read.csv("data/lightData.csv")
is <- read.csv("data/impervious_surface.csv")

#combine these
mdf <- left_join(ld, is)

#plot these linearly
ggplot(mdf, mapping = aes(x = Dev_1, y = meanLight, label = Site)) + #mapping tells ggplot that the data is in geographic space
    geom_text() +
    geom_point() +
    geom_smooth(method = "lm")

#Correlation of this

cor(mdf$meanLight, mdf$Dev_1)

ggplot(mdf, mapping = aes(x = Dev_10, y = meanLight, label = Site)) + #mapping tells ggplot that the data is in geographic space
    geom_text() +
    geom_point() +
    geom_smooth(method = "lm")

#Correlation of this

cor(mdf$meanLight, mdf$Dev_10)

#Probably can't run these variables in the same model because they are so correlated.  We can run
#them in separate models
