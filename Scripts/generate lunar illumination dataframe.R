library(lunar)

lunar.phase(as.Date("2022-06-01"))

df <- read.csv("data/cleanData.csv")


li <- lunar.illumination(unique(as.Date(df$Date)))

li_df <- data.frame(Date = unique(as.Date(df$Date)),
                    lunar.phase = li)
