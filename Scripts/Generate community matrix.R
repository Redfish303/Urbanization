library(dplyr)
library(labdsv)


d <- read.csv("data/cleanData.csv")

d2 <- d %>% 
    select(Site, scientificName, Count, Date)

#Function to create a data frame in community matrix format, including date
matrify2 <-  function(data) { 
    #Data must have columns: plot, SPEC, abundance measure,Year 
    if (ncol(data) != 4) 
        stop("data frame must have four column format")
    plt <- factor(data[, 1]) 
    spc <- factor(data[, 2])
    abu <- data[, 3]
    yrs <- factor(data[, 4])
    plt.codes <- sort(levels(factor(plt)))                                                     ##object with sorted plot numbers
    spc.codes <- levels(factor(spc))                                                           ##object with sorted SPEC names
    yrs.codes <- sort(levels(factor(yrs)))                                                     ##object with sorted sampling Years
    taxa <- matrix(0, nrow = length(plt.codes)*length(yrs.codes), ncol = length(spc.codes))    ##Create empty matrix with proper dimensions (unique(plotxYear) by # of SPEC)
    plt.list <- rep(plt.codes,length(yrs.codes))                                               ##Create a list of all the plot numbers (in order of input data) to add as an ID column at end of function
    yrs.list <- rep(yrs.codes,each=length(plt.codes))                                          ##Create a list of all the Year numbers (in order of input data) to add as an ID column at end of function
    col <- match(spc, spc.codes)                                                               ##object that determines the alphabetical order ranking of each SPEC in the spc.code list
    row.plt <- match(plt, plt.codes)                                                           ##object that determines the rank order ranking of each plot of the input data in the plt.code list
    row.yrs <- match(yrs,yrs.codes)                                                            ##object that determines the rank order ranking of each Year of the input data in the yrs.code list
    for (i in 1:length(abu)) {
        row <- (row.plt[i])+length(plt.codes)*(row.yrs[i]-1)                                   ##Determine row number by assuming each row represents a specific plot & year in an object of rep(plot,each=Year)
        if(!is.na(abu[i])) {                                                                   ##ONly use value if !is.na .. [ignore all is.NA values]
            taxa[row, col[i]] <- sum(taxa[row, col[i]], abu[i])                                  ##Add abundance measure of row i to the proper SPEC column and plot/Year row. Sum across all identical individuals.
        }
    }
    taxa <- data.frame(taxa)                                                                   ##Convert to data.frame for easier manipulation
    taxa <- cbind(plt.list,yrs.list,taxa)                                                      ##Add ID columns for plot and Year to each row already representing the abundance of Each SPEC of that given plot/Year.
    names(taxa) <- c('Plot','Year',spc.codes)
    taxa
}

#Generate community matrix with site and date
m <- matrify2(d2)

write.csv(m, "data/communitymatrixDate.csv", row.names = FALSE)

d3 <- d2 %>% 
    select(Site, scientificName, Count)

#Generate community matrix with only site


gb <- d %>% 
    group_by(Site, scientificName) %>% 
    summarise(abundance = sum(Count, na.rm = T)) %>%
    as.data.frame()

m3 <- matrify(gb)

write.csv(m3, "data/communitymatrixSite.csv")

