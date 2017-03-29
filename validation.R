## A test of density based clustering on LiDAR ground and canopy returns.
## sample area is recorded as having a stocking of 415 stems/ha
##

# Clear Workspace
rm(list = ls())


# Global Opts
options(digits = 10)
errorDBF <- "N:/ArcGIS/Tree Detection/filtered_canopy_3_mistakes.dbf"
itdCSV   <- "Results/Tree_Detection_filtered_canopy_3.csv"
canopyRD <- "filtered_canopy.rd"


# Attach Packages
library(LSD)
library(lidR)
library(dbscan)
library(tidyverse)

# Load externally defined functions
source("auxillary.R")


# read tree-locations from dbscan
itd   <- read_csv(itdCSV, progress = FALSE)
bad   <- foreign::read.dbf(errorDBF, as.is = TRUE)
cloud <- readRDS(canopyRD)


# Plot location of interest
inspect_bad_point <- function(i){
    
    plot_canopy(cloud, bad$X[i], bad$Y[i], 10)
    find_nearest(bad[i, ], itd) %>% 
        slice(itd, .) %>% 
        select(X, Y) %>% 
        data.matrix %>% 
        points(pch = 16, col = "darkgreen")
    
    
    gg <- plot_canopy_gg(cloud, bad$X[i], bad$Y[i], 10)
    pp <- find_nearest(bad[i, ], itd) %>% 
        slice(itd, .) %>% 
        select(X, Y) %>%
        geom_point(data = ., mapping = aes(colour = NULL), size = 4, shape = 16, colour = "darkgreen")
    gg + pp
    
}

inspect_bad_point(1)
