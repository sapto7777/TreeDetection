## A test of density based clustering on LiDAR ground and canopy returns.
## sample area is recorded as having a stocking of 415 stems/ha
##

# Clear Workspace
rm(list = ls())


# Global Opts
options(digits = 10)
file.out <- "Tree_Detection_filtered_canopy.csv"
canopyRD <- "filtered_canopy.rd"


# Attach Packages
library(LSD)
library(lidR)
library(dbscan)
library(tidyverse)

# Load externally defined functions
source("auxillary.R")


# Load data
cloud <- readRDS(canopyRD)


# Find average distance to nearest neighbour
cloud.dist.nn <- select(cloud, X, Y) %>% 
    data.matrix %>%
    kNNdist(k = 1) %>% 
    as.numeric

kNN_histogram(cloud.dist.nn, xlab = "Distance to nearest-neighbour")
epsilon <- 0.541

# Count number of points in epsilon neighbourhood
cloud.count.nn <- select(cloud, X, Y) %>%
    data.matrix %>%
    frNN(eps = epsilon, sort = FALSE) %>%
    `[[`("id") %>%
    map_dbl(length)


kNN_histogram(cloud.count.nn, xlab = "Number of points in epsilon-neighbourhood")
min.points <- 7


# run dbscan
fit_1 <- select(cloud, X, Y) %>% 
    data.matrix %>% 
    dbscan(eps = epsilon, minPts = min.points, bucketSize = 100)
cloud$Cluster <- fit_1$cluster


# Compute cluster centroids
tree.centres <- group_by(cloud, Cluster) %>% 
    summarise(X = median(X), Y = median(Y)) %>% 
    select(-Cluster)



### cluster again
trees.dist.nn <- select(tree.centres, X, Y) %>% 
    data.matrix %>%
    kNNdist(k = 1) %>% 
    as.numeric

kNN_histogram(trees.dist.nn, xlab = "Distance to nearest-neighbour")
epsilon <- 1.49

# Count number of points in epsilon neighbourhood
trees.count.nn <- select(tree.centres, X, Y) %>%
    data.matrix %>%
    frNN(eps = epsilon, sort = FALSE) %>%
    `[[`("id") %>%
    map_dbl(length)


kNN_histogram(trees.count.nn, xlab = "Number of points in epsilon-neighbourhood")
min.points <- 1


# run dbscan
fit_2 <- select(tree.centres, X, Y) %>%
    data.matrix %>% 
    dbscan(eps = epsilon, minPts = min.points, bucketSize = 100)
tree.centres$Cluster <- fit_2$cluster

tree.centres2 <- group_by(tree.centres, Cluster) %>% 
    summarise(X = median(X), Y = median(Y)) %>% 
    select(-Cluster)
cloud %>% with(heatscatter(X, Y))
points(tree.centres2, pch = 4, col = "green")


# Write tree locations to csv
write.csv(tree.centres2, file = file.out, row.names = FALSE)
