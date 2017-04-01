## A test of density based clustering on LiDAR ground and canopy returns.
## sample area is recorded as having a stocking of 415 stems/ha
##

# Clear Workspace
rm(list = ls())


# Global Opts
options(digits = 10)
# dataDIR  <- "E:/Attlas_aerosample/fraser_sample_cpts/NRM"
dataDIR  <- "C:/Users/Home/Dropbox/ITD"
dataLAS  <- "Area3_lidar.laz"
file.out <- "Tree_Detection_with_DBSCAN.csv"
canopyRD <- "filtered_canopy.rd"
cppFile  <- "find.cpp"


# Attach Packages
library(LSD)
library(lidR)
library(Rcpp)
library(dbscan)
library(tidyverse)


# Source External files
sourceCpp(cppFile)
source("auxillary.R")
source("kdtree.R")


# Import dataset
las <- paste(dataDIR, dataLAS, sep = "/") %>% readLAS


## Point Classification:
##  1 - Unclassified
##  2 - Ground
##  3 - Low Vegetation
##  4 - Medium Vegetation
##  5 - High Vegetation
##  6 - Building
##  7 - Noise
## 12 - Overlap

# Remove Unclassified and Noise points. Extract ground, high vege (above 5m) points and 
# then free memory.
las <- lasfilter(las, !(Classification == 1 | Classification == 7))
test <- lasfilter(las, Classification %in% c(2, 5), 
                  X > 479600, X < 479800, Y > 7124400, Y < 7124600) %>% 
    `@`("data") %>%
    dplyr::filter((Z == 0) | (Z > 5)) %>% 
    select(X, Y, Z, Classification, Intensity) %>% 
    rename(Type = Classification) %>% 
    arrange(X, Y) %>% 
    mutate(Wt = Intensity / max(Intensity))

rm(list = "las")
gc()


# Calculate some statistics usefull for cleaning the data, Re-classify stem returns as medium 
# vege.
stats   <- select(test, Z) %>% dplyr::filter(Z > 0) %>% summarise_each(funs(mean, sd))
stem.ht <- (stats$mean - 1.96 * stats$sd) %>% floor
test    <- mutate(test, Type = ifelse((Z < stem.ht) & (Type != 2), 4, Type))



# # Remove ground points that are too close to the tree, based on the average
# # distance between each stem point and the nearest ground point.
# stem.points   <- dplyr::filter(test, Type == 4) %>% 
#     select(X, Y) %>% 
#     as.matrix
# ground.points <- dplyr::filter(test, Type == 2) %>% 
#     select(X, Y) %>% 
#     as.matrix %>%
#     kdtree
# 
# stem.ground.prof <- apply(stem.points, 1, kNN, ground.points)
# stem.buff   <- map_dbl(stem.ground.prof, function(x) x$dist) %>% mean
# idxRmGround <- map_int(stem.ground.prof, function(x) {
#     if(x$dist < stem.buff){
#         x$id
#     } else {
#         NA_integer_
#     }
# }) %>% na.omit %>% which(test$Type == 2)[.]
# test <- slice(test, -idxRmGround)


### Filter canopy points which we can be confident are not at the tree location.
#   index the stem points and ground points, so that you know what need to be kept and what to 
#       remove.
#   Find the fixed-radius nearest neighbours of all stem points in the dataset, these points
#       must be retained in the final dataset.
#   Find the fixed-radius nearest neighbours of all ground points in the dataset, these points
#       must be removed (so long as they are not also a neighbour of the stem points).
#   search each neighbourhood and remove all those which do not contain a stem point.
#   save data.
ground.dist.nn <- dplyr::filter(test, Type == 2) %>%
    select(X, Y) %>%
    data.matrix %>%
    kNNdist(k = 1) %>% 
    as.numeric 
kNN_histogram(ground.dist.nn, xlab = "Distance to nearest-neighbour")

ground.buff <- 0.549
test.copy   <- thin_canopy(test, ground.buff)


dplyr::filter(test.copy, Type != 2) %>%
    saveRDS(file = canopyRD)



# Compare heatmaps
jpeg(filename = "canopy_before_filter.jpeg")
test %>% 
    dplyr::filter(X < 479650, Y > 7124550) %>% 
    with(heatscatter(X, Y))
dev.off()

jpeg(filename = "canopy_after_filter.jpeg")
test.copy %>% 
    dplyr::filter(X < 479650, Y > 7124550) %>% 
    with(heatscatter(X, Y))
dev.off()


# examine the number of points in each cluster
dplyr::filter(test.copy, X < 479650, Y > 7124550) %>%
    with(plot(X, Y))

test.copy %>% 
    dplyr::filter(Type == 4, X < 479650, Y > 7124550) %>% 
    with(symbols(x = X, y = Y, circles = rep(2*ground.buff, length(X)), 
                 inches = FALSE, fg = "red", add = TRUE))


