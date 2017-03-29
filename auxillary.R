## Auxillary functions to dbscan test

# Remove points which do not identify a tree
thin_canopy <- function(point.cloud, buffer){
    idxStem    <- point.cloud %>% with(which(Type == 4))
    idxGround  <- point.cloud %>% with(which(Type == 2))
    points.nn  <- select(point.cloud, X, Y) %>% frNN(eps = buffer, sort = FALSE)
    stem.nn    <- points.nn$id[idxStem]
    idxStem    <- unlist(stem.nn) %>% c(idxStem, .) %>% unique
    ground.nn  <- points.nn$id[idxGround]
    idx.remove <- ground.nn %>% map_lgl(match_cpp, idxStem) %>% `!` %>% which
    ground.nn[idx.remove] %>% unlist %>% unique %>% `-` %>% point.cloud[., ]
}


# Histogram for setting dbscan parameters
kNN_histogram <- function(x, nBins = 16, xlab = ""){
    
    maxN  <- max(x)
    bWth  <- maxN / nBins
    lbls  <- cut(seq(0, maxN, 1), seq(0, maxN, bWth), right = FALSE) %>% levels
    ggplot(data.frame(X = x), aes(x = X)) +
        geom_histogram(colour = "black", fill = "grey", bins = nBins) +
        scale_x_continuous(labels = lbls, breaks = seq(0, maxN-bWth, bWth)) + 
        theme_classic() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
        labs(x = xlab, y = "Count")
}



# Plot the section of the point cloud around a specified point
plot_canopy <- function(cloud, x, y, buffer){
    
    cloud.copy <- dplyr::filter(cloud, X > (x - buffer), X < (x + buffer), 
                                Y > (y - buffer), Y < (y + buffer))
    
    with(cloud.copy, heatscatter(X, Y))
    points(x, y, pch = 17, col = "black")
    
    return(NULL)
}


# ggplot implementation of plot_canopy
plot_canopy_gg <- function(cloud, x, y, buffer){
    
    cloud.copy <- dplyr::filter(cloud, X > (x - buffer), X < (x + buffer), 
                                Y > (y - buffer), Y < (y + buffer))
    
    ggplot(cloud.copy, aes(x = X, y = Y)) + 
        geom_point(aes(colour = as.factor(Type))) + 
        geom_point(data = NULL, x = x, y = y, shape = 17, size = 4) + 
        theme_classic() +
        theme(legend.position = "bottom")
}
    



# Find the index of the nearest point in a table, to some value.
find_nearest <- function(value, table){
    
    dx <- value$X - table$X
    dy <- value$Y - table$Y
    
    which.min(sqrt(dx^2 + dy^2))
}


