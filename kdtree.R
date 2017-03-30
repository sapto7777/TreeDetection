## kd-tree again:
##
## Function should recursively partition the data into a binary tree and log the median position
## for each node. A terminal node is generated when there are only two data points remaining.
##



kdtree <- function(data, id = 1:nrow(data), axis = 1){
    
    if(!is.matrix(data))
        stop("`data`` must be a matrix!")
    
    
    # Calculate the number of data points in the current node; as well as the dimension
    # of the space (assume all data points are of the same dimension as the first point).
    # ensure that the axis, in which the data will be partitioned, does not exceed the dimension
    # of the space (wrap it around if it does).
    nObs  <- nrow(data)
    depth <- ncol(data)
    if(axis > depth){
        axis <- (axis %% depth)
    }
    
    
    # Construct a node and order the data
    node <- list(child    = list(), 
                 id       = NULL,
                 centre   = NULL, 
                 terminal = FALSE,
                 axis     = axis)
    idx  <- order(data[, axis], decreasing = FALSE)
    
    
    # Partition data and create two child nodes, to be assigned to the current node. If
    # only two data points remain then a terminal node has been reached, assign the data to the
    # node.
    if(nObs == 1){
        node$centre      <- data[1, ]
        node$id          <- id
        node$child$count <- 0
    } else if(nObs == 2){
        node$centre      <- data[idx[1], ]
        node$id          <- id[idx[1]]
        node$child[[1]]  <- kdtree(data[idx[2], , drop = FALSE], id[idx[2]], axis = axis + 1)
        node$child$count <- 1
    } else {
        mid  <- ceiling(length(idx)/2)
        idx1 <- idx[1:(mid-1)]
        idx2 <- idx[(mid+1):nObs]
        
        node$id          <- id[idx[mid]]
        node$centre      <- data[idx[mid], ]
        node$child[[1]]  <- kdtree(data[idx1, , drop = FALSE], id[idx1], axis = axis + 1)
        node$child[[2]]  <- kdtree(data[idx2, , drop = FALSE], id[idx2], axis = axis + 1)
        node$child$count <- 2
    }
    
    return(node)
}


# retrieve closest point
kNN <- function(point, tree){
    
    depth  <- length(tree$centre)
    if(length(point) != depth)
        stop("`point` must have the same dimension as the data in the kdtree!")
    
    
    # recurse down the tree until you find the closest terminal node.
    # dx   <- dist_to_node(point, tree, axis)
    check <- point[tree$axis] > tree$centre[tree$axis]
    node  <- if(tree$child$count == 2){
        idx <- as.integer(check) + 1
        kNN(point, tree$child[[idx]])
    } else if(tree$child$count == 1 && check) {
        kNN(point, tree$child[[1]])
    } else {
        list(centre = tree$centre,
             id     = tree$id,
             child  = NULL)
    }
    
    # If the current node (tree) is closer to the point than the closest
    # terminal node (node) then return tree.
    # centre.dist <- sqrt(sum((point - tree$centre)^2))
    node$dist    <- sqrt(sum((point - node$centre)^2))
    current.dist <- sqrt(sum((point - tree$centre)^2))
    if(current.dist < node$dist){
        node <- list(centre = tree$centre,
                     id     = tree$id,
                     child  = NULL,
                     dist   = current.dist)
    }
    
    return(node)
}




# ### Test (each node should self-match):
# set.seed(117)
# x      <- rnorm(10 * 3)
# Data   <- matrix(x, nrow = 10, ncol = 3)
# tree   <- kdtree(Data)
# idxNN2 <- apply(Data, 1, function(x) kNN(x, tree)$id)
# identical(idxNN2, 1:length(idxNN2))
# 
# new.point    <- rnorm(3)
# predicted.nn <- kNN(new.point, tree)$id
# actual.nn    <- as.matrix(dist(rbind(Data, new.point)))["new.point", ]
# actual.nn    <- which.min(as.numeric(actual.nn[actual.nn > 0]))
# identical(actual.nn, predicted.nn)








































