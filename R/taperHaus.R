#' Tapering a Hausdorff spatial weight matrix
#' 
#'  This function takes a SpatialPolygons object 
#'  
#'@param shp a SpatialPolygons object.
#'@param k The number of nearest neighbors to retain. Defaults to NULL which will result in retaining all distances. 
#'@param f1 The percentage of region i to retain when calculating the directional Hausdorff distance from region i to region j.
#'@param f2 The percentage of region j to retain when calculating the directional Hausdorff distance from region j to i. Defaults to the value of f1. Note that specifying a different value will result in a non-symmetric matrix.
#'@param inverse Defaults to FALSE and returns requested distances directly. TRUE will return 1/distance. 
#'@param normalize Should the rows be normalized to sum to 1? Defaults to FALSE.
#'  
#'@return an nxn matrix of requested distances or inverse.
#'  
#'@export
taper_haus <- function(shp, k=NULL, f1, f2 = f1, inverse = FALSE, normalize = FALSE){
  n = nrow(shp@data)
  combs = combn(1:n,2)
  haus.dists <- matrix(nrow = n, ncol = n)
  if(!inverse){
    for(i in 1:ncol(combs)){
      haus.dists[combs[,i][1], combs[,i][2]] <- extHaus(shp[combs[,i][1],], shp[combs[,i][2],],f1,f2)
      #print(i)
      #print(i/3828)
    }
    ## k nearest neighbors
    if(!is.null(k)){
      for(i in 1:n){
        vals <- haus.dists[i,]
        to_zero <- order(vals, decreasing = F)[(k+1):n]
        vals[to_zero] <- 0
        haus.dists[i,] <- vals
      }
    }
  }else{
    for(i in 1:ncol(combs)){
      haus.dists[combs[,i][1], combs[,i][2]] <- 1/extHaus(shp[combs[,i][1],], shp[combs[,i][2],],f1,f2)
      #print(i)
      #print(i/3828)
    }
    ## k nearest neighbors
    if(!is.null(k)){
      for(i in 1:n){
        vals <- haus.dists[i,]
        to_zero <- order(vals, decreasing = T)[(k+1):n]
        vals[to_zero] <- 0
        haus.dists[i,] <- vals
      }
    }
  }
  ## covariance tapering approach
  ## create empirical variogram
  
  ## compute all pairs
  
  ## row normalize
  if(normalize){
    haus.dists <- apply(haus.dists, 1, function(x){x/sum(x)})
  }
  return(haus.dists)
}