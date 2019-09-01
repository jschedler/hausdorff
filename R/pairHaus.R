#' Pairwise Hausdorff distances
#' 
#'  This function takes a SpatialPolygonsDataFrame object 
#'  
#'@param shp a SpatialPolygonsDataFrame object.
#'  
#'@return 
#'  
#'@export
#'
#'
#'  n = nrow(shp@data)
pairHaus <- function(shp, vals = class(shp) == "SpatialPolygonsDataFrame", f1, f2 = f1, col){
  n = nrow(shp@data)
  combs = combn(1:n,2)
  if(vals){
    haus.dists.vals <- matrix(nrow = ncol(combs), ncol = 3)
    for(i in 1:ncol(combs)){
      haus.dists.vals[i,] <- c(extHaus(shp[combs[,i][1],], shp[combs[,i][2],],f1,f2),
                               shp@data[combs[,i][1],col], shp@data[combs[,i][2],col])
    
    }
    return(haus.dists.vals)
  }
  else{
    haus.dists <- matrix(nrow = n, ncol = n)
    for(i in 1:ncol(combs)){
    haus.dists[combs[,i][1], combs[,i][2]] <- extHaus(shp[combs[,i][1],], 
                                                      shp[combs[,i][2],],f1,f2)
    }
    return(haus.dists)
    
  }
}
