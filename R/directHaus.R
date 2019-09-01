#' Calculate the directional extended Hausdorff Distance
#' 
#'  This function takes a SpatialPolygons object 
#'  
#'@author Julia Schedler
#'
#'@param A,B region calculate the extended Hausdorff distance- SpatialPolygons or SpatialPolygonsDataFrame
#'@param f1 the percentage of area in B you want captured as a decimal (eg 10\% = .1)
#'@param f2 the percentage of area in A you want captured as a decimal (eg 10\% = .1)
#'@param overlap return the portion of A used to calculate the directional Hausdorff distance? Defaults to FALSE  
#'
#'@return The directional extended hausdorff distance A to B
#'@export
directHaus<- function(A, B, f1, f2, overlap = F){

  # generate points
  n = 10000
  a.coords <- spsample(A, n = n,type = "regular")
  ## points from A to B
  dists <- gDistance(a.coords, B, byid = T)
  ## find desired quantile of distances
  eps <- as.numeric(quantile(dists[1,], f1))
  if(overlap){
    ## visualize how much area was captured?
    buff<-buffer(B, width = eps, dissolve = T)
    overlap.region = gIntersection(buff, A);
    slot(overlap.region@polygons[[1]], "area")/slot(A@polygons[[1]], "area")
    overlap = slot(overlap.region@polygons[[1]], "area")/slot(A@polygons[[1]],"area")
    out <- list(eps, overlap.region, overlap);names(out) <- c("epsilon", "overlap.region", "overlap")
    return(out)
  }else{
    out <- list(eps);names(out) <- c("direct.haus")
    return(out)
  }
  
}

