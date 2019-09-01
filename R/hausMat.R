#' Creating a Hausdorff spatial weight matrix
#' 
#'  This function takes a SpatialPolygons object 
#'  
#'@param shp a SpatialPolygons object.
#'@param f1 The percentage (as a decimal) of region i to retain when calculating the directional Hausdorff distance from region i to region j.
#'@param f2 The percentage (as a decimal) of region j to retain when calculating the directional Hausdorff distance from region j to i. Defaults to the value of f1. Note that specifying a different value will result in a non-symmetric matrix.
#'@param fileout Should the resulting weight matrix be written to file? Defaults to FALSE 
#'@param filename If \code{fileout} is TRUE, the name for the file to be outputted.
#'  
#'@return an nxn matrix of requested distances.
#'  
#'@export
hausMat <- function(shp, f1, f2 = f1, fileout = FALSE, filename = NULL){
  n = nrow(shp@data)
  combs = combn(1:n,2)
  n.combs = ncol(combs)
  haus.dists <- matrix(nrow = n, ncol = n)
  for(i in 1:ncol(combs)){

    if(i %in% round(quantile(1:n.combs, probs = seq(0.1, 1, 0.1)))){print(paste(names(out)[i == out], "complete"))}

  }
  
  ## add 0's to diagonal
  diag(haus.dists) <- 0
  if(fileout){save(haus.dists, file =filename)}
  return(haus.dists)
}
