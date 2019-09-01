#' Create a variogram based on Hausdorff distances
#' 
#'  This function takes a SpatialPolygonsDataFrame object 
#'  
#'@param shp a SpatialPolygonsDataFrame object.
#'  
#'@return an nxn matrix of requested distances or inverse.
#'  
#'@export
#'
vgm_haus <- function(shp, col, f1, f2 = f1){
  ## get distances and squared differences for all pairs of observations
  n = nrow(shp@data)
  combs = combn(1:n,2)
  haus.dists.vals <- pairHaus(shp, col = col, f1=f1, f2=f2, vals = T)
  haus.dists.vals <- data.frame(haus.dists.vals)
  names(haus.dists.vals) <- c(paste("extHaus_", f1, sep = ""), "val_1", "val_2")
  haus.dists.vals$sq_diff <- (haus.dists.vals$val_1-haus.dists.vals$val_2)^2

  ## variogram binning
  ## bin cutoffs-- quantiles of distance distribution?
  bins <- n
}