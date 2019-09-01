#' Generate a Pixel intensity over a union of polygons
#' 
#'  This function takes a SpatialPolygons object 
#'  
#'@param shp a SpatialPolygons object.
#'@param n.sim the approximate number of points to sample to determine the dimensions of the pixel image
#'@param beta the ``true" value of the coefficient for the covariate which determines the intensity
#'@param surf object of class im. optional continuous covariate to include in model for intensity.
#'  
#'@return 
#'  
#'@export

polyIntensity <- function(shp, n.sim, beta = 0, surf=NULL,b1 = NULL, b2 = NULL, scale.factor = 1){
  ### 
  # generate pixel image using covariate
  ## smooth intensity surface generation
  ## sample regular points in owin, assign pixel values
  ## aggregate these to shp's
  if(!is.projected(shp)){warning("Data are unprojected.")}
  domain.shp <- gUnaryUnion(shp)
  shp.owin <- as(domain.shp, "owin")
  shp.owin.scaled <- rescale(shp.owin, scale.factor)
  pixels = spsample(domain.shp,n = n.sim, type = "regular")
  n.eff <- nrow(pixels@coords)
  domain.area <-spatstat::area(shp.owin)
  ## covariate value
  if(is.null(surf)){
    z <- matrix(rnorm(n.eff^2, mean = 10, sd = 1), nrow = n.eff) ## will need to change this to be something more meaningful; e.g. not random normal
    ## scale covariate by total area of domain
    z.scaled <- beta*z/domain.area
  }else{
    coords.combs <- expand.grid(pixels@coords[,1], pixels@coords[,2])
    z <- matrix(surf(coords.combs[,1], coords.combs[,2],b1,b2), nrow = n.eff, ncol = n.eff)
    z.scaled <- beta*z/domain.area
  }
  

    intensity.image <- list(coordinates(pixels)[,1]/scale.factor, ## image is getting cut off???
                            coordinates(pixels)[,2]/scale.factor, 
                            z.scaled); names(intensity.image) <- c("x", "y", "z")
  ##fixing plot order-- don't need this now but saving in case
  #out = slot(domain.shp, "polygons")
  #out2 = lapply(slot(out[[1]], "Polygons"), 
   #             function(x){y <- data.frame(slot(x, "coords")); y <- y[nrow(y):1,]; return(y)})
  #domain.shp.owin <- owin(poly = out2)
  #shp.intensity <- as.im(intensity.image, W = domain.shp.owin) ## runs faster when there are fewer pixels (when n.eff is smaller)
  shp.intensity <- as.im(intensity.image, W = as.polygonal(shp.owin.scaled))
  return(shp.intensity)
}  
