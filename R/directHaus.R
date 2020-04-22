#' Calculate the directional extended Hausdorff Distance
#' 
#'  This function takes a SpatialPolygons object 
#'  
#'@author Julia Schedler
#'
#'@param A,B region calculate the extended Hausdorff distance- SpatialPolygons or SpatialPolygonsDataFrame
#'@param f1 the percentage of area in B you want captured as a decimal (eg 10\% = .1)
#'@param f2 the percentage of area in A you want captured as a decimal (eg 10\% = .1); defaults to the value of f1
#'@param tol tolerance for selecting the epsilon buffer to yield desired f1. Default is 1/10000th the sampled directional distances.
#'@return The directional extended hausdorff distance from A to B
#'
#'@seealso \code{\link{extHaus}}, \code{\link{hausMat}}
#'
#'@importFrom rgeos gIntersection gDistance
#'@importFrom raster buffer
#'@importFrom stats sd quantile  
#'@importFrom methods slot
#'@importFrom sp SpatialPoints CRS proj4string spsample
#'
#'@export
#'
directHaus<- function(A, B, f1, f2=f1, tol = NULL){
  if(!is.projected(A) | !is.projected(B)){stop(paste("Spatial* object (inputs ", quote(A),", ", quote(B), ") must be projected. Try running ?spTransform().", sep = ""))}
  if(f1 ==1 || f2 == 1){stop("f = 1 equivalent to Hausdorff distancce; Use gDistance with hausdorff = T.")}
  #A.area = slot(A@polygons[[1]], "area")
  # generate points
  n = 10000
  a.coords <- spsample(A, n = n,type = "regular")
  ## points from A to B
    dists <- gDistance(a.coords, B, byid = T)

  if(is.null(tol)){
    tol = sd(dists[1,])/100
  }
  ## find desired quantile of distances
  eps <- as.numeric(quantile(dists[1,], f1))
  buff<- buffer(B, width = eps, dissolve = T)
  overlap.region = gIntersection(buff, A);
  overlap = slot(overlap.region@polygons[[1]], "area")/slot(A@polygons[[1]],"area");
  eps_diff = abs(f1-overlap)
  i = 2;k=1
  while(eps_diff>tol){
    if( abs(f1-overlap)<10^(-i) || k >10){i = i +1; k = 1}
    eps = eps*(1 + sign(f1-overlap)*10^(-i))
    
    buff<-buffer(B, width = eps, dissolve = T)
    overlap.region = gIntersection(buff, A);
    slot(overlap.region@polygons[[1]], "area")/slot(A@polygons[[1]], "area")
    overlap = slot(overlap.region@polygons[[1]], "area")/slot(A@polygons[[1]],"area");
    eps_diff = abs(f1-overlap);
    k = k+1
  }
  buff.coords = SpatialPoints(slot(overlap.region@polygons[[1]]@Polygons[[1]], "coords"), proj4string = CRS(proj4string(A)))
    eps =max(gDistance(buff.coords, B, byid = T))
    ## visualize how much area was captured?

    out <- list(eps);names(out) <- c("direct.haus")
    return(out)
}

