#' Extended Hausdorff Distance
#'
#' Calculates the extended Hausdorff distance from A to B for given quantiles and precision
#' 
#'@author Julia Schedler
#'
#'@param A,B region calculate the extended Hausdorff distance- SpatialPolygons or SpatialPolygonsDataFrame
#'@param f1 the percentage of area in B you want captured as a decimal (eg 10\% = .1)
#'@param f2 the percentage of area in A you want captured as a decimal (eg 10\% = .1)
#'@param pres the number of decimal places/precision  
#'
#'@return haus.dist: the extended hausdorff distance (max of directional from A to B and B to A)
#'@export
extHaus <- function(A, B, f1, f2=f1, pres = 3){
  if(is.null(gDifference(A,B))){return(0)} ## check if two regions are the same, if so return HD of zero.
  
  if(f1 == 1){
    A_to_B <- gDistance(A,B, hausdorff = T);
  }else if(f1 == 0){
    A_to_B <- gDistance(A,B)

  }else{
    A_to_B <- directHaus(A,B, f1, pres)$direct.haus[1]

  }
  if(f2 == 1){
    B_to_A <- gDistance(A,B, hausdorff = T)

  }else if(f2 == 0){
    B_to_A <- gDistance(A,B)
  }else{
    B_to_A <- directHaus(B,A, f2, pres)$direct.haus[1]

  }
  haus.dist = max(A_to_B, B_to_A)
  return(haus.dist)
}


