#' Create a K nearest neighbors matrix from a Hausdorff nearest neighbhor matrix
#' 
#' 
#'  This function takes a matrix of Hausdorff distances and returns a K-nearest neighbhors matrix 
#'  
#'@param mat matrix of (extended) Hausdorff distances
#'@param shp SpatialPolygons* object for the Hausdorff distances 
#'@param k the desired number of neighbors for each region
#'@param sym should the matrix be symmetrized? (can result in differing k for each region).
#'@param wt default "equal" (1 for neighbor, 0 for non-neighbor), can also be "inv" for inverse distance. 
#'@param dist.file The file location of a weight matrix to be used to generate the knn matrix. dist.file would have been saved using \code{save(matrix, dist.file)}. 
#'@param sub.set List of regions for which to compute distances. Defaults to all regions.
#'   
#'@return a listw object with specified properties
#'
#'@seealso \code{\link{extHaus}}  
#' 
#' @importFrom spdep knearneigh knn2nb nb2listw
#' @importFrom raster coordinates
#' @importFrom utils combn
#' 
#'@export
#'
knnHaus <- function(mat = NULL, shp, k, sym = T, wt = "equal", dist.file = NULL, sub.set = NULL){
  if(is.null(mat)&is.null(dist.file)){stop("You must specify a distance matrix or a file location of a distance matrix")}
  coords <- coordinates(shp)
  if(!is.null(dist.file)){
    obj <- load(dist.file)
    hausdists_mat <- get(obj)
  }else{
   hausdists_mat <- mat
  }
  if(is.null(sub.set)){
    sub.set = 1:nrow(hausdists_mat)
  }
  coords <- coords[sub.set,]
  w_pre <- knearneigh(coords, k=k)
  w_pre$nn <- t(apply(hausdists_mat[sub.set,sub.set], 1, function(x){order(x)[2:(k+1)]}))
  if(wt == "W"){
    W_haus <- nb2listw(knn2nb(w_pre, sym = T), style = "W");
    #W_haus$wts <- hausdists_mat[]
  }else{
    W_haus <- nb2listw(knn2nb(w_pre, sym = T), style = "B");
  }
   return(W_haus)
}