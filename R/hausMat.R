#' Creating a matrix of (extended) Hausdorff distances
#' 
#'  This function takes a SpatialPolygons object 
#'  
#'@param shp a SpatialPolygons object.
#'@param f1 The percentage (as a decimal) of region i to retain when calculating the directional Hausdorff distance from region i to region j.
#'@param f2 The percentage (as a decimal) of region j to retain when calculating the directional Hausdorff distance from region j to i. Defaults to the value of f1. Note that specifying a different value will result in a non-symmetric matrix.
#'@param fileout Should the resulting weight matrix be written to file? Defaults to FALSE 
#'@param filename If \code{fileout} is TRUE, the name for the file to be outputted.
#'@param ncores If \code{do.parallel} is true, the number of cores to be used for parallel computation. Default is 1. 
#'@param timer If T, records \code{Sys.time()} when the function is called and outputs the elapsed time along with the matrix. Default is F.
#'@param do.parallel If T, uses the number of cores specified using \code{ncores} to set up a cluster with the \code{foreach} package. 
#'  
#'@return an nxn matrix of requested distances.
#'
#'@seealso \code{\link{extHaus}}  
#' 
#' @importFrom parallel makePSOCKcluster stopCluster makeCluster
#' @importFrom foreach foreach %dopar%
#' @importFrom doParallel registerDoParallel
#' @importFrom utils combn
#' 
#'@export
#'
hausMat <- function(shp, f1, f2 = f1, fileout = FALSE, filename = NULL, ncores = 1, timer = F, do.parallel = T){
  if(timer){start <- Sys.time()}
  n = nrow(shp@data)
  combs = combn(1:n,2)
  n.combs = ncol(combs);
  haus.dists <- matrix(0,nrow = n, ncol = n)
  if(do.parallel){
    
    #out <- matrix(-1, nrow = 20, ncol = 20)
    start <- Sys.time()
    print("Setting up parallelization")
    cl <- makeCluster(ncores)
    registerDoParallel(cl)
    print("Computing...")
    out = foreach (i = 1:n.combs, .packages = c("rgeos", "sp", "raster", "hausdorff"),.combine = rbind, .export = "directHaus") %dopar% {
          
          extHaus(shp[combs[1,i],], shp[combs[2,i],], f1 = f1)
    }
    print("Completion time:")
    print(Sys.time()-start)
    #save(out, file = "hausdorff_columbus")
    stopCluster(cl)
    #closeAllConnections()  
  haus.dists[lower.tri(haus.dists, diag = F)] =  out
  haus.dists = haus.dists + t(haus.dists)
  }
  else{
    for(i in 1:n.combs){
      haus.dists[combs[1,i], combs[2,i]] <- extHaus(shp[combs[1,i],], shp[combs[2,i],], f1 = f1)
    }
    haus.dists = haus.dists + t(haus.dists)
  }
  ## add 0's to diagonal
  #if(fileout){save(haus.dists, file =filename)}
  return(haus.dists)
}
