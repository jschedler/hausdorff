directHaus<- function(A, B, f1, f2, overlap = F){
n = 10000
a.coords <- spsample(A, n = n, type = "regular")
a.coords_list = list()
for(i in 1:nrow(a.coords@coords)){a.coords_list = c(a.coords_list, a.coords[i,])}
cores=parallel::detectCores()
cl <- parallel::makeCluster(cores[1]-1) #not to overload your computer
doParallel::registerDoParallel(cl)
dists <- unlist(parallel::parLapply(cl, a.coords_list, gDistance, B))
parallel::stopCluster(cl)
eps <- quantile(dists, f1);
if (overlap) {
  buff.A = buffer(A, width = eps*2, dissolve = T)
  B.interesct = gIntersection(buff.A, B)
  buff <- buffer(B.interesct, width = eps, dissolve = T)
  overlap.region = gIntersection(buff, A)
  slot(overlap.region@polygons[[1]], "area")/slot(A@polygons[[1]], 
                                                  "area")
  overlap = slot(overlap.region@polygons[[1]], "area")/slot(A@polygons[[1]], 
                                                            "area")
  out <- list(eps, overlap.region, overlap)
  names(out) <- c("epsilon", "overlap.region", "overlap")
  return(out)
}
else {
  out <- list(eps)
  names(out) <- c("direct.haus")
  return(out)
}
}
