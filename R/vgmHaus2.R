#' Creating a Variogram from Hausdorff Distances
#' 
#'  This function takes a SpatialPolygons object 
#'  
#'@param shp a SpatialPolygons object.
#'@param ... arguments to be passed to \code{\link{hausMat}}
#'@param hausMat a weight matrix outputted by \code{\link{hausMat}}
#'@param cut "bbox" or "max" method of specifying distance cutoff
#'@param bbox if no shp is specified, the bbox of your shapefile
#'@param z variable for which to compute variogram
#'@param model parametric form of the variogram model to be estimated; defaults to "Exp".
#'  
#'@return an empirical variogram with estimated parameters.  
#'  
#'@export
vgmHaus2 <- function(shp, hausMat=NA, cut = "max", nbins = 15, 
                    vg.model = "Exp", outcome, ...){
  #if(is.na(hausMat)){}
  #convert to vector (is this most efficient, prob not)
  dists =as.numeric(hausdists_mat)
  #shp.p <- SpatialPointsDataFrame(meuse[,1:2], data = meuse[,3:14])
  
  
  if(cut == "bbox"){
    ## calculate diagonal of bounding box
    bbox.dist = dist(t(bbox(shp)))
    dist.cut = bbox.dist/3
  }else if(cut == "max"){
    ## calculate the maximum distance
    max.dist = max(dists)
    dist.cut = max.dist/2
  }

  
  ## subset dists to only ones less than cutoff
  dists = subset(dists, dists<= dist.cut & dists >0)

  ## if we use histogram bins we definitely have more than 30 obs per bin, start with that
  ## create indicator of bin
  ## start with smallest bin that has 30 points
  from =  dists[order(dists)[200]]
  bins = c(0,seq(from = from, to = dist.cut, length.out = nbins))
  #bins = seq(from = 0, to = dist.cut, length.out = nbins)
  #bins = c(quantile(dists, probs = seq(from = 0, to = 1, length.out = nbins)))
  dists.binned = cut(dists, bins, include.lowest = T)
  
  ## identify which bin each pair falls into and compute variance (assuming constant mean)
  ## doing it for counts
    z = outcome
    vg = rep(NA, times = length(bins)-1)
  tots = rep(NA, times = length(bins)-1)
  for(i in 1:(length(bins)-1)){
    lower = bins[i]
    upper = bins[i+1]
    out.temp = apply(hausMat, 1, function(x){which(x<upper & x>=lower)})
    sum = 0
    tot = 0
    for(j in 1:length(out.temp)){
      try(if(i == 1){
        idx = out.temp[[j]]
        sum = sum +  sum((z[idx] - rep(z[j], times = length(idx))) ^2)
        tot = tot + length(out.temp[[j]]) -1
      }
      else{
        idx = c(j, out.temp[[j]])
        sum = sum +  sum((z[idx] - rep(z[j], times = length(idx))) ^2)
        tot = tot + length(out.temp[[j]])
      }
      )
      
    }
    vg[i] = (1/(2*tot))*sum 
    tots[i] = tot
  }
  vg = data.frame(vg)
  row.names(vg) = levels(dists.binned)
  
  ### estimate the range, sill, nugget
  # can do this by simply replacing the values in another vgm object.
  shp.p <- SpatialPoints(shp)
  
  vg.temp <- variogram(z~1, shp.p)
  ## replace with HD
  vg.hd <- vg.temp[1:length(tots),]
  vg.hd$np <- tots
  vg.hd$dist <- bins[-1]
  vg.hd$gamma <- vg$vg
  vg.hd$dir.hor <- vg.hd$dir.ver <-  0
  vg.hd$id <- "var1"
  plot(vg.hd)
  ## estimate parameters
  vgm.model = vgm(model = vg.model,nugget = vg$vg[1])
  #vg.fit = fit.variogram(vg.hd, model = vgm.model)
  tryCatch({vg.fit = fit.variogram(vg.hd, model = vgm.model)},
           warning = function(w){vg.fit <<- F})  #vg.fit = NULL
  ##now that we have the range parameter we can use it to sparsify the 
  ## distance matrix
  #range <- vg.fit$range[2]
  #sum(dists<range)/length(dists)
  
  #str(haus.dists)
  #haus.dists.temp = haus.dists
  #haus.dists.temp[haus.dists>range] = 0
  #num_neigh = apply(haus.dists.temp, 1, function(x){sum(x!=0)})
  return(list(vg.hd, vg.fit))
}
