## Convert Soil Moisture Active Passive (SMAP) retrievals into relative wetness (0-1) with respect to the dry and wet extremes 

MAP_wetness = function(SMAP_data, prob1 = 0.02, prob2= 0.98) {
  
  if (is.vector(SMAP_data)) {
    SMAP_wt = quantile(SMAP_data, prob = prob1, na.rm = T)
    SMAP_fc = quantile(SMAP_data, prob = prob2, na.rm = T)
    wetness = (SMAP_data-SMAP_wt)/(SMAP_fc-SMAP_wt) } else if (is.data.frame(SMAP_data) |is.matrix(SMAP_data)){
     
      wetness = apply(SMAP_data, 2, function(x) {
             SMAP_wt = quantile(x, prob=prob1, na.rm=T)
        SMAP_fc = quantile(x, prob=prob2, na.rm=T)
        return( (x-SMAP_wt)/(SMAP_fc-SMAP_wt))
      }) 
    }
  return(wetness)
} 
