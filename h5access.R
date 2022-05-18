install.packages("BiocManager")
library(BiocManager)
BiocManager::install("rhdf5")
library(rhdf5)

PROJ_LATLON = '+proj=longlat +datum=WGS84'

# view the structure 
h5ls("spatial_parameters_v6_5hru_CSIROgrids_SR_DR_IMP.h5")

> h5ls("spatial_parameters_v6_5hru_CSIROgrids_SR_DR_IMP.h5")
         group                   name       otype  dclass       dim
0            /             dimensions   H5I_GROUP                  
1  /dimensions hypsometric_percentile H5I_DATASET INTEGER        20
2  /dimensions               latitude H5I_DATASET   FLOAT       681
3  /dimensions              longitude H5I_DATASET   FLOAT       841
4            /             parameters   H5I_GROUP                  
5  /parameters                f_grass H5I_DATASET   FLOAT 841 x 681
6  /parameters           f_impervious H5I_DATASET   FLOAT 841 x 681
7  /parameters            f_irrigated H5I_DATASET   FLOAT 841 x 681
8  /parameters                 f_tree H5I_DATASET   FLOAT 841 x 681
9  /parameters                f_water H5I_DATASET   FLOAT 841 x 681
10 /parameters                f_zeros H5I_DATASET   FLOAT 841 x 681


### Read target attribute

ftree = h5read("spatial_parameters_v6_5hru_CSIROgrids_SR_DR_IMP.h5", "parameters/f_tree")
dim(ftree)

## Check ordering
lat = h5read("spatial_parameters_v6_5hru_CSIROgrids_SR_DR_IMP.h5", "dimensions/latitude")
lon = h5read("spatial_parameters_v6_5hru_CSIROgrids_SR_DR_IMP.h5", "dimensions/longitude")

> head(lat); tail(lat)
[1] -10.00 -10.05 -10.10 -10.15 -10.20 -10.25
[1] -43.75 -43.80 -43.85 -43.90 -43.95 -44.00

### We need to take transpose, because if not, then the resolution is incorrect (nrow becomes longitudes)

ftree_r_temp <- raster(ftree, crs =PROJ_LATLON, xmn=111.975,xmx=154.025,ymn=-44.025,ymx=-9.975)
> ftree_r_temp.  
class      : RasterLayer 
dimensions : 841, 681, 572721  (nrow, ncol, ncell)
resolution : 0.06174743, 0.04048751  (x, y) 

  # correct way.. ##
array_ftree <- as.matrix(t(ftree))
ftree_r <- raster(array_ftree, crs =PROJ_LATLON, xmn=111.975,xmx=154.025,ymn=-44.025,ymx=-9.975)
writeRaster(ftree_r,"ftree.tif")

#### GET THE CORRECT RESOLUTION!
> ftree_r
class      : RasterLayer 
dimensions : 681, 841, 572721  (nrow, ncol, ncell)
resolution : 0.05, 0.05  (x, y)


