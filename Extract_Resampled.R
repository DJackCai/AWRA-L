#### Most important script: subset the rectangular grid to the catchment shapefile,
#### then extract the forcing data,  ancillary static parameter data & satellite data from the respective directory

library(raster)
library(tidyverse)
library(sf)
path2AWAP = '/g/data/fj8/BoM/AWRA/DATA/CLIMATE/'
PROJ_LATLON = '+proj=longlat +datum=WGS84'
storepath<-"/scratch/os22/dc0105/AWRA/forcings/"
source("/scratch/os22/dc0105/AWRA/codes/funcs_shp_grids.R")

shp.bound = shapefile("/scratch/os22/dc0105/AWRA/codes/ANU-RUNOFF-DA_catchment_boundaries.shp")
proj4string(shp.bound) = CRS(PROJ_LATLON)

shp.bound = shapefile("ANU-RUNOFF-DA_catchment_boundaries.shp")
### Catchment information: IDs and grids 

# group_catch_id = c("121002","136203", "130319", "422319", "416305", "422338", "206014",
#                    "110003", "229650", "402206",  "926002")
# 
# grid_121002 = as.data.frame(expand.grid(seq(147.75,147.95,0.05),seq(-20.25,-19.90,0.05)))
# 
# grid_136203 = as.data.frame(expand.grid(seq(151.55,151.80,0.05),seq(-26.90,-26.75,0.05)))
# 
# grid_130319 = as.data.frame(expand.grid(seq(150.50,150.80,0.05),seq(-24.25,-24.05,0.05)))
# 
# grid_422319 = as.data.frame(expand.grid(seq(152.00,152.40,0.05),seq(-28.05,-27.95,0.05)))
# 
# grid_416305 = as.data.frame(expand.grid(seq(150.95,151.25,0.05),seq(-28.75,-28.55,0.05)))
# 
# grid_422338 = as.data.frame(expand.grid(seq(151.40,151.65,0.05),seq(-28.30,-28.00,0.05)))
# 
# grid_206014 = as.data.frame(expand.grid(seq(151.75,152.05,0.05),seq(-30.50,-30.20,0.05)))
# 
# grid_110003 = as.data.frame(expand.grid(seq(145.40,145.65,0.05),seq(-17.45,-17.25,0.05)))
# 
# grid_229650 = as.data.frame(expand.grid(seq(145.90,146.05,0.05),seq(-37.80,-37.70,0.05)))
# 
# grid_402206 = as.data.frame(expand.grid(seq(146.90, 147.10,0.05),seq(-36.70,-36.50,0.05)))
# 
# # grid_403205 = as.data.frame(expand.grid(seq(146.90, 147.15,0.05),seq(-37.00,-36.70,0.05)))
# 
# grid_926002 = as.data.frame(expand.grid(seq(142.40, 142.70,0.05),seq(-11.90,-11.70,0.05)))
# grid_list = list(grid_121002,  grid_136203, grid_130319, grid_422319, grid_416305,
#                  grid_422338,  grid_206014, grid_110003, grid_229650, grid_402206,grid_926002)


group_catch_id = c("405227", "405218", "237200", "497", "226222", "206025")

grid_405227 = as.data.frame(expand.grid(seq(145.85, 146.20,0.05),seq(-37.65,-37.30,0.05)))
grid_405218 = as.data.frame(expand.grid(seq(146.20, 146.60,0.05),seq(-37.45,-37.20,0.05)))
grid_237200 = as.data.frame(expand.grid(seq(142.15, 142.40,0.05),seq(-38.35,-37.90,0.05)))
grid_229661 = as.data.frame(expand.grid(seq(145.90, 146.00,0.05),seq(-37.65,-37.55,0.05)))
grid_226222 = as.data.frame(expand.grid(seq(145.75, 145.90,0.05),seq(-37.90,-37.80,0.05)))
grid_206025 = as.data.frame(expand.grid(seq(151.40, 151.75,0.05),seq(-30.90,-30.35,0.05)))
grid_403205 = as.data.frame(expand.grid(seq(146.90, 147.15,0.05),seq(-37.00,-36.65,0.05)))
grid_497 = as.data.frame(expand.grid(seq(146.15, 146.40,0.05),seq(-42.05,-41.90,0.05)))

grid_list = list(grid_405227, grid_405218, grid_237200,
                 grid_497, grid_226222, grid_206025)

N_catch = length(grid_list)
inter_ID_list = sapply(1:N_catch, function(x) NULL)
spatialgrid_list = sapply(1:N_catch, function(x) NULL)

source('funcs_shp_grids.R')
for (i in 1:length(grid_list)) {
  catid = group_catch_id[i]
  grid_catch = grid_list[[i]]
  grid_shp = shp.bound[which(shp.bound$siteid == as.character(catid)),]
  proj4string(grid_shp) = CRS(PROJ_LATLON)
  # subset the points
  grid_inter_out = shp_area_prop(points_coords = grid_catch, 
                                 shp_data = grid_shp, thres = 0.1)
  grid_catch_subset = grid_catch[grid_inter_out$finalID,]  
  
  inter_ID_list[[i]] = grid_inter_out$finalID
  spatialgrid_list[[i]] = SpatialPoints(grid_catch_subset, proj4string=CRS(PROJ_LATLON))
  
}


###### Module 2: Extract the forcings based on the obtained spatial grids ###### 

# dates_of_forcing = seq(as.Date('1992-01-01'),as.Date('2014-12-31'),1)


VARs = c('rain_day',"temp_max_day","temp_min_day","solar_exposure_day","wind" )
DATA = c()

for (num in 1:length(spatialgrid_list)) {
  # collate data for the specific catchment 
  catname = group_catch_id[num]
  grid_catch_sp = spatialgrid_list[[num]]  # spatial grid for the specific catchment
  
  dates_of_forcing = seq(as.Date('1992-01-01'),as.Date('2014-12-31'),1)
  years = unique(format(dates_of_forcing,'%Y')) 
  
  for (i in 1:length(VARs)){
    
    outfile = paste0(storepath,catname,VARs[i],'_resampled_19922014.csv')
    
    # Loop through files and extract the data from the nc. file
    for (k in 1:length(years)) {
      if (k == 1) {
        x = brick(paste0(path2AWAP,VARs[i],'/',VARs[i],'_',years[k],'.nc'))
        DATA = t(as.matrix(raster::extract(x,grid_catch_sp)))
      } else {
        x = brick(paste0(path2AWAP,VARs[i],'/',VARs[i],'_',years[k],'.nc'))
        DATA = rbind(DATA,t(as.matrix(raster::extract(x,grid_catch_sp))))
      }
    } 
    # store data by columns of locations _ grid_catch (a table)
    
    NGRID = ncol(DATA)  
    
    write.table(cbind(format(dates_of_forcing,'%Y'),format(dates_of_forcing,'%m'),format(dates_of_forcing,'%d'),DATA),
                file=outfile,row.names=FALSE,
                col.names=c('YYYY','MM','DD',paste0("grid",1:NGRID)),quote=FALSE,sep=',')
    rm(DATA)
  }
  print(paste0("-----------AWAP data for ",group_catch_id[num], " has finished loading --------"))
  
  ### Static parameters ####
  
  fhru_r = raster("/scratch/os22/dc0105/AWRA/ftree_h5.tif")
  fhru_data = t(as.matrix(raster::extract(fhru_r,grid_catch_sp)))
  PET_r =  raster("/scratch/os22/dc0105/AWRA/AWRA_meanPET.tif")
  PET_data = t(as.matrix(raster::extract(PET_r,grid_catch_sp)))
  
  path_Smax = "/g/data/fj4/awra-cms/ancillary/awral_version6_soil_layer_max.nc"
  x_s0max = brick(path_Smax, varname = "s0max")
  x_ssmax = brick(path_Smax, varname = "ssmax")
  x_sdmax = brick(path_Smax, varname = "sdmax")
  s0max_data = t(as.matrix(raster::extract(x_s0max,grid_catch_sp)))
  ssmax_data = t(as.matrix(raster::extract(x_ssmax,grid_catch_sp)))
  sdmax_data = t(as.matrix(raster::extract(x_sdmax,grid_catch_sp)))
  
  static_par = rbind(fhru_data, s0max_data, ssmax_data, sdmax_data, PET_data)
  NGRID = ncol(static_par)  
  static_file = paste0(storepath,catname,'_resampled_static.csv')
  write.table(static_par,file = static_file,row.names=F,
              col.names=paste0("grid",1:NGRID),quote=FALSE,sep=',')
  
  print(paste0("-----------Static parameter for ",group_catch_id[num], " has finished loading --------"))
  
  ### SMAP ####
  
  dates_of_interest = seq(as.Date('2015-01-01'),as.Date('2019-12-31'),1)
  path2SMAP = '/g/data/fj4/SatelliteSoilMoistureProducts/SMAP/AUS_annual/SMAP_0.05deg_Australia_'
  years = unique(format(dates_of_interest,'%Y'))
  
  SMAP_DATA = c()
  for (k in 1:length(years)){
    if (k == 1) {
      x_smap = brick(paste0(path2SMAP,years[k],'.nc'))
      SMAP_DATA = t(as.matrix(raster::extract(x_smap,grid_catch_sp)))
    } else {
      x_smap = brick(paste0(path2SMAP,years[k],'.nc'))
      SMAP_DATA = rbind(SMAP_DATA,t(as.matrix(raster::extract(x_smap,grid_catch_sp))))
      # print(paste0("SMAP Data for ", years[k], " is finished"))
    }
  }
  NGRID = ncol(SMAP_DATA)
  write.table(cbind(format(dates_of_interest,'%Y'),format(dates_of_interest,'%m'),format(dates_of_interest,'%d'),SMAP_DATA),
              file=paste0(storepath, catname,"_resampled_SMAP1519",'.csv'),row.names=FALSE,col.names=c('YYYY','MM','DD',paste0("grid",1:NGRID)),
              quote=FALSE,sep=',')
  
  print(paste0("SMAP Data for ", group_catch_id[num], " has finished loading"))
  ## LAI
  
  path2MODISLAI = "/g/data/fj4/MODIS_LAI/AU/nc/"
  dates_of_interest = seq(as.Date('2015-01-01'),as.Date('2018-12-31'),1)
  # year_points = format(dates_of_interest,'%Y') 
  years = unique(format(dates_of_interest,'%Y')) 
  
  LAI_data  =c()
  for (k in 1:length(years)) {
    if (k==1) {
      x = brick(paste0(path2MODISLAI,"MOD15A2H.",years[k],"_AU_AWRAgrd.nc"))
      LAI_data = t(as.matrix(raster::extract(x,grid_catch_sp)))
    } else {
      x = brick(paste0(path2MODISLAI,"MOD15A2H.",years[k],"_AU_AWRAgrd.nc"))
      LAI_data = rbind(LAI_data, t(as.matrix(raster::extract(x,grid_catch_sp))))
      
    }
    NGRID = ncol(LAI_data)  
    LAI_8day_file = paste0(storepath, group_catch_id[num], "_resampled_MOD15A2H_8day1518.csv")
    write.table(LAI_data,file = LAI_8day_file,row.names=F,
                col.names=paste0("grid",1:NGRID),quote=FALSE,sep=',')
    
  }
  print(paste0("----------- LAI for ",group_catch_id[num], " has finished loading --------"))
  
}

#### 2015-18 forcings #####

VARs = c('rain_day',"temp_max_day","temp_min_day","solar_exposure_day","wind" )
DATA = c()

for (num in 1:length(spatialgrid_list)) {
  # collate data for the specific catchment 
  catname = group_catch_id[num]
  grid_catch_sp = spatialgrid_list[[num]]  # spatial grid for the specific catchment
  
  dates_of_forcing = seq(as.Date('2013-01-01'),as.Date('2021-12-31'),1)
  years = unique(format(dates_of_forcing,'%Y')) 
  
  for (i in 1:length(VARs)){
    
    outfile = paste0(storepath,catname,VARs[i],'_resampled_201321.csv')
    
    # Loop through files and extract the data from the nc. file
    for (k in 1:length(years)) {
      if (k == 1) {
        x = brick(paste0(path2AWAP,VARs[i],'/',VARs[i],'_',years[k],'.nc'))
        DATA = t(as.matrix(raster::extract(x,grid_catch_sp)))
      } else {
        x = brick(paste0(path2AWAP,VARs[i],'/',VARs[i],'_',years[k],'.nc'))
        DATA = rbind(DATA,t(as.matrix(raster::extract(x,grid_catch_sp))))
      }
    } 
    # store data by columns of locations _ grid_catch (a table)
    
    NGRID = ncol(DATA)  
    
    write.table(cbind(format(dates_of_forcing,'%Y'),format(dates_of_forcing,'%m'),format(dates_of_forcing,'%d'),DATA),
                file=outfile,row.names=FALSE,
                col.names=c('YYYY','MM','DD',paste0("grid",1:NGRID)),quote=FALSE,sep=',')
    rm(DATA)
  }
  print(paste0("-----------AWAP data for ",group_catch_id[num], " has finished loading --------"))
  
}

