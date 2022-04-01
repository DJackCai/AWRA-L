# From each 8-day LAI composite, identify those belonging to the month and take the mean
suppressWarnings(library(raster))
PROJ_LATLON = '+proj=longlat +datum=WGS84' # for spatial grid

for (num in 1:length(group_catch)) {
  # collate data for the specific catchment 
  catname = group_catch[num]
  grid_catch_sp = spatialgrid_list[[num]]  # spatial grid for the specific catchment
  
  path2LAI = "./LAI/"
  Months = seq(as.Date('2015-01-01'),as.Date('2019-12-31'),by='month')

  LAI_monthly = c()
  for (k in 1:length(Months)) {
  # file name in the form of xxxx-xx 
  fl = paste0(format(Months[k],'%Y%m'))
  # list all files belonging to that month 
  files_LAI = list.files(path = path2LAI, full.names = T,pattern = fl)
   
  # stack all the LAI composite rasters 
  LAI_stack = stack(files_LAI)
  # extract all composite data for the month
  LAI_data = t(as.matrix(extract(LAI_stack,grid_catch_sp)))  
  # take monthly mean
  LAI_monthly = rbind(LAI_monthly, apply(LAI_data,2,mean))
}

# Store data 
NGRID = ncol(LAI_monthly)   
LAI_monthly_file = paste0(storepath,catname,"monthlyLAI_1519.csv")
write.table(LAI_monthly,file = LAI_monthly_file,row.names=F,
            col.names=paste0("grid",1:NGRID),quote=FALSE,sep=',')
print(paste0("LAI data for",group_catch[num],"has finished loading"))

}
