## Quick way to collate the full series of the different model forcing of the model 
# here for illustration, using 2013-2019

dates_1321 <- seq(as.Date("2013-01-01"),as.Date("2021-12-31"),1)
calib_start <- which(dates_1321 == '2015-01-01') 
calib_end <- which(dates_1321 == '2019-12-31')

## function to assign the full forcing list to one 0.05*0.05 grid cell
grid_forcing_extract <- function(catname, gridnum) {
  
  # gridnum: string like "grid11" 
  
  # "366:" here means a one-year spin-up period in 2014 
  PRECIP <- as.matrix(read.csv(paste0(catname,"rain_day.csv"),header = T))[366:calib_end,]
  RAD    = as.matrix(read.csv(paste0(catname, "solar_exposure_day.csv"),header = T))[366:calib_end,]
  TMIN   = as.matrix(read.csv(paste0(catname,"temp_min_day.csv")))[366:calib_end,]
  TMAX   = as.matrix(read.csv(paste0(catname, "temp_max_day.csv")))[366:calib_end,]
  U2 <- as.matrix(read.csv(paste0(catname,"wind.csv")))[366:calib_end,]
  
  PG_grid1 = PRECIP[,gridnum]
  
  RG_grid1 = RAD[,gridnum]
  RG_grid1[RG_grid1 < 0.1] = 0.1
  RG_grid1 = 11.57*RG_grid1
  
  TA_grid1 = TMIN[,gridnum] + 0.75*(TMAX[,gridnum]-TMIN[,gridnum])
  PE_grid1 = 610.8 * exp(17.27*TMIN[,gridnum]/(237.3+TMIN[,gridnum]))
  
  PAIR_grid1 = 97500.00
  U2_grid1<- U2[,gridnum]
  
  # entire time period of different variables of forcing for the grid 
  all_forcing_grid1 <- list(PG=PG_grid1,RG=RG_grid1,TA=TA_grid1,
                            PE=PE_grid1,U2=U2_grid1)
  
  return(all_forcing_grid1)
  
}
