### Functions that helps extract the catchment characteristics #### 



dates_1321 = dates_seq(2013,2021)
start_calib = which(dates_1321 == '2014-01-01')   # spin up starts in 2014
end_calib = which(dates_1321 == '2017-12-31')   
start_Vali = which(dates_1321 == '2018-01-01')   # spin up starts in 2014
end_Vali = which(dates_1321 == '2018-12-31')   

dates_9214 = dates_seq(1992,2014)
start_calib_9414 = which(dates_9214 == '1994-01-01')   # spin up starts in 2014
end_calib_9414 = which(dates_9214 == '2014-12-31')   
 
      #### baseflow separation (June 6th) #####
 
pack_url = "https://cran.r-project.org/src/contrib/Archive/EcoHydRology/EcoHydRology_0.4.12.1.tar.gz"
install.packages(pack_url, repos=NULL, type="source",dependencies = T)
library(EcoHydRology); library(imputeTS)
install.packages("imputeTS")

Q_405264_9414 = read.zoo(paste0("./Streamdata/Qobs_",405264,"_19942014.csv"),header=T,
                         sep=",",index.column = 1)$Q
Q_405264_1518 = read.zoo(paste0("./Streamdata/Qobs_",405264,"_1518.csv"),header=T,
                         sep=",",index.column = 1)$Q
Q_405264_full = c(Q_405264_9414,Q_405264_1518)%>%na_interpolation(.,option = "spline")

bf_405264 = BaseflowSeparation(streamflow = as.vector(Q_405264_full), filter_parameter = 0.925, passes = 3)

plot(dates_seq(1994,2018), Logflow(Q_405264_full), type = 'l', 
        xlab = "Date", ylab = "Streamflow (mm)", lwd = 0.8)
lines(dates_seq(1994,2018),Logflow(bf_405264$bt), type = 'l', col = 'red' )

bf_vol = bf_405264$bt
sum(bf_405264$bt)/sum(Q_405264_full)

 ## low baseflow contribution site? #### 

Q_143110_9414 = read.zoo(paste0("./Streamdata/Qobs_",143110,"_19942014.csv"),header=T,
                         sep=",",index.column = 1)$Q
Q_143110_1518 = read.zoo(paste0("./Streamdata/Qobs_",143110,"_1518.csv"),header=T,
                         sep=",",index.column = 1)$Q
Q_143110_full = c(Q_143110_9414,Q_143110_1518)%>%na_interpolation(.,option = "spline")

bf_143110 = BaseflowSeparation(streamflow = as.vector(Q_143110_full), filter_parameter = 0.925, passes = 3)

plot(dates_seq(1994,2018), Logflow(Q_143110_full), type = 'l', 
     xlab = "Date", ylab = "Streamflow (mm)", lwd = 0.5, 
     main = "Baseflow separation @ Bremer Catchment", cex.main = 1.1)
lines(dates_seq(1994,2018),Logflow(bf_143110$bt), type = 'l', col = 'red' )

sum(bf_143110$bt)/sum(Q_143110_full)






annual_sum = function(full_ts,year1,year2) {
  years = format(DateSeq(year1,year2),"%Y")
  stopifnot(length(full_ts) == length(years))
  full_df = data.frame(Year = years, value = full_ts)
  annual_data = full_df%>%group_by(Year)%>%
    summarise(annual = sum(value,na.rm=T))
  return(annual_data$annual) 
  
}

mean_annual = function(full_ts,year1,year2) {
  years = format(DateSeq(year1,year2),"%Y")
  stopifnot(length(full_ts) == length(years))
  full_df = data.frame(Year = years, value = full_ts)
  annual_data = full_df%>%group_by(Year)%>%
    summarise(annual = sum(value,na.rm=T))
  mean_annual = mean(annual_data$annual)
  
  return( mean_annual)
  
}

Ss_Qg_coupling = function(catid, cormethod = "spearman") {
  
  STATIC_PAR <- as.matrix(read.csv(paste0("./forcings/",catid,"_resampled_static.csv")))
  N_GRID <- ncol(STATIC_PAR)
  
  all_forcing_9414 <- grid_forcing_catchment_longterm(catname = catid,N_GRID = N_GRID ,
                                                      start = start_calib_9414, end= end_calib_9414)
  all_forcing_1418 <- grid_forcing_catchment(catname = catid,N_GRID = N_GRID ,
                                             start = 366, end= end_Vali)
  all_forcing_1518 <- grid_forcing_catchment(catname = catid,N_GRID = N_GRID ,
                                             start = 731, end= end_Vali)
  
  par_opt_Q = as.matrix(read.table(paste0(catid,"_Qopt_Q_Resamp_19942014.txt"),header=T))
  
  fit_mod_9414 <- Run_AWRAL_calib_Q(par_opt_Q, all_forcing = all_forcing_9414, 
                                    STATIC_PAR =STATIC_PAR, N_GRID)
  fit_mod_1518 <- Run_AWRAL_calib_Q(par_opt_Q, all_forcing = all_forcing_1418,  
                                    STATIC_PAR =STATIC_PAR, N_GRID)
  
  S0mean_9414 = fit_mod_9414$S0mean
  Ssmean_9414 = fit_mod_9414$Ssmean
  QG_9414 = fit_mod_9414$QG
  
  S0mean_1518 = fit_mod_1518$S0mean[-c(1:365), ]
  Ssmean_1518 = fit_mod_1518$Ssmean[-c(1:365), ]
  QG_1518 = fit_mod_1518$QG[-c(1:365), ]
  
  S0mean_9418 = rbind(S0mean_9414, S0mean_1518 )
  Ssmean_9418 = rbind(Ssmean_9414, Ssmean_1518)
  QG_9418 = rbind(QG_9414, QG_1518)
  
  
  #### 
  SE_9414= fit_mod_9414$SE
  SE_1518  = fit_mod_1518$SE[-c(1:365), ]
  SE_9418 = rbind(SE_9414, SE_1518)
  
  ### vertical coupling between Ss and groundwater recharge 
  cor_SsQG_9418 = mapply(function(x,y) { return(cor(x,y,use = "p",method = 'spearman')) }, 
                         as.data.frame(Ssmean_9418), as.data.frame(QG_9418))
  cor_SsQG_1518 = mapply(function(x,y) { return(cor(x,y,use = "p",method = 'spearman')) }, 
                         as.data.frame(Ssmean_1518), as.data.frame(QG_1518))
  catch_cor_SsQG_9418 =  mean(cor_SsQG_9418,na.rm=T)
  catch_cor_SsQG_1518 =  mean(cor_SsQG_1518,na.rm=T)
  
  ### vertical coupling between Ss and saturation excess (fsat)
  cor_SsSE_9418 = mapply(function(x,y) { return(cor(x,y,use = "p",method = 'spearman')) }, 
                         as.data.frame(Ssmean_9418), as.data.frame(SE_9418))
  cor_SsSE_1518 = mapply(function(x,y) { return(cor(x,y,use = "p",method = 'spearman')) }, 
                         as.data.frame(Ssmean_1518), as.data.frame(SE_1518))
  catch_cor_SsSE_9418 =  mean(cor_SsSE_9418,na.rm=T)
  catch_cor_SsSE_1518 =  mean(cor_SsSE_1518,na.rm=T)
  
  coup_df = data.frame(R_SsQG = catch_cor_SsQG_9418, R_SsQG_1518 = catch_cor_SsQG_1518,
                       R_SsSE = catch_cor_SsSE_9418, R_SsSE_1518 = catch_cor_SsSE_1518)
  
  
  return(coup_df)
  
  
  
  
}



catchment_mining_upd = function(catid, cormethod = "spearman") {
  
  STATIC_PAR <- as.matrix(read.csv(paste0("./forcings/",catid,"_resampled_static.csv")))
  N_GRID <- ncol(STATIC_PAR)
  shp_info = site_shp(id =  catid)
  area = shp_info$area_km2
  all_forcing_9414 <- grid_forcing_catchment_longterm(catname = catid,N_GRID = N_GRID ,
                                                      start = start_calib_9414, end= end_calib_9414)
  all_forcing_1418 <- grid_forcing_catchment(catname = catid,N_GRID = N_GRID ,
                                             start = 366, end= end_Vali)
   all_forcing_1518 <- grid_forcing_catchment(catname = catid,N_GRID = N_GRID ,
                                             start = 731, end= end_Vali)
  ## humidity  ####
  PG_9418 = rbind(all_forcing_9414$PG, all_forcing_1518$PG)
  meanPET  = as.vector(STATIC_PAR[5,])
  meanP   = colMeans(PG_9418)
  grid_humidity = meanP/meanPET  # long-term humidity at each grid 
  catch_humidity = mean(grid_humidity)
  
  ## ftree #### 
  grid_ftree = STATIC_PAR[1,]
  catch_ftree = mean(grid_ftree)
  
  ## mean annual rainfall
  grid_annual_rain = apply(PG_9418, 2, annual_sum, year1 = 1994, year2 = 2018)
  catch_annual_rain = apply(grid_annual_rain,1,mean,na.rm=T)  ## annual rain ts
  
  grid_mean_annual_rain = apply(PG_9418, 2, mean_annual, year1 = 1994, year2 = 2018)
  
  catch_annual_meanP = mean(grid_mean_annual_rain)
  
  ### mean annual sum of streamflow #### 
  Qval_9414 = read.zoo(paste0("./Streamdata/Qobs_",catid,"_19942014.csv"),header=T,
                       sep=",",index.column = 1)$Q%>%as.numeric()
  Qval_1518 = read.zoo(paste0("./Streamdata/Qobs_",catid,"_1518.csv"),header=T,
                       sep=",",index.column = 1)$Q%>%as.numeric()
  
  Qval_9418 = c(Qval_9414,Qval_1518)
  catch_annual_Q = annual_sum(Qval_9418,year1 = 1994, year2 = 2018)
  catch_meanQ = mean(catch_annual_Q)
  
  ### average runoff coefficient for Q>0.01 mm/d #### 
  Q_days_full = which(Qval_9418 >= 0.005)
  
  minP_9418 = apply(PG_9418,1,min)
  
  P_Q_index = which(Qval_9418 >= 0.005 & minP_9418>0)
  
  PG_9418_subset = PG_9418[P_Q_index,]
  Qval_subset = Qval_9418[P_Q_index]

  
  Qval_subset = matrix(Qval_subset, length(Qval_subset),1)  
  
  ROC_daily_9418_grids = 1/(sweep(PG_9418_subset, MARGIN = 1, Qval_subset, FUN = "/"))
  ROC_daily_9418_catch = apply(ROC_daily_9418_grids,1,mean,na.rm=T)
  ROC_daily_mean = mean(ROC_daily_9418_catch,na.rm=T)

  
  ## SMAP data feature #### 
  SMAP_dat_site = as.matrix(read.csv(paste0("./forcings/",catid,"_resampled_SMAP1519.csv")))[,paste0("grid",1:N_GRID)]
  
  ### UTC 1.1 matches 1.2 accumulated rainfall & streamflow 
  SMAP_dat_shift = apply(SMAP_dat_site,2, dplyr::lag, n= 1)
  SMAP_dat_shift_1518 = SMAP_dat_shift[1:1461,]
  
  # function(x) {return(c(NA,x[-length(x)]))} )
  SM_mean_grids = apply(SMAP_dat_shift_1518, 2, mean, na.rm=T)
  SM_mean_catch = mean(SM_mean_grids)
  
  SM_sd_grids = apply(SMAP_dat_shift_1518, 2, sd, na.rm=T)
  SM_sd_catch = mean(SM_sd_grids)
  
  #### Diagnosing runoff mechanism: combined metrics ###### 
  cor(SMAP_lag1518[,1],Qval_1518,use='p',method = cormethod)
  
  Q_days_1518 = which(Qval_1518 >= 0.005)
  Q_1518_Qdays =  Qval_1518[Q_days_1518]
  
  ### 1) SMAP vs Q #####
  SMAP_lag1518 = apply(SMAP_dat_shift_1518,2,dplyr::lag,n=1)
  
  SMAP_Qdays = SMAP_dat_shift_1518[Q_days_1518,]
  SAMP_Qdays_lag = SMAP_lag1518[Q_days_1518, ]
  
  cor_SMAP_Q = apply(SMAP_Qdays,2,function(x) {cor(x,Q_1518_Qdays, use="p",method = cormethod)})
  lag1_SMAP_Q = apply(SAMP_Qdays_lag,2,function(x) {cor(x,Q_1518_Qdays, use="p",method = cormethod)})
  
  catch_cor_SMAPQ = mean(cor_SMAP_Q,na.rm=T)
  catch_lag1_SMAPQ = mean(lag1_SMAP_Q,na.rm=T)
  
  ### 2) Lag1 Autocorrelation of streamflow (1994-2018) ######
  
  Q_fill = Qval_9418
  Q_fill[-c(Q_days_full)] = NA  ## fill low flow period to be NA
  
  Q_fill_1518 = Qval_1518
  Q_fill_1518[-c(Q_days_1518)] = NA
  
  acf1_Q_9418 = acf(Q_fill,lag.max = 1,plot = F,na.action = na.pass)[["acf"]][2]
  acf1_Q_1518 = acf(Q_fill_1518,lag.max = 1,plot = F,na.action = na.pass)[["acf"]][2]
    
  
  ### 3) Control of rainfall (whole series)   #####
  
  PG_9418_lag1 = apply(PG_9418,2,dplyr::lag,n=1)
  PG_9418_lag2 = apply(PG_9418,2,dplyr::lag,n=2)
  PG_9418_lag3 = apply(PG_9418,2,dplyr::lag,n=3)
  
  Rain_Qdays = PG_9418[P_Q_index,]
  Rain_Qdays_lag1 = PG_9418_lag1[P_Q_index,]
  Rain_Qdays_lag2 = PG_9418_lag2[P_Q_index,]
  Rain_Qdays_lag3 = PG_9418_lag3[P_Q_index,]
  
  Q_full_Qdays = Qval_9418[P_Q_index]
  
  cor_Rain_Q  = apply(Rain_Qdays,2,function(x) {cor(x,Q_full_Qdays, use="p",method = "spearman")})
  lag1_Rain_Q = apply(Rain_Qdays_lag1,2,function(x) {cor(x,Q_full_Qdays, use="p",method = "spearman")})
  lag2_Rain_Q = apply(Rain_Qdays_lag2,2,function(x) {cor(x,Q_full_Qdays, use="p",method = "spearman")})
  lag3_Rain_Q = apply(Rain_Qdays_lag3,2,function(x) {cor(x,Q_full_Qdays, use="p",method = "spearman")})
  
  catch_cor_RQ = mean(cor_Rain_Q, na.rm=T)
  catch_lag1_RQ = mean(lag1_Rain_Q,na.rm=T)
  catch_lag2_RQ = mean(lag2_Rain_Q,na.rm=T)
  catch_lag3_RQ = mean(lag3_Rain_Q,na.rm=T)
  
  ### 4) Vertical coupling between S0 and Ss and SG, 1994-2018  #####
  
  par_opt_Q = as.matrix(read.table(paste0(catid,"_Qopt_Q_Resamp_19942014.txt"),header=T))
  
  fit_mod_9414 <- Run_AWRAL_calib_Q(par_opt_Q, all_forcing = all_forcing_9414, 
                               STATIC_PAR =STATIC_PAR, N_GRID)
  fit_mod_1518 <- Run_AWRAL_calib_Q(par_opt_Q, all_forcing = all_forcing_1418,  
                                    STATIC_PAR =STATIC_PAR, N_GRID)
  
  S0mean_9414 = fit_mod_9414$S0mean
  Ssmean_9414 = fit_mod_9414$Ssmean
  SG_9414 = fit_mod_9414$SG
  
  S0mean_1518 = fit_mod_1518$S0mean[-c(1:365), ]
  Ssmean_1518 = fit_mod_1518$Ssmean[-c(1:365), ]
  SG1518 = fit_mod_1518$SG[-c(1:365),]
  
  # cor_SsSG_9414 = mapply(function(x,y) { return(cor(x,y,use = "p",method = 'spearman')) }, 
  #                        as.data.frame(Ssmean_9414), as.data.frame(SG_9414))
  
  cor_SsSG_1518 = mapply(function(x,y) { return(cor(x,y,use = "p",method = 'spearman')) }, 
                         as.data.frame(Ssmean_1518), as.data.frame(SG1518))
  
  S0mean_9418 = rbind(S0mean_9414, S0mean_1518 )
  Ssmean_9418 = rbind(Ssmean_9414, Ssmean_1518)
  SG_9418 = rbind(SG_9414, SG1518)
  

  cor_SsSG_9418 = mapply(function(x,y) { return(cor(x,y,use = "p",method = 'spearman')) }, 
                         as.data.frame(Ssmean_9418), as.data.frame(SG_9418))
  
  catch_cor_SsSG_9418 =  mean(cor_SsSG_9418,na.rm=T)
  catch_cor_SsSG_1518 =  mean(cor_SsSG_1518,na.rm=T)
  
  ### 5) coupling between S0/Ss with Qsim (first gridded, then average) 
  
  Qtot_9414 = fit_mod_9414$QTOT
  Qtot_1518 = fit_mod_1518$QTOT[-c(1:365),]
  Qtot_9418 = rbind(Qtot_9414, Qtot_1518)
  
  Qsim_9414 = apply(Qtot_9414, 1, mean)
  Qsim_1518 = apply(Qtot_1518, 1, mean)
  Qsim_9418 = apply(Qtot_9418, 1, mean)
  
  S0mean_9418_lag = apply(S0mean_9418,2,dplyr::lag, n= 1)
  Ssmean_9418_lag = apply(Ssmean_9418,2,dplyr::lag, n= 1)
  S0mean_1518_lag = apply(S0mean_1518,2,dplyr::lag, n= 1)
  Ssmean_1518_lag = apply(Ssmean_1518,2,dplyr::lag, n= 1)
  
  ##### coupling from 1994 to 2018 #####
  
  Qsim_days_full = which(Qsim_9418>=0.005)
    
  Qsim_9418_Qdays = Qsim_9418[Qsim_days_full]
  S0mean_9418_Qdays = S0mean_9418[Qsim_days_full,]
  S0mean_9418_Qdays_lag = S0mean_9418_lag[Qsim_days_full,]
  
  Ssmean_9418_Qdays = Ssmean_9418[Qsim_days_full,]
  Ssmean_9418_Qdays_lag = Ssmean_9418_lag[Qsim_days_full,]
  
  cor_S0_Q_9418 = apply(S0mean_9418_Qdays,2,function(x) {cor(x,Qsim_9418_Qdays, use="p",method = 'spearman')})
  cor_Ss_Q_9418 = apply(Ssmean_9418_Qdays,2,function(x) {cor(x,Qsim_9418_Qdays, use="p",method = 'spearman')})
  lag_S0_Q_9418 = apply(S0mean_9418_Qdays_lag,2,function(x) {cor(x,Qsim_9418_Qdays, use="p",method = 'spearman')})
  lag_Ss_Q_9418 = apply(Ssmean_9418_Qdays_lag,2,function(x) {cor(x,Qsim_9418_Qdays, use="p",method = 'spearman')})
  
  #### coupling from 2015 to 2018 #####
  Qsim_days_1518 = which(Qsim_1518>=0.01)
  
  Qsim_1518_Qdays = Qsim_1518[Qsim_days_1518]
  S0mean_1518_Qdays = S0mean_1518[Qsim_days_1518,]
  S0mean_1518_Qdays_lag = S0mean_1518_lag[Qsim_days_1518,]
  
  Ssmean_1518_Qdays = Ssmean_1518[Qsim_days_1518,]
  Ssmean_1518_Qdays_lag = Ssmean_1518_lag[Qsim_days_1518,]
  
  cor_S0_Q_1518 = apply(S0mean_1518_Qdays,2,function(x) {cor(x,Qsim_1518_Qdays, use="p",method = 'spearman')})
  cor_Ss_Q_1518 = apply(Ssmean_1518_Qdays,2,function(x) {cor(x,Qsim_1518_Qdays, use="p",method = 'spearman')})
  lag_S0_Q_1518 = apply(S0mean_1518_Qdays_lag,2,function(x) {cor(x,Qsim_1518_Qdays, use="p",method = 'spearman')})
  lag_Ss_Q_1518 = apply(Ssmean_1518_Qdays_lag,2,function(x) {cor(x,Qsim_1518_Qdays, use="p",method = 'spearman')})
  
  ## catchment average for coupling 
  
  catch_cor_S0Q_9418 = mean(cor_S0_Q_9418)
  catch_cor_SsQ_9418 = mean(cor_Ss_Q_9418)
  catch_lag_S0Q_9418 = mean(lag_S0_Q_9418)
  catch_lag_SsQ_9418 = mean(lag_Ss_Q_9418)
  
  catch_cor_S0Q_1518 = mean(cor_S0_Q_1518)
  catch_cor_SsQ_1518 = mean(cor_Ss_Q_1518)
  catch_lag_S0Q_1518 = mean(lag_S0_Q_1518)
  catch_lag_SsQ_1518 = mean(lag_Ss_Q_1518)

  
  ### 6) Overland vs baseflow #### 
  
  
  QR_9414= fit_mod_9414$QR
  QG_9414 = fit_mod_9414$QG
  QR_1518  = fit_mod_1518$QR[-c(1:365), ]
  QG_1518  = fit_mod_1518$QG[-c(1:365), ]
  QR_9418 = rbind(QR_9414, QR_1518)
  QG_9418 = rbind(QG_9414, QG_1518)
  
  ROf_days_9418 = which(apply(QR_9418,1,min) > 0)
  ROf_days_1518 = which(apply(QR_1518,1,min) > 0)
  
  QR_9418_Qdays = QR_9418[ROf_days_9418,]
  QG_9418_Qdays = QG_9418[ROf_days_9418,]
  QG_prop_9418 = mapply(function(x,y) {x/(x+y)}, as.data.frame(QG_9418_Qdays),
                        as.data.frame(QR_9418_Qdays))
  QG_prop_grids_9418 = apply(QG_prop_9418,2,median)
  catch_QG_prop_9418 = mean(QG_prop_grids_9418)
    
  
  QR_1518_Qdays = QR_1518[ROf_days_1518,]
  QG_1518_Qdays = QG_1518[ROf_days_1518,]
  QG_prop_1518 = mapply(function(x,y) {x/(x+y)}, as.data.frame(QG_1518_Qdays),
                        as.data.frame(QR_1518_Qdays))
  
  QG_prop_grids_1518 = apply(QG_prop_1518,2,median)
  catch_QG_prop_1518 = mean(QG_prop_grids_1518)
 
  
  #### 7) saturation excess vs infiltration excess 
  
  IE_9414= fit_mod_9414$IE
  IE_1518  = fit_mod_1518$IE[-c(1:365), ]
  IE_9418 = rbind(IE_9414, IE_1518)
  
  SE_9414= fit_mod_9414$SE
  SE_1518  = fit_mod_1518$SE[-c(1:365), ]
  SE_9418 = rbind(SE_9414, SE_1518)
  
  ### SE/SE+IE
  SE_9418_Qdays = SE_9418[ROf_days_9418,]
  IE_9418_Qdays = IE_9418[ROf_days_9418,]
  SE_prop_9418 = mapply(function(x,y) {x/(x+y)}, as.data.frame(SE_9418_Qdays),
                        as.data.frame(IE_9418_Qdays))
  SE_prop_grids_9418 = apply(SE_prop_9418,2,median)
  catch_SE_prop_9418 = mean(SE_prop_grids_9418 ,na.rm=T)
  
  SE_1518_Qdays = SE_1518[ROf_days_1518,]
  IE_1518_Qdays = IE_1518[ROf_days_1518,]
  SE_prop_1518 = mapply(function(x,y) {x/(x+y)}, as.data.frame(SE_1518_Qdays),
                        as.data.frame(IE_1518_Qdays))
  
  SE_prop_grids_1518 = apply(SE_prop_1518,2,median)
  catch_SE_prop_1518 = mean(SE_prop_grids_1518,na.rm=T)
  
  
  ### vertical coupling between Ss and groundwater recharge 
  cor_SsQG_9418 = mapply(function(x,y) { return(cor(x,y,use = "p",method = 'spearman')) }, 
                         as.data.frame(Ssmean_9418), as.data.frame(QG_9418))
  cor_SsQG_1518 = mapply(function(x,y) { return(cor(x,y,use = "p",method = 'spearman')) }, 
                         as.data.frame(Ssmean_1518), as.data.frame(QG_1518))
  catch_cor_SsQG_9418 =  mean(cor_SsQG_9418,na.rm=T)
  catch_cor_SsQG_1518 =  mean(cor_SsQG_1518,na.rm=T)
  
  ### vertical coupling between Ss and saturation excess (fsat)
  cor_SsSE_9418 = mapply(function(x,y) { return(cor(x,y,use = "p",method = 'spearman')) }, 
                         as.data.frame(Ssmean_9418), as.data.frame(SE_9418))
  cor_SsSE_1518 = mapply(function(x,y) { return(cor(x,y,use = "p",method = 'spearman')) }, 
                         as.data.frame(Ssmean_1518), as.data.frame(SE_1518))
  catch_cor_SsSE_9418 =  mean(cor_SsSE_9418,na.rm=T)
  catch_cor_SsSE_1518 =  mean(cor_SsSE_1518,na.rm=T)
  
  # coup_df = data.frame(R_SsQG = catch_cor_SsQG_9418, R_SsQG_1518 = catch_cor_SsQG_1518,
  #                      R_SsSE = catch_cor_SsSE_9418, R_SsSE_1518 = catch_cor_SsSE_1518)
  
  catch_summary = data.frame(humidity = catch_humidity, ftree =catch_ftree, area = area, 
                             meanP = catch_annual_meanP, meanQ = catch_meanQ,
                             ROC = ROC_daily_mean, SMAPmean = SM_mean_catch, SMAPsd = SM_sd_catch,
                             R_SMAPQ = catch_cor_SMAPQ, lag1_SMAP_Q = catch_lag1_SMAPQ,
                             acf1_Q = acf1_Q_9418, acf1_Q_1518 = acf1_Q_1518, 
                             R_Qrain = catch_cor_RQ, lag1_Qrain = catch_lag1_RQ, lag2_Qrain = catch_lag2_RQ,
                             lag3_Qrain = catch_lag3_RQ, 
                             R_SsSG = catch_cor_SsSG_9418, R_SsSG_1518 = catch_cor_SsSG_1518,
                             R_S0Qsim = catch_cor_S0Q_9418, R_S0Qsim_1518 = catch_cor_S0Q_1518,
                             R_SsQsim = catch_cor_SsQ_9418, R_SsQsim_1518 = catch_cor_SsQ_1518,
                             lag1_S0Qsim = catch_lag_S0Q_9418, lag1_S0Qsim_1518 = catch_lag_S0Q_1518,
                             lag1_SsQsim = catch_lag_SsQ_9418, lag1_SsQsim_1518 = catch_lag_SsQ_1518,
                             
                             QG_prop = catch_QG_prop_9418, QG_prop_1518 = catch_QG_prop_1518,
                             SE_prop = catch_SE_prop_9418,  SE_prop_1518 = catch_SE_prop_1518,
                             R_SsQG = catch_cor_SsQG_9418, R_SsQG_1518 = catch_cor_SsQG_1518,
                             R_SsSE = catch_cor_SsSE_9418, R_SsSE_1518 = catch_cor_SsSE_1518) 
                             
  print(paste0("--------  Catchment features for",catid," finish collating ---------------"))
  return(catch_summary)
  
  
  
}
