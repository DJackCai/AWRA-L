### use observed and modelled data to compute daily ROC for the catchments ####

dailyROC_catch = function(catid, rain_thres = 0.1) {
  
  STATIC_PAR <- as.matrix(read.csv(paste0("./forcings/",catid,"_resampled_static.csv")))
  N_GRID <- ncol(STATIC_PAR)
  par_opt_Q = as.matrix(read.table(paste0("./resamp_opt_pars/",catid,"_Qopt_Q_Resamp_19942014.txt"),header=T))
  
  catch_dailyQ = read.zoo(paste0("./Streamdata/Qobs_",catid,"_1518.csv"),header=T,
                          sep=",",index.column = 1)$Q%>%as.vector()
  all_forcing_1418 <- grid_forcing_catchment(catname = catid,N_GRID = N_GRID ,
                                             start = 366, end= 2191)
  PG_1518  = all_forcing_1418$PG[-c(1:365),]
  
  ## catchment daily P from the rainfall-runoff data 
  CMS_RR_data <-read.zoo(paste0("~/Downloads/FinalProject_MATH3133//rainfall_runoff_",as.character(catid),".csv"),
                         skip = 15,header=T,sep=",",index.column = 1,format = "%d/%m/%Y")
  start_index <- which(index(CMS_RR_data)==as.Date(paste0(2015,"-01-01")))
  end_index <-which(index(CMS_RR_data)==as.Date(paste0(2018,"-12-31")))
  CMS_RR_data_sub = CMS_RR_data[start_index:end_index, ]
  
  catch_dailyP = CMS_RR_data_sub$rain.mm.d.  # lumped data 
  catch_AWAP_PG1518 = apply(PG_1518,1,mean,na.rm=T)  # disaggregated data 
  #  catch_dailyP = apply(PG_1518,1,mean,na.rm=T)
  
  ### subset of dates for scatterplotting 
  rain_index = which(catch_dailyP > 0.1)
  
  ## SMAP data 
  SMAP_data = as.matrix(read.csv(paste0("./forcings/",catid,"_resampled_SMAP1519.csv")))[1:1461,paste0("grid",1:N_GRID)]
  catch_SMAP = apply(SMAP_data, 1, mean, na.rm = T)
  catch_SMAP  = dplyr::lag(catch_SMAP,n = 1)
  
  ## Simulated Q and SM 
  fit_mod_1518 = Run_AWRAL_calib_Q(par_opt_Q, all_forcing = all_forcing_1418,  
                                   STATIC_PAR =STATIC_PAR, N_GRID)
  Qsim_1518 = apply(fit_mod_1518$QTOT[-c(1:365), ],1,mean)
  S0mean_1518  = fit_mod_1518$S0mean[-c(1:365), ]
  catch_S0 = dplyr::lag(apply(S0mean_1518,1,mean), n = 1)
  
  hydro_data = data.frame(lump_rain = catch_dailyP, AWAP_rain = catch_AWAP_PG1518, 
                          Q_obs = catch_dailyQ, Qsim = Qsim_1518,
                          S0_obs = catch_SMAP, S0_sim = catch_S0) %>% 
    mutate(ROC_obs = catch_dailyQ/catch_dailyP, ROC_sim = Qsim_1518/catch_AWAP_PG1518)
  
  ## compute ROC for rain days (Alvarez-Garreton et al., 2014)
  
  hydro_data_sub = hydro_data[rain_index,] %>% mutate_all(function(x){round(x,3)})
  
  # output the hydroclimatic data 
  return(hydro_data_sub)
}
