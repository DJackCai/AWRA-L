##### Evaluation & visualisation of streamflow performance on a monthly scale 
#### Motivation is that specific calibration/assimilation scheme may be more effective at some seasons but less efective in other seasons


#### Monthly performance metrics ##### (not suitable for events, though)
## inspired by Zhang et al. (2009) [1st April]

## monthly sum #### 
aggregate_monthly <- function(vals, date1, date2) {
  
  Q_data = data.frame(Date = seq(as.Date(date1), as.Date(date2),1), Q = vals)
  Q_data$year_month = floor_date(Q_data$Date, "month")
  Q_data_month = Q_data%>%group_by(year_month)%>%
    summarise(monthly = sum(Q,na.rm=T)) %>% dplyr::select(monthly)
  
  return(Q_data_month$monthly)
}


### Inverted plot of monthly average rainfall ### 


rainfall_month = function(rain_month, scale_fac ) {
  
  par(xaxs="i",yaxs="i",mar=c(5,4,4,4.3),mgp=c(2.5,1,0))
  plot(1:12,rain_month,type="h", xlim = c(0.5,12.5),
       ylim=c(max(rain_month)*scale_fac,0),lwd=1.2,
       xlab=NA, ylab=NA,col="blue",lend="square",axes=F)
  axis(4)
  mtext("Rainfall (mm/month)",side=4,outer=T,line=-2)
  par(new=T)
  
}

 ## average monthly sum #### 

ts_month_avg = function(full_ts, date1 = "2015-01-01", date2  = "2018-12-31") {
  # % compute the average monthly value of a time series
  
  # sum the rain for each month 
  full_dates = seq(as.Date(date1), as.Date(date2),1)
  full_data = data.frame(Date = full_dates, val = rain_ts)
  full_data$year_month = floor_date(full_data$Date, "month")
  
  full_data_month = full_data%>%group_by(year_month)%>%
    summarise(monthly = sum(val,na.rm=T))%>%mutate(Month = month(year_month))
  
  ## average over years for each month
  data_month_avg = full_data_month %>% group_by(Month) %>%
    summarise(mean_val = mean(monthly))
  return(data_month_avg)
}

      ### performance for **monthly sum*** streamflow ####

Eval_summary_monthly <- function(monthly_obs,monthly_sim,perc=0.98,...) {
  KGE_val <- klingupta(mod = monthly_sim,obs = monthly_obs)
  NSE_val <- hydromad::nseStat(obs = monthly_obs, mod = monthly_sim)
  NSE_log_val <- hmadstat("r.sq.log")(Q = monthly_obs, X = monthly_sim)
  NSE_sqrt_val <- hmadstat("r.sq.sqrt")(Q = monthly_obs, X = monthly_sim)
  Bias_FHV_val <- BiasFHV(Qsim = monthly_sim ,Qobs = monthly_obs, perc=perc)
  
  Volume_bias <- hmadstat("rel.bias")(Q=monthly_obs,X=monthly_sim)
  corr <- cor(monthly_obs,monthly_sim,use="p")
  
  
  perform <- data.frame(R=corr, KGE = KGE_val,NSE = NSE_val,NSE_log = NSE_log_val, 
                        NSE_sqrt = NSE_sqrt_val, Bias_FHV = Bias_FHV_val,
                        AVE = Volume_bias)
  perform_round<- round(perform,4)
  return(perform_round)
}


  #### May 31th:  month specific evaluation (still daily step) #### 

month_perform <- function(obs, mod,  year1, year2, subset_index ) {
  
  Date_ts = dates_seq(year1, year2)
  Q_data = data.frame(Year = year(Date_ts), Month = month(Date_ts), 
                      obs = obs, mod = mod)
  Q_data_sub = Q_data[-c(1:subset_index),]
  
  month_perf_mat = NULL
  for (i in 1:12) {
    
    Q_data_month = Q_data%>%filter(Month == i)
    
    mod_month = Q_data_month$mod; obs_month = Q_data_month$obs
    KGE_val <- klingupta(mod = mod_month,obs = obs_month)
    NSE_val <- hydromad::nseStat(obs = obs_month,mod = mod_month)
    NSE_log_val <- hmadstat("r.sq.log")(Q = obs_month, X = mod_month)
    Volume_bias <- hmadstat("rel.bias")(Q = obs_month, X = mod_month)
    corr <- cor(obs_month, mod_month , use="p")
    var_ratio = sd(mod_month, na.rm=T)/sd(obs_month, na.rm=T)
    RMSE = RMSE(x = obs_month, y =mod_month)
    sd_month = sd(obs_month, na.rm=T)
    norm_RMSE = RMSE/sd_month
    
    month_perf_val <- c(Bias = Volume_bias, R=corr, var_ratio = var_ratio, KGE = KGE_val, 
                        NSE = NSE_val,NSE_log = NSE_log_val, norm_RMSE = norm_RMSE) 
    month_perf_mat <- rbind(month_perf_mat, month_perf_val) 
    
  }
  # internal vector 
  month_perf_df = data.frame(Month = month.abb, month_perf_mat )
  rownames(month_perf_df) = month.abb
  return(month_perf_df)
}


