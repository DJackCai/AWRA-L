## Functions that calculate the efficiency of data assimilation/
## calibraiton scheme compared to the baseline open-loop simulations 
## Reference: Aubert, D., Loumagne, C. and Oudin, L., 2003. Sequential assimilation of soil moisture and streamflow data in a conceptual rainfall–runoff model, Journal of Hydrology, 280: 145-161. 


Eff = function(Q_OL, Q_Assim, Qobs) {
  # Calculate the efficiency to evaluate the benefits in DA
  SS_Assim = sum((Q_Assim-Qobs)**2,na.rm=T)
  SS_OL = sum((Q_OL-Qobs)**2,na.rm=T)
  
  ## Eff >0 means SS_Assim is smaller, showing improvement 
  
  Eff = 1 - SS_Assim/SS_OL 
  return(Eff)
} 


Eff_log = function(Q_OL, Q_Assim, Qobs) {
  # Calculate the efficiency to evaluate the benefits in DA
  Q_OL_log = log(Q_OL+0.005)
  Q_Assim_log = log(Q_Assim+0.005)
  Qobs_log = log(Qobs+0.005)
  
  SS_Assim = sum((Q_Assim_log - Qobs_log)**2,na.rm=T)
  SS_OL = sum((Q_OL_log - Qobs_log)**2,na.rm=T)
  
  ## Eff >0 means SS_Assim is smaller, showing improvement 
  
  Eff_log = 1 - SS_Assim/SS_OL 
  return(Eff_log)
} 


Bias_Eff = function(Q_OL, Q_Assim, Qobs) {
  
  mu_OL = mean(Q_OL,na.rm=T)
  mu_Assim = mean(Q_Assim, na.rm=T)
  mu_Qobs = mean(Qobs, na.rm=T)
  
  bias_OL = abs(hmadstat("rel.bias")(Q=Qobs,X=Q_OL))
  bias_assim = abs(hmadstat("rel.bias")(Q=Qobs,X=Q_Assim))
  
  bias_Eff = 1 - bias_assim/bias_OL
  
}

Eff_summary = function(Q_OL, Q_Assim, Qobs){
  ## %% summarise the efficiency/benefits of model-data integration scheme 
  Eff_NSE = Eff(Q_OL, Q_Assim, Qobs)
  Eff_log = Eff_log(Q_OL, Q_Assim, Qobs)
  Eff_bias = Bias_Eff(Q_OL, Q_Assim, Qobs)
  
  Eff_results = t(c(Eff_NSE,Eff_log,Eff_bias))
  colnames(Eff_results) = c("NSE","NSElog","Bias")
  return(Eff_results)
  
}


