####  AWRA-L full dynamic run  #### 

Run_AWRAL_calib_GB <- function(pars,all_forcing,STATIC_PAR,N_GRID=1)  {
  
  ## all_forcing: full time series of each forcing variable for the site 
  ## STATIC_PAR: the static parameters including ftree, 3 SzFC and mean potential evapotranspiration
  
  II = rbind(1,1)
  
  Init_PARAMS <- PARAMS_set(II,STATIC_PAR = STATIC_PAR)
   
  #####  Update the initial parameters by the pars to be optimised  #####

  Init_PARAMS$Sgref <- pars[1]; Init_PARAMS$Ud01 <- pars[2]
  Init_PARAMS$FsoilEmax1 <- pars[3];  Init_PARAMS$S_sls1 <- pars[4]
  Init_PARAMS$S_sls2 <- pars[5]; Init_PARAMS$ER_frac_ref1 <- pars[6]
  Init_PARAMS$FdrainFC1 <- pars[7]; Init_PARAMS$beta1 <- pars[8];
  Init_PARAMS$LAImax1 <- pars[9]
  
  PARAMS <- Init_PARAMS  # updated parameter
  
  ## Extract forcing data for ALL TIME
  
  PG <- all_forcing$PG
  RG <- all_forcing$RG
  TA <- all_forcing$TA
  PE <- all_forcing$PE
  U2 <- all_forcing$U2
  PAIR = rep(97500.00,1)

  ## Forcing for one time step
  FORCING = empty_forcing()  # external function
  
  ######  Updated spatialised parameters (for those identified as insensitive, continue using the regression relationship adopted in the report)
  meanP   = mean(PG)
  meanPET = STATIC_PAR$meanPET
  humidity = meanP / meanPET
  
  #Sgref= pmax(0.0,  8.15 * meanP^2.34)
  # FdrainFC = pmax(pmin(0.0685*rbind(humidity,humidity)^3.179,0.3),0.005)
  K_gw   = 0.047*(humidity^(-0.0508))
  K_rout = 0.141*meanPET + 0.284
  
  ##  BACK-updating 
  PARAMS$K_gw    = K_gw
  PARAMS$K_rout  = K_rout
  #PARAMS$Sgref    = Sgref
  
  ### update states 
  STATES = list(
    S0 = 0.5 * PARAMS$S0FC,
    Ss = 0.5 * PARAMS$SsFC,
    Sd = PARAMS$wdlimU*PARAMS$SdFC,
    Sg = 0.01*meanP/PARAMS$K_gw, 
    Sr = 0,
    Mleaf = (PARAMS$SLA * 0+2) / PARAMS$SLA) 
  
  ## Storage
  
  NL <- length(PG)  # define number of time steps 
  S0mean     = matrix(NA,NL,1)
 # QTOT       = matrix(NA,NL,1)
  
  ######## Time stepping  ######
  for (k in 1:(NL-1)) {
  
    # loop over each time step's forcing
    FORCING$Pg   = rbind(PG[k],PG[k])
    FORCING$Rg   = rbind(RG[k],RG[k])
    FORCING$Ta   = rbind(TA[k],TA[k])
    FORCING$pe   = rbind(PE[k],PE[k])
    FORCING$pair = PAIR
    # FORCING$u2   = U2
    FORCING$u2   = rbind(U2[k],U2[k])
    
    # Run for single time step by calling external timestepping function
    OUTPUTS = AWRAL_TimeStep(forcing =FORCING,state = STATES,par = PARAMS )

    #### Update the model states, to be used to propogate next time step 
    ## still a N*2 matrix accounting for each fhru
    
    STATES$S0    = OUTPUTS$S0
    STATES$Ss    = OUTPUTS$Ss
    STATES$Sd    = OUTPUTS$Sd
    STATES$Sg    = OUTPUTS$Sg
    STATES$Sr    = OUTPUTS$Sr
    STATES$Mleaf = OUTPUTS$Mleaf
    
    ## Store the key states and fluxes, same as length of forcings
    S0mean[k,] = c(1,1) %*% (STATES$S0 * PARAMS$Fhru)  # combining with Fhru to give one single, grid-level estimate
    QTOT[k,]   = OUTPUTS$Qtot        # versatile, to be used for streamflow calibration
  }
  return(list(S0mean=S0mean,QTOT=QTOT)) }    
