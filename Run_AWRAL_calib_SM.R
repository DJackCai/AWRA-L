########  AWRA-L full dynamic run using calibrated parameters   ######### 

# Due to the different sensitivity of parameters at each catchment, 
# the length and the components of "pars" will differ. 
# make sure all the data read in are in the matrix format, using the as.matrix() command. 

Run_AWRAL_calib_SM <- function(pars,all_forcing,STATIC_PAR,N_GRID=1)  {
  
  ## all_forcing: full time series of each forcing variable for the site 
  ## states: vector of initial states (passed unchanged)
  II = rbind(N_GRID,N_GRID)
  
  Init_PARAMS <- PARAMS_set(II,STATIC_PAR = STATIC_PAR)
  
  #####  Update the initial parameters by the pars to be optimised  #####
  
  ###### in the Fhru format
  
  Init_PARAMS$Vc <- II*c(pars[1],0.65)
  Init_PARAMS$Gfrac_max <- II*c(0.3,pars[2])
  
  Init_PARAMS$Ud0 = II*c(pars[3],0)
  Init_PARAMS$FsoilEmax = II * c(pars[4],pars[5])
  Init_PARAMS$S_sls = II * c(pars[6],pars[7])
  Init_PARAMS$ER_frac_ref = II*c(pars[8],0.05)
  Init_PARAMS$FdrainFC = II * c(pars[9],pars[10]) 
  Init_PARAMS$beta <- II * c(pars[11],pars[12])
 
  PARAMS <- Init_PARAMS  # updated parameter
  
  ## Extract forcing data for ALL TIME
  PG <- all_forcing$PG
  RG <- all_forcing$RG
  TA <- all_forcing$TA
  PE <- all_forcing$PE
  U2 <- all_forcing$U2
  PAIR = rep(97500.00,1)
  
  ## Forcing for one time step
  FORCING = empty_forcing()
  
  ###  Updated spatialised parameters (for those insensitive)
  meanP   = mean(PG)
  meanPET = STATIC_PAR$meanPET
  humidity = meanP / meanPET
  
  K_gw   = 0.047*(humidity^(-0.0508))
  K_rout = 0.141*meanPET + 0.284
  Sgref = pmax(0.0,  8.15 * meanP^2.34)
  
  ##  BACK-updating 
  PARAMS$Sgref = Sgref
  PARAMS$K_gw    = K_gw
  PARAMS$K_rout  = K_rout
  
  ### update states 
  STATES = list(
    S0 = 0.5 * PARAMS$S0FC,
    Ss = 0.5 * PARAMS$SsFC,
    Sd = PARAMS$wdlimU*PARAMS$SdFC,
    Sg = 0.01*meanP/PARAMS$K_gw, 
    Sr = 0,
    Mleaf = (PARAMS$SLA * 0+2) / PARAMS$SLA) 
  
  ## Storage
  
  NL <- length(PG)  # define number of time steps; here PG is a vector
  S0mean     = matrix(NA,NL,1)
  QTOT       = matrix(NA,NL,1)
  LAI = matrix(NA,NL,1)
  
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
    
    # Run for single time step
    OUTPUTS = AWRAL_TimeStep(forcing =FORCING,state = STATES,par = PARAMS )
    
    # Assign the new state - to be used as forecast at next
    STATES$S0    = OUTPUTS$S0
    STATES$Ss    = OUTPUTS$Ss
    STATES$Sd    = OUTPUTS$Sd
    STATES$Sg    = OUTPUTS$Sg
    STATES$Sr    = OUTPUTS$Sr
    STATES$Mleaf = OUTPUTS$Mleaf
    
    ## Store the key states and fluxes, same as length of forcings
    S0mean[k,] = c(1,1) %*% (STATES$S0 * PARAMS$Fhru)
    QTOT[k,]   = OUTPUTS$Qtot
    LAI[k,] = OUTPUTS$LAI
  }
  return(list(S0mean=S0mean,QTOT=QTOT,LAI=LAI)) }
