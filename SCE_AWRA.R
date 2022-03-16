### AWRA-L SCE interface in R, using hydromad package #### 

# Run Shuffle complex evolution optimisation of sensitive parameters in AWRA
# Idea adapted from Shin & Choi (2018) Combining an R-Based Evolutionary Algorithm and Hydrological Model for Effective Parameter Calibration 
https://www.mdpi.com/2073-4441/10/10/1339

source("AWRAL_TimeStep_Func.R")
library(hydromad)
source("Qsim_Eval.R")


# Define SCE control parameters 

## SCE control par
control.SCE = list(trace = 1, ncomplex = 2, maxit =1)
control.SCE = modifyList(list(fnscale = -1), control.SCE) 
if (isTRUE(hydromad.getOption("trace"))){
  control$trace <- 1 } 
bestModel <- NULL
bestFunVal <- Inf*control.SCE$fnscale


## define forcing period

dates_1321 <- seq(as.Date("2013-01-01"),as.Date("2021-12-31"),1)
calib_start <- which(dates_1321 == '2015-01-01') 
calib_end <- which(dates_1321 == '2019-12-31')

# extract entire forcing data & ancillary static parameters 
all_forcing_GB_grid11 <- grid_forcing_extract("GoulburnRiver_","grid11")

STATIC_PAR <- as.matrix(read.csv("GoulburnRiver_static.csv"))
static_grid11 <- STATIC_PAR[,"grid11"]
static_par_grid11 <- list(ftree = static_grid11[1], S0FC = static_grid11[2],
                         SsFC = static_grid11[3],  SdFC = static_grid11[4],
                         meanPET = static_grid11[5])
                         
                         
######  Define parameter range & initial values for those sensitive parameters to be optimised

Ud01 = c(0.1,7)
FsoilEmax1   = c(0.1,0.8)
S_sls1 = c(0.05,0.6)
ER_frac_ref1 = c(0.02,0.28)
FdrainFC1 = c(0.005,0.1)
beta1 = c(0.7,8.4)
LAImax1 = c(5,12)

parlist_GB<-list(Ud01 = Ud01,
              FsoilEmax1   = FsoilEmax1,
              S_sls1 = S_sls1, 
              ER_frac_ref1 = ER_frac_ref1,
              FdrainFC1 = FdrainFC1,
              beta1 = beta1,
              LAImax1 = LAImax1) 

lower_GB <-sapply(parlist_GB, min) ; upper_GB <- sapply(parlist_GB, max)
initpars_GB <-sapply(parlist_GB,mean)


###### do_sce: objective function to be optimised using SMAP soil moisture data 
## input: par, then additional inputs to the model run to get the likelihood
## the pars will be passed to the model function, so the model has to be able to take individual elements of *par*  

do_sce_SMAP_GB<-function(pars,SMAPts,all_forcing,STATIC_PAR){
  
  # input: par vector (to be updated at each step)
  # all_forcing: list of entire forcing, for the specific grid 
  # SMAPts: SMAPts for the grid 
  # STATIC_PAR: static parameter
  
  update_Ud01 <- pars[1];
  update_FsoilEmax1 <- pars[2]; 
  update_S_sls1 <- pars[3];
  update_ER_frac_ref1 <- pars[4]
  update_FdrainFC1<- pars[5];   update_beta1 <- pars[6]
  update_LAImax1 <- pars[7]; 

  FORCING = empty_forcing() 
  
  ## Run model - call the run function that takes par vector as inputs
  thisMod <- Run_AWRAL_calib_GB(pars=pars,all_forcing = all_forcing,
                                STATIC_PAR=STATIC_PAR, N_GRID = 1 )
  S0mean <- thisMod$S0mean[-c(1:365),]

  SM_sim <- S0mean
  SMAP_obs <- SMAPts
  stopifnot(length(SM_sim)==length(SMAP_obs))
  cor_SMAP <- cor(SM_sim,SMAP_obs,use="p")
  thisVal<- cor_SMAP
  
  if (isTRUE(thisVal*control.SCE$fnscale < bestFunVal*control.SCE$fnscale)) {
    bestModel <<- thisMod
    bestFunVal <<- thisVal}
  return(thisVal)  # function to be optimised
}


###### Run SCE calibration 

# reference calibration data 
GB_SMAP1519 <- read.csv("GoulburnRiver_SMAP1519.csv")
SMAP_GB_grid11 <- GB_SMAP1519[,"grid11"]

# add the inputs to model + SCE related parameter range & statrting conditions & control 

SCE.GB1<-SCEoptim(do_sce_SMAP_GB, par = initpars_GB,
                          all_forcing= all_forcing_GB_grid11, 
                          STATIC_PAR =static_par_grid11,
                          SMAPts = SMAP_GB_grid11,
                          lower = lower_GB, upper = upper_GB, 
                          control = control.SCE)
                          
##### fit the model using the optimised parameters, return relevant water balance outputs ######

fit_GB_grid11 <- Run_AWRAL_calib_GB(SCE.GB1$par,all_forcing = all_forcing_GB_grid11, STATIC_PAR =static_par_grid11,N_GRID = 1)

cor(fit_GB_grid11$S0mean[-c(1:365)],SMAP_GB_grid11,use="p")  # correlation with SMAP. using a 1-year spin-up

Qtot1_GB_SMCA_1518 <- fit_GB_grid11$QTOT[366:1826,]  ## filter into year 2015-18, grid-level Qtot



