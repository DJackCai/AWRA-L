catname = "Matong_"
Matong_catch <- as.data.frame(expand.grid(seq(146.50,146.60,0.05),seq(-37.05,-36.80,0.05)))

N_GRID <- nrow(Matong_catch)

# Extract all the forcings across all grids in this catchment 

all_forcing_Matong <- grid_forcing_catchment(catname = "Matong_",N_GRID = N_GRID )

# Static parameter
STATIC_PAR <- as.matrix(read.csv("Matong_static.csv"))

# Open-loop parameters with default setting, simply utilise the PARAMS_set function

PARAMS_OL <- PARAMS_set(rbind(rep(1,N_GRID),rep(1,N_GRID)),STATIC_PAR =STATIC_PAR )

# Update spatialised parameters 

meanPET  = as.vector(STATIC_PAR[5,])
meanP   = colMeans(all_forcing_Matong$PG)
humidity = meanP/meanPET
Sgref = pmax(0.0,  8.15 * meanP^2.34)
FdrainFC = pmax(pmin(0.0685*rbind(humidity,humidity)^3.179,0.3),0.005)
K_gw   = 0.047*humidity^-0.0508
K_rout = 0.141*meanPET + 0.284

PARAMS_OL$Sgref    = Sgref   
PARAMS_OL$FdrainFC = FdrainFC 
PARAMS_OL$K_gw     = K_gw     
PARAMS_OL$K_rout   = K_rout   

## Initial states
STATES_Matong = initial_states(PARAMS = PARAMS_OL,N_GRID = N_GRID, meanP = meanP)

## Vectorised run of the model
OL_Matong <- awra_run_func(N_grid = N_GRID, ALL_FORCING = all_forcing_Matong,
                           STATES = STATES_Matong, PARAMS = PARAMS_OL)
     
# Compute the Qtot from 2015 to 2018, removing the spin-up in 2014
Qtot_Matong_OL_1518 <- apply(OL_Matong$QTOT[366:1826,],1,mean)  




                           
                     
