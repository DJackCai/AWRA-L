## Two types of parameter assignment ## 

# 1) In sensitivity analysis, will change all the parameters, so input is a matrix of all parameters with parameter names 

PARAMS_ALL_assign <- function(II,Parasamp,STATIC_PAR=NULL){
  
  ## II: the 2 x NGRID "template" for each grid and two HRUs
  ## Parasamp: matrix with colnames for parameters 
  ## STATIC_PAR: optional to use spatial parameters derived for AWRA-L v6
  
  
  if (is.null(STATIC_PAR)) {
    X = Parasamp
    PARAMS = list(
      Nhru        = 2,
      Fhru        = II * c(0.5,0.5),
      # LAI 
      SLA         = II * c(X[,"SLA1"],X[,"SLA2"]),
      # FRACTIONAL VEGETATION COVER 
      LAIref      = II * c(X[,"LAIref1"],X[,"LAIref2"]),
      # saturated fraction - notice here Sgref is a N*1 vector, mimic the spatialise step
      Sgref       = II[1,] * c(X[,"Sgref"]),
      # storage at field capacity - wetness 
      S0FC        = II * c(30,30),
      SsFC        = II * c(200,200),
      SdFC        = II * c(1000,1000),
      # Fraction of day - Calculate E0 and other effective forcing
      fday        = II * c(0.5,0.5),    # 
      # Maximum transpiration
      Vc          = II * c(X[,"Vc1"],X[,"Vc2"]),
      # shortwave radiation balance
      alb_dry     = II * c(X[,"alb_dry1"],X[,"alb_dry2"]),
      alb_wet     = II * c(X[,"alb_wet1"],X[,"alb_wet2"]),
      w0ref_alb   = II * c(X[,"w0ref_alb1"],X[,"w0ref_alb2"]),
      
      # Groudn heat flux parameter
      Gfrac_max   = II * c(X[,"Gfrac_max1"],X[,"Gfrac_max2"]),     
      fvegref_G   = II * c(X[,"fvegref_G1"],X[,"fvegref_G2"]),
      
      # Aerodynamic conductance 
      hveg	    = II * c(X[,"hveg1"],X[,"hveg2"]),
      # Root uptake 
      Us0         = II * c(X[,"Us01"],X[,"Us02"]),   ## physiological maximum root water update 
      Ud0         = II * c(X[,"Ud01"],0),
      wslimU      = II * c(X[,"wslimU1"],X[,"wslimU2"]),    # uptake limiting relative water content
      wdlimU      = II * c(X[,"wdlimU1"],X[,"wdlimU2"]),
      # Canopy conductance 
      cGsmax      = II * c(X[,"cGsmax1"],X[,"cGsmax2"]),
      # soil evaporation
      FsoilEmax   = II * c(X[,"FsoilEmax1"],X[,"FsoilEmax2"]),
      w0limE      = II * c(X[,"w0limE1"],X[,"w0limE2"]),
      # Open water evaporation
      # FwaterE     = II * c(X[,"FwaterE1"],X[,"FwaterE2"]), 
      FwaterE     = II * c(0.7,0.7),  
      # Rainfall interception evaporation
      S_sls       = II * c(X[,"S_sls1"],X[,"S_sls2"]),
      ER_frac_ref = II * c(X[,"ER_frac_ref1"],X[,"ER_frac_ref2"]),
      
      # Surface runoff
      InitLoss    = II * c(X[,"InitLoss1"],X[,"InitLoss2"]),
      PrefR       = II * c(X[,"PrefR1"],X[,"PrefR2"]),
      
      # Drainage (S0,Ss,Sd)
      FdrainFC    = II * c(X[,"FdrainFC1"],X[,"FdrainFC2"]),
      beta        = II * c(X[,"beta1"],X[,"beta2"]),
      # Capillary rise 
      Fgw_conn    = II * c(1,1),
      # Routing for groundwater and streamflow discharge 
      K_gw        = X[,"K_gw"],      # gets replaced through spatialising step below
      K_rout      = X[,"K_rout"],      # gets replaced through spatialising step below
      
      # VEGETATION ADJUSTMENT FOR EQUILIBRIUM BIOMASS
      LAImax      = II * c(X[,"LAImax1"],X[,"LAImax2"]),
      Tgrow       = II * c(X[,"Tgrow1"],X[,"Tgrow2"]),
      Tsenc       = II * c(X[,"Tsenc1"],X[,"Tsenc2"]))  } else {
        
        if (is.list(STATIC_PAR)) {
          ftree_val <- STATIC_PAR$ftree
          S0FC_val <- STATIC_PAR$S0FC
          SsFC_val <- STATIC_PAR$SsFC
          SdFC_val <- STATIC_PAR$SdFC   } else if (is.matrix(STATIC_PAR)) {
            
            ftree_val <- STATIC_PAR[1,]
            S0FC_val <- STATIC_PAR[2,]
            SsFC_val <- STATIC_PAR[3,]
            SdFC_val <- STATIC_PAR[4,]
          } else if (is.vector(STATIC_PAR)) {
            ftree_val <- STATIC_PAR[1]
            S0FC_val <- STATIC_PAR[2]
            SsFC_val <- STATIC_PAR[3]
            SdFC_val <- STATIC_PAR[4]
            
          }
        
        ### substitute in the static parameters,
        X = Parasamp
        PARAMS = list(
          Nhru        = 2,
          Fhru        = II * c(ftree_val,1-ftree_val),
          # LAI 
          SLA         = II * c(X[,"SLA1"],X[,"SLA2"]),
          # FRACTIONAL VEGETATION COVER 
          LAIref      = II * c(X[,"LAIref1"],X[,"LAIref2"]),
          # saturated fraction - notice here Sgref is a N*1 vector, mimic the spatialise step
          Sgref       = II[1,] * c(X[,"Sgref"]),
          # storage at field capacity - wetness 
          S0FC        = II * c(S0FC_val,S0FC_val),
          SsFC        = II * c(SsFC_val,SsFC_val),
          SdFC        = II * c(SdFC_val,SdFC_val),
          # Fraction of day - Calculate E0 and other effective forcing
          fday        = II * c(0.5,0.5),    # 
          # Maximum transpiration
          Vc          = II * c(X[,"Vc1"],X[,"Vc2"]),
          # shortwave radiation balance
          alb_dry     = II * c(X[,"alb_dry1"],X[,"alb_dry2"]),
          alb_wet     = II * c(X[,"alb_wet1"],X[,"alb_wet2"]),
          w0ref_alb   = II * c(X[,"w0ref_alb1"],X[,"w0ref_alb2"]),
          
          # Groudn heat flux parameter
          Gfrac_max   = II * c(X[,"Gfrac_max1"],X[,"Gfrac_max2"]),     
          fvegref_G   = II * c(X[,"fvegref_G1"],X[,"fvegref_G2"]),
          
          # Aerodynamic conductance 
          hveg      = II * c(X[,"hveg1"],X[,"hveg2"]),
          # Root uptake 
          Us0         = II * c(X[,"Us01"],X[,"Us02"]),   ## physiological maximum root water update 
          Ud0         = II * c(X[,"Ud01"],0),
          wslimU      = II * c(X[,"wslimU1"],X[,"wslimU2"]),    # uptake limiting relative water content
          wdlimU      = II * c(X[,"wdlimU1"],X[,"wdlimU2"]),
          # Canopy conductance 
          cGsmax      = II * c(X[,"cGsmax1"],X[,"cGsmax2"]),
          # soil evaporation
          FsoilEmax   = II * c(X[,"FsoilEmax1"],X[,"FsoilEmax2"]),
          w0limE      = II * c(X[,"w0limE1"],X[,"w0limE2"]),
          # Open water evaporation
          # FwaterE     = II * c(X[,"FwaterE1"],X[,"FwaterE2"]), 
          FwaterE     = II * c(0.7,0.7),  
          # Rainfall interception evaporation
          S_sls       = II * c(X[,"S_sls1"],X[,"S_sls2"]),
          ER_frac_ref = II * c(X[,"ER_frac_ref1"],X[,"ER_frac_ref2"]),
          
          # Surface runoff
          InitLoss    = II * c(X[,"InitLoss1"],X[,"InitLoss2"]),
          PrefR       = II * c(X[,"PrefR1"],X[,"PrefR2"]),
          
          # Drainage (S0,Ss,Sd)
          FdrainFC    = II * c(X[,"FdrainFC1"],X[,"FdrainFC2"]),
          beta        = II * c(X[,"beta1"],X[,"beta2"]),
          # Capillary rise 
          Fgw_conn    = II * c(1,1),
          # Routing for groundwater and streamflow discharge 
          K_gw        = X[,"K_gw"],      # gets replaced through spatialising step below
          K_rout      = X[,"K_rout"],      # gets replaced through spatialising step below
          
          # VEGETATION ADJUSTMENT FOR EQUILIBRIUM BIOMASS
          LAImax      = II * c(X[,"LAImax1"],X[,"LAImax2"]),
          Tgrow       = II * c(X[,"Tgrow1"],X[,"Tgrow2"]),
          Tsenc       = II * c(X[,"Tsenc1"],X[,"Tsenc2"])) 
        
        
      }
  
  
  return(PARAMS)
  
}


### 2) set default parameters and then add the parameters to be optimised 

PARAMS_set <- function(II,STATIC_PAR =NULL){
  ## II: the 2 x NGRID "template" for each grid and two HRUs
  ## STATIC_PAR: list object of static parameters 
  # ftree, 3 parameters for SzFC, and meanPET
  
  if (is.null(STATIC_PAR)) {
    PARAMS = list(
      Nhru        = 2,
      Fhru        = II * c(0.5,0.5),
      # LAI 
      SLA         = II * c(3,10),
      # FRACTIONAL VEGETATION COVER 
      LAIref      = II * c(2.5,1.4),
      # saturated fraction
      Sgref       = II * c(20,20),
      # storage at field capacity - wetness 
      S0FC        = II * c(30,30),
      SsFC        = II * c(200,200),
      SdFC        = II * c(1000,1000),
      # Fraction of day - Calculate E0 and other effective forcing
      fday        = II * c(0.5,0.5),    # 
      # Maximum transpiration
      Vc          = II * c(0.35,0.65),
      # shortwave radiation balance
      alb_dry     = II * c(0.26,0.26),
      alb_wet     = II * c(0.16,0.16),
      w0ref_alb   = II * c(0.3,0.3),
      
      # Groudn heat flux parameter
      Gfrac_max   = II * c(0.3,0.3),     
      fvegref_G   = II * c(0.22,0.22),
      
      # Aerodynamic conductance 
      hveg	    = II * c(10,0.5),
      # Root uptake 
      Us0         = II * c(6,6),   ## physiological maximum root water update 
      Ud0         = II * c(4,0),
      wslimU      = II * c(0.3,0.3),    # uptake limiting relative water content
      wdlimU      = II * c(0.3,0.3),
      # Canopy conductance 
      cGsmax      = II * c(0.03,0.03),
      # soil evaporation
      FsoilEmax   = II * c(0.2,0.5),
      w0limE      = II * c(0.85,0.85),
      # Open water evaporation
      FwaterE     = II * c(0.7,0.7), 
      
      # Rainfall interception evaporation
      S_sls       = II * c(0.1,0.1),
      ER_frac_ref = II * c(0.2,0.05),
      
      # Surface runoff
      InitLoss    = II * c(5,5),
      PrefR       = II * c(150,150),
      
      # Drainage (S0,Ss,Sd)
      FdrainFC    = II * c(0.029,0.029),
      beta        = II * c(4.5,4.5),
      # Capillary rise 
      Fgw_conn    = II * c(1,1),
      # Routing for groundwater and streamflow discharge 
      K_gw        = 0.06,      # gets replaced through spatialising step below
      K_rout      = 0.77,      # gets replaced through spatialising step below
      
      # VEGETATION ADJUSTMENT FOR EQUILIBRIUM BIOMASS
      LAImax      = II * c(8,8),
      Tgrow       = II * c(1000,150),
      Tsenc       = II * c(60,10)) }     else {
        
        ### If static parameters for all grids are supplied #### 
        if (is.list(STATIC_PAR)) {
          ftree_val <- STATIC_PAR$ftree
          S0FC_val <- STATIC_PAR$S0FC
          SsFC_val <- STATIC_PAR$SsFC
          SdFC_val <- STATIC_PAR$SdFC   } else if (is.matrix(STATIC_PAR)) {
            
            ftree_val <- STATIC_PAR[1,]
            S0FC_val <- STATIC_PAR[2,]
            SsFC_val <- STATIC_PAR[3,]
            SdFC_val <- STATIC_PAR[4,]
          }
        
        PARAMS = list(
          Nhru        = 2,
          Fhru        = rbind(ftree_val,1-ftree_val), 
          # LAI 
          SLA         = II * c(3,10),
          # FRACTIONAL VEGETATION COVER 
          LAIref      = II * c(2.5,1.4),
          # saturated fraction
          Sgref       = II * c(20,20),
          # storage at field capacity - wetness 
          S0FC        = rbind(S0FC_val,S0FC_val),
          SsFC        = rbind(SsFC_val,SsFC_val),
          SdFC        = rbind(SdFC_val,SdFC_val),
          # Fraction of day - Calculate E0 and other effective forcing
          fday        = II * c(0.5,0.5),    # 
          # Maximum transpiration
          Vc          = II * c(0.35,0.65),
          # shortwave radiation balance
          alb_dry     = II * c(0.26,0.26),
          alb_wet     = II * c(0.16,0.16),
          w0ref_alb   = II * c(0.3,0.3),
          
          # Groudn heat flux parameter
          Gfrac_max   = II * c(0.3,0.3),     
          fvegref_G   = II * c(0.22,0.22),
          
          # Aerodynamic conductance 
          hveg	    = II * c(10,0.5),
          # Root uptake 
          Us0         = II * c(6,6),   ## physiological maximum root water update 
          Ud0         = II * c(4,0),
          wslimU      = II * c(0.3,0.3),    # uptake limiting relative water content
          wdlimU      = II * c(0.3,0.3),
          # Canopy conductance 
          cGsmax      = II * c(0.03,0.03),
          # soil evaporation
          FsoilEmax   = II * c(0.2,0.5),
          w0limE      = II * c(0.85,0.85),
          # Open water evaporation
          FwaterE     = II * c(0.7,0.7), 
          
          # Rainfall interception evaporation
          S_sls       = II * c(0.1,0.1),
          ER_frac_ref = II * c(0.2,0.05),
          
          # Surface runoff
          InitLoss    = II * c(5,5),
          PrefR       = II * c(150,150),
          
          # Drainage (S0,Ss,Sd)
          FdrainFC    = II * c(0.029,0.029),
          beta        = II * c(4.5,4.5),
          # Capillary rise 
          Fgw_conn    = II * c(1,1),
          # Routing for groundwater and streamflow discharge 
          K_gw        = 0.06,      # gets replaced through spatialising step below
          K_rout      = 0.77,      # gets replaced through spatialising step below
          
          # VEGETATION ADJUSTMENT FOR EQUILIBRIUM BIOMASS
          LAImax      = II * c(8,8),
          Tgrow       = II * c(1000,150),
          Tsenc       = II * c(60,10))
        
      }
  
  
  return(PARAMS)
  
}


#### Scenario 3 
