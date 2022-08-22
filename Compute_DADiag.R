#### Various internal diagnostics of a data assimiation system ######

# useful reference: Reichle, R. H., De Lannoy, G. J. M., Liu, Q., Koster, R. D., Kimball, J. S., Crow, W. T., Ardizzone, J. V., Chakraborty, P., Collins, D. W., Conaty, A. L., Girotto, M., Jones, L. A., Kolassa, J., Lievens, H., Lucchesi, R. A. and Smith, E. B., 2017. Global Assessment of the SMAP Level-4 Surface and Root-Zone Soil Moisture Product Using Assimilation Diagnostics, Journal of Hydrometeorology, 18(12): 3217-3237. 


file_compute_Inno =  function(dir,catname) {
  
  ## read all different files 
  Innov_f = read.csv(paste0(dir, catname, "_Innov_f.csv"))
  Innov_f[Innov_f <= -1000] = NA
  Innov_a = read.csv(paste0(dir, catname, "_Innov_a.csv"))
  Innov_a[Innov_a <= -1000] = NA
  
  HPfHT_all = read.csv(paste0(dir, catname, "_HPfHT_mat.csv"))
  R_Err = as.matrix(read.table(paste0(dir, catname, "_SMAP_Err.txt"),header = T))
  
  S01_AI = read.csv(paste0(dir, catname, "_Increment_S01.csv"))
  S02_AI = read.csv(paste0(dir, catname, "_Increment_S02.csv"))
  Ss1_AI = read.csv(paste0(dir, catname, "_Increment_Ss1.csv"))
  Ss2_AI = read.csv(paste0(dir, catname, "_Increment_Ss2.csv"))
  
  # compute forecast innovation
  N_GRID = ncol(Innov_f)
  SD_Inno_f  = SD_Inno_a = rep(0, N_GRID)
  
  # temmporal mean of forecast innovation (expected to be unbiased)
  Mean_Inno = apply(Innov_f, 2, mean, na.rm=T)
  
  # temporal mean of analysis increment (expected to be unbiased)
  Mean_S01_AI = apply(S01_AI, 2, mean, na.rm=T)
  Mean_S02_AI = apply(S02_AI, 2, mean, na.rm=T)
  Mean_Ss1_AI = apply(Ss1_AI, 2, mean, na.rm=T)
  Mean_Ss2_AI = apply(Ss2_AI, 2, mean, na.rm=T)
  Mean_AI_mat = rbind(Mean_S01_AI, Mean_S02_AI, Mean_Ss1_AI, Mean_Ss2_AI)
  
  # temporal standard deviation (HPfHT + R) #### 
  for (i in 1:N_GRID) {
    HPfHT = HPfHT_all[,i]
    Rt = (R_Err[i] **2)
    # Normalised innovation
    stopifnot(length(Innov_f[,i])==length(HPfHT))
    Norm_Inno_f = Innov_f[,i]/(sqrt(HPfHT +Rt ))
    SD_Inno_f[i] = sd(Norm_Inno_f,na.rm=T)
    
    Norm_Inno_a = Innov_a[,i]/(sqrt(HPfHT +Rt ))
    SD_Inno_a[i] = sd(Norm_Inno_a,na.rm=T)
    
  }
  
  return( list(Mean_Inno = Mean_Inno, SD_Inno_f = SD_Inno_f, 
               SD_Inno_a = SD_Inno_a, 
               Catch_Mean = mean(Mean_Inno,na.rm=T), Catch_SD = mean(SD_Inno_f, na.rm=T), 
               Mean_AI_mat = Mean_AI_mat ))
               
}
