> head(SMAPCA_Diff_df,2)
      ID    KGE   Bias   NSE NSEsqrt NSElog              status
1 110003 -0.092 -0.222 0.060   0.087   0.05 High-impact-routing
2 121002 -0.003 -0.124 0.008  -0.182  -3.41  Low-impact-routing
  humidity lag1_SMAP_Q lag1_SsQsim humid_fac
1    1.025       0.542       0.692       Wet
2    0.456       0.316       0.594       Dry


## find the proportion of improved cases in different catchment conditions 

SMAPCA_Diff_df %>% group_by(humid_fac) %>%
    summarise(total = n(), log_improve = length(which(NSElog >0)))
    
    # A tibble: 2 × 3
  humid_fac total log_improve
  <fct>     <int>       <int>
1 Dry          14           3
2 Wet          18          16

# This summary of "16 out of 18 catchment gets improved in NSElof" is useful in writing. 
