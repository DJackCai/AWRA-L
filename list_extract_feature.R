
## 1) list of list, each list is a summary of catchment conditions 
## 2) extract specific features for each catchment and colalte them into a data frame 

catch_feat_G1 = c("area", "catch_humidity", "catch_ftree", "catch_annual_meanP",
                  "catch_medQ","catch_meanQ", "ROC_daily_mean", 
                  "SM_mean_catch", "SM_sd_catch")
          
# here although the data frame is in correct form, the columsn are in list and cannot be properly written by write.csv()

catch_feat_temp = do.call(rbind, lapply(catch_info_list,function(x) {return(x[catch_feat_G1])}))%>%as.data.frame()
  
### after collating to data frame. unlist each column and then combine to data frame 
  
  catch_feat_df = NULL
  for (i in 1:length(catch_feat_G1)) {
    feat = unlist(catch_feat_temp[[i]])
    catch_feat_df = data.frame(cbind(catch_feat_df, feat))
  }
  
  
