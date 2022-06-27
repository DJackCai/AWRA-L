# Quick way to assign streamflow data frames from csv file, convert to zoo object 

library(zoo)
group_catname = c("401217", "405229","206014","143110","405264",
                  "403217", "403244", "408202","403222", "416305",
                  "422338", "121002", "130319", "403232", "401210",
                  "136301", "229650", "142001","401212",  "419053", "422319",
                  "137201", "238235", "606001", "926002","110003", "402206",
                  "405227", "405218", "237200", "497", "226222", "206025","401230")

for (i in 1:length(group_catname)) {
  
  catid = group_catname[i]
  Qobs_data = read.zoo(paste0("./Streamdata/Qobs_",catid,"_1518.csv"),header=T,
                         sep=",",index.column = 1)
  assign(paste0("Qobs_",catid),Qobs_data,envir = globalenv())
  
  
}
