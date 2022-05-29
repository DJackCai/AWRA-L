### xyplot rainfall and streamflow ##### 

rainfall_plot = function(rain_ts, scale_fac ) {
  
  par(xaxs="i",yaxs="i",mar=c(5,4,4,4.3),mgp=c(2.5,1,0))
  plot(dates_seq(2015,2018),rain_ts,type="h",
       ylim=c(max(rain_ts)*scale_fac,0),lwd=1.2,
       xlab=NA, ylab=NA,col="blue",lend="square",axes=F)
  axis(4)
  mtext("Rainfall (mm/day)",side=4,outer=T,line=-2)
  par(new=T)
  
}

rainfall_plot(rain_ts= apply(all_forcing_401212$PG[-c(1:365),],1,mean)[366:(366+365)], 
              scale_fac = 1.8)

plot(DateSeq(2016,2016), Logflow(Qobs_401212$Q)[366:(366+365)],
     main = "Nariel Creek catchment (401212)",
     xlab = "Date",col="black",lwd = 2.3,lty = 2,ylim =  c(-3,4),
     ylab = "Streamflow (log scale)",axes = T, type="l")

lines(DateSeq(2016,2016), Logflow(Qtot_401212_wet_mean5)[366:(366+365)], type="l",lwd=1.2,col="orange",lty=1)
lines(DateSeq(2016,2016), Logflow(Qtot_401212_OL_1518)[366:(366+365)], type="l",lwd=1.3,col="grey70",lty=1)
legend("topleft",c("observed","OL","EnKF"), 
       col = c("black","grey70","orange"), 
       lty = c(2,1,1), lwd = c(1.8,1,1))
