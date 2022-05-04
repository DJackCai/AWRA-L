### xyplot with rainfall and streamflow

## inverted rainfall column charts
par(xaxs="i",yaxs="i",mar=c(5,4,4,4.3),mgp=c(2.5,1,0))
plot(dates_seq(2015,2018),all_forcing_GB$PG[,11],type="h",
     ylim=c(max(all_forcing_GB$PG[,11])*1.7,0),lwd=1.2,
     xlab=NA, ylab=NA,col="blue",lend="square",axes=F)
axis(4)
mtext("Rainfall (mm/day)",side=4,outer=T,line=-2)
par(new=T)


### streamflow time series

plot(DateSeq(2015,2018),Qobs_data$Q,
     main="Goulburn River catchment (405264)",
     xlab="Year",  ylab="Streamflow (mm/day)",axes=T,type='n')

lines(DateSeq(2015,2018),Qtot_GB_OL_1518,type="l",lwd=1,col="cyan",lty=1). # Open loop model 
lines(DateSeq(2015,2018),Qtot1_GB_EFSM_T7_1518,type="l",lwd=0.9,col="red",lty=1) # calibrated model 

lines(DateSeq(2015,2018),Qobs_data$Q,type="l",col="black",lwd=1.4,lty=2). ## observed hydrograph 
    
