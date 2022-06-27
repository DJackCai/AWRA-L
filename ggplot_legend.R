### Example codes to demonstrate how to specify legend components when there are different plot types (box, points,lines)
### with different colours to be specified 

ggplot(Eff_catch_bind) + geom_point(aes(x= lag1_SMAP_Q, y = NSE,col = "SMAP"), shape =1) +theme_classic() + 
  graph.theme.beta + theme(plot.title = element_text(hjust =  0.5)) + 
  geom_smooth(data = Eff_catch_bind%>%filter(NSE>-1),
              aes(x=lag1_SMAP_Q, y=NSE), method = "lm",se = F, col = "black",size = 0.5) +
  geom_hline(yintercept = 0, col = 'grey70', linetype = "dashed") +
  geom_point(aes(x=lag1_SsQsim, y=NSE, col = "AWRA Ss"),shape = 1) + 
  geom_smooth(data = Eff_catch_bind%>%filter(NSE>-1),
              aes(x=lag1_SsQsim, y=NSE), method = "lm",se = F, size = 0.5, col = "red") + 
  
  geom_point(aes(x=lag1_S0Qsim, y=NSE, col = "AWRA S0"),shape = 1) + 
  geom_smooth(data = Eff_catch_bind%>%filter(NSE>-1),
              aes(x=lag1_S0Qsim, y=NSE), method = "lm",se = F, size = 0.5, col = "royalblue2") + 
  
  scale_colour_manual(name = "SM state", values = c("AWRA Ss" = "red", "SMAP" = "black", "AWRA S0" = "royalblue2")) +
  guides(col = guide_legend(title = "SM state",title.hjust = 0.5))  +
  labs(x = "Lag R (SM and Q)",y = "Efficiency (NSE)") + scale_y_continuous(limits=c(-0.3,0.4))
  
  
 ### In the same data frame & colour specified by a factor variable: use scale_fill_manual to achieve the legend adjustment 
 
 ggplot(data = Fdrain_df_melt, aes(ID, value, fill = Case )) + 
  stat_boxplot(geom = 'errorbar', width = 0.2, coef = 1.5,position = position_dodge(0.75)) + 
  geom_boxplot() + theme_classic() + graph.theme.beta +
  labs(x = "Catchment",y = expression(FdrainFC[1])) + 
  # guides(fill = guide_legend(label = c("Uncalibrated","SMAP- \nCalibrated")))
 ### specify title, specific colours and labels appeared on the legend
  scale_fill_manual(name = "Scenario",values = c("cyan","orange"),label = c("Uncalibrated","SMAP- \nCalibrated"))
  
  

#### note1 : increase the spacing between legend labels ##### 

ggplot(mtcars, aes(y = factor(cyl), fill = factor(cyl))) + 
  geom_bar() +
  ## specify spacing
  theme(legend.spacing.y = unit(1.0, 'cm'))  +
  ## important additional element to make sure it goes from top to bottom and thus increase the space vertically
  guides(fill = guide_legend(byrow = TRUE))

## note2: can adjust the legend title by matching aesthetics in "labs()"
## example: https://stackoverflow.com/questions/23635662/editing-legend-text-labels-in-ggplot

ggplot(mtcars, aes(x=mpg, y=disp, size=hp, 
                   col=as.factor(cyl), shape=as.factor(gear))) +
  geom_point() +
## correspondingly specify legend titles for size, colour and shape
  labs(x="miles per gallon", y="displacement", size="horsepower", 
       col="# of cylinders", shape="# of gears")

#### 27/Jun/2022: Add legend to the raster in base plot ###
# reference: https://stackoverflow.com/questions/9436947/legend-properties-when-legend-only-t-raster-package
# key: the use of legend.args - line: vertically, adj: horizontally

plot(delta_r,xlab = "Longitude",  ylab = "Latitude", legend.args = list(text = expression(delta),line = 0.5, 
                        adj = 0.2, cex = 1.4))


