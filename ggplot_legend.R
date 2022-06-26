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
  
  
