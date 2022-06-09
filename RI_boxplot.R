# Coloured boxplot based on a factor level ### 

ggplot(data = Eff_catch_bind) + geom_boxplot(aes(x=status, y =NSElog),
                                fill=c("royalblue2", "firebrick2")) +
  theme_classic() + graph.theme.beta + 
  geom_hline(yintercept = 0, col = 'grey70', linetype = "dashed") +
  labs(x = "Routing Impact",y = "Efficiency (NSElog)")  
