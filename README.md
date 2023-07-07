# bar-and-boxplots
##PLOT## (ONLY FP)
#organize the x axis based on the samples order
data_sum$Sample <- as.factor(data_sum$Sample)
data_sum$Sample <- factor(data_sum$Sample, levels = c("Cont-0", "A-3", "A-6", "A-12", "B-3", "B-6", "B-12", "AB-3", "AB-6", "AB-12"))


#BAR Plot
ggplot(data_sum, aes(x = Sample, y = mean , fill = Sample, colour = Sample)) +
  stat_summary(geom = "bar", fun = mean, width=0.75, colour = "black", alpha = 0.8, position = position_dodge2(preserve = "single")) +
  #stat_summary(aes(label=round(..y..,2)), fun=mean, geom="text", size=6, position = position_dodge(0.9)) #checking the numbers of the means 
  theme(axis.title.y = element_text(size=12, face="bold", color = "black"), 
        axis.title.x = element_text(size = 12, face = "bold", color = "black"),
        axis.text.x = element_text(size=11, color = "black"),
        axis.text.y = element_text(size=11, color = "black"),
        panel.background = element_rect(fill="grey98", colour = "black")) +
  labs(x = "Light (UV) -Time (h)", fill="Sample") +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), position = position_dodge(0.75), width = 0.15, color="black") + #Error bars
  labs(y= "Total digestible Protein (g/100 g DW)") +  #legends
  theme(legend.title = element_text(colour="black", size=11, 
                                    face="bold")) +
  theme(legend.text = element_text(colour="black", size=10, 
                                   face="bold")) +
  theme(legend.background = element_rect(fill="white",
                                         size=0.5, linetype="solid", 
                                         colour ="black")) +
  ylim(0,50) +
  facet_grid(.~Cultivar, labeller = label_both) +
  geom_signif(
    y_position = c(42, 47), xmin = c(1, 1), xmax = c(7, 10),
    annotation = c("p<0.0001", "p<0.002"), tip_length = 0, color = "black"
  ) +
  scale_fill_manual(values = c("seashell3", "darkslategray4", "lightsalmon", "burlywood1", "rosybrown", "ivory1", "darkseagreen", "lightblue3", "indianred3", "khaki3"))

##BOX PLOT##

FP1.plot$Sample <- as.factor(FP1.plot$Sample)
FP1.plot$Sample <- factor(FP1.plot$Sample, levels = c("Cont-0", "A-3", "A-6", "A-12", "B-3", "B-6", "B-12", "AB-3", "AB-6", "AB-12"))

#Need to add colors and legend, also, change the color of the axes. 
ggplot(FP1.plot) +
  aes(x = Sample, y = protein, fill=Sample) +
  geom_boxplot() +
  xlab(expression("Light")) +
  ylab(expression("Total digestible protein (g/100g DW)")) +
  facet_grid(.~Cultivar, labeller = label_both) +
  theme(axis.title.y = element_text(size=12, color = "black"), 
        axis.title.x = element_text(size = 12, color = "black"),
        #axis.text.x = element_text(size=10, color = "black"),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_text(size=10, color = "black"),
        panel.background = element_rect(fill="grey98", colour = "black")) +
  #labs(x = "Light (UV) -Time (h)", fill="Sample") +
  labs(x = NULL) +
  ylim(0,60) +
  facet_wrap(.~Cultivar, labeller = label_both) +
  theme(strip.background = element_rect(fill = "grey90")) +
  theme(strip.text = element_text(size= 11, face= "bold", color = "black")) +
  geom_signif(
    comparisons = list(c("Cont-0", "B-12"),
                       c("Cont-0", "AB-12")),
    map_signif_level = TRUE,
    y_position = c(42, 47),
    annotations = c("***", "**")) +
  theme(legend.position = "none") +
  scale_x_discrete(labels = c('Control','A-3', 'A-6', 'A-12', 'B-3', 'B-6', 'B-12', 'AB-3', 'AB-6', 'AB-12')) +
  scale_fill_manual(values = c("white", "white", "white", "white", "white", "white", "white", "white", "white", "white"))
  #theme(legend.title = element_text(colour="black", size=11, 
                                    #face="bold")) +
  #theme(legend.text = element_text(colour="black", size=10, 
                                   #face="bold")) +
  #theme(legend.background = element_rect(fill="white",
                                        # size=0.5, linetype="solid", 
                                         #colour ="black"))
