

### Generate plots ###

# ---- Figure 1: CH4 emissions measured and estimated ----
#Section 1
S5_CH4<-ggplot()+geom_point(aes(dt_CH45$time,dt_CH45$CH4_slurry_pig,color="Measured"))+
  geom_line(aes(dt_CH45$time,dt_CH45$CH4_slurry_pig),color="grey80",size=0.3)+
  geom_line(aes(dt_CH45$time,dt_CH45$CH4_A_pig,color="Predicted: Arrhenius lnA = 31.3"))+
  geom_line(aes(dt_CH45$time,dt_CH45$CH4_A_lnA_pig,color="Predicted: Arrhenius lnA' = 30.3"))+
  geom_line(aes(dt_CH45$time,dt_CH45$CH4_ABM_pig,color="Predicted: ABM"))+
  geom_point(aes(MPR_S5$time,MPR_S5$Rate_S5_Max,color="MPR"),size=2,shape=17)+
  geom_line(aes(dt_CH45$time,dt_CH45$A_default,color="Predicted: Danish NIR"),linetype=2)+
  theme_bw()+xlab("Time, days")+ylab(expression("Emissions, g pig"^-1*" day"^-1*""))+
  theme(legend.position = "none") +
  scale_color_manual("",values=c("black","grey","green","blue","cyan","red","blue"))+
  ggtitle("a) Section 1")+  theme(panel.grid = element_blank())+
  geom_point(aes(dt_CH4_rate_S5$time,dt_CH4_rate_S5$Rate_S5_Max,color="MPR"),size=2,shape=17)+
  geom_rect(aes(xmin=0, xmax=77, ymin=-Inf, ymax=Inf,fill="Batch 1"), alpha = 0.09, colour = NA)+
  geom_rect(aes(xmin=95, xmax=190, ymin=-Inf, ymax=Inf,fill="Batch 2"), alpha = 0.09, colour = NA)+
  geom_rect(aes(xmin=197, xmax=276, ymin=-Inf, ymax=Inf,fill="Batch 3"), alpha = 0.09, colour = NA)+
  geom_rect(aes(xmin=281, xmax=358, ymin=-Inf, ymax=Inf,fill="Batch 4"), alpha = 0.09, colour = NA)+
  theme(plot.title = element_text(size = 9))+
  xlim(c(0,276))+
  scale_fill_manual('',values = c('grey1','grey30',"grey60","grey80"))

#Section 2
S6_CH4<-ggplot()+geom_point(aes(dt_CH46$time,dt_CH46$CH4_slurry_pig,color="Measured"))+
  geom_line(aes(dt_CH46$time,dt_CH46$CH4_slurry_pig),color="grey80",size=0.3)+
  geom_line(aes(dt_CH46$time,dt_CH46$CH4_A_pig,color="Predicted: Arrhenius lnA = 31.3"))+
  geom_line(aes(dt_CH46$time,dt_CH46$CH4_A_lnA_pig,color="Predicted: Arrhenius lnA' = 30.3"))+
  geom_line(aes(dt_CH46$time,dt_CH46$CH4_ABM_pig,color="Predicted: ABM "))+
  geom_point(aes(MPR_S6$time,MPR_S6$Rate_S6_Max,color="MPR"),size=2,shape=17)+
  geom_line(aes(dt_CH46$time,dt_CH46$A_default,color="Predicted: Danish NIR"),linetype=2)+
  ggtitle("b) Section 2")+
  theme_bw()+xlab("Time, days")+ylab(expression("Emissions, g pig"^-1*" day"^-1*""))+
  # theme(legend.position = "none") +
  scale_color_manual("",values=c("black","grey","green","blue","cyan","red","blue"))+
  #scale_color_manual("",values=c("black","red","green","blue","red","darkgreen","blue"))+
  theme(panel.grid = element_blank())+
  geom_rect(aes(xmin=0, xmax=75, ymin=-Inf, ymax=Inf,fill="Batch 1"), alpha = 0.09, colour = NA)+
  geom_rect(aes(xmin=96, xmax=180, ymin=-Inf, ymax=Inf,fill="Batch 2"), alpha = 0.09, colour = NA)+
  geom_rect(aes(xmin=187, xmax=281, ymin=-Inf, ymax=Inf,fill="Batch 3"), alpha = 0.09, colour = NA)+
  theme(plot.title = element_text(size = 9))+
  xlim(c(0,290))+
  scale_fill_manual('',values = c('grey1','grey30',"grey60","grey80"))

legend <- get_legend(S6_CH4)
S5_CH4 <- S5_CH4 +  theme(legend.position = "none") 
S6_CH4 <- S6_CH4 +  theme(legend.position = "none") 


group1 <- plot_grid(S5_CH4,S6_CH4, ncol = 1, align = "hv")
Fig1<-plot_grid(group1,legend,rel_widths = c(1, .5))
ggsave(plot=Fig1, filename= here("plots","Figure1.png"), width=9, height=5, bg="white", dpi=500)

# ---- Figure 2: Microbial analysis ----

DNA<-ggplot(datlong, aes(widths, relative, fill = Genus)) +
  geom_col() +
  scale_x_continuous(breaks = c(1,2,3.5,4.5,6.5,8,9,11,12.5,14.5,16),
                     labels = c("","","", "",
                                "","", "",
                                "","",
                                "",""))+
  theme_bw() +
  xlab("") +
  ylab(expression("Relative mcrA gene abundance (%)")) +
  ylim(c(0,130))+
  geom_vline(xintercept = c(2.75,7.25,11.75,15.25), linetype = "dotted", color = "grey")+
  geom_vline(xintercept = c(5.5,10,13.5), linetype = "dashed", color = "black")+
  geom_text(x=1.5, y=108, label="Before flush",size=4.5,color="grey30")+
  geom_text(x=4, y=108, label="After flush",size=4.5,color="grey30")+
  geom_text(x=6.35, y=108, label="Before flush",size=4.5,color="grey30")+
  geom_text(x=8.5, y=108, label="After flush",size=4.5,color="grey30")+
  geom_text(x=10.85, y=108, label="Before flush",size=4.5,color="grey30")+
  geom_text(x=12.5, y=108, label="After flush",size=4.5,color="grey30")+
  geom_text(x=14.35, y=108, label="Before flush",size=4.5,color="grey30")+
  geom_text(x=16, y=108, label="After flush",size=4.5,color="grey30")+
  geom_text(x=2.75, y=125, label="Day 0",size=5,color="black")+
  geom_text(x=7.75, y=125, label="Day 35",size=5,color="black")+
  geom_text(x=11.75, y=125, label="Day 63",size=5,color="black")+
  geom_text(x=15.25, y=125, label="Day 77",size=5,color="black")+
  scale_fill_manual("Genus",values = c("grey","#9590FF","#00BFC4","#FF62BC","#E76BF3","#39B600","#F8766D","#00B0F6","#D89000","#A3A500"),
                    labels = c("Unidentified","Candidatus_Methanomethylophilus", "organism_methanogenic",  
                               "Methanospirillum","Methanomassiliicoccales", "Methanoculleus", 
                               "Methanocorpusculum","Methanosaeta", 
                               "Methanobrevibacter","Methanosphaera"))+
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(legend.position = c("none"))+
  theme(axis.text = element_text(size = 14),  # Increase size of tick labels
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 14))+
  ggtitle("b) DNA analysis")

Prot<-ggplot(df_Activity, aes(widths, relative, fill = Genus)) +
  geom_col() +
  scale_x_continuous(breaks = c(1,2,3.5,4.5,6.5,8,9,11,12.5,14.5,16), 
                     labels = c("Solid residue","Bulk slurry","Bulk slurry", "Solid residue",  
                                "Bulk slurry","Bulk slurry", "Liquid residue", 
                                "Bulk slurry","Bulk slurry", 
                                "Bulk slurry","Bulk slurry"))+
  theme_bw() +
  xlab("") +
  ylab(expression("Relative methanogenic activity (%)")) +
  ylim(c(0,130))+
  geom_vline(xintercept = c(2.75,7.25,11.75,15.25), linetype = "dotted", color = "grey")+
  geom_vline(xintercept = c(5.5,10,13.5), linetype = "dashed", color = "black")+
  geom_text(x=1.5, y=108, label="Before flush",size=4.5,color="grey30")+
  geom_text(x=4, y=108, label="After flush",size=4.5,color="grey30")+
  geom_text(x=6.36, y=108, label="Before flush",size=4.5,color="grey30")+
  geom_text(x=8.5, y=108, label="After flush",size=4.5,color="grey30")+
  geom_text(x=10.85, y=108, label="Before flush",size=4.5,color="grey30")+
  geom_text(x=12.5, y=108, label="After flush",size=4.5,color="grey30")+
  geom_text(x=14.35, y=108, label="Before flush",size=4.5,color="grey30")+
  geom_text(x=16, y=108, label="After flush",size=4.5,color="grey30")+
  geom_text(x=2.75, y=125, label="Day 0",size=5,color="black")+
  geom_text(x=7.75, y=125, label="Day 35",size=5,color="black")+
  geom_text(x=11.75, y=125, label="Day 63",size=5,color="black")+
  geom_text(x=15.25, y=125, label="Day 77",size=5,color="black")+
  scale_fill_manual("",values = c("grey","#00B0F6","#A3A500","pink","blue","yellow","#FF62BC","orange","coral2","purple",
                                  "#D89000","brown","#39B600"),
                    labels = c("Rest","Methanosaeta", "Methanosphaera","Methanoplanus", "Methanocaldococcus",  
                               "Methanobacterium","Methanospirillum", "Methanofollis", 
                               "Methanohalophilus","Methanolobus", 
                               "Methanobrevibacter","Methanosarcina","Methanoculleus"))+
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(legend.position = c("none"))+
  theme(axis.text = element_text(size = 14),  # Increase size of tick labels
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 14))+
  ggtitle("c) Protein analysis")

##Legends
Common<-data.frame(x=c(1,2,3,4,5,6),y=c(5,5,5,5,5,5),Genus = c("Methanospirillum","Methanoculleus",
                                                               "Methanocorpusculum","Methanobrevibacter","Methanosphaera","Methanosaeta"))

df_DNA<-data.frame(x=c(1,2,3,4),y=c(5,5,5,5),Genus = c("Unidentified","Candidatus_Methanomethylophilus",
                                                       "organism_methanogenic","Methanomassiliicoccales"
))
df_Prot<-data.frame(x=c(1,2,3,4,5,6,7,8),y=c(5,5,5,5,5,5,5,5),Genus = c("Rest","Methanoplanus", "Methanocaldococcus",  
                                                                        "Methanobacterium", "Methanofollis", 
                                                                        "Methanohalophilus","Methanolobus", 
                                                                        "Methanosarcina"))
Common_legend<-ggplot(Common, aes(x = x, y = y, fill = Genus))+
  geom_col() +  scale_fill_manual("Common Genus",
                                  values = c("#FF62BC", "#39B600", "#F8766D", "#D89000", "#A3A500","#00B0F6"),
                                  labels = c("Methanospirillum", "Methanoculleus", 
                                             "Methanocorpusculum", "Methanobrevibacter", "Methanosphaera","Methanosaeta")) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14,face = "bold"))+
  guides(fill = guide_legend(nrow = 2)) 


DNA_legend<-ggplot(df_DNA,aes(x,y,fill=Genus))+geom_col()+
  scale_fill_manual("Only DNA detected Genus",values = c("grey","#9590FF","#00BFC4","#E76BF3"),
                    labels = c("Unidentified","Candidatus_Methanomethylophilus", 
                               "organism_methanogenic","Methanomassiliicoccales"))+
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14,face = "bold"))+
  guides(fill = guide_legend(nrow = 2)) 

Prot_legend<-ggplot(df_Prot,aes(x,y,fill=Genus))+geom_col()+
  scale_fill_manual("only protein detected Genus",values = c("grey","pink","blue","yellow","orange","coral2","purple",
                                                             "brown"),
                    labels = c("Rest","Methanoplanus", "Methanocaldococcus",  
                               "Methanobacterium", "Methanofollis", 
                               "Methanohalophilus","Methanolobus", 
                               "Methanosarcina"))+
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14,face = "bold"))+
  guides(fill = guide_legend(nrow = 3)) 

qPCR<-ggplot(dat_qPCR, aes(widths, `Cell count mcrA`/0.1)) +
  geom_col() + geom_line(data=Batch4,aes(Breaks,Mean_CH4/Volume*scale_factor))+
  geom_point(data=Batch4,aes(Breaks,Mean_CH4/Volume*scale_factor))+
  geom_point(data=Batch4,aes(Breaks,(Mean_CH4 - sd_CH4)/ Volume * scale_factor),shape=45,size=7)+
  geom_point(data=Batch4,aes(Breaks,(Mean_CH4 + sd_CH4)/ Volume * scale_factor),shape=45,size=7)+
  geom_segment(data=Batch4,aes(x = Breaks, xend = Breaks, 
                               y = (Mean_CH4 - sd_CH4)/ Volume * scale_factor, 
                               yend = (Mean_CH4 + sd_CH4)/ Volume * scale_factor), linetype = "solid",size=0.5) +
  
  scale_x_continuous(breaks = c(1,2,3.5,4.5,6.5,8,9,11,12.5,14.5,16),
                     labels = c("","","", "",
                                "","", "",
                                "","",
                                "",""))+
  # labels = c("Solid residue","Bulk slurry","Bulk slurry", "Solid residue",
  #            "Bulk slurry","Bulk slurry", "Liquid residue",
  #            "Bulk slurry","Bulk slurry",
  #            "Bulk slurry","Bulk slurry"))+
  theme_bw() +
  xlab("") +
  #ylab(expression("mcrA copies mL"^-1)) +
  scale_y_continuous(labels = label_scientific(),
                     name = expression("mcrA copies mL"^-1), 
                     sec.axis = sec_axis(~ . / scale_factor, name = expression("CH"[4]* " Emissions, g day"^-1*" m"^-3*""))
  )+
  
  coord_cartesian(ylim = c(0,454677922*12))+
  geom_vline(xintercept = c(2.75,7.25,11.75,15.25), linetype = "dotted", color = "grey")+
  geom_vline(xintercept = c(5.5,10,13.5), linetype = "dashed", color = "black")+
  geom_text(x=1.5, y=384677922*12, label="Before flush",size=4.5,color="grey30")+
  geom_text(x=4, y=384677922*12, label="After flush",size=4.5,color="grey30")+
  geom_text(x=6.35, y=384677922*12, label="Before flush",size=4.5,color="grey30")+
  geom_text(x=8.5, y=384677922*12, label="After flush",size=4.5,color="grey30")+
  geom_text(x=10.85, y=384677922*12, label="Before flush",size=4.5,color="grey30")+
  geom_text(x=12.5, y=384677922*12, label="After flush",size=4.5,color="grey30")+
  geom_text(x=14.35, y=384677922*12, label="Before flush",size=4.5,color="grey30")+
  geom_text(x=16, y=384677922*12, label="After flush",size=4.5,color="grey30")+
  geom_text(x=2.75, y=444677922*12, label="Day 0",size=5,color="black")+
  geom_text(x=7.75, y=444677922*12, label="Day 35",size=5,color="black")+
  geom_text(x=11.75, y=444677922*12, label="Day 63",size=5,color="black")+
  geom_text(x=15.25, y=444677922*12, label="Day 77",size=5,color="black")+
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(axis.text = element_text(size = 14),  # Increase size of tick labels
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 14))+
  ggtitle(expression("a) qPCR and CH"[4]*" Emissions"))

## Join activity and presence
# Function to split labels into multiple lines
split_labels <- function(labels, num_lines) {
  split_indices <- seq(1, length(labels), length.out = num_lines + 1)
  lapply(1:num_lines, function(i) {
    start <- split_indices[i]
    end <- split_indices[i + 1] - 1
    paste(labels[start:end], collapse = "\n")
  })
}

qPCR <-qPCR+theme(plot.margin = margin(0, 0, -10, 0),  # Adjust vertical margins
                  axis.title.x = element_blank(),
                  axis.text.x = element_blank())


DNA <-DNA+theme(plot.margin = margin(0, 0, -100, 0),  # Adjust vertical margins
                axis.title.x = element_blank(),
                axis.text.x = element_blank())

Prot <-Prot+theme(plot.margin = margin(0, 0, 0, 0))


grobs <- ggplotGrob(Common_legend)$grobs
legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]
grobs <- ggplotGrob(DNA_legend)$grobs
legend_DNA <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]
grobs <- ggplotGrob(Prot_legend)$grobs
legend_Prot <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]

Fig2pre <- plot_grid(qPCR,DNA,Prot, ncol = 1, align = "hv",rel_heights = c(1, 1))
Fig2pre2 <- plot_grid(Fig2pre, legend, nrow = 2, align = "v", axis = "b",rel_heights = c(1,0.1))
Fig2pre3 <- plot_grid(Fig2pre2, legend_DNA, nrow = 2, align = "v", axis = "b",rel_heights = c(1,0.1))
Fig2 <- plot_grid(Fig2pre3, legend_Prot, nrow = 2, align = "v", axis = "b",rel_heights = c(1,0.1))
ggsave(plot=Fig2,here("plots","Figure2.png"), width = 12, height = 15,bg='white')

# ---- CRDS memory effects ----
FigS1<-ggplot()+geom_point(aes(Dat$date.time[1e6:1.005e6],Dat$CH4_dry[1e6:1.005e6],color="discarded points"),shape=1)+
  geom_point(aes(Dat$date.time[1e6:1.005e6],Dat$MPVPosition[1e6:1.005e6]*5,
                 color="valve position"),shape=3,size=0.3)+theme_bw()+
  theme(panel.grid=element_blank())+
  xlab("")+
  scale_y_continuous(expression("CH"[4]*" concentration, ppm"),
                     sec.axis = sec_axis(~ . /5, name = expression("valve position")))+
  theme(legend.title = element_blank())+
  geom_point(aes(List_BGt[[313]]$date.time,List_BGt[[313]]$CH4_dry,color="included points"),shape=1)+
  geom_point(aes(List_S5[[316]]$date.time,List_S5[[316]]$CH4_dry,color="included points"),shape=1)+
  geom_point(aes(List_S5[[315]]$date.time,List_S5[[315]]$CH4_dry,color="included points"),shape=1)+
  geom_point(aes(List_S6[[314]]$date.time,List_S6[[314]]$CH4_dry,color="included points"),shape=1)+
  geom_point(aes(List_BGt[[314]]$date.time,List_BGt[[314]]$CH4_dry,color="included points"),shape=1)+
  geom_point(aes(List_BG_roof[[315]]$date.time,List_BG_roof[[315]]$CH4_dry,color="included points"),shape=1)+
  scale_color_manual("",values=c("grey80","black","red"))+
  geom_vline(xintercept=Dat$date.time[1.001571e6+1],linetype="dashed",color="red",linewidth=0.4)+
  geom_vline(xintercept=Dat$date.time[1.001571e6+1]+minutes(10),linetype="dashed",color="red",linewidth=0.4)+
  geom_vline(xintercept=Dat$date.time[1.001571e6+1]+minutes(20),linetype="dashed",color="red",linewidth=0.4)+
  geom_vline(xintercept=Dat$date.time[1.001571e6+1]+minutes(30),linetype="dashed",color="red",linewidth=0.4)+
  geom_vline(xintercept=Dat$date.time[1.001571e6+1]+minutes(40),linetype="dashed",color="red",linewidth=0.4)+
  geom_vline(xintercept=Dat$date.time[1.001571e6+1]-minutes(10),linetype="dashed",color="red",linewidth=0.4)+
  annotate("text", x = Dat$date.time[1.001571e6+1]-minutes(15), y = 30, label = "Trailer BG", size = 4, color = "black")+
  annotate("text", x = Dat$date.time[1.001571e6+1]-minutes(5), y = 30, label = "Section 1", size = 4, color = "black")+
  annotate("text", x = Dat$date.time[1.001571e6+1]+minutes(5), y = 30, label = "Roof BG", size = 4, color = "black")+
  annotate("text", x = Dat$date.time[1.001571e6+1]+minutes(15), y = 30, label = "Section 2", size = 4, color = "black")+
  annotate("text", x = Dat$date.time[1.001571e6+1]+minutes(25), y = 30, label = "Trailer BG", size = 4, color = "black")+
  annotate("text", x = Dat$date.time[1.001571e6+1]+minutes(35), y = 30, label = "Section 1", size = 4, color = "black")
ggsave(plot=FigS1,here("plots","FigureS1.png"),width = 8, height = 6)

# ---- Figure S2: CO2 mass balance equation to fill VR ----
VR5<-ggplot()+geom_point(aes(daily_avg_Flow$`as.Date(Time_1)`,daily_avg_Flow$DailyAverage_Flow),shape=1,size=4)+
  geom_line(aes(daily_avg_Conc$`as.Date(Time_2)`,daily_avg_Conc$VR),color='blue')+theme_bw()+
  xlab("")+ylab(expression("VR, m"^3*" h"^-1*""))+ggtitle("a) Section 1")

VR6<-ggplot()+geom_point(aes(daily_avg_Flow$`as.Date(Time_1)`[1:70],daily_avg_Flow$DailyAverage_Flow6[1:70]),shape=1,size=4)+
  geom_line(aes(daily_avg_Conc$`as.Date(Time_2)`,daily_avg_Conc$VR6),color='green')+theme_bw()+
  xlab("")+ylab(expression("VR, m"^3*" h"^-1*""))+ggtitle("b) Section 2")

FigS2 <- plot_grid(VR5,VR6, ncol = 1, align = "hv")
ggsave(plot=FigS2,here("plots","FigureS2.png"), width = 9, height = 5,bg='white')

# ---- Figure S3: Slurry temperature ----
T_S5<-df_S5 %>% 
  mutate(Time_S5 = floor_date(Time_S5, "hour")) %>%
  group_by(Time_S5) %>%
  summarize(Temperature_S5 = mean(Temperature_S5))

T_S6<-df_S6 %>% 
  mutate(Time_S6 = floor_date(Time_S6, "hour")) %>%
  group_by(Time_S6) %>%
  summarize(Temperature_S6 = mean(Temperature_S6))

T5<-ggplot()+geom_line(aes(df_S5$Time_S5[which(df_S5$Sensor_Position_S5=="5.1")],df_S5$Temperature_S5[which(df_S5$Sensor_Position_S5=="5.1")],color="5.1"))+
  geom_line(aes(df_S5$Time_S5[which(df_S5$Sensor_Position_S5=="5.2")],df_S5$Temperature_S5[which(df_S5$Sensor_Position_S5=="5.2")],color="5.2"))+
  geom_line(aes(df_S5$Time_S5[which(df_S5$Sensor_Position_S5=="5.3")],df_S5$Temperature_S5[which(df_S5$Sensor_Position_S5=="5.3")],color="5.3"))+
  geom_line(aes(T_S5$Time_S5,T_S5$Temperature_S5),color="black")+
  xlab("")+ylab("Temperature, °C")+theme_bw()+ggtitle("a) Section 1")+
  xlim(c(min(df_S5$Time_S5[which(df_S5$Sensor_Position_S5=="5.1")]),
         max(df_S5$Time_S5[which(df_S5$Sensor_Position_S5=="5.1")])))+
  theme(legend.title = element_blank())

T6<-ggplot()+geom_line(aes(df_S6$Time_S6[which(df_S6$Sensor_Position_S6=="6.1")],df_S6$Temperature_S6[which(df_S6$Sensor_Position_S6=="6.1")],color="6.1"))+
  geom_line(aes(df_S6$Time_S6[which(df_S6$Sensor_Position_S6=="6.2")],df_S6$Temperature_S6[which(df_S6$Sensor_Position_S6=="6.2")],color="6.2"))+
  geom_line(aes(df_S6$Time_S6[which(df_S6$Sensor_Position_S6=="6.3")],df_S6$Temperature_S6[which(df_S6$Sensor_Position_S6=="6.3")],color="6.3"))+
  geom_line(aes(T_S6$Time_S6,T_S6$Temperature_S6),color="black")+
  xlab("")+ylab("Temperature, °C")+theme_bw()+ggtitle("b) Section 2")+
  theme(legend.title = element_blank())

FigS3 <- plot_grid(T5,T6, ncol = 1, align = "hv")
ggsave(plot=FigS3,here("plots","FigureS3.png"), width = 9, height = 5,bg='white')

# ---- Figure S4: Relative trends checks ----

S5_massT<-ggplot()+geom_point(aes(dt_CH45$time,(dt_CH45$CH4_Total-min(dt_CH45$CH4_Total,na.rm=TRUE))/(max(dt_CH45$CH4_Total,na.rm=TRUE)-min(dt_CH45$CH4_Total,na.rm=TRUE)),color="Measured CH4"))+
  geom_line(aes(slurry_mass_S5$time,(slurry_mass_S5$slurry_mass-min(slurry_mass_S5$slurry_mass))/(max(slurry_mass_S5$slurry_mass)-min(slurry_mass_S5$slurry_mass[1:20])),color="Slurry mass"))+
  geom_line(aes(df_temp_C_S5$time,(df_temp_C_S5$temp_C-min(df_temp_C_S5$temp_C))/(max(df_temp_C_S5$temp_C)-min(df_temp_C_S5$temp_C)),color="Temperature"))+
  geom_line(aes(out_S5_2.0_Default$time,(out_S5_2.0_Default$CH4_emis_rate-min(out_S5_2.0_Default$CH4_emis_rate))/(max(out_S5_2.0_Default$CH4_emis_rate)-min(out_S5_2.0_Default$CH4_emis_rate)),color="ABM"))+
  geom_line(aes(out_S5_2.0_Default$time,(out_S5_2.0_Default$CH4_emis_rate_A-min(out_S5_2.0_Default$CH4_emis_rate_A))/(max(out_S5_2.0_Default$CH4_emis_rate_A)-min(out_S5_2.0_Default$CH4_emis_rate_A)),color="Arrhenius"))+
  theme_bw()+xlab("Time, days")+ylab("Normalized values")+
  theme(panel.grid = element_blank())+
  scale_color_manual("",values=c("green","blue","black","brown","orange","cyan","orange"))+
  geom_rect(aes(xmin=0, xmax=77, ymin=-Inf, ymax=Inf,fill="Batch 1"), alpha = 0.09, colour = NA)+
  geom_rect(aes(xmin=95, xmax=190, ymin=-Inf, ymax=Inf,fill="Batch 2"), alpha = 0.09, colour = NA)+
  geom_rect(aes(xmin=197, xmax=276, ymin=-Inf, ymax=Inf,fill="Batch 3"), alpha = 0.09, colour = NA)+  theme(plot.title = element_text(size = 9))+ 
  scale_fill_manual('',values = c('grey1','grey30',"grey60","grey80"))+
  xlim(c(0,276))+
  ggtitle("a) Section 1")+
  theme(legend.position = "none") 

S6_massT<-ggplot()+geom_point(aes(dt_CH46$time,(dt_CH46$CH4_Total-min(dt_CH46$CH4_Total,na.rm=TRUE))/(max(dt_CH46$CH4_Total,na.rm=TRUE)-min(dt_CH46$CH4_Total,na.rm=TRUE)),color="Measured CH4"))+
  geom_line(aes(slurry_mass_S6$time,(slurry_mass_S6$slurry_mass-min(slurry_mass_S6$slurry_mass))/(max(slurry_mass_S6$slurry_mass)-min(slurry_mass_S6$slurry_mass[1:20])),color="Slurry mass"))+
  geom_line(aes(df_temp_C_S6$time,(df_temp_C_S6$temp_C-min(df_temp_C_S6$temp_C))/(max(df_temp_C_S6$temp_C)-min(df_temp_C_S6$temp_C)),color="Temperature"))+
  geom_line(aes(out_S6_2.0_Default$time,(out_S6_2.0_Default$CH4_emis_rate-min(out_S6_2.0_Default$CH4_emis_rate))/(max(out_S6_2.0_Default$CH4_emis_rate)-min(out_S6_2.0_Default$CH4_emis_rate)),color="ABM"))+
  geom_line(aes(out_S6_2.0_Default$time,(out_S6_2.0_Default$CH4_emis_rate_A-min(out_S6_2.0_Default$CH4_emis_rate_A))/(max(out_S6_2.0_Default$CH4_emis_rate_A)-min(out_S6_2.0_Default$CH4_emis_rate_A)),color="Arrhenius"))+
  theme_bw()+xlab("Time, days")+ylab("Normalized values")+
  theme(panel.grid = element_blank())+
  scale_color_manual("",values=c("green","blue","black","brown","orange","cyan","orange"))+
  geom_rect(aes(xmin=0, xmax=75, ymin=-Inf, ymax=Inf,fill="Batch 1"), alpha = 0.09, colour = NA)+
  geom_rect(aes(xmin=96, xmax=180, ymin=-Inf, ymax=Inf,fill="Batch 2"), alpha = 0.09, colour = NA)+
  geom_rect(aes(xmin=187, xmax=281, ymin=-Inf, ymax=Inf,fill="Batch 3"), alpha = 0.09, colour = NA)+
  theme(plot.title = element_text(size = 9))+ 
  scale_fill_manual('',values = c('grey1','grey30',"grey60","grey80"))+
  ggtitle("b) Section 2")+
  xlim(c(0,282))+
  theme(legend.position = "none")

AmT<-ggplot()+geom_point(aes(dt_CH46$time,(dt_CH46$CH4_Total-min(dt_CH46$CH4_Total,na.rm=TRUE))/(max(dt_CH46$CH4_Total,na.rm=TRUE)-min(dt_CH46$CH4_Total,na.rm=TRUE)),color="Measured CH4"))+
  geom_line(aes(slurry_mass_S6$time,(slurry_mass_S6$slurry_mass-min(slurry_mass_S6$slurry_mass))/(max(slurry_mass_S6$slurry_mass)-min(slurry_mass_S6$slurry_mass[1:20])),color="Slurry mass"))+
  geom_line(aes(df_temp_C_S6$time,(df_temp_C_S6$temp_C-min(df_temp_C_S6$temp_C))/(max(df_temp_C_S6$temp_C)-min(df_temp_C_S6$temp_C)),color="Temperature"))+
  geom_line(aes(out_S6_2.0_Default$time,(out_S6_2.0_Default$CH4_emis_rate-min(out_S6_2.0_Default$CH4_emis_rate))/(max(out_S6_2.0_Default$CH4_emis_rate)-min(out_S6_2.0_Default$CH4_emis_rate)),color="ABM"))+
  geom_line(aes(out_S6_2.0_Default$time,(out_S6_2.0_Default$CH4_emis_rate_A-min(out_S6_2.0_Default$CH4_emis_rate_A))/(max(out_S6_2.0_Default$CH4_emis_rate_A)-min(out_S6_2.0_Default$CH4_emis_rate_A)),color="Arrhenius"))+
  theme_bw()+xlab("Time, days")+ylab("Normalized values")+
  theme(panel.grid = element_blank())+
  scale_color_manual("",values=c("green","blue","black","brown","orange","cyan","orange"))+
  geom_rect(aes(xmin=0, xmax=75, ymin=-Inf, ymax=Inf,fill="Batch 1"), alpha = 0.09, colour = NA)+
  geom_rect(aes(xmin=96, xmax=180, ymin=-Inf, ymax=Inf,fill="Batch 2"), alpha = 0.09, colour = NA)+
  geom_rect(aes(xmin=187, xmax=281, ymin=-Inf, ymax=Inf,fill="Batch 3"), alpha = 0.09, colour = NA)+
  theme(plot.title = element_text(size = 9))+ 
  scale_fill_manual('',values = c('grey1','grey30',"grey60","grey80"))+
  ggtitle("b) Section 2")+
  xlim(c(0,282))

grobs <- ggplotGrob(AmT)$grobs
legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]
group1 <- plot_grid(S5_massT,S6_massT, ncol = 1, align = "hv")
FigS4<-plot_grid(group1,legend,rel_widths = c(1, .5))
ggsave(plot=FigS4,here("plots","FigureS4.png"), width = 9, height = 5,bg='white')

# ---- Figure S5: Evaporation rate sensitivity ----


S6_EvapRate<-ggplot()+geom_point(aes(dt_CH46$time,dt_CH46$CH4_slurry_pig,color="Measured"))+
  geom_line(aes(dt_CH46$time,dt_CH46$CH4_slurry_pig),color="grey80",size=0.3)+
  geom_line(aes(dt_CH46$time,dt_CH46$CH4_ABM_pig,color="ER = 1.3 (used in simulations)"))+
  geom_line(aes(dt_CH46$time,dt_CH46$CH4_ABM_Evap2_pig,color="ER = 2"))+
  geom_line(aes(dt_CH46$time,dt_CH46$CH4_ABM_Evap05_pig,color="ER = 0.5"))+
  #geom_line(aes(dt_CH46$time,dt_CH46$A_default,color="Predicted: Danish NIR"),linetype=2)+
  ggtitle("b) Section 2")+
  theme_bw()+xlab("Time, days")+ylab(expression("Emissions, g pig"^-1*" day"^-1*""))+
  #theme(legend.position = "none") +
  scale_color_manual("",values=c("blue","grey","orange","black","red","darkgreen","blue","orange"))+
  #scale_color_manual("",values=c("black","red","green","blue","red","darkgreen","blue"))+
  theme(panel.grid = element_blank())+
  geom_rect(aes(xmin=0, xmax=75, ymin=-Inf, ymax=Inf,fill="Batch 1"), alpha = 0.09, colour = NA)+
  geom_rect(aes(xmin=96, xmax=180, ymin=-Inf, ymax=Inf,fill="Batch 2"), alpha = 0.09, colour = NA)+
  geom_rect(aes(xmin=187, xmax=281, ymin=-Inf, ymax=Inf,fill="Batch 3"), alpha = 0.09, colour = NA)+
  theme(plot.title = element_text(size = 9))+
  xlim(c(0,282))+
  scale_fill_manual('',values = c('grey1','grey30',"grey60","grey80"))
ggsave(plot=S6_EvapRate, here("plots","FigureS5.png"), width=9, height=5, bg="white", dpi=500)

# ---- Figure S6: Optimized abm ----

S6_CH4_opt<-ggplot()+geom_point(aes(dt_CH46$time,dt_CH46$CH4_slurry_pig,color="Measured"))+
  geom_line(aes(dt_CH46$time,dt_CH46$CH4_slurry_pig),color="grey80",size=0.3)+
  geom_line(aes(dt_CH46$time,dt_CH46$CH4_ABM_pig,color="ABM default (q = 1, H = 1, re = 0.9)"))+
  geom_line(aes(dt_CH46$time,dt_CH46$CH4_ABM_optim_pig,color="ABM optimized (q = 1.5, H = 0.5, re = 1)"))+
  ggtitle("b) Section 2")+
  theme_bw()+xlab("Time, days")+ylab(expression("Emissions, g pig"^-1*" day"^-1*""))+
  scale_color_manual("",values=c("lightgreen","red","black","blue","cyan","red","blue"))+
  theme(panel.grid = element_blank())+
  geom_rect(aes(xmin=0, xmax=75, ymin=-Inf, ymax=Inf),fill="grey1",alpha = 0.09, colour = NA)+
  geom_rect(aes(xmin=96, xmax=180, ymin=-Inf, ymax=Inf),fill="grey30", alpha = 0.09, colour = NA)+
  geom_rect(aes(xmin=187, xmax=281, ymin=-Inf, ymax=Inf),fill="grey60", alpha = 0.09, colour = NA)+
  theme(plot.title = element_text(size = 9))+
  xlim(c(0,290))

S5_CH4_opt<-ggplot()+geom_point(aes(dt_CH45$time,dt_CH45$CH4_slurry_pig,color="Measured"))+
  geom_line(aes(dt_CH45$time,dt_CH45$CH4_slurry_pig),color="grey80",size=0.3)+
  geom_line(aes(dt_CH45$time,dt_CH45$CH4_ABM_pig,color="ABM default (q = 1, H = 1, re = 0.9)"))+
  geom_line(aes(dt_CH45$time,dt_CH45$CH4_ABM_optim_pig,color="ABM optimized (q = 0.69, H = 0.91, re = 1.6)"))+
  theme_bw()+xlab("Time, days")+ylab(expression("Emissions, g pig"^-1*" day"^-1*""))+
  scale_color_manual("",values=c("lightgreen","red","black","blue","cyan","red","blue"))+
  ggtitle("a) Section 1")+  theme(panel.grid = element_blank())+
  geom_rect(aes(xmin=0, xmax=77, ymin=-Inf, ymax=Inf),fill='grey1', alpha = 0.09, colour = NA)+
  geom_rect(aes(xmin=95, xmax=190, ymin=-Inf, ymax=Inf),fill='grey30', alpha = 0.09, colour = NA)+
  geom_rect(aes(xmin=197, xmax=276, ymin=-Inf, ymax=Inf),fill="grey60", alpha = 0.09, colour = NA)+
  geom_rect(aes(xmin=281, xmax=358, ymin=-Inf, ymax=Inf),fill="grey80", alpha = 0.09, colour = NA)+
  theme(plot.title = element_text(size = 9))+
  xlim(c(0,276))

FigS6 <- plot_grid(S5_CH4_opt,S6_CH4_opt, ncol = 1, align = "hv")
ggsave(plot=FigS6, here("plots","FigureS6.png"), width=9, height=5, bg="white", dpi=500)

# ---- Figure S7: Optimized Arrhenius ----

S6_CH4_A_opt<-ggplot()+geom_point(aes(dt_CH46$time,dt_CH46$CH4_slurry_pig,color="Measured"))+
  geom_line(aes(dt_CH46$time,dt_CH46$CH4_slurry_pig),color="grey80",size=0.3)+
  geom_line(aes(dt_CH46$time,dt_CH46$CH4_A_pig,color="Arrhenius lnA = 31.3"))+
  geom_line(aes(dt_CH46$time,dt_CH46$CH4_A_lnA_pig,color="Arrhenius lnA' = 30.3"))+
  geom_line(aes(dt_CH46$time,dt_CH46$CH4_A_optim_pig,color="Arrhenius lnA = 31.7"))+
  geom_line(aes(dt_CH46$time,dt_CH46$CH4_A_lnA_optim_pig,color="Arrhenius lnA' = 31.5"))+
  ggtitle("b) Section 2")+
  theme_bw()+xlab("Time, days")+ylab(expression("Emissions, g pig"^-1*" day"^-1*""))+
  scale_color_manual("",values=c("lightpink","purple","lightblue","darkblue","black","red","blue"))+
  theme(panel.grid = element_blank())+
  geom_rect(aes(xmin=0, xmax=75, ymin=-Inf, ymax=Inf),fill="grey1",alpha = 0.09, colour = NA)+
  geom_rect(aes(xmin=96, xmax=180, ymin=-Inf, ymax=Inf),fill="grey30", alpha = 0.09, colour = NA)+
  geom_rect(aes(xmin=187, xmax=281, ymin=-Inf, ymax=Inf),fill="grey60", alpha = 0.09, colour = NA)+
  theme(plot.title = element_text(size = 9))+
  xlim(c(0,290))

S5_CH4_A_opt<-ggplot()+geom_point(aes(dt_CH45$time,dt_CH45$CH4_slurry_pig,color="Measured"))+
  geom_line(aes(dt_CH45$time,dt_CH45$CH4_slurry_pig),color="grey80",size=0.3)+
  geom_line(aes(dt_CH45$time,dt_CH45$CH4_A_pig,color="Arrhenius lnA = 31.3"))+
  geom_line(aes(dt_CH45$time,dt_CH45$CH4_A_lnA_pig,color="Arrhenius lnA' = 30.3"))+
  geom_line(aes(dt_CH45$time,dt_CH45$CH4_A_optim_pig,color="Arrhenius lnA = 31.7"))+
  geom_line(aes(dt_CH45$time,dt_CH45$CH4_A_lnA_optim_pig,color="Arrhenius lnA' = 31.4"))+
  theme_bw()+xlab("Time, days")+ylab(expression("Emissions, g pig"^-1*" day"^-1*""))+
  scale_color_manual("",values=c("lightpink","purple","lightblue","darkblue","black","red","blue"))+
  ggtitle("a) Section 1")+  theme(panel.grid = element_blank())+
  geom_rect(aes(xmin=0, xmax=77, ymin=-Inf, ymax=Inf),fill='grey1', alpha = 0.09, colour = NA)+
  geom_rect(aes(xmin=95, xmax=190, ymin=-Inf, ymax=Inf),fill='grey30', alpha = 0.09, colour = NA)+
  geom_rect(aes(xmin=197, xmax=276, ymin=-Inf, ymax=Inf),fill="grey60", alpha = 0.09, colour = NA)+
  geom_rect(aes(xmin=281, xmax=358, ymin=-Inf, ymax=Inf),fill="grey80", alpha = 0.09, colour = NA)+
  theme(plot.title = element_text(size = 9))+
  xlim(c(0,276))

FigS7 <- plot_grid(S5_CH4_A_opt,S6_CH4_A_opt, ncol = 1, align = "hv")
ggsave(plot=FigS7, here("plots","FigureS7.png"), width=9, height=5, bg="white", dpi=500)

# ---- Figure S8: mcra gen analysis: Heatmap of genus and Species ----
metadata$sample_type_supl <- dplyr::recode(metadata$sample_type,
                                      "Time0_Before"       = "Day0_B_BS",
                                      "Time0_Solid_Before" = "Day0_B_SF",
                                      "Time0_After"        = "Day0_A_BS",
                                      "Time0_Solid_After"  = "Day0_A_SF",
                                      "Time1_Before"       = "Day35_B_BS",
                                      "Time1_After"        = "Day35_A_BS",
                                      "Time1_Liquid"       = "Day35_A_LF",
                                      "Time2_Before"       = "Day63_B_BS",
                                      "Time2_After"        = "Day63_A_BS",
                                      "Time3_Before"       = "Day77_B_BS",
                                      "Time3_After"        = "Day77_A_BS")

metadata$sample_type_supl <- factor(metadata$sample_type_supl , levels = c("Day0_B_SF", "Day0_B_BS", "Day0_A_BS",
                                                                 "Day0_A_SF","Day35_B_BS","Day35_A_BS",
                                                                 "Day35_A_LF","Day63_B_BS","Day63_A_BS",
                                                                 "Day77_B_BS","Day77_A_BS"))
d_mcra <- amp_load(otutable = otutable_mcra,
                   metadata = metadata)
DNA_mcra<-amp_heatmap(d_mcra,
               tax_aggregate = "Genus",
               tax_add = "Species",
               min_abundance = 0.1,
               facet_by = "sample_type_supl",
               tax_show = 15)

ggsave(plot=DNA_mcra,here("plots","FigureS8.png"), width = 15, height = 8)

# ---- Figure S9: Activity analysis: Heatmap of Species ----
df_prots$sample_type_supl <- dplyr::recode(df_prots$SampleID,
                                           "Time0_Before"       = "Day0_B_BS",
                                           "Time0_Solid_Before" = "Day0_B_SF",
                                           "Time0_After"        = "Day0_A_BS",
                                           "Time0_Solid_After"  = "Day0_A_SF",
                                           "Time1_Before"       = "Day35_B_BS",
                                           "Time1_After"        = "Day35_A_BS",
                                           "Time1_Liquid_After" = "Day35_A_LF",
                                           "Time2_Before"       = "Day63_B_BS",
                                           "Time2_After"        = "Day63_A_BS",
                                           "Time3_Before"       = "Day77_B_BS",
                                           "Time3_After"        = "Day77_A_BS")

df_prots$sample_type_supl <- factor(df_prots$sample_type_supl , levels = c("Day0_B_SF", "Day0_B_BS", "Day0_A_BS",
                                                           "Day0_A_SF","Day35_B_BS","Day35_A_BS",
                                                           "Day35_A_LF","Day63_B_BS","Day63_A_BS",
                                                           "Day77_B_BS","Day77_A_BS"))


R<-which(df_prots$Genus=="Prevotella")
df_prots$Prevotella <- rep(df_prots$Tax_counts[R], each = nrow(data))

df_prots$Relative_prevotella<-df_prots$Tax_counts/df_prots$Prevotella

##Genus
Gns<-tapply(df_prots$Tax_counts,list(df_prots$Genus,df_prots$sample_type_supl),sum,na.rm=TRUE)
RelP<-tapply(df_prots$Relative_prevotella,list(df_prots$Genus,df_prots$sample_type_supl),sum,na.rm=TRUE)


pT_Gns<-Gns
pP_Gns<-RelP

for (i in 1:11){
  pT_Gns[,i]<-Gns[,i]/sum(Gns[,i])*100
  
}

# Remove non identified things
pT_Gns<-pT_Gns[-1,]
RelP<-RelP[-1,]



##Heatmap

Sum<-rowSums(pT_Gns)
pT_Gns_F10 <- cbind(pT_Gns, RowSum = Sum)
Sortmaxmin<-order(pT_Gns_F10[, 12],decreasing=TRUE)
pT_Gns_F10<-pT_Gns_F10[Sortmaxmin,]
pT_Gns_F10<-pT_Gns_F10[,-12]
A<-pheatmap(pT_Gns_F10[1:25,],display_numbers = TRUE,
            number_color = "black",cluster_rows = FALSE,
            cluster_cols = FALSE,
            fontsize_number = 8,legend=FALSE,main="Genus percentage relative to total sum")
ggsave(plot=A,here("plots", "FigureS9.png"), width = 7, height = 5)

# ---- Figure S10: Activity analysis: Heatmap of Species ----
d_V1V8 <- amp_load(otutable = otutable_V1V8,
                   metadata = metadata)
DNA_V1V8<-amp_heatmap(d_V1V8,
               tax_aggregate = "Phylum",
               min_abundance = 0.1,
               facet_by = "sample_type",
               tax_show = 15)
ggsave(plot=DNA_V1V8,here("plots","FigureS10.png"), width = 15, height = 8)
