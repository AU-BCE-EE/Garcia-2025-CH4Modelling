library(cowplot)
library(ggplot2)
library(readxl)
### Figures Paper III ###
setwd("O:/Tech_BCE/Environmental Engineering/Air Quality Engineering/Pablo García/METEMIS/ABM model")
load("Model output/Model_paper_new.RData")

load("RData/CH4_AVG.RData")
load("RData/Enteric_AVG.RData")
load("RData/Pigs.RData")
load("RData/Slurry_T.RData")
load("RData/Sprod_S5.RData")
load("RData/Sprod_S6.RData")
A_default<-read.table("Rfiles/A_default.txt")
Flow<-read.table("Rfiles/Flow.txt",header = TRUE)
Room_T<-read.table("Rfiles/Room_T.txt",header = TRUE)
  
Batch4<-read_excel("Batch4_MA.xlsx")
Batch4$Date<-as.Date(c("2023-02-14","2023-02-14","2023-03-21","2023-03-21",
                       "2023-04-18","2023-04-18","2023-05-02","2023-05-02"))
Time_0<-as.Date("2022-05-09") #Start of Section 5

library(data.table)
class(Batch4)<-c("data.table","data.frame")
Batch4[, etime := as.numeric(Date - Time_0)]
Batch4$Mean_CH4<-Batch4$Mean_CH4*24 #g CH4/day
Batch4$sd_CH4<-Batch4$sd_CH4*24 #g CH4/day

df_B4<-data.frame(time = Batch4$etime, CH4_Total = Batch4$Mean_CH4,CH4_enteric=NaN,
                  Pigs = Batch4$Pigs)
df_Model_S5<-data.frame(time = out_S5_2.0_new$time , CH4_ABM_opt = out_S5_2.0_new$CH4_emis_rate,
                        CH4_A_opt = out_S5_2.0_new$CH4_A_emis_rate,
                        CH4_ABM = out_S5_2.0$CH4_emis_rate,
                        CH4_A = out_S5_2.0$CH4_A_emis_rate)
                       
df_Measured_S5<-data.frame(time = daily_avg_S5$Group.1, CH4_Total = daily_avg_S5$x,
                           CH4_enteric = daily_Entavg_S5$x)


class(df_Measured_S5)<-c("data.table","data.frame")
class(df_Model_S5)<-c("data.table","data.frame")
class(df_B4)<-c("data.table","data.frame")
df_B4[,CH4_Total_pig :=CH4_Total/mean(Pigs)]


dt_merged<-merge(df_Measured_S5,Pigs_S5,by="time",all=TRUE)
dt_CH4<-merge(dt_merged,df_Model_S5,by="time",all=TRUE)
dt_CH4[,CH4_slurry :=CH4_Total-CH4_enteric]
dt_CH4$Batch<-NaN
dt_CH4$Batch[1:77]<-1
dt_CH4$Batch[95:191]<-2
dt_CH4$Batch[197:276]<-3
dt_CH4$Batch[281:358]<-4


Subset_B1<-dt_CH4[dt_CH4$Batch == 1,]
Subset_B1[,CH4_slurry_pig :=CH4_slurry/mean(Pigs)]
Subset_B1[,CH4_ABM_pig :=CH4_ABM/mean(Pigs)]
Subset_B1[,CH4_A_pig :=CH4_A/mean(Pigs)]
Subset_B1[,CH4_ABM_opt_pig :=CH4_ABM_opt/mean(Pigs)]
Subset_B1[,CH4_A_opt_pig :=CH4_A_opt/mean(Pigs)]


Subset_B2<-dt_CH4[dt_CH4$Batch == 2,]
Subset_B2[,CH4_slurry_pig :=CH4_slurry/mean(Pigs)]
Subset_B2[,CH4_ABM_pig :=CH4_ABM/mean(Pigs)]
Subset_B2[,CH4_A_pig :=CH4_A/mean(Pigs)]
Subset_B2[,CH4_ABM_opt_pig :=CH4_ABM_opt/mean(Pigs)]
Subset_B2[,CH4_A_opt_pig :=CH4_A_opt/mean(Pigs)]


Subset_B3<-dt_CH4[dt_CH4$Batch == 3,]
Subset_B3[,CH4_slurry_pig :=CH4_slurry/mean(Pigs)]
Subset_B3[,CH4_ABM_pig :=CH4_ABM/mean(Pigs)]
Subset_B3[,CH4_A_pig :=CH4_A/mean(Pigs)]
Subset_B3[,CH4_ABM_opt_pig :=CH4_ABM_opt/mean(Pigs)]
Subset_B3[,CH4_A_opt_pig :=CH4_A_opt/mean(Pigs)]


Subset_B4<-dt_CH4[dt_CH4$Batch == 4,]
Subset_B4[,CH4_slurry_pig :=CH4_slurry/mean(Pigs)]
Subset_B4[,CH4_ABM_pig :=CH4_ABM/mean(Pigs)]
Subset_B4[,CH4_A_pig :=CH4_A/mean(Pigs)]
Subset_B4[,CH4_ABM_opt_pig :=CH4_ABM_opt/mean(Pigs)]
Subset_B4[,CH4_A_opt_pig :=CH4_A_opt/mean(Pigs)]


dt_plot5<-rbind(Subset_B1,Subset_B2,Subset_B3,Subset_B4)
dt_plot5$A_default<-A_default

dt_plot5[, .(mean_pigs = mean(Pigs),
                              sd_pigs = sd(Pigs)),
                          by = .(Batch)]

dt_plot5[, .(mean_pigs = mean(feed_rate),
             sd_pigs = sd(feed_rate)),
         by = .(Batch)]

##Slurry temperature add
df_temp_C_S5$time<-floor(df_temp_C_S5$time)
class(df_temp_C_S5)<-c("data.table","data.frame")
Temp_S5<-df_temp_C_S5[, .(mean_T = mean(temp_C)),
         by = .(time)]
data_merged<-merge(dt_plot5,Temp_S5,by="time",all=TRUE)
data_merged[, .(mean_T = mean(mean_T,na.rm=TRUE),
             sd_T = sd(mean_T,na.rm=TRUE)),
         by = .(Batch)]

##Room temperature and Flow add
class(Flow)<-c("data.table","data.frame")
class(Room_T)<-c("data.table","data.frame")
Flow$time<-as.numeric(as.Date(anytime(Flow$Flow_All.date.time))-as.Date("2022-05-09"))
Room_T$time<-as.numeric(as.Date(anytime(Room_T$Flow_All.date.time))-as.Date("2022-05-09"))
RTemp_S5<-Room_T[, .(mean_RoomT = mean(T_5)),
                      by = .(time)]
Flow_S5<-Flow[, .(mean_Flow = mean(Flow_All..Airflow.Section_5.)),
                 by = .(time)]
data_FT<-merge(Flow_S5,RTemp_S5,by="time",all=TRUE)
data_merged2<-merge(data_merged,data_FT,by="time",all=TRUE)
data_merged2[, .(mean_T = mean(mean_RoomT,na.rm=TRUE),
                sd_T = sd(mean_RoomT,na.rm=TRUE),
                mean_F = mean(mean_Flow,na.rm=TRUE)/3600,
                  sd_F = sd(mean_Flow,na.rm=TRUE)/3600),
            by = .(Batch)]
write.table(dt_plot5, file = "Emissions_S5.txt", sep = "\t", row.names = TRUE, col.names = NA)


file_list1 <- list.files(path = "../", pattern = "rates2\\.txt$", full.names = TRUE,recursive = TRUE)
file_list2 <- list.files(path = "../", pattern = "Rates2\\.txt$", full.names = TRUE,recursive = TRUE)
file_list <- c(file_list1,file_list2)
# Read each file and combine them into a single data.frame
combined_data <- file_list %>%
  lapply(read.table, header = TRUE, sep = "\t") %>%  # Adjust separator as needed
  bind_rows()
combined_data[combined_data == Inf | combined_data == -Inf] <- NA

Rate_S5_24h<-rowMeans(combined_data[,c(1,3,5)],na.rm=TRUE)
Rate_S5_Max<-rowMeans(combined_data[,c(2,4,6)],na.rm=TRUE)
Rate_S6_24h<-rowMeans(combined_data[,c(7,9,11)],na.rm=TRUE)
Rate_S6_Max<-rowMeans(combined_data[,c(8,10,12)],na.rm=TRUE)

dt_CH4_rate_S5<-data.table(time = c(22,50,64,113,141,155,225,255,267),
                           Rate_S5_24h = Rate_S5_24h, 
                           Rate_S5_Max = Rate_S5_Max)

dt_CH4_rate_S6<-data.table(time = c(26,54,68,117,145,159,208,236,250),
                           Rate_S6_24h = Rate_S6_24h, 
                           Rate_S6_Max = Rate_S6_Max)

## Compare with SO Petersen #####################################################
file_list1 <- list.files(path = "../", pattern = "rates\\.txt$", full.names = TRUE,recursive = TRUE)
file_list2 <- list.files(path = "../", pattern = "Rates\\.txt$", full.names = TRUE,recursive = TRUE)
file_list <- c(file_list1,file_list2)
# Read each file and combine them into a single data.frame
combined_data <- file_list %>%
  lapply(read.table, header = TRUE, sep = "\t") %>%  # Adjust separator as needed
  bind_rows()
combined_data[combined_data == Inf | combined_data == -Inf] <- NA

Rate_S5_24h<-rowMeans(combined_data[,c(1,3,5)],na.rm=TRUE)/24
Rate_S5_Max<-rowMeans(combined_data[,c(2,4,6)],na.rm=TRUE)/24
Rate_S6_24h<-rowMeans(combined_data[,c(7,9,11)],na.rm=TRUE)/24
Rate_S6_Max<-rowMeans(combined_data[,c(8,10,12)],na.rm=TRUE)/24
#################################################################################




##Plot
S5_CH4<-ggplot()+geom_point(aes(dt_plot5$time,dt_plot5$CH4_slurry_pig,color="Measured"))+
  geom_line(aes(dt_plot5$time,dt_plot5$CH4_slurry_pig),color="grey80",size=0.3)+
  #geom_point(aes(df_B4$time,df_B4$CH4_Total_pig,color="Measured"))+
  #geom_line(aes(df_B4$time,df_B4$CH4_Total_pig,color="Measured"),color="grey80",size=0.3)+
  #geom_point(aes(out_S5_2.0_new$time,out_S5_2.0_new$CH4_A_emis_rate,color="Predicted: Arrhenius model"))+
  #geom_point(aes(out_S5_2.0_new$time,out_S5_2.0_new$CH4_emis_rate,color="Predicted: ABM_Model"))+
  geom_line(aes(dt_plot5$time,dt_plot5$CH4_A_pig,color="Predicted: Arrhenius model"))+
  geom_line(aes(dt_plot5$time,dt_plot5$CH4_ABM_pig,color="Predicted: ABM"))+
  geom_line(aes(dt_plot5$time,dt_plot5$CH4_A_opt_pig,color="Predicted: optimized Arrhenius model"))+
  geom_line(aes(dt_plot5$time,dt_plot5$CH4_ABM_opt_pig,color="Predicted: optimized ABM"))+
  geom_line(aes(dt_plot5$time,dt_plot5$A_default,color="Predicted: Danish NIR"),linetype=2)+
  geom_point(aes(dt_CH4_rate_S5$time,dt_CH4_rate_S5$Rate_S5_Max,color="MPR"),size=2,shape=17)+
  ggtitle("a) Section 1")+
  theme_bw()+xlab("Time, days")+ylab(expression("Emissions, g pig"^-1*" day"^-1*""))+
  theme(legend.position = "none") +
  scale_color_manual("",values=c("black","grey60","green","cyan","red","darkgreen","blue"))+
  #scale_color_manual("",values=c("black","red","green","blue","red","darkgreen","blue"))+
  theme(panel.grid = element_blank())+
  geom_rect(aes(xmin=0, xmax=77, ymin=-Inf, ymax=Inf,fill="Batch 1"), alpha = 0.09, colour = NA)+
geom_rect(aes(xmin=95, xmax=190, ymin=-Inf, ymax=Inf,fill="Batch 2"), alpha = 0.09, colour = NA)+
geom_rect(aes(xmin=197, xmax=276, ymin=-Inf, ymax=Inf,fill="Batch 3"), alpha = 0.09, colour = NA)+
#geom_rect(aes(xmin=281, xmax=358, ymin=-Inf, ymax=Inf,fill="Batch 4"), alpha = 0.09, colour = NA)+
  theme(plot.title = element_text(size = 9))+
  xlim(c(0,276))+
  scale_fill_manual('',values = c('grey1','grey30',"grey60","grey80"))
  

### Section 6

df_Model_S6<-data.frame(time = out_S6_2.0$time , CH4_ABM = out_S6_2.0$CH4_emis_rate,
                        CH4_A = out_S6_2.0$CH4_A_emis_rate,CH4_ABM_opt = out_S6_2.0_new$CH4_emis_rate,
                        CH4_A_opt = out_S6_2.0_new$CH4_A_emis_rate)
df_Measured_S6<-data.frame(time = daily_avg_S6$Group.1, CH4_Total = daily_avg_S6$x,
                           CH4_enteric = daily_Entavg_S6$x)


class(df_Measured_S6)<-c("data.table","data.frame")
class(df_Model_S6)<-c("data.table","data.frame")
class(df_B4)<-c("data.table","data.frame")


dt_merged<-merge(df_Measured_S6,Pigs_S6,by="time",all=TRUE)
dt_CH4<-merge(dt_merged,df_Model_S6,by="time",all=TRUE)
dt_CH4[,CH4_slurry :=CH4_Total-CH4_enteric]
dt_CH4$Batch<-NaN
dt_CH4$Batch[1:75]<-1
dt_CH4$Batch[96:180]<-2
dt_CH4$Batch[187:280]<-3


Subset_B1<-dt_CH4[dt_CH4$Batch == 1,]
Subset_B1[,CH4_slurry_pig :=CH4_slurry/mean(Pigs)]
Subset_B1[,CH4_ABM_pig :=CH4_ABM/mean(Pigs)]
Subset_B1[,CH4_A_pig :=CH4_A/mean(Pigs)]
Subset_B1[,CH4_ABM_opt_pig :=CH4_ABM_opt/mean(Pigs)]
Subset_B1[,CH4_A_opt_pig :=CH4_A_opt/mean(Pigs)]

Subset_B2<-dt_CH4[dt_CH4$Batch == 2,]
Subset_B2[,CH4_slurry_pig :=CH4_slurry/mean(Pigs)]
Subset_B2[,CH4_ABM_pig :=CH4_ABM/mean(Pigs)]
Subset_B2[,CH4_A_pig :=CH4_A/mean(Pigs)]
Subset_B2[,CH4_ABM_opt_pig :=CH4_ABM_opt/mean(Pigs)]
Subset_B2[,CH4_A_opt_pig :=CH4_A_opt/mean(Pigs)]

Subset_B3<-dt_CH4[dt_CH4$Batch == 3,]
Subset_B3[,CH4_slurry_pig :=CH4_slurry/mean(Pigs)]
Subset_B3[,CH4_ABM_pig :=CH4_ABM/mean(Pigs)]
Subset_B3[,CH4_A_pig :=CH4_A/mean(Pigs)]
Subset_B3[,CH4_ABM_opt_pig :=CH4_ABM_opt/mean(Pigs)]
Subset_B3[,CH4_A_opt_pig :=CH4_A_opt/mean(Pigs)]


dt_plot6<-rbind(Subset_B1,Subset_B2,Subset_B3)
dt_plot6$A_default<-A_default

dt_plot6[, .(mean_pigs = mean(Pigs),
             sd_pigs = sd(Pigs)),
         by = .(Batch)]

dt_plot6[, .(mean_pigs = mean(feed_rate,na.rm=TRUE),
             sd_pigs = sd(feed_rate,na.rm=TRUE)),
         by = .(Batch)]

##Slurry temperature add
df_temp_C_S6$time<-floor(df_temp_C_S6$time)
class(df_temp_C_S6)<-c("data.table","data.frame")
Temp_S6<-df_temp_C_S6[, .(mean_T = mean(temp_C)),
                      by = .(time)]
data_merged<-merge(dt_plot6,Temp_S6,by="time",all=TRUE)
data_merged[, .(mean_T = mean(mean_T,na.rm=TRUE),
                sd_T = sd(mean_T,na.rm=TRUE)),
            by = .(Batch)]
##Room temperature and Flow add
Flow$time<-as.numeric(as.Date(anytime(Flow$Flow_All.date.time))-as.Date("2022-05-05"))
Room_T$time<-as.numeric(as.Date(anytime(Room_T$Flow_All.date.time))-as.Date("2022-05-05"))
RTemp_S6<-Room_T[, .(mean_RoomT = mean(T_6)),
                 by = .(time)]
Flow_S6<-Flow[, .(mean_Flow = mean(Flow_All..Airflow.Section_6.)),
              by = .(time)]
data_FT<-merge(Flow_S6,RTemp_S6,by="time",all=TRUE)
data_merged2_6<-merge(data_merged,data_FT,by="time",all=TRUE)
data_merged2_6[, .(mean_T = mean(mean_RoomT,na.rm=TRUE),
                 sd_T = sd(mean_RoomT,na.rm=TRUE),
                 mean_F = mean(mean_Flow,na.rm=TRUE)/3600,
                 sd_F = sd(mean_Flow,na.rm=TRUE)/3600),
             by = .(Batch)]

write.table(dt_plot6, file = "Emissions_S6.txt", sep = "\t", row.names = TRUE, col.names = NA)

#Plot

S6_CH4<-ggplot()+geom_point(aes(dt_plot6$time,dt_plot6$CH4_slurry_pig,color="Measured"))+
  geom_line(aes(dt_plot6$time,dt_plot6$CH4_slurry_pig),color="grey80",size=0.3)+
  geom_line(aes(dt_plot6$time,dt_plot6$CH4_A_pig,color="Predicted: Arrhenius model"))+
  geom_line(aes(dt_plot6$time,dt_plot6$CH4_A_opt_pig,color="Predicted: optimized Arrhenius model"))+
  geom_line(aes(dt_plot6$time,dt_plot6$CH4_ABM_pig,color="Predicted: ABM"))+
  geom_line(aes(dt_plot6$time,dt_plot6$CH4_ABM_opt_pig,color="Predicted: optimized ABM"))+
  geom_line(aes(dt_plot6$time,dt_plot6$A_default,color="Predicted: Danish NIR"),linetype=2)+
  geom_point(aes(dt_CH4_rate_S6$time,dt_CH4_rate_S6$Rate_S6_Max,color="MPR"),size=2,shape=17)+
  ggtitle("b) Section 2")+
  theme_bw()+xlab("Time, days")+ylab(expression("Emissions, g pig"^-1*" day"^-1*""))+
  theme(legend.position = "none") +
  scale_color_manual("",values=c("black","grey","green","cyan","red","darkgreen","blue"))+
  #scale_color_manual("",values=c("black","red","green","blue","red","darkgreen","blue"))+
  theme(panel.grid = element_blank())+
  geom_rect(aes(xmin=0, xmax=75, ymin=-Inf, ymax=Inf,fill="Batch 1"), alpha = 0.09, colour = NA)+
  geom_rect(aes(xmin=96, xmax=180, ymin=-Inf, ymax=Inf,fill="Batch 2"), alpha = 0.09, colour = NA)+
  geom_rect(aes(xmin=187, xmax=281, ymin=-Inf, ymax=Inf,fill="Batch 3"), alpha = 0.09, colour = NA)+
  theme(plot.title = element_text(size = 9))+ 
  scale_fill_manual('',values = c('grey1','grey30',"grey60","grey80"))

A<-ggplot()+geom_point(aes(dt_plot5$time,dt_plot5$CH4_slurry_pig,color="Measured"))+
  geom_line(aes(dt_plot5$time,dt_plot5$CH4_slurry_pig),color="grey80",size=0.3)+
  geom_point(aes(df_B4$time,df_B4$CH4_Total_pig,color="Measured"))+
  geom_line(aes(df_B4$time,df_B4$CH4_Total_pig,color="Measured"),color="grey80",size=0.3)+
  #geom_point(aes(out_S5_2.0_new$time,out_S5_2.0_new$CH4_A_emis_rate,color="Predicted: Arrhenius model"))+
  #geom_point(aes(out_S5_2.0_new$time,out_S5_2.0_new$CH4_emis_rate,color="Predicted: ABM_Model"))+
  geom_line(aes(dt_plot5$time,dt_plot5$CH4_A_pig,color="Predicted: Arrhenius model"))+
  geom_line(aes(dt_plot5$time,dt_plot5$CH4_ABM_pig,color="Predicted: ABM"))+
  geom_line(aes(dt_plot5$time,dt_plot5$CH4_ABM_pig,color="Predicted: optimized ABM"))+
  geom_line(aes(dt_plot5$time,dt_plot5$CH4_A_pig,color="Predicted: optimized Arrhenius Model"))+
  geom_line(aes(dt_plot5$time,dt_plot5$A_default,color="Predicted: Danish NIR"),linetype=2)+
  geom_point(aes(dt_CH4_rate_S5$time,dt_CH4_rate_S5$Rate_S5_Max,color="MPR"),size=2,shape=17)+
  theme_bw()+xlab("Time, days")+ylab(expression("Emissions, g pig"^-1*" day"^-1*""))+
  scale_color_manual("",values=c("black","grey","green","cyan","red","darkgreen","blue"))+
  #scale_color_manual("",values=c("black","red","green","blue","red","darkgreen","blue"))+
  theme(panel.grid = element_blank())+
  geom_rect(aes(xmin=0, xmax=77, ymin=-Inf, ymax=Inf,fill="Batch 1"), alpha = 0.09, colour = NA)+
  geom_rect(aes(xmin=95, xmax=171, ymin=-Inf, ymax=Inf,fill="Batch 2"), alpha = 0.09, colour = NA)+
  geom_rect(aes(xmin=197, xmax=276, ymin=-Inf, ymax=Inf,fill="Batch 3"), alpha = 0.09, colour = NA)+
  #geom_rect(aes(xmin=281, xmax=358, ymin=-Inf, ymax=Inf,fill="Batch 4"), alpha = 0.09, colour = NA)+
  scale_fill_manual('',values = c('grey1','grey30',"grey60","grey80"))

legend <- get_legend(A)
group1 <- plot_grid(S5_CH4,S6_CH4, ncol = 1, align = "hv")
Fig1<-plot_grid(group1,legend,rel_widths = c(1, .5))
ggsave(plot=Fig1, filename="Figures/Fig1_ModelCH4.png", width=9, height=5, bg="white", dpi=500)

### Figure 5 ###
library(scales)

### RMSE function ###
RMSE<-function(x_real,x_model){
  s2 = (x_real-x_model)^2
  s2 <- s2[!is.na(s2)]
  s <- sum(sqrt(s2)/length(s2))
  return(s)
}
## Table 2 data
#RMSE
dt_plot5
dt_RMSE_S5 <- dt_plot5[, .(RMSE_ABM = RMSE(CH4_slurry_pig,CH4_ABM_pig),
                           RMSE_A = RMSE(CH4_slurry_pig,CH4_A_pig),
                           RMSE_ABM_opt = RMSE(CH4_slurry_pig,CH4_ABM_opt_pig),
                           RMSE_A_opt = RMSE(CH4_slurry_pig,CH4_A_opt_pig)),
                          by = .(Batch)]

dt_RMSE_S6 <- dt_plot6[, .(RMSE_ABM = RMSE(CH4_slurry_pig,CH4_ABM_pig),
                           RMSE_A = RMSE(CH4_slurry_pig,CH4_A_pig),
                           RMSE_ABM_opt = RMSE(CH4_slurry_pig,CH4_ABM_opt_pig),
                           RMSE_A_opt = RMSE(CH4_slurry_pig,CH4_A_opt_pig)),
                       by = .(Batch)]

#CH4 g pig day
load("RData/mintegrate.RData")
library(zoo)
#Section 5
# Interpolate NaN values using na.approx at CH4_Total and CH4_enteric
dt_plot5$CH4_Total[105:120] <- na.approx(dt_plot5$CH4_Total[105:185])
dt_plot5$CH4_enteric[105:120] <- na.approx(dt_plot5$CH4_enteric[105:185])
dt_plot5$CH4_Total[200:250] <- na.approx(dt_plot5$CH4_Total[200:250])
dt_plot5$CH4_enteric[200:250] <- na.approx(dt_plot5$CH4_enteric[200:250])

dt_plot5[, cum_CH4_Total_Measured := mintegrate(time, CH4_Total, method = 'r', lwr = 0), by = c("Batch")]
dt_plot5[, cum_CH4_Total_Slurry := mintegrate(time, CH4_Total-CH4_enteric, method = 'r', lwr = 0), by = c("Batch")]
dt_plot5[, cum_CH4_Total_ABM := mintegrate(time, CH4_ABM, method = 'r', lwr = 0), by = c("Batch")]
dt_plot5[, cum_CH4_Total_A := mintegrate(time, CH4_A, method = 'r', lwr = 0), by = c("Batch")]
dt_plot5[, cum_CH4_Total_ABM_opt := mintegrate(time, CH4_ABM_opt, method = 'r', lwr = 0), by = c("Batch")]
dt_plot5[, cum_CH4_Total_A_opt := mintegrate(time, CH4_A_opt, method = 'r', lwr = 0), by = c("Batch")]

dt_CH4T_S5 <- dt_plot5[, .(T_CH4_measured = max(cum_CH4_Total_Measured,na.rm=TRUE),
                           T_CH4_slurry = max(cum_CH4_Total_Slurry,na.rm=TRUE),
                           T_CH4_ABM = max(cum_CH4_Total_ABM,na.rm=TRUE),
                           T_CH4_A = max(cum_CH4_Total_A,na.rm=TRUE),
                           T_CH4_ABM_opt = max(cum_CH4_Total_ABM_opt,na.rm=TRUE),
                           T_CH4_A_opt = max(cum_CH4_Total_A_opt,na.rm=TRUE),
                           Days = max(time)-min(time),
                           Pigs = mean(Pigs)),
                       by = .(Batch)]
dt_CH4T_S5[,CH4_pig_day_measured :=T_CH4_measured/Days/Pigs]
dt_CH4T_S5[,CH4_pig_day_slurry :=T_CH4_slurry/Days/Pigs]
dt_CH4T_S5[,CH4_pig_day_ABM :=T_CH4_ABM/Days/Pigs]
dt_CH4T_S5[,CH4_pig_day_A :=T_CH4_A/Days/Pigs]
dt_CH4T_S5[,CH4_pig_day_ABM_opt :=T_CH4_ABM_opt/Days/Pigs]
dt_CH4T_S5[,CH4_pig_day_A_opt :=T_CH4_A_opt/Days/Pigs]
DFSSDF
#Section 6
# Interpolate NaN values using na.approx at CH4_Total and CH4_enteric
dt_plot6$CH4_Total[105:120] <- na.approx(dt_plot6$CH4_Total[105:120])
dt_plot6$CH4_enteric[105:120] <- na.approx(dt_plot6$CH4_enteric[105:120])
dt_plot6$CH4_Total[200:250] <- na.approx(dt_plot6$CH4_Total[200:250])
dt_plot6$CH4_enteric[200:250] <- na.approx(dt_plot6$CH4_enteric[200:250])

dt_plot6[, cum_CH4_Total_Measured := mintegrate(time, CH4_Total, method = 'r', lwr = 0), by = c("Batch")]
dt_plot6[, cum_CH4_Total_Slurry := mintegrate(time, CH4_Total-CH4_enteric, method = 'r', lwr = 0), by = c("Batch")]
dt_plot6[, cum_CH4_Total_ABM := mintegrate(time, CH4_ABM, method = 'r', lwr = 0), by = c("Batch")]
dt_plot6[, cum_CH4_Total_A := mintegrate(time, CH4_A, method = 'r', lwr = 0), by = c("Batch")]
dt_plot6[, cum_CH4_Total_ABM_opt := mintegrate(time, CH4_ABM_opt, method = 'r', lwr = 0), by = c("Batch")]
dt_plot6[, cum_CH4_Total_A_opt := mintegrate(time, CH4_A_opt, method = 'r', lwr = 0), by = c("Batch")]

dt_CH4T_S6 <- dt_plot6[, .(T_CH4_measured = max(cum_CH4_Total_Measured,na.rm=TRUE),
                           T_CH4_slurry = max(cum_CH4_Total_Slurry,na.rm=TRUE),
                           T_CH4_ABM = max(cum_CH4_Total_ABM,na.rm=TRUE),
                           T_CH4_A = max(cum_CH4_Total_A,na.rm=TRUE),
                           T_CH4_ABM_opt = max(cum_CH4_Total_ABM_opt,na.rm=TRUE),
                           T_CH4_A_opt = max(cum_CH4_Total_A_opt,na.rm=TRUE),
                           Days = max(time)-min(time),
                           Pigs = mean(Pigs)),
                       by = .(Batch)]

dt_CH4T_S6[,CH4_pig_day_measured :=T_CH4_measured/Days/Pigs]
dt_CH4T_S6[,CH4_pig_day_slurry :=T_CH4_slurry/Days/Pigs]
dt_CH4T_S6[,CH4_pig_day_ABM :=T_CH4_ABM/Days/Pigs]
dt_CH4T_S6[,CH4_pig_day_A :=T_CH4_A/Days/Pigs]
dt_CH4T_S6[,CH4_pig_day_ABM_opt :=T_CH4_ABM_opt/Days/Pigs]
dt_CH4T_S6[,CH4_pig_day_A_opt :=T_CH4_A_opt/Days/Pigs]

##quantiles from 6 measurements
qCH4<-rbind(dt_CH4T_S5,dt_CH4T_S6)
qCH4<-qCH4[-4,]
quantile(qCH4$CH4_pig_day_measured, probs = c(0.25, 0.5, 0.75)) 
quantile(qCH4$CH4_pig_day_ABM, probs = c(0.25, 0.5, 0.75)) 
quantile(qCH4$CH4_pig_day_A, probs = c(0.25, 0.5, 0.75)) 
 ##quantiles from other studies
Fr<-c(16.3,16,21,26,6,4.2,43,23,10,4.1,28.5,7.4,4.5,4.9,25.5,19.1,17.7,30.9,7.0)
quantile(Fr, probs = c(0.25, 0.5, 0.75)) 
max(Fr)


### Figure 2 ###

### Microbes identification script ###

##DNA ANALYSIS

# load libraries
library(ampvis2)
library(readxl)
library(ggplot2)
library(dplyr)


# set working directory

# load data
metadata <- readxl::read_excel("../Microbial analysis/DNA/metadataPablo.xlsx")
otutable_V1V8 <- read_excel("../Microbial analysis/DNA/otutable_pablo_v1-8.xlsx")
otutable_mcra <- read_excel("../Microbial analysis/DNA/otutable_paclo_mcra.xlsx")

# create ampvis object
d_V1V8 <- amp_load(otutable = otutable_V1V8,
                   metadata = metadata)

d_mcra <- amp_load(otutable = otutable_mcra,
                   metadata = metadata)



##Plot relative abundance
df_mcra<-amp_export_long(d_mcra)

Mcra<-tapply(df_mcra$count,list(df_mcra$Genus,df_mcra$sample_type),sum,na.rm=TRUE)
Rel_Mcra<-as.data.frame(Mcra)
Rel_Mcra$Genus<-c("Unidentified","Candidatus_Methanomethylophilus","Methanobrevibacter",
                  "Methanocorpusculum","Methanoculleus","Methanomassiliicoccales","Methanosaeta",
                  "Methanosphaera","Methanospirillum","organism_methanogenic")
class(Rel_Mcra)<-c("data.table","data.frame")

datlong <- melt(Rel_Mcra, id.vars = c("Genus"))

datlong[,sum :=sum(value),by=c("variable")]
datlong[,relative :=value/sum*100,by=c("variable")]

datlong$variable <- factor(datlong$variable, levels = c("Time0_Solid_Before","Time0_Before","Time0_After", "Time0_Solid_After",  
                                                        "Time1_Before","Time1_After", "Time1_Liquid", 
                                                        "Time2_Before","Time2_After", 
                                                         "Time3_Before","Time3_After"))
datlong$Genus <- factor(datlong$Genus, levels = c("Unidentified","Candidatus_Methanomethylophilus", "organism_methanogenic",  
                                                        "Methanospirillum","Methanomassiliicoccales", "Methanoculleus", 
                                                        "Methanocorpusculum","Methanosaeta", 
                                                        "Methanobrevibacter","Methanosphaera"))

datlong$widths<-1
datlong$widths[which(datlong$variable=="Time0_Solid_Before")]<-1
datlong$widths[which(datlong$variable=="Time0_Before")]<-2
datlong$widths[which(datlong$variable=="Time0_After")]<-3.5
datlong$widths[which(datlong$variable=="Time0_Solid_After")]<-4.5
datlong$widths[which(datlong$variable=="Time1_Before")]<-6.5
datlong$widths[which(datlong$variable=="Time1_After")]<-8
datlong$widths[which(datlong$variable=="Time1_Liquid")]<-9
datlong$widths[which(datlong$variable=="Time2_Before")]<-11
datlong$widths[which(datlong$variable=="Time2_After")]<-12.5
datlong$widths[which(datlong$variable=="Time3_Before")]<-14.5
datlong$widths[which(datlong$variable=="Time3_After")]<-16

##Plot




DNA<-ggplot(datlong, aes(widths, relative, fill = Genus)) +
  geom_col() +
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


##PROTEIN ANALYSIS
dat_prot<-read.table(file="../Microbial analysis/Proteins/Tax_counts3.tsv",header=TRUE, sep = "\t")

Genus<-rep(dat_prot$genus,11)
#Species<-rep(data$species,11)
Tax_counts<-c(dat_prot$PG1_HK_JLN_REMESH,dat_prot$PG2_HK_JLN_REMESH,dat_prot$PG3_HK_JLN_REMESH,
              dat_prot$PG4_HK_JLN_REMESH,dat_prot$PG5_HK_JLN_REMESH,dat_prot$PG6_HK_JLN_REMESH,
              dat_prot$PG7_HK_JLN_REMESH,dat_prot$PG8_HK_JLN_REMESH,dat_prot$PG9_HK_JLN_REMESH,
              dat_prot$PG10_HK_JLN_REMESH,dat_prot$PG11_HK_JLN_REMESH)
SampleID<-c(rep("Time0_Before",nrow(dat_prot)),rep("Time0_Solid_Before",nrow(dat_prot)),rep("Time0_After",nrow(dat_prot)),
            rep("Time0_Solid_After",nrow(dat_prot)),rep("Time1_Before",nrow(dat_prot)),rep("Time1_After",nrow(dat_prot)),
            rep("Time1_Liquid_After",nrow(dat_prot)),rep("Time2_Before",nrow(dat_prot)),rep("Time2_After",nrow(dat_prot)),
            rep("Time3_Before",nrow(dat_prot)),rep("Time3_After",nrow(dat_prot)))

df_prots<-cbind.data.frame(Genus,
                           Tax_counts,SampleID)
matches <- grepl("Methano", df_prots$Genus)
df_Methanogens<-df_prots[matches,]
class(df_Methanogens)<-c("data.table","data.frame")

df_Methanogens[,sum :=sum(Tax_counts,na.rm=TRUE),by=c("SampleID")]
df_Methanogens[,relative :=Tax_counts/sum*100,by=c("SampleID")]

df_Methanogens$SampleID <- factor(df_Methanogens$SampleID, levels = c("Time0_Solid_Before","Time0_Before","Time0_After", "Time0_Solid_After",  
                                                        "Time1_Before","Time1_After", "Time1_Liquid_After", 
                                                        "Time2_Before","Time2_After", 
                                                        "Time3_Before","Time3_After"))

##Select more abundants
Abundance <- df_Methanogens[, .(sum = sum(relative,na.rm=TRUE)),
                          by = .( Genus)]
A_order<-Abundance[order(-sum), ]
Most_abundant_genus<-A_order$Genus[1:9]
Most_abundant_genus<-c(Most_abundant_genus,"Methanosphaera","Methanoplanus","Methanothrix")
df_Activity<-df_Methanogens[df_Methanogens$Genus %in% Most_abundant_genus, ]

Abundance_per_sample <- df_Methanogens[, .(sum = sum(relative,na.rm=TRUE)),
                            by = .( SampleID,Genus)]
Abundance_per_sample[Abundance_per_sample$Genus == "Methanosphaera",]

Abundance_per_sample[401:440,][order(-sum)]



sum_relative <- df_Activity[, sum(relative,na.rm=TRUE), by = SampleID]

diff_relative <- 100 - sum_relative$V1

# Create a new data.table with the rows to be added
new_rows <- data.table(Genus = "Rest",
                       Tax_counts = NA,
                       SampleID = sum_relative$SampleID,
                       sum = diff_relative,
                       relative = diff_relative)

# Bind the new rows to the original data.table
df_Activity <- rbind(df_Activity, new_rows)

df_Activity$widths<-1
df_Activity$widths[which(df_Activity$SampleID=="Time0_Solid_Before")]<-1
df_Activity$widths[which(df_Activity$SampleID=="Time0_Before")]<-2
df_Activity$widths[which(df_Activity$SampleID=="Time0_After")]<-3.5
df_Activity$widths[which(df_Activity$SampleID=="Time0_Solid_After")]<-4.5
df_Activity$widths[which(df_Activity$SampleID=="Time1_Before")]<-6.5
df_Activity$widths[which(df_Activity$SampleID=="Time1_After")]<-8
df_Activity$widths[which(df_Activity$SampleID=="Time1_Liquid_After")]<-9
df_Activity$widths[which(df_Activity$SampleID=="Time2_Before")]<-11
df_Activity$widths[which(df_Activity$SampleID=="Time2_After")]<-12.5
df_Activity$widths[which(df_Activity$SampleID=="Time3_Before")]<-14.5
df_Activity$widths[which(df_Activity$SampleID=="Time3_After")]<-16

##Plot
df_Activity$Genus <- factor(df_Activity$Genus, levels = c("Rest","Methanothrix", "Methanosphaera","Methanoplanus", "Methanocaldococcus",  
                                                  "Methanobacterium","Methanospirillum", "Methanofollis", 
                                                  "Methanohalophilus","Methanolobus", 
                                                  "Methanobrevibacter","Methanosarcina","Methanoculleus"))



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
## qPCR
dat_qPCR <- read_excel("../Microbial analysis/DNA/qPCR.xlsx")
class(dat_qPCR)<-c("data.table","data.frame")
dat_qPCR$widths<-1
dat_qPCR$widths[which(dat_qPCR$SampleID=="Time0_Solid_Before")]<-1
dat_qPCR$widths[which(dat_qPCR$SampleID=="Time0_Before")]<-2
dat_qPCR$widths[which(dat_qPCR$SampleID=="Time0_After")]<-3.5
dat_qPCR$widths[which(dat_qPCR$SampleID=="Time0_Solid_After")]<-4.5
dat_qPCR$widths[which(dat_qPCR$SampleID=="Time1_Before")]<-6.5
dat_qPCR$widths[which(dat_qPCR$SampleID=="Time1_After")]<-8
dat_qPCR$widths[which(dat_qPCR$SampleID=="Time1_Liquid_After")]<-9
dat_qPCR$widths[which(dat_qPCR$SampleID=="Time2_Before")]<-11
dat_qPCR$widths[which(dat_qPCR$SampleID=="Time2_After")]<-12.5
dat_qPCR$widths[which(dat_qPCR$SampleID=="Time3_Before")]<-14.5
dat_qPCR$widths[which(dat_qPCR$SampleID=="Time3_After")]<-16

##Add CH4 emissions
Batch4$Breaks<-c(1.5,4,6.5,8.5,11,12.5,14.5,16)
##Scaling secondary axis
range_y1 <- range(dat_qPCR$`Cell count mcrA`/0.1)
range_y2 <- range(Batch4$Mean_CH4/Batch4$Volume)
scale_factor <- max(range_y1) / max(range_y2)

library(scales)
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




Fig2pre <- plot_grid(qPCR,DNA,Prot, ncol = 1, align = "hv",rel_heights = c(1, 1))
Fig2pre2 <- plot_grid(Fig2pre, get_legend(Common_legend), nrow = 2, align = "v", axis = "b",rel_heights = c(1,0.1))
Fig2pre3 <- plot_grid(Fig2pre2, get_legend(DNA_legend), nrow = 2, align = "v", axis = "b",rel_heights = c(1,0.1))
Fig2 <- plot_grid(Fig2pre3, get_legend(Prot_legend), nrow = 2, align = "v", axis = "b",rel_heights = c(1,0.1))
ggsave(plot=Fig2,"Figures/Fig2_Microbes.png", width = 12, height = 15,bg='white')





