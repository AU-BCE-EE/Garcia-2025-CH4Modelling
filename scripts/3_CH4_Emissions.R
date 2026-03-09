

### Measured CH4 and Enteric CH4 ###

## ---- Number of pigs ----
Pigs_S5<-read.table("Rfiles/N_pigs_S5.txt") 
Pigs_S6<-read.table("Rfiles/N_pigs_S6.txt")

## ---- Measured CH4  ----
daily_avg_S5<-read.table(here("data","daily_CH4_S5.txt")) 
daily_avg_S6<-read.table(here("data","daily_CH4_S6.txt"))
Batch4<-read_excel(here("data","Batch4_MA.xlsx"))
Batch4$Date<-as.Date(c("2023-02-14","2023-02-14","2023-03-21","2023-03-21",
                       "2023-04-18","2023-04-18","2023-05-02","2023-05-02"))
Time_0<-as.Date("2022-05-09") #Start of Section 5
class(Batch4)<-c("data.table","data.frame")
Batch4[, etime := as.numeric(Date - Time_0)]
Batch4$Mean_CH4<-Batch4$Mean_CH4*24 #g CH4/day
Batch4$sd_CH4<-Batch4$sd_CH4*24 #g CH4/day

## ---- Estimated enteric CH4  ----
Data<-read.table(here("data","df_ABM2.txt"),header=TRUE, sep = "\t")
Data$Datetime<-anytime(Data$Datetime)
S5_values<-which(Data$Section==5  | Data$Section_C==5  | Data$Section_d==5 | Data$Section_Rate==5)
Data_S5<-Data[S5_values,]
days_S5 <- as.numeric(difftime(tail(Data_S5$Datetime[!is.na(Data_S5$Datetime)], 1),Data_S5$Datetime[1]))
S6_values<-which(Data$Section==6  | Data$Section_C==6  | Data$Section_d==6 | Data$Section_Rate==6)
Data_S6<-Data[S6_values,]
days_S6 <- as.numeric(difftime(tail(Data_S6$Datetime[!is.na(Data_S6$Datetime)], 1),Data_S6$Datetime[1]))

# Energy and Total Solids
Pos_5<-which(Feed_data$Section==56 | Feed_data$Section==5)
S5_Energy<-Feed_data$`MJ/kg TS`[Pos_5]
S5_TS<-Feed_data$`TS (%)`[Pos_5]
S5_Energy_time<-Feed_data$Date[Pos_5]
I_Energy_S5<-approxfun(S5_Energy_time,S5_Energy)
I_TS_S5<-approxfun(S5_Energy_time,S5_TS)

# Feed consumption rate
S5_feed_rate<-Data_S5$Feed_Rate[!is.na(Data_S5$Feed_Rate)]
S5_feed_rate_time<-Data_S5$Datetime[!is.na(Data_S5$Feed_Rate)]
S5_feed_rate_days<-as.numeric(diff(S5_feed_rate_time)) #days
S5_feed_rate_cum<-c(0,(cumsum(S5_feed_rate_days)))
S5_Pigs_feed<-I_pigs_S5(S5_feed_rate_time)
S5_Pigs_feed<-S5_Pigs_feed[-1] #Syncronize pigs and feed amount
S5_Pigs_feed[length(S5_Pigs_feed)+1]<-0 #Make sure feed and pigs are same length
Pigs_S5<-data.frame(time = round(S5_feed_rate_cum),Pigs = S5_Pigs_feed)
class(Pigs_S5)<-c("data.table","data.frame")
# Calculated total feed consumption per day per pig
S5_feed_per_pig_rate<-S5_feed_rate/S5_Pigs_feed
Inf_Pos<-which(S5_feed_per_pig_rate==Inf)
S5_feed_per_pig_rate[Inf_Pos]<-0 #Kg/pig/day
Pigs_S5$feed_rate<-S5_feed_per_pig_rate

# Calculate the energy consumption per pig per day (assuming only Solids have energy)
S5_Energy<-I_Energy_S5(as.Date(S5_feed_rate_time)) #Interpolate energy. Could be more precise to assume from 30-60Kg one type and from 60-115Kg another instead of interpolate
S5_TS<-I_TS_S5(as.Date(S5_feed_rate_time))  #Interpolate Total solids. Could be more precise to assume from 30-60Kg one type and from 60-115Kg another instead of interpolate
S5_Energy[1:10]<-S5_Energy[10] #Extrapolate assuming same as last known value
S5_TS[1:10]<-S5_TS[10] #Extrapolate assuming same as last known value
S5_Energy[235:257]<-S5_Energy[235] #Extrapolate assuming same as last known value
S5_TS[235:257]<-S5_TS[235] #Extrapolate assuming same as last known value

GE_S5<-S5_feed_per_pig_rate*(S5_TS/100)*S5_Energy #MJ/pig/day
Y_m<-0.24 #% 
n_S5<-S5_Pigs_feed #Pig number

#Enteric Fermentation in g per pig per day
Ent_CH4_S5_pig<-GE_S5*Y_m/100/0.05565 #g/pig/day #Why by the number of pigs??? Ask Frederik
Ent_CH4_S5_day<-Ent_CH4_S5_pig*S5_Pigs_feed #g/day

Int_Enteric_S5<-approxfun(S5_feed_rate_cum,Ent_CH4_S5_day)


Ent_CH4_S5_day_plot<-Int_Enteric_S5(S5_CH4_measured_cum)

df_Enteric_S5<-data.frame(time = S5_CH4_measured_cum,CH4_ent = Ent_CH4_S5_day_plot,
                          Datetime = S5_CH4_measured_time)

df_Enteric_S5$group<-floor(df_Enteric_S5$time)

daily_Entavg_S5<-aggregate(df_Enteric_S5$CH4_ent, list(df_Enteric_S5$group), FUN=mean)

##Section 6##
# Energy and Total Solids
Pos_6<-which(Feed_data$Section==56 | Feed_data$Section==6)
S6_Energy<-Feed_data$`MJ/kg TS`[Pos_6]
S6_TS<-Feed_data$`TS (%)`[Pos_6]
S6_Energy_time<-Feed_data$Date[Pos_6]
I_Energy_S6<-approxfun(S6_Energy_time,S6_Energy)
I_TS_S6<-approxfun(S6_Energy_time,S6_TS)

# Feed consumption rate
S6_feed_rate<-Data_S6$Feed_Rate[!is.na(Data_S6$Feed_Rate)]
S6_feed_rate_time<-Data_S6$Datetime[!is.na(Data_S6$Feed_Rate)]
S6_feed_rate_days<-as.numeric(diff(S6_feed_rate_time)) #days
S6_feed_rate_cum<-c(0,(cumsum(S6_feed_rate_days)))
S6_Pigs_feed<-I_pigs_S6(S6_feed_rate_time)
S6_Pigs_feed<-S6_Pigs_feed[-1] #Syncronize pigs and feed amount
S6_Pigs_feed[length(S6_Pigs_feed)+1]<-0 #Make sure feed and pigs are same length

Pigs_S6<-data.frame(time = round(S6_feed_rate_cum),Pigs = S6_Pigs_feed)
class(Pigs_S6)<-c("data.table","data.frame")

# Calculated total feed consumption per day per pig
S6_feed_per_pig_rate<-S6_feed_rate/S6_Pigs_feed
Inf_Pos<-which(S6_feed_per_pig_rate==Inf)
S6_feed_per_pig_rate[Inf_Pos]<-0 #Kg/pig/day
Pigs_S6$feed_rate<-S6_feed_per_pig_rate

# Calculate the energy consumption per pig per day (assuming only Solids have energy)
S6_Energy<-I_Energy_S6(as.Date(S6_feed_rate_time)) #Interpolate energy. Could be more precise to assume from 30-60Kg one type and from 60-115Kg another instead of interpolate
S6_TS<-I_TS_S6(as.Date(S6_feed_rate_time))  #Interpolate Total solids. Could be more precise to assume from 30-60Kg one type and from 60-115Kg another instead of interpolate
S6_Energy[1:10]<-S6_Energy[10] #Extrapolate assuming same as last known value
S6_TS[1:10]<-S6_TS[10] #Extrapolate assuming same as last known value
S6_Energy[235:257]<-S6_Energy[235] #Extrapolate assuming same as last known value
S6_TS[235:257]<-S6_TS[235] #Extrapolate assuming same as last known value

GE_S6<-S6_feed_per_pig_rate*(S6_TS/100)*S6_Energy #MJ/pig/day
Y_m<-0.24 #% 
n_S6<-S6_Pigs_feed #Pig number

#Enteric Fermentation in g per pig per day
Ent_CH4_S6_pig<-GE_S6*Y_m/100/0.05565 #g/pig/day #Why by the number of pigs??? Ask Frederik
Ent_CH4_S6_day<-Ent_CH4_S5_pig*S6_Pigs_feed #g/day
Int_Enteric_S6<-approxfun(S6_feed_rate_cum,Ent_CH4_S6_day)
Ent_CH4_S6_day_plot<-Int_Enteric_S6(S6_CH4_measured_cum)

df_Enteric_S6<-data.frame(time = S6_CH4_measured_cum,CH4_ent = Ent_CH4_S6_day_plot, 
                          Datetime = S6_CH4_measured_time)
df_Enteric_S6$group<-floor(df_Enteric_S6$time)

daily_Entavg_S6<-aggregate(df_Enteric_S6$CH4_ent, list(df_Enteric_S6$group), FUN=mean)
