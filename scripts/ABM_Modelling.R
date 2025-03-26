library(anytime)
library(ABM)
Sys.setenv(LANG = "en")
library(tidyr)
library(zoo)
library(dplyr)
library(readxl)
library(ggplot2)


## ABM run ##


setwd(".../Rfiles") #Choose own directory
Data<-read.table(file="df_ABM2.txt",header=TRUE, sep = "\t")
Data$Datetime<-anytime(Data$Datetime)

### Total number of days ###

##Section 5##
S5_values<-which(Data$Section==5  | Data$Section_C==5  | Data$Section_d==5 | Data$Section_Rate==5)
Data_S5<-Data[S5_values,]
days_S5 <- as.numeric(difftime(tail(Data_S5$Datetime[!is.na(Data_S5$Datetime)], 1),Data_S5$Datetime[1]))

##Section 6##
S6_values<-which(Data$Section==6  | Data$Section_C==6  | Data$Section_d==6 | Data$Section_Rate==6)
Data_S6<-Data[S6_values,]
days_S6 <- as.numeric(difftime(tail(Data_S6$Datetime[!is.na(Data_S6$Datetime)], 1),Data_S6$Datetime[1]))

### Same time step as "real" emission ###
##Section 5##
times_S5<-Data_S5$Datetime[!is.na(Data_S5$CH4_g_d)]
##Section 6##
times_S6<-Data_S6$Datetime[!is.na(Data_S6$CH4_g_d)]

### Slurry mass data frame ###
##Section 5##
S5_slurry_mass<-Data_S5$Slurry_Mass[!is.na(Data_S5$Slurry_Mass)]
S5_slurry_mass<-c(S5_slurry_mass[1],S5_slurry_mass)
S5_slurry_mass_time<-Data_S5$Datetime[!is.na(Data_S5$Slurry_Mass)]
S5_slurry_mass_time<-c(anytime("2022-05-09 00:00:00 CEST"),S5_slurry_mass_time)
S5_slurry_mass_secs<-diff(S5_slurry_mass_time) #Seconds
S5_slurry_mass_days<-as.numeric(S5_slurry_mass_secs/(60*60*24)) #Days
S5_slurry_mass_cum<-c(0,(cumsum(S5_slurry_mass_days)))

df_slurry_mass_S5<-data.frame(time=S5_slurry_mass_cum,
                           slurry_mass=S5_slurry_mass)
df_slurry_mass_S5<-distinct(df_slurry_mass_S5)
df_slurry_mass_S5<-df_slurry_mass_S5[-51,]

S5_Int_slurry_mass<-approxfun(df_slurry_mass_S5$time,df_slurry_mass_S5$slurry_mass)


##Section 6##
S6_slurry_mass<-Data_S6$Slurry_Mass[!is.na(Data_S6$Slurry_Mass)]
S6_slurry_mass<-c(S6_slurry_mass[1],S6_slurry_mass)
S6_slurry_mass_time<-Data_S6$Datetime[!is.na(Data_S6$Slurry_Mass)]
S6_slurry_mass_time<-c(anytime("2022-05-05 00:00:00 CEST"),S6_slurry_mass_time)
S6_slurry_mass_secs<-diff(S6_slurry_mass_time) #Seconds
S6_slurry_mass_days<-as.numeric(S6_slurry_mass_secs/(60*60*24)) #Days
S6_slurry_mass_cum<-c(0,cumsum(S6_slurry_mass_days))

df_slurry_mass_S6<-data.frame(time=S6_slurry_mass_cum,
                              slurry_mass=S6_slurry_mass)
df_slurry_mass_S6<-distinct(df_slurry_mass_S6)

S6_Int_slurry_mass<-approxfun(df_slurry_mass_S6$time,df_slurry_mass_S6$slurry_mass)

### Depth ###
storage_depth<-0.5

### Surface accesible by animals ###
floor_area<-(5.064*2.374)*16

### Total surface area ###
area <- (5.064*2.374)*16+19.174*1

### Slurry temperature ###
##Section 5##
S5_slurry_temp<-Data_S5$Temperature_C[!is.na(Data_S5$Temperature_C)]
S5_slurry_temp_time<-Data_S5$Datetime[!is.na(Data_S5$Temperature_C)]
S5_slurry_temp_secs<-diff(S5_slurry_temp_time) #Seconds
S5_slurry_temp_days<-as.numeric(S5_slurry_temp_secs/(60*60*24)) #Days
S5_slurry_temp_cum<-c(0,(cumsum(S5_slurry_temp_days)))

df_temp_C_S5<-data.frame(time = S5_slurry_temp_cum,
                              temp_C = S5_slurry_temp)
df_temp_C_S5<-distinct(df_temp_C_S5)
## Add for Batch 4
df_add_temp<-data.frame(time = c(281,358),temp_C = c(15.8,17.7))
df_temp_C_S5<-rbind(df_temp_C_S5,df_add_temp)
##Section 6##
S6_slurry_temp<-Data_S6$Temperature_C[!is.na(Data_S6$Temperature_C)]
S6_slurry_temp_time<-Data_S6$Datetime[!is.na(Data_S6$Temperature_C)]
S6_slurry_temp_secs<-diff(S6_slurry_temp_time) #Seconds
S6_slurry_temp_days<-as.numeric(S6_slurry_temp_secs/(60*60*24)) #Days
S6_slurry_temp_cum<-c(0,(cumsum(S6_slurry_temp_days)))

df_temp_C_S6<-data.frame(time = S6_slurry_temp_cum,
                         temp_C = S6_slurry_temp)
df_temp_C_S6<-distinct(df_temp_C_S6)


### MEASURED CH4 (g/day) ###
##Section 5##
S5_CH4_measured<-Data_S5$CH4_g_d[!is.na(Data_S5$CH4_g_d)]
S5_CH4_measured_time<-Data_S5$Datetime[!is.na(Data_S5$CH4_g_d)]

##Sincronize with ABM###############################
S5_CH4_measured_time<-S5_CH4_measured_time[-c(1:217)]
S5_CH4_measured<-S5_CH4_measured[-c(1:217)]
#S5_CH4_measured_time[1]<-anytime("2022-05-09 00:00:00 CEST")
######################################################

S5_CH4_measured_secs<-diff(S5_CH4_measured_time) #Seconds
S5_CH4_measured_days<-as.numeric(S5_CH4_measured_secs/(60*60*24)) #Days
S5_CH4_measured_cum<-c(0,(cumsum(S5_CH4_measured_days)))

df_CH4_measured_S5out<-data.frame(time = S5_CH4_measured_cum,
                              CH4 =S5_CH4_measured,
                              Days = S5_CH4_measured_time)

Outliers_S5<-which(df_CH4_measured_S5out$CH4>25000)
df_CH4_measured_S5<-df_CH4_measured_S5out[-c(Outliers_S5),]
df_CH4_measured_S5$group<-floor(df_CH4_measured_S5$time)

daily_avg_S5<-aggregate(df_CH4_measured_S5$CH4, list(df_CH4_measured_S5$group), FUN=mean)
save(daily_avg_S5,file="RData/Sprod_S5.RData")


##Section 6##
S6_CH4_measured<-Data_S6$CH4_g_d[!is.na(Data_S6$CH4_g_d)]
S6_CH4_measured_time<-Data_S6$Datetime[!is.na(Data_S6$CH4_g_d)]
##Sincronize with ABM###############################
S6_CH4_measured_time<-S6_CH4_measured_time[-c(1:74)]
S6_CH4_measured<-S6_CH4_measured[-c(1:74)]
######################################################
S6_CH4_measured_secs<-diff(S6_CH4_measured_time) #Seconds
S6_CH4_measured_days<-as.numeric(S6_CH4_measured_secs/(60*60*24)) #Days
S6_CH4_measured_cum<-c(0,(cumsum(S6_CH4_measured_days)))


df_CH4_measured_S6out<-data.frame(time = S6_CH4_measured_cum,
                               CH4 =S6_CH4_measured,
                               Days = S6_CH4_measured_time)

Outliers_S6<-which(df_CH4_measured_S6out$CH4>25000)
df_CH4_measured_S6<-df_CH4_measured_S6out[-c(Outliers_S6),]
df_CH4_measured_S6$group<-floor(df_CH4_measured_S6$time)

daily_avg_S6<-aggregate(df_CH4_measured_S6$CH4, list(df_CH4_measured_S6$group), FUN=mean)
save(daily_avg_S6,file="RData/Sprod_S6.RData")


### MEASURED CH4 (g/day/pig) ###
##Section 5##
S5_N_pigs<-Data_S5$Pigs_Number[!is.na(Data_S5$Pigs_Number)]
S5_N_pigs_time<-Data_S5$Datetime[!is.na(Data_S5$Pigs_Number)]

I_pigs_S5<-approxfun(S5_N_pigs_time,S5_N_pigs)
Int_pigs_S5<-round(I_pigs_S5(S5_CH4_measured_time))

S5_CH4_gd_pig<-S5_CH4_measured/Int_pigs_S5
Inf_Pos<-which(S5_CH4_gd_pig==Inf)
S5_CH4_gd_pig[Inf_Pos]<-0

##Section 6##
S6_N_pigs<-Data_S6$Pigs_Number[!is.na(Data_S6$Pigs_Number)]
S6_N_pigs_time<-Data_S6$Datetime[!is.na(Data_S6$Pigs_Number)]

I_pigs_S6<-approxfun(S6_N_pigs_time,S6_N_pigs)
Int_pigs_S6<-round(I_pigs_S6(S6_CH4_measured_time))

S6_CH4_gd_pig<-S6_CH4_measured/Int_pigs_S6
Inf_Pos<-which(S6_CH4_gd_pig==Inf)
S6_CH4_gd_pig[Inf_Pos]<-0


### Washing intervals ###
##Section 5##
Start_5<-as.Date(Data_S5$Datetime[500])
Wash_51<-as.Date(Data_S5$Datetime[5352]) ##Picarro off from 24 to 27, no humidity increase from 28-->Assumption: they clean on the 27
Wash_52<-as.Date(Data_S5$Datetime[11897]) ##  Picarro off on the 26, no humidity increase--> Assumption: they clean on the 27

wash_int_S5<-c(as.numeric(difftime(Wash_51,Start_5,units="days")),
               as.numeric(difftime(Wash_52,Start_5,units="days")))
#Assumption-->  100Kg of water per average pig number during the batch one day before next round starts
S5_wash<-rep(0,nrow(df_slurry_mass_S5))
df_slurry_mass_S5$wash_water<-S5_wash
S51_wash_water<-100*mean(Int_pigs_S5[17:2699],na.rm=TRUE) # Kg water/day
S52_wash_water<-100*mean(Int_pigs_S5[3245:6346],na.rm=TRUE) # Kg water/day
S53_wash_water<-100*283.6585 # Kg water/day, 283.6585 = mean


df_wash_S5<-data.frame(time = c(wash_int_S5[1],wash_int_S5[2],279),
                       slurry_mass = c(46535.495,47169.343,27498.247),
                       wash_water = c(rep(S51_wash_water,1),rep(S52_wash_water,1),rep(S53_wash_water,1)))
df_slurry_water_S5<-rbind.data.frame(df_slurry_mass_S5,df_wash_S5)
df_slurry_water_S5<-df_slurry_water_S5[order(df_slurry_water_S5$time),]
df_add<-data.frame(time = c(281.46,281.54,316.50,316.6,344.48,344.58,358.48,358.58),
                     slurry_mass = c(27498.24688,8460.99904,50765.99424,8460.99904,
                     33843.99616,8460.99904,31728.7464,10576.2488),
                     wash_water = 0)
df_slurry_water_S5<-rbind(df_slurry_water_S5,df_add)

  ##Section 6##
Start_6<-as.Date(Data_S6$Datetime[277])
Wash_61<-as.Date(Data_S6$Datetime[4890]) # Based on water concentration, washing clearly on day 20
Wash_62<-as.Date(Data_S6$Datetime[10580]) # Picarro off from 1 to 4. Water in some of these days. Assumption: on the 1st

wash_int_S6<-c(as.numeric(difftime(Wash_61,Start_6,units="days")),
               as.numeric(difftime(Wash_62,Start_6,units="days")))

#Assumption--> 3 days in cleaning and 100Kg of water per average pig number during the batch
S6_wash<-rep(0,nrow(df_slurry_mass_S6))
df_slurry_mass_S6$wash_water<-S6_wash
S61_wash_water<-100*mean(Int_pigs_S6[98:2726],na.rm=TRUE) # Kg water/day
S62_wash_water<-100*mean(Int_pigs_S6[3471:5750],na.rm=TRUE) # Kg water/day


df_wash_S6<-data.frame(time = c(wash_int_S6[1],wash_int_S6[2]),
                       slurry_mass = c(29613.497,28325.479),
                       wash_water = c(rep(S61_wash_water,1),rep(S62_wash_water,1)))
df_slurry_water_S6<-rbind.data.frame(df_slurry_mass_S6,df_wash_S6)
df_slurry_water_S6<-df_slurry_water_S6[order(df_slurry_water_S6$time),]

### CALCULATED CH4 ENTERIC EMISSION (g/day/pig) ###

setwd(".../data") #Choose own directory
Feed_data<-read_excel("Feed_analysis_R.xlsx")
Feed_data$Date<-c(as.Date("2022-05-17"),as.Date("2022-06-14"),as.Date("2022-07-12"),
                  as.Date("2022-08-16"),as.Date("2022-09-27"),as.Date("2022-10-25"),
                  as.Date("2022-11-15"),as.Date("2022-12-06"),as.Date("2023-01-03"),
                  as.Date("2022-12-13"),as.Date("2023-01-17"),as.Date("2023-01-10"))
Feed_data$Section<-c(56,56,56,56,56,56,6,5,5,6,5,6)

##Section 5##
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
Ent_CH4_S5_pig<-GE_S5*Y_m/100/0.05565 #g/pig/day 
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

setwd(".../RData") #Choose own directory

save(daily_Entavg_S6,daily_Entavg_S5,file = "Enteric_AVG.RData")
save(daily_avg_S5,daily_avg_S6,file = "CH4_AVG.RData")
save(Pigs_S5,Pigs_S6,file = "Pigs.RData")
##Add Enteric emission to df_ABM dataframe
add=1

if (add==1){
Datetime<-c(Data$Datetime,df_Enteric_S5$Datetime,df_Enteric_S6$Datetime)
CH4_g_d<-c(Data$CH4_g_d,rep(NaN,length(df_Enteric_S5$Datetime)),rep(NaN,length(df_Enteric_S6$Datetime)))
Section_d<-c(Data$Section_d,rep(NaN,length(df_Enteric_S5$Datetime)),rep(NaN,length(df_Enteric_S6$Datetime)))
Temperature_C<-c(Data$Temperature_C,rep(NaN,length(df_Enteric_S5$Datetime)),rep(NaN,length(df_Enteric_S6$Datetime)))
Section_C<-c(Data$Section_C,rep(NaN,length(df_Enteric_S5$Datetime)),rep(NaN,length(df_Enteric_S6$Datetime)))
Pigs_Number<-c(Data$Pigs_Number,rep(NaN,length(df_Enteric_S5$Datetime)),rep(NaN,length(df_Enteric_S6$Datetime)))
Pigs_Mass<-c(Data$Pigs_Mass,rep(NaN,length(df_Enteric_S5$Datetime)),rep(NaN,length(df_Enteric_S6$Datetime)))
Slurry_Mass<-c(Data$Slurry_Mass,rep(NaN,length(df_Enteric_S5$Datetime)),rep(NaN,length(df_Enteric_S6$Datetime)))
Feed_Rate<-c(Data$Feed_Rate,rep(NaN,length(df_Enteric_S5$Datetime)),rep(NaN,length(df_Enteric_S6$Datetime)))
Batch_Rate<-c(Data$Batch_Rate,rep(NaN,length(df_Enteric_S5$Datetime)),rep(NaN,length(df_Enteric_S6$Datetime)))
Section_Rate<-c(Data$Section_Rate,rep(NaN,length(df_Enteric_S5$Datetime)),rep(NaN,length(df_Enteric_S6$Datetime)))
Section<-c(Data$Section,rep(NaN,length(df_Enteric_S5$Datetime)),rep(NaN,length(df_Enteric_S6$Datetime)))
Batch<-c(Data$Batch,rep(NaN,length(df_Enteric_S5$Datetime)),rep(NaN,length(df_Enteric_S6$Datetime)))
Comments<-c(Data$Comments,rep(NaN,length(df_Enteric_S5$Datetime)),rep(NaN,length(df_Enteric_S6$Datetime)))
CH4_Enteric<-c(rep(NaN,nrow(Data)),df_Enteric_S5$CH4_ent,df_Enteric_S6$CH4_ent)
Section_Ent<-c(rep(NaN,nrow(Data)),rep(5,nrow(df_Enteric_S5)),rep(6,nrow(df_Enteric_S6)))

df_ABM3<-cbind.data.frame(Datetime,CH4_g_d,Temperature_C,Pigs_Number,
                         Pigs_Mass,Slurry_Mass,Feed_Rate,Batch_Rate,Section_Rate,
                         Section,Batch,Section_C,Section_d,Comments,CH4_Enteric,Section_Ent)

df_ABM3 <- df_ABM3[order(df_ABM3$Datetime), ]
setwd(".../Rfiles") #Choose own directory
write.table(df_ABM3, file = "df_ABM3.txt", sep = "\t", row.names = TRUE, col.names = NA)
}else{}


### FRESH MANURE COMPOSITION URINE/FEACES ###
setwd(".../data") #Choose own directory
Data_Composition<-read_excel("Composition_R.xlsx")
Data_Slurry<-read_excel("Slurry_pH_R.xlsx")
COD_conv <- c(xa_dead = 0.73, RFd = 0.8444792, iNDF = 0.8444792, starch = 0.8444792, Cfat = 0.3117844, CP = 0.6541602,
              VFA = 0.9383125)
Manure_dens<-1 #Kg/L
Urine_dens<-1 #Kg/L
Faeces_dens<-1 #Kg/L

## Section 5 ##

##Slurry
#pH (Values at the start and finish are assume to be the same as next/previous
Pos_5S<-which(Data_Slurry$Section==5)
S5_pH_S<-Data_Slurry$pH[Pos_5S]
S5_pH_S_Time<-Data_Slurry$Date[Pos_5S]
S5_pH_S_days<-as.numeric(diff(S5_pH_S_Time)) #days
S5_pH_S_cum<-c(0,cumsum(S5_pH_S_days))
df_S5_pH<-data.frame(time=S5_pH_S_cum,pH = S5_pH_S )
### Add for batch 4 ###
averages <- colMeans(df_S5_pH)
averages_df <- data.frame(t(replicate(4, averages)))
averages_df$time<-c(281,316,344,358)
df_S5_pH<-rbind(df_S5_pH,averages_df)
## Urine
Pos_5U<-which(Data_Composition$Section==5 & Data_Composition$Type == "Urine")
S5_Composition_timeU<-Data_Composition$Time[Pos_5U]

#pH (USe the slurry pH over time)
S5_pH<-Data_Composition$pH[Pos_5U]


# SO4
Ratio_S_SO4<-32.06/96.06
S5_SO4_mgL<-Data_Composition$`SO4_mg/L`[Pos_5U] #mg SO4/L urine
S5_SO4<-S5_SO4_mgL/Urine_dens/1000 #g SO4/Kg urine
S5_SO4_S<-S5_SO4*Ratio_S_SO4 #g S/Kg urine


# Urea
S5_TN_U<-Data_Composition$`TN_g/Kg`[Pos_5U] #g Urine-TN/Kg urine
S5_Urea_N<-S5_TN_U*0.75 #g N/Kg urine--> Assuming Urea N is equal to urine N * 0.75 
#VFA
S5_VFA_mgL_U<-Data_Composition$`VFA_mg/L`[Pos_5U] #mg VFA/L Urine
S5_VFA_U<-S5_VFA_mgL_U/Urine_dens/1000 #g VFA/Kg Urine
S5_VFA_COD_U<-S5_VFA_U/COD_conv[7] #g COD/Kg Urine


#TAN
S5_TAN_U<-S5_TN_U*(1-0.75) #g Rest-N/Kg Urine 

#Ash
S5_VS_U<-Data_Composition$`VS_%`[Pos_5U] #%-VS/DM 
S5_Ash_U<-100-S5_VS_U #%-Ash/DM 
S5_Ash_g_Kg_U <- ((S5_Ash_U* Data_Composition$`DM_%`[Pos_5U]/100)/100)*1000 # g Ash/ Kg Urine

#VS (Assume everything is degradable) ###Recalculate with Urea mass and substract to VS-->assume 100% degradable
S5_VS_Pct_U<-(S5_VS_U* Data_Composition$`DM_%`[Pos_5U]/100) #%-VS/ Urine--> Kg VS/100Kg Urine
S5_VS_g_Kg_U <-(S5_VS_Pct_U/100)*1000 # g VS/ Kg Urine
Ratio_Urea_Nitrogen<-60.06/(14*2)
S5_Urea_g_Kg<-S5_Urea_N*Ratio_Urea_Nitrogen #g Urea/Kg Urine
S5_VSd_A_U<-S5_VS_g_Kg_U-S5_Urea_g_Kg ## Assume that all VS are degradable after Urea substraction
S5_VSnd_A_U<-S5_VSd_A_U-S5_VSd_A_U ## No VSnd in Urine

# Data frame for Urine
df_Urine<-data.frame(time = as.Date(S5_Composition_timeU),pH_U = S5_pH, SO4_U = S5_SO4_S,
                     Urea_U = S5_Urea_N, VFA_U = S5_VFA_COD_U, TAN_U = S5_TAN_U, 
                     Ash_U = S5_Ash_g_Kg_U, VSd_A_U = S5_VSd_A_U, VSnd_A_U = S5_VSnd_A_U) 

## Faeces
Pos_5F<-which(Data_Composition$Section==5 & Data_Composition$Type == "Faeces")

#TAN 
Ratio_N_NH4 <- 14/18
S5_TAN<-Data_Composition$`NH4-N_g/Kg`[Pos_5F] #g NH4/Kg Faeces
S5_TAN_F <-S5_TAN/Ratio_N_NH4 #g N/Kg Faeces

#VFA
S5_VFA_mgL_F<-Data_Composition$`VFA_mg/L`[Pos_5F] #mg VFA/L Faeces
S5_VFA_F<-S5_VFA_mgL_F/Faeces_dens/1000 #g VFA/Kg Faeces
S5_VFA_COD_F<-S5_VFA_F/COD_conv[7] #g COD/Kg Faeces


#CP 
S5_CP<-(Data_Composition$`TN_g/Kg`[Pos_5F]-S5_TAN)*6.25-0 #g CP/Kg Faeces --> Assuming faeces 0 Urea
S5_CP_COD<-S5_CP/COD_conv[6] # g COD/Kg Faeces

#NDF Substract NDF - RDF 
RFd<-29.1 #gCOD/kg Assumption
Fibers_Faeces<-read_excel("Fibers_R.xlsx")
Pos_5Fibers<-which(Fibers_Faeces$Section == 5)
S5_NDF_F_time<-as.Date(Fibers_Faeces$Date[Pos_5Fibers])
S5_NDF_F<-Fibers_Faeces$`NDF_%`[Pos_5Fibers] #% NDF/DM -->  in Kg NDF / 100 Kg DM
S5_NDF_F_g_Kg<-((S5_NDF_F*Fibers_Faeces$`DM_%`[Pos_5Fibers]/100)/100)*1000 #g NDF/ Kg faeces
S5_NDF_F_COD<-S5_NDF_F_g_Kg/COD_conv[3] #g COD / Kg Faeces
df_Fibers_F<-data.frame(time = S5_NDF_F_time,NDF_F = S5_NDF_F_COD)

#Ash
S5_VS_F<-Data_Composition$`VS_%`[Pos_5F] #%-VS/DM 
S5_Ash_F<-100-S5_VS_F #%-Ash/DM 
S5_Ash_g_Kg_F <- ((S5_Ash_F* Data_Composition$`VS_%`[Pos_5F]/100)/100)*1000 # g Ash/ Kg Faeces

#VS
S5_VS_F<-Data_Composition$`VS_%`[Pos_5F] #%-VS/DM 
S5_VS_Pct_F<-(S5_VS_F* Data_Composition$`DM_%`[Pos_5F]/100) #%-VS/ Faeces--> Kg VS/100Kg Faeces
S5_VS_Kg_Kg_F <-((S5_VS_F* Data_Composition$`DM_%`[Pos_5F]/100)/100) # Kg VS/ Kg Faeces

###############VF_degradable ######################
###############Theoretical methane potential####### 
# (units NL CH4/Kg VS)
VFA_Bt <- 373 
Eth_Bt <- 730 #Ethanol--> assume 0
CP_Bt <- 496
CL_Bt <- 1014 
Lignin_Bt <- 727
HCel_Bt <- 415
Cel_Bt <- 415
CH_Bt <- 415 #Carbohydrates --> assume the remaining %

##Composition in % of volatile solids
S5_DM_F<-Data_Composition$`DM_%`[Pos_5F]
#VFA
S5_VFA_gKg<-S5_VFA_F*1/(S5_VS_Pct_F/100) # g VFA/Kg VS
S5_VFA_PctVS<-S5_VFA_gKg/1000*100 #% VFA/VS
S5_VFA_PctVS[5:10]<-S5_VFA_PctVS[5]
#CP
S5_CP_gKg<-S5_CP*1/(S5_VS_Pct_F/100) # g CP/Kg VS
S5_CP_PctVS<-S5_CP_gKg/1000*100 #% CP/VS
S5_CP_PctVS[5:10]<-S5_CP_PctVS[5]

#Lignin (equal to ADL)
S5_ADL_F<-Fibers_Faeces$`ADL_%`[Pos_5Fibers] #% ADL/DM -->  in Kg ADL / 100 Kg DM
S5_ADL_PctVS<-S5_ADL_F[1:2]*(100/S5_VS_F[1:2]) #% ADL/VS
S5_ADL_PctVS[3:4]<-S5_ADL_F[3:4]*(100/S5_VS_F[3:4]) #% ADL/VS
S5_ADL_PctVS[5:10]<-S5_ADL_F[5:10]*(100/S5_VS_F[5]) #% ADL/VS

#Hemicellulose (equal to NDF-ADF)
S5_ADF_F<-Fibers_Faeces$`ADF_%`[Pos_5Fibers] #% ADL/DM -->  in Kg ADF / 100 Kg DM
S5_ADF_PctVS<-S5_ADF_F[1:2]*(100/S5_VS_F[1:2]) #% ADF/VS
S5_ADF_PctVS[3:4]<-S5_ADF_F[3:4]*(100/S5_VS_F[3:4]) #% ADF/VS
S5_ADF_PctVS[5:10]<-S5_ADF_F[5:10]*(100/S5_VS_F[5]) #% ADF/VS
S5_NDF_PctVS<-S5_NDF_F[1:2]*(100/S5_VS_F[1:2]) #% NDF/VS
S5_NDF_PctVS[3:4]<-S5_NDF_F[3:4]*(100/S5_VS_F[3:4]) #% NDF/VS
S5_NDF_PctVS[5:10]<-S5_NDF_F[5:10]*(100/S5_VS_F[5]) #% NDF/VS
S5_HCel_PctVS <- S5_NDF_PctVS - S5_ADF_PctVS #% Hemicellulose/VS

#Cellulose (equal to ADF-ADL)
S5_Cel_PctVS <- S5_ADF_PctVS - S5_ADL_PctVS

#Lipids 
Fat_Faeces<-read_excel("Fat_R.xlsx")
Pos_5Fat<-which(Fat_Faeces$Section == 5)
S5_Cfat_F_time<-as.Date(Fat_Faeces$Date[Pos_5Fat])
S5_CL_F<-Fat_Faeces$`FAT_%`[Pos_5Fat] #% Fat/DM -->  in Kg Fat / 100 Kg DM
S5_Cfat_F_g_Kg<-((S5_CL_F*Fibers_Faeces$`DM_%`[Pos_5Fibers]/100)/100)*1000 #g Fat/ Kg faeces
S5_Cfat_F_COD<-S5_Cfat_F_g_Kg/COD_conv[5] #g COD / Kg Faeces
df_Fat_F<-data.frame(time = S5_Cfat_F_time,Cfat_F = S5_Cfat_F_COD)
# S5_CL_F<-79.62 #g/KgDM
S5_CL_PctVS<-(((S5_CL_F[1:2]*10)*(100/S5_VS_F[1:2]))/1000)*100 #% CL/ VS
S5_CL_PctVS[3:4]<-(((S5_CL_F[3:4]*10)*(100/S5_VS_F[3:4]))/1000)*100 #% CL/ VS
S5_CL_PctVS[5:6]<-(((S5_CL_F[5:6]*10)*(100/S5_VS_F[5:6]))/1000)*100 #% CL/ VS
S5_CL_PctVS[7:8]<-(((S5_CL_F[7:8]*10)*(100/S5_VS_F[5:6]))/1000)*100 #% CL/ VS
S5_CL_PctVS[9:10]<-(((S5_CL_F[9:10]*10)*(100/S5_VS_F[5:6]))/1000)*100 #% CL/ VS


# Carbohydrates (remaining percentage)
S5_CH_PctVS <- 100 - S5_Cel_PctVS-S5_HCel_PctVS-S5_ADL_PctVS-S5_CP_PctVS-S5_VFA_PctVS-S5_CL_PctVS

##Theoretical BMP
S5_t_BMP <- S5_CH_PctVS/100*CH_Bt+S5_Cel_PctVS/100*Cel_Bt+S5_HCel_PctVS/100*HCel_Bt+
         S5_ADL_PctVS/100*Lignin_Bt+S5_CP_PctVS/100*CP_Bt+S5_VFA_PctVS/100*VFA_Bt+
         S5_CL_PctVS/100*CL_Bt #NL CH4 / Kg VS
##Measured BMP
BMP_data<-read_excel("BMP_R.xlsx")
Pos_5BMP<-which(BMP_data$Section==5 | BMP_data$Section==56)
S5_BMP<-BMP_data$Mean_BMP[Pos_5BMP] #L CH4/Kg VS
S5_BMP_time<-BMP_data$Date[Pos_5BMP]
## Ratio degradable, non degradable VS (same proportion as BMP_t,BMP)
Ratio_BMP<-S5_BMP[1:2]/S5_t_BMP[1:2]
Ratio_BMP[3:5]<-S5_BMP[3:5]/S5_t_BMP[3]
Ratio_BMP[6:8]<-S5_BMP[6:8]/S5_t_BMP[c(5,7,9)]

##VS degradable non degradable
S5_VSd_A_g_Kg_F<-S5_VS_Kg_Kg_F[1:2]*1000*Ratio_BMP[1:2] #g/Kg faeces
S5_VSd_A_g_Kg_F[3:5]<-S5_VS_Kg_Kg_F[3]*1000*Ratio_BMP[3:5]
S5_VSd_A_g_Kg_F[6:8]<-S5_VS_Kg_Kg_F[5]*1000*Ratio_BMP[6:8]
S5_VSnd_A_g_Kg_F<-S5_VS_Kg_Kg_F[1:2]*1000*(1-Ratio_BMP[1:2]) #g/Kg faeces
S5_VSnd_A_g_Kg_F[3:5]<-S5_VS_Kg_Kg_F[3]*1000*(1-Ratio_BMP[3:5])
S5_VSnd_A_g_Kg_F[6:8]<-S5_VS_Kg_Kg_F[5]*1000*(1-Ratio_BMP[6:8])
df_VS_F<-data.frame(time = as.Date(S5_BMP_time), VSd_A_F = S5_VSd_A_g_Kg_F,
                    VSnd_A_F = S5_VSnd_A_g_Kg_F)
# Data frame for Faeces
df_Faeces<-data.frame(time = as.Date(S5_Composition_timeU),TAN_F = S5_TAN, VFA_F = S5_VFA_COD_F,
                     CP_F = S5_CP_COD, Ash_F = S5_Ash_g_Kg_F) 
df_Fibers_F<-merge(df_Fibers_F,df_Fat_F,by="time",all=TRUE)
df_Faeces<-merge(df_Fibers_F,df_Faeces,by="time",all=TRUE)
df_Faeces<-df_Faeces %>%
  fill(TAN_F,VFA_F,CP_F,NDF_F,Ash_F,Cfat_F, .direction = "updown")
df_Faeces<-merge(df_VS_F,df_Faeces,by="time",all=TRUE)
df_Faeces$VSd_A_F[7]<-df_Faeces$VSd_A_F[6]
df_Faeces$VSnd_A_F[7]<-df_Faeces$VSnd_A_F[6]
df_Faeces<-df_Faeces %>%
  fill(VSd_A_F,VSnd_A_F,TAN_F,VFA_F,CP_F,NDF_F,Ash_F,Cfat_F, .direction = "updown")

## Constants
Sulfide<-rep(man_pars2.0$conc_fresh$sulfide,length(S5_Composition_timeU))
xa_dead<-rep(man_pars2.0$conc_fresh$xa_dead,length(S5_Composition_timeU))
RFd<-rep(29.1,length(S5_Composition_timeU)) 
VSd<-rep(0,length(S5_Composition_timeU))
dens<-rep(man_pars2.0$dens,length(S5_Composition_timeU))

# Data frame for constants
df_Constants<-data.frame(time = as.Date(S5_Composition_timeU),Sulfide_K = Sulfide, xa_dead_K = xa_dead,
                      RFd_K = RFd, VSd_K = VSd,dens_K = dens)


## From feed
S5_TS_feed<-Feed_data$`TS (%)`[Pos_5] # %DM
S5_time_feed<-Feed_data$Date[Pos_5] 

# Starch
S5_Starch<-Feed_data$`Starch_pct in dry matter`[Pos_5] # %/DM
S5_Starch_Pct<-S5_TS_feed*S5_Starch/100 #% in total --> assume in Kg starch/100 Kg feed
S5_Starch_g_kg<- (S5_Starch_Pct/100)*1000 #g starch/Kg feed 
S5_Starch_COD <- S5_Starch_g_kg/COD_conv[4] #g COD/Kg

# Fat
S5_Cfat<-Feed_data$`Fat_pct in dry matter`[Pos_5] # %/DM
S5_Cfat_Pct<-S5_TS_feed*S5_Cfat/100 #% in total --> assume in Kg fat/100 Kg feed
S5_Cfat_g_kg<- (S5_Cfat_Pct/100)*1000 #g fat/Kg slurry 
S5_Cfat_COD <- S5_Cfat_g_kg/COD_conv[5] #g COD/Kg

# TN
S5_TN_feed<-Feed_data$`N_pct in dry matter`[Pos_5] # %/DM
S5_TN_feed_Pct<-S5_TS_feed*S5_TN_feed/100 #% in total --> assume in Kg N/100 Kg feed
S5_TN_feed_g_kg<- (S5_TN_feed_Pct/100)*1000 #g N /Kg feed 

#CP
S5_CP_f<-(S5_TN_feed_g_kg-0)*6.25-0 #g CP/Kg feed --> Assuming feed 0 Urea and 0 TAN
S5_CP_COD_f<-S5_CP_f/COD_conv[6] # g COD/Kg Faeces

# ANDF Same as NDF
S5_NDF_feed<-Feed_data$`ANDF_pct in dry matter`[Pos_5] # %/DM
S5_NDF_feed_Pct<-S5_TS_feed*S5_NDF_feed/100 #% in total 
S5_NDF_feed_g_kg<- (S5_NDF_feed_Pct/100)*1000 #g NDF /Kg feed 
S5_NDF_COD <- S5_NDF_feed_g_kg/COD_conv[3] #g COD/Kg

#Ash
S5_Ash_feed<-Feed_data$`Ash_pct in dry matter`[Pos_5] # %/DM
S5_Ash_feed_Pct<-S5_TS_feed*S5_Ash_feed/100 #% in total --> assume in Kg Ash/ 100 Kg feed
S5_Ash_feed_g_kg<- (S5_Ash_feed_Pct/100)*1000 #g Ash /Kg feed 

#VS
S5_VS_feed<-100-S5_Ash_feed 
S5_VS_feed_Pct<-S5_TS_feed*S5_VS_feed/100 #% in total --> assume in Kg VS/ 100 Kg feed
S5_VS_feed_g_kg<- (S5_VS_feed_Pct/100)*1000 #g VS /Kg feed 

#Calculated vs measured DM
Calculated_DM<-(S5_Starch_g_kg+S5_Cfat_g_kg+S5_CP_f+S5_NDF_feed_g_kg+S5_Ash_feed_g_kg)/1000 #Kg DM / Kg feed

# Data frame for feed
df_feed<-data.frame(time = as.Date(S5_time_feed),Starch_f = S5_Starch_COD, fat_f = S5_Cfat_COD,
                         TN_f = S5_TN_feed_g_kg, NDF_f = S5_NDF_COD, Ash_f = S5_Ash_feed_g_kg,
                         CP_f = S5_CP_COD_f,VS_f = S5_VS_feed_g_kg)


## Merge dataframes of urine, faeces, constants and feed into one and fill in missing values
df_Composition_S5<-merge(merge(merge(df_Urine, df_Faeces, by = "time", all = TRUE), 
                               df_Constants, by = "time", all = TRUE), df_feed, by = "time", 
                         all = TRUE)
df_Composition_S5<-distinct(df_Composition_S5)
df_Composition_S5$NDF_F[16]<-mean(df_Composition_S5$NDF_F[15:17],na.rm=TRUE)
df_Composition_S5$VSnd_A_F[16]<-mean(df_Composition_S5$VSnd_A_F[15:17],na.rm=TRUE)
df_Composition_S5$VSd_A_F[16]<-mean(df_Composition_S5$VSd_A_F[15:17],na.rm=TRUE)
df_Composition_S5$NDF_f[15]<-mean(df_Composition_S5$NDF_f[14:16],na.rm=TRUE)
df_Composition_S5 <- tidyr::fill(df_Composition_S5, pH_U,SO4_U,Urea_U,VFA_U,TAN_U,
                                 Ash_U,VSd_A_U,VSnd_A_U,VSd_A_F,VSnd_A_F,NDF_F,TAN_F,VFA_F,CP_F,
                                 Ash_F,Cfat_F,Sulfide_K,xa_dead_K,RFd_K,VSd_K,dens_K)
df_Composition_S5$Batch <- c(1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,3)

df_Composition_S5<-df_Composition_S5 %>%
  dplyr::group_by(Batch) %>%
  fill(Starch_f,fat_f,TN_f,NDF_f,Ash_f,CP_f,VS_f, .direction = "downup") %>%
  dplyr::ungroup()
df_Composition_S5<-as.data.frame(df_Composition_S5)



## Dataframe to add to the model
#Assumptions: (ask Frederik)
# 1/ ratio urine/faeces = 3.56;
# 2/ New slurry is form of urine and faeces. Calculations per Kg of new slurry following the given proportions
# 3/ A 2% of the feed falls into the slurry
# 4/ New slurry mass is calculated as the average slurry mass in a period right after they flush to right before
# 3/ Urine, faeces and manure have a density of 1 Kg/L

Ratio_UF<-3.56
Feed_pct<-0.02 #feed spilled to the pit per Kg of feed used 
Faeces_pct<-(1)/(Ratio_UF+1) #per Kg new slurry
Urine_pct<-Faeces_pct*Ratio_UF #Per Kg new slurry

### Slurry production rate
Time_Input<-sort(unique(c(round(S5_feed_rate_cum),df_slurry_mass_S5$time)))
S5_slurry_mass_daily<-S5_Int_slurry_mass(Time_Input)
Time_Input<-append(Time_Input,195,after=215)
Time_Input<-append(Time_Input,195.2,after=216)
S5_slurry_mass_daily<-append(S5_slurry_mass_daily,47169.343 ,after=215)
S5_slurry_mass_daily<-append(S5_slurry_mass_daily,35060.198 ,after=216)

### Total slurry
Fresh_Slurry_511<-S5_slurry_mass_daily[29]-S5_slurry_mass_daily[1]
Fresh_Slurry_512<-S5_slurry_mass_daily[63]-S5_slurry_mass_daily[30]
Fresh_Slurry_513<-S5_slurry_mass_daily[81]-S5_slurry_mass_daily[64]
Fresh_Slurry_514<-S5_slurry_mass_daily[96]-S5_slurry_mass_daily[82]
Fresh_Slurry_521<-S5_slurry_mass_daily[123]-S5_slurry_mass_daily[99]
Fresh_Slurry_522<-S5_slurry_mass_daily[157]-S5_slurry_mass_daily[124]
Fresh_Slurry_523<-S5_slurry_mass_daily[174]-S5_slurry_mass_daily[158]
Fresh_Slurry_524<-S5_slurry_mass_daily[216]-S5_slurry_mass_daily[175]
Fresh_Slurry_531<-S5_slurry_mass_daily[254]-S5_slurry_mass_daily[220]
Fresh_Slurry_532<-S5_slurry_mass_daily[287]-S5_slurry_mass_daily[255] ##Somehow in 1 week less slurry (Leack or missanotation??)
Fresh_Slurry_533<-S5_slurry_mass_daily[303]-S5_slurry_mass_daily[288]
Fresh_Slurry_534<-S5_slurry_mass_daily[315]-S5_slurry_mass_daily[304]

### Total used feed and total slurry
Total_feed_51<-208317.3
Total_Fresh_Slurry_51<-sum(Fresh_Slurry_511,Fresh_Slurry_512,Fresh_Slurry_513,Fresh_Slurry_514)
Total_feed_52<-202279.1
Total_Fresh_Slurry_52<-sum(Fresh_Slurry_521,Fresh_Slurry_522,Fresh_Slurry_523,Fresh_Slurry_524)
Total_feed_53<-201520
Total_Fresh_Slurry_53<-sum(Fresh_Slurry_531,Fresh_Slurry_532,Fresh_Slurry_533,Fresh_Slurry_534)

#Calculate VSd_A/VSnd_A from feed based on the VSnd_A from faeces. Assumption--> amount in faeces should match amount in feed
S5_Total_Faeces<-c(rep(Total_Fresh_Slurry_51*Faeces_pct,length(which(df_Composition_S5$Batch==1))),
                   rep(Total_Fresh_Slurry_52*Faeces_pct,length(which(df_Composition_S5$Batch==2))),
                   rep(Total_Fresh_Slurry_53*Faeces_pct,length(which(df_Composition_S5$Batch==3))))
S5_Total_feed<-c(rep(Total_feed_51,length(which(df_Composition_S5$Batch==1))),
                   rep(Total_feed_52,length(which(df_Composition_S5$Batch==2))),
                   rep(Total_feed_53,length(which(df_Composition_S5$Batch==3))))

S5_VSnd_A_Total_F<-df_Composition_S5$VSnd_A_F*S5_Total_Faeces # g VSnd
S5_VSnd_A_feed<-S5_VSnd_A_Total_F/S5_Total_feed #g VSnd/Kg feed
S5_VSd_A_feed<-df_Composition_S5$VS_f-S5_VSnd_A_feed #gVSd / Kg feed
df_Composition_S5$VSd_A_f<-S5_VSd_A_feed
df_Composition_S5$VSnd_A_f<-S5_VSnd_A_feed




### MODEL 2.0 ###
#Assumptions: (ask Frederik)
# 1/ 17% of feed DM is excreeted as faeces DM
# 2/ New slurry is form of urine, faeces. usE AVERAGE OF dm
# 3/ The remaing part of 2% of feed and 17 of feed DM is urine
# 4/ New slurry mass is calculated as the average slurry mass in a period right after they flush to right before
# 3/ Urine, faeces and manure have a density of 1 Kg/L

 S5_Faeces_DM<-Total_feed_51*(mean(S5_TS_feed[1:3])/100)*0.17 #Kg DM Faeces (Assuming this is the DM of the faeces ->Ask Frederik)
 S5_Faeces_DM[2]<-Total_feed_52*(mean(S5_TS_feed[4:6])/100)*0.17
 S5_Faeces_DM[3]<-Total_feed_53*(mean(S5_TS_feed[7:9])/100)*0.17
 
 S5_Faeces<-S5_Faeces_DM[1]*(100/S5_DM_F[1]) 
 S5_Faeces[2]<-S5_Faeces_DM[2]*(100/S5_DM_F[3])
 S5_Faeces[3]<-S5_Faeces_DM[3]*(100/S5_DM_F[5])
 

 
 Faeces_pct51<-S5_Faeces[1]/Total_Fresh_Slurry_51
 Faeces_pct52<-S5_Faeces[2]/Total_Fresh_Slurry_52
 Faeces_pct53<-S5_Faeces[3]/Total_Fresh_Slurry_53

 Feed_pct51<-Feed_pct*Total_feed_51/Total_Fresh_Slurry_51
 Feed_pct52<-Feed_pct*Total_feed_52/Total_Fresh_Slurry_52
 Feed_pct53<-Feed_pct*Total_feed_53/Total_Fresh_Slurry_53
 
 Urine_pct51<- 1-Faeces_pct51-Feed_pct51
 Urine_pct52<- 1-Faeces_pct52-Feed_pct52
 Urine_pct53<- 1-Faeces_pct53-Feed_pct53
 
 Faeces_pct_2.0<-c(rep(Faeces_pct51,length(which(df_Composition_S5$Batch==1))),
               rep(Faeces_pct52,length(which(df_Composition_S5$Batch==2))),
               rep(Faeces_pct53,length(which(df_Composition_S5$Batch==3))))
 Urine_pct_2.0<-c(rep(Urine_pct51,length(which(df_Composition_S5$Batch==1))),
              rep(Urine_pct52,length(which(df_Composition_S5$Batch==2))),
              rep(Urine_pct53,length(which(df_Composition_S5$Batch==3))))
 Feed_pct_2.0<-c(rep(Feed_pct51,length(which(df_Composition_S5$Batch==1))),
             rep(Feed_pct52,length(which(df_Composition_S5$Batch==2))),
             rep(Feed_pct53,length(which(df_Composition_S5$Batch==3))))
 Fresh_slurry<-c(rep(Total_Fresh_Slurry_51,length(which(df_Composition_S5$Batch==1))),
                 rep(Total_Fresh_Slurry_52,length(which(df_Composition_S5$Batch==2))),
                 rep(Total_Fresh_Slurry_53,length(which(df_Composition_S5$Batch==3))))
 
 
 ### Add default data from the 4th round ###
 averages <- colMeans(df_Composition_S5[,2:32]) ## Average of previous 3 rounds
 
 averages_df <- data.frame(t(replicate(4, averages)))
 averages_df$Batch<-4
 time_df <- data.frame(time = as.Date(c("2023-02-14","2023-03-21",
                                        "2023-04-18","2023-05-02")))
 add_df <- cbind(time_df,averages_df)
 df_Composition_S5<-rbind(df_Composition_S5, add_df)
 
 
 Final_S5_time<-df_Composition_S5$time
 Final_S5_time_days<-as.numeric(diff(Final_S5_time)) #days
 Final_S5_time_cum<-c(0,cumsum(Final_S5_time_days))
 Final_S5_Sulfide<-df_Composition_S5$Sulfide_K #Default
 Final_S5_Urea<-(Fresh_slurry*Faeces_pct_2.0*0+
                   Fresh_slurry*Urine_pct_2.0*df_Composition_S5$Urea_U+
                   Fresh_slurry*Feed_pct_2.0*0)/(Fresh_slurry) #g N/Kg new slurry
 Final_S5_SO4<-(Fresh_slurry*Faeces_pct_2.0*0+
                  Fresh_slurry*Urine_pct_2.0*df_Composition_S5$SO4_U+
                  Fresh_slurry*Feed_pct_2.0*0)/(Fresh_slurry) #g S/Kg new slurry
 Final_S5_TAN<-(Fresh_slurry*Faeces_pct_2.0*df_Composition_S5$TAN_F+
                  Fresh_slurry*Urine_pct_2.0*df_Composition_S5$TAN_U+
                  Fresh_slurry*Feed_pct_2.0*0)/(Fresh_slurry) #g N/Kg new slurry /// Assumptions: Rest of TN from Urine is TAN (1-0.75))
 Final_S5_Starch<-(Fresh_slurry*Faeces_pct_2.0*0+
                     Fresh_slurry*Urine_pct_2.0*0+
                     Fresh_slurry*Feed_pct_2.0*df_Composition_S5$Starch_f)/(Fresh_slurry) #g COD/Kg
 Final_S5_VFA<-(Fresh_slurry*Faeces_pct_2.0*df_Composition_S5$VFA_F+
                  Fresh_slurry*Urine_pct_2.0*df_Composition_S5$VFA_U+
                  Fresh_slurry*Feed_pct_2.0*0)/(Fresh_slurry) #g COD/Kg new slurry
 Final_S5_xa_dead <- df_Composition_S5$xa_dead_K #Default
 Final_S5_Cfat<-(Fresh_slurry*Faeces_pct_2.0*df_Composition_S5$Cfat_F+
                   Fresh_slurry*Urine_pct_2.0*0+
                   Fresh_slurry*Feed_pct_2.0*df_Composition_S5$fat_f)/(Fresh_slurry) #g COD/Kg new slurry
 Final_S5_CP<-(Fresh_slurry*Faeces_pct_2.0*df_Composition_S5$CP_F+
                 Fresh_slurry*Urine_pct_2.0*0+
                 Fresh_slurry*Feed_pct_2.0*df_Composition_S5$CP_f)/(Fresh_slurry) # g COD/Kg new slurry  /// Assumption: No Urea in faeces, no urea or TAN in feed 
 Final_S5_RFd <- df_Composition_S5$RFd_K #Default (From Frederik email)
 Final_S5_NDF<-(Fresh_slurry*Faeces_pct_2.0*df_Composition_S5$NDF_F+
                  Fresh_slurry*Urine_pct_2.0*0+
                  Fresh_slurry*Feed_pct_2.0*df_Composition_S5$NDF_f)/(Fresh_slurry) # g COD/Kg new slurry 
 Final_S5_iNDF <- Final_S5_NDF-Final_S5_RFd #g COD/Kg new slurry
 Final_S5_VSd <- df_Composition_S5$VSd_K #Default
 Final_S5_VSd_A<-(Fresh_slurry*Faeces_pct_2.0*df_Composition_S5$VSd_A_F +
                    Fresh_slurry*Urine_pct_2.0*df_Composition_S5$VSd_A_U+
                    Fresh_slurry*Feed_pct_2.0*df_Composition_S5$VSd_A_f)/(Fresh_slurry) # g VS/Kg new slurry /// To Ask: fraction of degradable in urine and feed
 Final_S5_VSnd_A<-(Fresh_slurry*Faeces_pct_2.0*df_Composition_S5$VSnd_A_F +
                     Fresh_slurry*Urine_pct_2.0*df_Composition_S5$VSnd_A_U+
                     Fresh_slurry*Feed_pct_2.0*df_Composition_S5$VSnd_A_f)/(Fresh_slurry) # g VS/Kg new slurry  /// To Ask: fraction of non degradable in urine and feed
 Final_S5_ash<-(Fresh_slurry*Faeces_pct_2.0*df_Composition_S5$Ash_F +
                  Fresh_slurry*Urine_pct_2.0*df_Composition_S5$Ash_U+
                  Fresh_slurry*Feed_pct_2.0*df_Composition_S5$Ash_f)/(Fresh_slurry) # g ash/Kg new slurry  
 Final_S5_pH<-mean(df_Composition_S5$pH_U) # Only considering from urine (no pH measured for faeces) /// Ask Frederik: how to make it change over time
 Final_S5_dens <- mean(df_Composition_S5$dens_K) #Default
 
 df_man_pars_S5_2.0 <- list(conc_fresh = data.frame(time = Final_S5_time_cum, sulfide = Final_S5_Sulfide, urea = Final_S5_Urea, 
                                                    sulfate = Final_S5_SO4, TAN = Final_S5_TAN, starch = Final_S5_Starch, 
                                                    VFA = Final_S5_VFA, xa_dead = Final_S5_xa_dead, Cfat = Final_S5_Cfat, 
                                                    CP = Final_S5_CP, RFd = Final_S5_RFd, iNDF = Final_S5_iNDF, VSd = Final_S5_VSd, 
                                                    VSd_A = Final_S5_VSd_A, VSnd_A = Final_S5_VSnd_A, ash = Final_S5_ash), 
                            pH = df_S5_pH, dens = Final_S5_dens)

## Section 6 ##
##Slurry
#pH (Values at the start and finish are assume to be the same as next/previous)
Pos_6S<-which(Data_Slurry$Section==6)
S6_pH_S<-Data_Slurry$pH[Pos_6S]
S6_pH_S_Time<-Data_Slurry$Date[Pos_6S]
S6_pH_S_days<-as.numeric(diff(S6_pH_S_Time)) #days
S6_pH_S_cum<-c(0,cumsum(S6_pH_S_days))
df_S6_pH<-data.frame(time=S6_pH_S_cum,pH = S6_pH_S )

## Urine
Pos_6U<-which(Data_Composition$Section==6 & Data_Composition$Type == "Urine")
S6_Composition_timeU<-Data_Composition$Time[Pos_6U]

#pH
S6_pH<-Data_Composition$pH[Pos_6U]


# SO4
Ratio_S_SO4<-32.06/96.06
S6_SO4_mgL<-Data_Composition$`SO4_mg/L`[Pos_6U] #mg SO4/L urine
S6_SO4<-S6_SO4_mgL/Urine_dens/1000 #g SO4/Kg urine
S6_SO4_S<-S6_SO4*Ratio_S_SO4 #g S/Kg urine


# Urea
S6_TN_U<-Data_Composition$`TN_g/Kg`[Pos_6U] #g Urine-TN/Kg urine
S6_Urea_N<-S6_TN_U*0.75 #g N/Kg urine--> Assuming Urea N is equal to urine N * 0.75 

#VFA
S6_VFA_mgL_U<-Data_Composition$`VFA_mg/L`[Pos_6U] #mg VFA/L Urine
S6_VFA_U<-S6_VFA_mgL_U/Urine_dens/1000 #g VFA/Kg Urine
S6_VFA_COD_U<-S6_VFA_U/COD_conv[7] #g COD/Kg Urine


#TAN
S6_TAN_U<-S6_TN_U*(1-0.75) #g Rest-N/Kg Urine

#Ash
S6_VS_U<-Data_Composition$`VS_%`[Pos_6U] #%-VS/DM 
S6_Ash_U<-100-S6_VS_U #%-Ash/DM 
S6_Ash_g_Kg_U <- ((S6_Ash_U* Data_Composition$`DM_%`[Pos_6U]/100)/100)*1000 # g Ash/ Kg Urine

#VS (Assume everything is degradable)
S6_VS_Pct_U<-(S6_VS_U* Data_Composition$`DM_%`[Pos_6U]/100) #%-VS/ Urine--> Kg VS/100Kg Urine
S6_VS_g_Kg_U <-(S6_VS_Pct_U/100)*1000 # g VS/ Kg Urine
Ratio_Urea_Nitrogen<-60.06/(14*2)
S6_Urea_g_Kg<-S6_Urea_N*Ratio_Urea_Nitrogen #g Urea/Kg Urine
S6_VSd_A_U<-S6_VS_g_Kg_U-S6_Urea_g_Kg ## Assume that all VS are degradable after Urea substraction
S6_VSnd_A_U<-S6_VSd_A_U-S6_VSd_A_U ## No VSnd in Urine

# Data frame for Urine
df_Urine_S6<-data.frame(time = as.Date(S6_Composition_timeU),pH_U = S6_pH, SO4_U = S6_SO4_S,
                     Urea_U = S6_Urea_N, VFA_U = S6_VFA_COD_U, TAN_U = S6_TAN_U, 
                     Ash_U = S6_Ash_g_Kg_U, VSd_A_U = S6_VSd_A_U, VSnd_A_U = S6_VSnd_A_U) 

## Faeces
Pos_6F<-which(Data_Composition$Section==6 & Data_Composition$Type == "Faeces")

#TAN 
Ratio_N_NH4 <- 14/18
S6_TAN<-Data_Composition$`NH4-N_g/Kg`[Pos_6F] #g NH4-N/Kg Faeces
S6_TAN_F <-S6_TAN/Ratio_N_NH4 #g N/Kg Faeces

#VFA
S6_VFA_mgL_F<-Data_Composition$`VFA_mg/L`[Pos_6F] #mg VFA/L Faeces
S6_VFA_F<-S6_VFA_mgL_F/Faeces_dens/1000 #g VFA/Kg Faeces
S6_VFA_COD_F<-S6_VFA_F/COD_conv[7] #g COD/Kg Faeces


#CP 
S6_CP<-(Data_Composition$`TN_g/Kg`[Pos_6F]-S6_TAN)*6.25-0 #g CP/Kg Faeces --> Assuming faeces 0 Urea
S6_CP_COD<-S6_CP/COD_conv[6] # g COD/Kg Faeces

#NDF Substract NDF - RDF 
RFd<-man_pars2.0$conc_fresh$RFd #gCOD/kg  use the one from man_pars2 instead of 29.1 to avoid negatives
Fibers_Faeces<-read_excel(".../data/Fibers_R.xlsx") #Choose own directory
Pos_6Fibers<-which(Fibers_Faeces$Section == 6)
S6_NDF_F_time<-as.Date(Fibers_Faeces$Date[Pos_6Fibers])
S6_NDF_F<-Fibers_Faeces$`NDF_%`[Pos_6Fibers] #% NDF/DM -->  in Kg NDF / 100 Kg DM
S6_NDF_F_g_Kg<-((S6_NDF_F*Fibers_Faeces$`DM_%`[Pos_6Fibers]/100)/100)*1000 #g NDF/ Kg faeces
S6_NDF_F_COD<-S6_NDF_F_g_Kg/COD_conv[3] #g COD / Kg Faeces
df_Fibers_F_S6<-data.frame(time = S6_NDF_F_time,NDF_F = S6_NDF_F_COD)

#Ash
S6_VS_F<-Data_Composition$`VS_%`[Pos_6F] #%-VS/DM 
S6_Ash_F<-100-S6_VS_F #%-Ash/DM 
S6_Ash_g_Kg_F <- ((S6_Ash_F* Data_Composition$`VS_%`[Pos_6F]/100)/100)*1000 # g Ash/ Kg Faeces

#VS
S6_VS_F<-Data_Composition$`VS_%`[Pos_6F] #%-VS/DM 
S6_VS_Pct_F<-(S6_VS_F* Data_Composition$`DM_%`[Pos_6F]/100) #%-VS/ Faeces--> Kg VS/100Kg Faeces
S6_VS_Kg_Kg_F <-((S6_VS_F* Data_Composition$`DM_%`[Pos_6F]/100)/100) # Kg VS/ Kg Faeces

###############VF_degradable ######################
###############Theoretical methane potential####### 
#(units NL CH4/Kg VS)
VFA_Bt <- 373 
Eth_Bt <- 730 #Ethanol--> assume 0
CP_Bt <- 496
CL_Bt <- 1014 #Lipids-->assume same as Frederik paper (Provisional)
Lignin_Bt <- 727
HCel_Bt <- 415
Cel_Bt <- 415
CH_Bt <- 415 #Carbohydrates --> assume the remaining %

##Composition in % of volatile solids
S6_DM_F<-Data_Composition$`DM_%`[Pos_6F]
#VFA
S6_VFA_gKg<-S6_VFA_F*1/(S6_VS_Pct_F/100) # g VFA/Kg VS
S6_VFA_PctVS<-S6_VFA_gKg/1000*100 #% VFA/VS
S6_VFA_PctVS[5:10]<-S6_VFA_PctVS[5]
#CP
S6_CP_gKg<-S6_CP*1/(S6_VS_Pct_F/100) # g CP/Kg VS
S6_CP_PctVS<-S6_CP_gKg/1000*100 #% CP/VS
S6_CP_PctVS[5:10]<-S6_CP_PctVS[5]

#Lignin (From Ali's paper is equal to ADL)
S6_ADL_F<-Fibers_Faeces$`ADL_%`[Pos_6Fibers] #% ADL/DM -->  in Kg ADL / 100 Kg DM
S6_ADL_PctVS<-S6_ADL_F[1:2]*(100/S6_VS_F[1:2]) #% ADL/VS
S6_ADL_PctVS[3:4]<-S6_ADL_F[3:4]*(100/S6_VS_F[3:4]) #% ADL/VS
S6_ADL_PctVS[5:10]<-S6_ADL_F[5:10]*(100/S6_VS_F[5]) #% ADL/VS

#Hemicellulose (equal to NDF-ADF)
S6_ADF_F<-Fibers_Faeces$`ADF_%`[Pos_6Fibers] #% ADL/DM -->  in Kg ADF / 100 Kg DM
S6_ADF_PctVS<-S6_ADF_F[1:2]*(100/S6_VS_F[1:2]) #% ADF/VS
S6_ADF_PctVS[3:4]<-S6_ADF_F[3:4]*(100/S6_VS_F[3:4]) #% ADF/VS
S6_ADF_PctVS[5:10]<-S6_ADF_F[5:10]*(100/S6_VS_F[5]) #% ADF/VS
S6_NDF_PctVS<-S6_NDF_F[1:2]*(100/S6_VS_F[1:2]) #% NDF/VS
S6_NDF_PctVS[3:4]<-S6_NDF_F[3:4]*(100/S6_VS_F[3:4]) #% NDF/VS
S6_NDF_PctVS[5:10]<-S6_NDF_F[5:10]*(100/S6_VS_F[5]) #% NDF/VS
S6_HCel_PctVS <- S6_NDF_PctVS - S6_ADF_PctVS #% Hemicellulose/VS

#Cellulose (equal to ADF-ADL)
S6_Cel_PctVS <- S6_ADF_PctVS - S6_ADL_PctVS

#Lipids 
Fat_Faeces<-read_excel(".../data/Fat_R.xlsx") #Choose own directory
Pos_6Fat<-which(Fat_Faeces$Section == 6)
S6_Cfat_F_time<-as.Date(Fat_Faeces$Date[Pos_6Fat])
S6_CL_F<-Fat_Faeces$`FAT_%`[Pos_6Fat] #% Fat/DM -->  in Kg Fat / 100 Kg DM
S6_Cfat_F_g_Kg<-((S6_CL_F*Fibers_Faeces$`DM_%`[Pos_6Fibers]/100)/100)*1000 #g Fat/ Kg faeces
S6_Cfat_F_COD<-S6_Cfat_F_g_Kg/COD_conv[5] #g COD / Kg Faeces
df_Fat_F_S6<-data.frame(time = S6_Cfat_F_time,Cfat_F = S6_Cfat_F_COD)
S6_CL_PctVS<-(((S6_CL_F[1:2]*10)*(100/S6_VS_F[1:2]))/1000)*100 #% CL/ VS
S6_CL_PctVS[3:4]<-(((S6_CL_F[3:4]*10)*(100/S6_VS_F[3:4]))/1000)*100 #% CL/ VS
S6_CL_PctVS[5:6]<-(((S6_CL_F[5:6]*10)*(100/S6_VS_F[5:6]))/1000)*100 #% CL/ VS
S6_CL_PctVS[7:8]<-(((S6_CL_F[7:8]*10)*(100/S6_VS_F[5:6]))/1000)*100 #% CL/ VS
S6_CL_PctVS[9:10]<-(((S6_CL_F[9:10]*10)*(100/S6_VS_F[5:6]))/1000)*100 #% CL/ VS


# Carbohydrates (remaining percentage)
S6_CH_PctVS <- 100 - S6_Cel_PctVS-S6_HCel_PctVS-S6_ADL_PctVS-S6_CP_PctVS-S6_VFA_PctVS-S6_CL_PctVS

##Theoretical BMP
S6_t_BMP <- S6_CH_PctVS/100*CH_Bt+S6_Cel_PctVS/100*Cel_Bt+S6_HCel_PctVS/100*HCel_Bt+
  S6_ADL_PctVS/100*Lignin_Bt+S6_CP_PctVS/100*CP_Bt+S6_VFA_PctVS/100*VFA_Bt+
  S6_CL_PctVS/100*CL_Bt #NL CH4 / Kg VS
##Measured BMP
BMP_data<-read_excel("../data/BMP_R.xlsx") #Choose own directory
Pos_6BMP<-which(BMP_data$Section==6 | BMP_data$Section==56)
S6_BMP<-BMP_data$Mean_BMP[Pos_6BMP] #L CH4/Kg VS
S6_BMP_time<-BMP_data$Date[Pos_6BMP]
## Ratio degradable, non degradable VS (same proportion as BMP_t,BMP)
Ratio_BMP<-S6_BMP[1:2]/S6_t_BMP[1:2]
Ratio_BMP[3:5]<-S6_BMP[3:5]/S6_t_BMP[3]
Ratio_BMP[6:8]<-S6_BMP[6:8]/S6_t_BMP[c(5,7,9)]

##VS degradable non degradable
S6_VSd_A_g_Kg_F<-S6_VS_Kg_Kg_F[1:2]*1000*Ratio_BMP[1:2] #g/Kg faeces
S6_VSd_A_g_Kg_F[3:5]<-S6_VS_Kg_Kg_F[3]*1000*Ratio_BMP[3:5]
S6_VSd_A_g_Kg_F[6:8]<-S6_VS_Kg_Kg_F[5]*1000*Ratio_BMP[6:8]
S6_VSnd_A_g_Kg_F<-S6_VS_Kg_Kg_F[1:2]*1000*(1-Ratio_BMP[1:2]) #g/Kg faeces
S6_VSnd_A_g_Kg_F[3:5]<-S6_VS_Kg_Kg_F[3]*1000*(1-Ratio_BMP[3:5])
S6_VSnd_A_g_Kg_F[6:8]<-S6_VS_Kg_Kg_F[5]*1000*(1-Ratio_BMP[6:8])
df_VS_F_S6<-data.frame(time = as.Date(S6_BMP_time), VSd_A_F = S6_VSd_A_g_Kg_F,
                    VSnd_A_F = S6_VSnd_A_g_Kg_F)
# Data frame for Faeces
df_Faeces_S6<-data.frame(time = as.Date(S6_Composition_timeU),TAN_F = S6_TAN, VFA_F = S6_VFA_COD_F,
                      CP_F = S6_CP_COD, Ash_F = S6_Ash_g_Kg_F) 
df_Fibers_F_S6<-merge(df_Fibers_F_S6,df_Fat_F_S6,by="time",all=TRUE)
df_Faeces_S6<-merge(df_Fibers_F_S6,df_Faeces_S6,by="time",all=TRUE)
df_Faeces_S6<-df_Faeces_S6 %>%
  fill(TAN_F,VFA_F,CP_F,NDF_F,Ash_F,Cfat_F, .direction = "updown")
df_Faeces_S6<-merge(df_VS_F_S6,df_Faeces_S6,by="time",all=TRUE)
df_Faeces_S6$VSd_A_F[7]<-df_Faeces_S6$VSd_A_F[6]
df_Faeces_S6$VSnd_A_F[7]<-df_Faeces_S6$VSnd_A_F[6]
df_Faeces_S6<-df_Faeces_S6 %>%
  fill(VSd_A_F,VSnd_A_F,TAN_F,VFA_F,CP_F,NDF_F,Ash_F,Cfat_F, .direction = "updown")

## Constants
Sulfide<-rep(man_pars2.0$conc_fresh$sulfide,length(S6_Composition_timeU))
xa_dead<-rep(man_pars2.0$conc_fresh$xa_dead,length(S6_Composition_timeU))
RFd<-rep(29.1,length(S6_Composition_timeU)) 
VSd<-rep(0,length(S6_Composition_timeU))
dens<-rep(man_pars2.0$dens,length(S6_Composition_timeU))

# Data frame for constants
df_Constants<-data.frame(time = as.Date(S6_Composition_timeU),Sulfide_K = Sulfide, xa_dead_K = xa_dead,
                         RFd_K = RFd, VSd_K = VSd,dens_K = dens)


## From feed
S6_TS_feed<-Feed_data$`TS (%)`[Pos_6] # %DM
S6_time_feed<-Feed_data$Date[Pos_6] 

# Starch
S6_Starch<-Feed_data$`Starch_pct in dry matter`[Pos_6] # %/DM
S6_Starch_Pct<-S6_TS_feed*S6_Starch/100 #% in total --> assume in Kg starch/100 Kg feed
S6_Starch_g_kg<- (S6_Starch_Pct/100)*1000 #g starch/Kg feed 
S6_Starch_COD <- S6_Starch_g_kg/COD_conv[4] #g COD/Kg

# Fat
S6_Cfat<-Feed_data$`Fat_pct in dry matter`[Pos_6] # %/DM
S6_Cfat_Pct<-S6_TS_feed*S6_Cfat/100 #% in total --> assume in Kg fat/100 Kg feed
S6_Cfat_g_kg<- (S6_Cfat_Pct/100)*1000 #g fat/Kg feed 
S6_Cfat_COD <- S6_Cfat_g_kg/COD_conv[5] #g COD/Kg feed

# TN
S6_TN_feed<-Feed_data$`N_pct in dry matter`[Pos_6] # %/DM
S6_TN_feed_Pct<-S6_TS_feed*S6_TN_feed/100 #% in total --> assume in Kg N/100 Kg feed
S6_TN_feed_g_kg<- (S6_TN_feed_Pct/100)*1000 #g N /Kg feed 

#CP
S6_CP_f<-(S6_TN_feed_g_kg-0)*6.25-0 #g CP/Kg feed --> Assuming feed 0 Urea and 0 TAN
S6_CP_COD_f<-S6_CP_f/COD_conv[6] # g COD/Kg Faeces

# ANDF Same as NDF
S6_NDF_feed<-Feed_data$`ANDF_pct in dry matter`[Pos_6] # %/DM
S6_NDF_feed_Pct<-S6_TS_feed*S6_NDF_feed/100 #% in total 
S6_NDF_feed_g_kg<- (S6_NDF_feed_Pct/100)*1000 #g NDF /Kg feed 
S6_NDF_COD <- S6_NDF_feed_g_kg/COD_conv[3] #g COD/Kg

#Ash
S6_Ash_feed<-Feed_data$`Ash_pct in dry matter`[Pos_6] # %/DM
S6_Ash_feed_Pct<-S6_TS_feed*S6_Ash_feed/100 #% in total --> assume in Kg N/ 100 Kg feed
S6_Ash_feed_g_kg<- (S6_Ash_feed_Pct/100)*1000 #g Ash /Kg feed 

#VS
S6_VS_feed<-100-S6_Ash_feed 
S6_VS_feed_Pct<-S6_TS_feed*S6_VS_feed/100 #% in total --> assume in Kg VS/ 100 Kg feed
S6_VS_feed_g_kg<- (S6_VS_feed_Pct/100)*1000 #g VS /Kg feed 

#Calculated vs measured DM
Calculated_DM_S6<-(S6_Starch_g_kg+S6_Cfat_g_kg+S6_CP_f+S6_NDF_feed_g_kg+S6_Ash_feed_g_kg)/1000 #Kg DM / Kg feed

# Data frame for feed
df_feed_S6<-data.frame(time = as.Date(S6_time_feed),Starch_f = S6_Starch_COD, fat_f = S6_Cfat_COD,
                    TN_f = S6_TN_feed_g_kg, NDF_f = S6_NDF_COD, Ash_f = S6_Ash_feed_g_kg,
                    CP_f = S6_CP_COD_f,VS_f = S6_VS_feed_g_kg)


## Merge dataframes of urine, faeces, constants and feed into one and fill in missing values
df_Composition_S6<-merge(merge(merge(df_Urine_S6, df_Faeces_S6, by = "time", all = TRUE), 
                               df_Constants, by = "time", all = TRUE), df_feed_S6, by = "time", 
                         all = TRUE)
df_Composition_S6<-distinct(df_Composition_S6)
df_Composition_S6$NDF_F[16]<-mean(df_Composition_S6$NDF_F[15:17],na.rm=TRUE)
df_Composition_S6$VSnd_A_F[16]<-mean(df_Composition_S6$VSnd_A_F[15:17],na.rm=TRUE)
df_Composition_S6$VSd_A_F[16]<-mean(df_Composition_S6$VSd_A_F[15:17],na.rm=TRUE)
df_Composition_S6$NDF_f[15]<-mean(df_Composition_S6$NDF_f[14:16],na.rm=TRUE)
df_Composition_S6 <- tidyr::fill(df_Composition_S6, pH_U,SO4_U,Urea_U,VFA_U,TAN_U,
                                 Ash_U,VSd_A_U,VSnd_A_U,VSd_A_F,VSnd_A_F,NDF_F,TAN_F,VFA_F,CP_F,
                                 Ash_F,Cfat_F,Sulfide_K,xa_dead_K,RFd_K,VSd_K,dens_K)
df_Composition_S6$Batch <- c(1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,3)

df_Composition_S6<-df_Composition_S6 %>%
  dplyr::group_by(Batch) %>%
  fill(Starch_f,fat_f,TN_f,NDF_f,Ash_f,CP_f,VS_f, .direction = "downup") %>%
  dplyr::ungroup()
df_Composition_S6<-as.data.frame(df_Composition_S6)




Feed_pct<-0.02 #feed spilled to the pit per Kg of feed used 
Faeces_pct<-(1)/(Ratio_UF+1) #per Kg new slurry
Urine_pct<-Faeces_pct*Ratio_UF #Per Kg new slurry

Time_Input_S6<-sort(unique(c(round(S6_feed_rate_cum),df_slurry_mass_S6$time)))
S6_slurry_mass_daily<-S6_Int_slurry_mass(Time_Input_S6)


### Total slurry from Section 6
Fresh_Slurry_611<-S6_slurry_mass_daily[33]-S6_slurry_mass_daily[1]
Fresh_Slurry_612<-S6_slurry_mass_daily[67]-S6_slurry_mass_daily[34]
Fresh_Slurry_613<-S6_slurry_mass_daily[86]-S6_slurry_mass_daily[68]
Fresh_Slurry_614<-S6_slurry_mass_daily[94]-S6_slurry_mass_daily[87]
Fresh_Slurry_621<-S6_slurry_mass_daily[126]-S6_slurry_mass_daily[100]
Fresh_Slurry_622<-S6_slurry_mass_daily[160]-S6_slurry_mass_daily[127]
Fresh_Slurry_623<-S6_slurry_mass_daily[177]-S6_slurry_mass_daily[161]
Fresh_Slurry_624<-S6_slurry_mass_daily[199]-S6_slurry_mass_daily[178] ## Height is extrapollated based on pig number and previous increase
Fresh_Slurry_631<-S6_slurry_mass_daily[230]-S6_slurry_mass_daily[203]
Fresh_Slurry_632<-S6_slurry_mass_daily[264]-S6_slurry_mass_daily[231] 
Fresh_Slurry_633<-S6_slurry_mass_daily[280]-S6_slurry_mass_daily[265]
Fresh_Slurry_634<-S6_slurry_mass_daily[305]-S6_slurry_mass_daily[281] ##I will assume pigs were out when they flushed

## Total Feed and total slurry  
Total_feed_61<-187932
Total_Fresh_Slurry_61<-sum(Fresh_Slurry_611,Fresh_Slurry_612,Fresh_Slurry_613,Fresh_Slurry_614)
Total_feed_62<-174768
Total_Fresh_Slurry_62<-sum(Fresh_Slurry_621,Fresh_Slurry_622,Fresh_Slurry_623,Fresh_Slurry_624)
Total_feed_63<-211146.4
Total_Fresh_Slurry_63<-sum(Fresh_Slurry_631,Fresh_Slurry_632,Fresh_Slurry_633,Fresh_Slurry_634)

#Calculate VSd_A/VSnd_A from feed based on the VSnd_A from faeces. Assumption--> amount in faeces should match amount in feed
S6_Total_Faeces<-c(rep(Total_Fresh_Slurry_61*Faeces_pct,length(which(df_Composition_S6$Batch==1))),
                   rep(Total_Fresh_Slurry_62*Faeces_pct,length(which(df_Composition_S6$Batch==2))),
                   rep(Total_Fresh_Slurry_63*Faeces_pct,length(which(df_Composition_S6$Batch==3))))
S6_Total_feed<-c(rep(Total_feed_61,length(which(df_Composition_S6$Batch==1))),
                 rep(Total_feed_62,length(which(df_Composition_S6$Batch==2))),
                 rep(Total_feed_63,length(which(df_Composition_S6$Batch==3))))

S6_VSnd_A_Total_F<-df_Composition_S6$VSnd_A_F*S6_Total_Faeces # g VSnd
S6_VSnd_A_feed<-S6_VSnd_A_Total_F/S6_Total_feed #g VSnd/Kg feed
S6_VSd_A_feed<-df_Composition_S6$VS_f-S6_VSnd_A_feed #gVSd / Kg feed
df_Composition_S6$VSd_A_f<-S6_VSd_A_feed
df_Composition_S6$VSnd_A_f<-S6_VSnd_A_feed


### MODEL 2.0 ###
#Assumptions:
# 1/ 17% of feed DM is excreeted as faeces DM
# 2/ New slurry is form of urine, faeces. usE AVERAGE OF dm
# 3/ The remaing part of 2% of feed and 17 of feed DM is urine
# 4/ New slurry mass is calculated as the average slurry mass in a period right after they flush to right before
# 3/ Urine, faeces and manure have a density of 1 Kg/L

S6_Faeces_DM<-Total_feed_61*(mean(S6_TS_feed[1:3])/100)*0.17 #Kg DM Faeces (Assuming this is the DM of the faeces ->Ask Frederik)
S6_Faeces_DM[2]<-Total_feed_62*(mean(S6_TS_feed[4:6])/100)*0.17
S6_Faeces_DM[3]<-Total_feed_63*(mean(S6_TS_feed[7:9])/100)*0.17

S6_Faeces<-S6_Faeces_DM[1]*(100/S6_DM_F[1]) 
S6_Faeces[2]<-S6_Faeces_DM[2]*(100/S6_DM_F[3])
S6_Faeces[3]<-S6_Faeces_DM[3]*(100/S6_DM_F[5])



Faeces_pct61<-S6_Faeces[1]/Total_Fresh_Slurry_61
Faeces_pct62<-S6_Faeces[2]/Total_Fresh_Slurry_62
Faeces_pct63<-S6_Faeces[3]/Total_Fresh_Slurry_63

Feed_pct61<-Feed_pct*Total_feed_61/Total_Fresh_Slurry_61
Feed_pct62<-Feed_pct*Total_feed_62/Total_Fresh_Slurry_62
Feed_pct63<-Feed_pct*Total_feed_63/Total_Fresh_Slurry_63

Urine_pct61<- 1-Faeces_pct61-Feed_pct61
Urine_pct62<- 1-Faeces_pct62-Feed_pct62
Urine_pct63<- 1-Faeces_pct63-Feed_pct63

Faeces_pct_2.0<-c(rep(Faeces_pct61,length(which(df_Composition_S6$Batch==1))),
              rep(Faeces_pct62,length(which(df_Composition_S6$Batch==2))),
              rep(Faeces_pct63,length(which(df_Composition_S6$Batch==3))))
Urine_pct_2.0<-c(rep(Urine_pct61,length(which(df_Composition_S6$Batch==1))),
             rep(Urine_pct62,length(which(df_Composition_S6$Batch==2))),
             rep(Urine_pct63,length(which(df_Composition_S6$Batch==3))))
Feed_pct_2.0<-c(rep(Feed_pct61,length(which(df_Composition_S6$Batch==1))),
            rep(Feed_pct62,length(which(df_Composition_S6$Batch==2))),
            rep(Feed_pct63,length(which(df_Composition_S6$Batch==3))))
Fresh_slurry<-c(rep(Total_Fresh_Slurry_61,length(which(df_Composition_S6$Batch==1))),
                rep(Total_Fresh_Slurry_62,length(which(df_Composition_S6$Batch==2))),
                rep(Total_Fresh_Slurry_63,length(which(df_Composition_S6$Batch==3))))

Final_S6_time<-df_Composition_S6$time
Final_S6_time_days<-as.numeric(diff(Final_S6_time)) #days
Final_S6_time_cum<-c(0,cumsum(Final_S6_time_days))

Final_S6_Sulfide<-df_Composition_S6$Sulfide_K #Default
Final_S6_Urea<-(Fresh_slurry*Faeces_pct_2.0*0+
                  Fresh_slurry*Urine_pct_2.0*df_Composition_S6$Urea_U+
                  Fresh_slurry*Feed_pct_2.0*0)/(Fresh_slurry) #g N/Kg new slurry
Final_S6_SO4<-(Fresh_slurry*Faeces_pct_2.0*0+
                 Fresh_slurry*Urine_pct_2.0*df_Composition_S6$SO4_U+
                 Fresh_slurry*Feed_pct_2.0*0)/(Fresh_slurry) #g S/Kg new slurry
Final_S6_TAN<-(Fresh_slurry*Faeces_pct_2.0*df_Composition_S6$TAN_F+
                 Fresh_slurry*Urine_pct_2.0*df_Composition_S6$TAN_U+
                 Fresh_slurry*Feed_pct_2.0*0)/(Fresh_slurry) #g N/Kg new slurry /// Assumptions: Rest of TN from Urine is TAN (1-0.75))
Final_S6_Starch<-(Fresh_slurry*Faeces_pct_2.0*0+
                    Fresh_slurry*Urine_pct_2.0*0+
                    Fresh_slurry*Feed_pct_2.0*df_Composition_S6$Starch_f)/(Fresh_slurry) #g COD/Kg
Final_S6_VFA<-(Fresh_slurry*Faeces_pct_2.0*df_Composition_S6$VFA_F+
                 Fresh_slurry*Urine_pct_2.0*df_Composition_S6$VFA_U+
                 Fresh_slurry*Feed_pct_2.0*0)/(Fresh_slurry) #g COD/Kg new slurry
Final_S6_xa_dead <- df_Composition_S6$xa_dead_K #Default
Final_S6_Cfat<-(Fresh_slurry*Faeces_pct_2.0*df_Composition_S6$Cfat_F+
                  Fresh_slurry*Urine_pct_2.0*0+
                  Fresh_slurry*Feed_pct_2.0*df_Composition_S6$fat_f)/(Fresh_slurry) #g COD/Kg new slurry
Final_S6_CP<-(Fresh_slurry*Faeces_pct_2.0*df_Composition_S6$CP_F+
                Fresh_slurry*Urine_pct_2.0*0+
                Fresh_slurry*Feed_pct_2.0*df_Composition_S6$CP_f)/(Fresh_slurry) # g COD/Kg new slurry  /// Assumption: No Urea in faeces, no urea or TAN in feed 
Final_S6_RFd <- df_Composition_S6$RFd_K #Default (From Frederik email)
Final_S6_NDF<-(Fresh_slurry*Faeces_pct_2.0*df_Composition_S6$NDF_F+
                 Fresh_slurry*Urine_pct_2.0*0+
                 Fresh_slurry*Feed_pct_2.0*df_Composition_S6$NDF_f)/(Fresh_slurry) # g COD/Kg new slurry 
Final_S6_iNDF <- Final_S6_NDF-Final_S6_RFd #g COD/Kg new slurry
Final_S6_VSd <- df_Composition_S6$VSd_K #Default
Final_S6_VSd_A<-(Fresh_slurry*Faeces_pct_2.0*df_Composition_S6$VSd_A_F +
                   Fresh_slurry*Urine_pct_2.0*df_Composition_S6$VSd_A_U+
                   Fresh_slurry*Feed_pct_2.0*df_Composition_S6$VSd_A_f)/(Fresh_slurry) # g VS/Kg new slurry /// To Ask: fraction of degradable in urine and feed
Final_S6_VSnd_A<-(Fresh_slurry*Faeces_pct_2.0*df_Composition_S6$VSnd_A_F +
                    Fresh_slurry*Urine_pct_2.0*df_Composition_S6$VSnd_A_U+
                    Fresh_slurry*Feed_pct_2.0*df_Composition_S6$VSnd_A_f)/(Fresh_slurry) # g VS/Kg new slurry  /// To Ask: fraction of non degradable in urine and feed
Final_S6_ash<-(Fresh_slurry*Faeces_pct_2.0*df_Composition_S6$Ash_F +
                 Fresh_slurry*Urine_pct_2.0*df_Composition_S6$Ash_U+
                 Fresh_slurry*Feed_pct_2.0*df_Composition_S6$Ash_f)/(Fresh_slurry) # g ash/Kg new slurry  
Final_S6_pH<-mean(df_Composition_S6$pH_U) # Only considering from urine (no pH measured for faeces) /// Ask Frederik: how to make it change over time
Final_S6_dens <- mean(df_Composition_S6$dens_K) #Default

df_man_pars_S6_2.0 <- list(conc_fresh = data.frame(time = Final_S6_time_cum, sulfide = Final_S6_Sulfide, urea = Final_S6_Urea, 
                                                   sulfate = Final_S6_SO4, TAN = Final_S6_TAN, starch = Final_S6_Starch, 
                                                   VFA = Final_S6_VFA, xa_dead = Final_S6_xa_dead, Cfat = Final_S6_Cfat, 
                                                   CP = Final_S6_CP, RFd = Final_S6_RFd, iNDF = Final_S6_iNDF, VSd = Final_S6_VSd, 
                                                   VSd_A = Final_S6_VSd_A, VSnd_A = Final_S6_VSnd_A, ash = Final_S6_ash), 
                           pH = df_S6_pH, dens = Final_S6_dens)



###EVAPORATION RATE (Kg/m2/day)
setwd(".../Batch 1/Rfiles") #Choose own directory
T1<-read.table( file = "TH_A.txt",header=TRUE, sep = "\t")
setwd(".../Batch 2/Rfiles") #Choose own directory
T2<-read.table( file = "TH_A.txt",header=TRUE, sep = "\t")
setwd(".../Batch 3/Rfiles") #Choose own directory
T3<-read.table( file = "TH_A.txt",header=TRUE, sep = "\t")

days_Ev<-5 #Days between measurements 
mass_1<-29613.49664 #Kg slurry #Measured by me on the 21st (Section was washed and no pigs)
mass_2<-27498.24688 #Kg slurry #Measured by Jens on the 26th--> Assumption: difference due to evaporation
Evap_Rate<-abs((mass_2-mass_1)/days_Ev)/(area) #Kg/m2/day

ER <- function(A, Xs, X,EC) {
  A <-area
  Xs <- 0.62198*pws/(pa - pws)  
  EC  <- 25+18*v
  X<-0.013 #90%RH and 20C  #Kg/Kg
    return(EC*A*(Xs-X)/3600)
}
Temp_1<- mean(T1$T_5[c(6000:16503,16505:26000)]) +273 #K
Temp_2<- mean(T2$T_5[c(7000:27000)]) +273 #K
Temp_3<- mean(T3$T_5[c(7000:29000)]) +273 #K
Temp<-mean(Temp_1,Temp_2,Temp_3)
pws <- exp(1)^(77.3450 + 0.0057*Temp - 7235/Temp)/Temp^(8.2)
pa<-101325 #Pa atmospheric pressure
v<-0.02 #m/s #air velocity in slurry surface
E_Calculated<-ER() #Kg/s
E_m2_day<-(E_Calculated*60*60*24)/area


###SLURRY PRODUCTION RATE (Kg/pig/batch)

#Batch 1
SPR_511<-Fresh_Slurry_511/mean(S5_Pigs_feed[1:24])
SPRD_511<-Fresh_Slurry_511/mean(S5_Pigs_feed[1:24])/(S5_feed_rate_cum[24]-S5_feed_rate_cum[1])
SPR_512<-Fresh_Slurry_512/mean(S5_Pigs_feed[24:52])
SPRD_512<-Fresh_Slurry_512/mean(S5_Pigs_feed[24:52])/(S5_feed_rate_cum[52]-S5_feed_rate_cum[24])
SPR_513<-Fresh_Slurry_513/mean(S5_Pigs_feed[52:66])
SPRD_513<-Fresh_Slurry_513/mean(S5_Pigs_feed[52:66])/(S5_feed_rate_cum[66]-S5_feed_rate_cum[52])
SPR_514<-Fresh_Slurry_514/mean(S5_Pigs_feed[66:77])
SPRD_514<-Fresh_Slurry_514/mean(S5_Pigs_feed[66:77])/(S5_feed_rate_cum[77]-S5_feed_rate_cum[66])
SPR_51<-sum(SPR_511,SPR_512,SPR_513,SPR_514) #Kg/pig
SPRD_51<-SPR_51/(S5_feed_rate_cum[77]-S5_feed_rate_cum[1]) #Kg/pig/day

#Batch 2
SPR_521<-Fresh_Slurry_521/mean(S5_Pigs_feed[79:98])
SPRD_521<-SPR_521/(S5_feed_rate_cum[98]-S5_feed_rate_cum[79])
SPR_522<-Fresh_Slurry_522/mean(S5_Pigs_feed[98:126])
SPRD_522<-SPR_522/(S5_feed_rate_cum[126]-S5_feed_rate_cum[98])
SPR_523<-Fresh_Slurry_523/mean(S5_Pigs_feed[126:140])
SPRD_523<-SPR_523/(S5_feed_rate_cum[140]-S5_feed_rate_cum[126])
# SPR_524_1<-(S5_slurry_mass_daily[189]-S5_slurry_mass_daily[175])/mean(S5_Pigs_feed[140:153])
# SPR_524_2<-(S5_slurry_mass_daily[196]-S5_slurry_mass_daily[190])/mean(S5_Pigs_feed[154:159])
# SPR_524_3<-(S5_slurry_mass_daily[207]-S5_slurry_mass_daily[197])/mean(S5_Pigs_feed[160:170])
# SPR_524_4<-(S5_slurry_mass_daily[214]-S5_slurry_mass_daily[208])/mean(S5_Pigs_feed[171:175])
# SPRD_524<-sum(SPR_524_1,SPR_524_2,SPR_524_3,SPR_524_4)/(S5_feed_rate_cum[175]-S5_feed_rate_cum[140])
SPR_524<-Fresh_Slurry_524/mean(S5_Pigs_feed[140:175])
SPRD_524<-SPR_524/(S5_feed_rate_cum[175]-S5_feed_rate_cum[140])
SPR_52<-sum(SPR_521,SPR_522,SPR_523,SPR_524) #Kg/pig
SPRD_52<-SPR_52/(S5_feed_rate_cum[175]-S5_feed_rate_cum[79]) #Kg/pig/day

#Batch 3
SPR_531<-Fresh_Slurry_531/mean(S5_Pigs_feed[177:206])
SPRD_531<-SPR_531/(S5_feed_rate_cum[206]-S5_feed_rate_cum[177])
SPR_532<-Fresh_Slurry_532/mean(S5_Pigs_feed[206:234])
SPRD_532<-SPR_532/(S5_feed_rate_cum[234]-S5_feed_rate_cum[206]) ##Less SPRD than previous->Leak/missanotation
SPR_533<-Fresh_Slurry_533/mean(S5_Pigs_feed[234:248])
SPRD_533<-SPR_533/(S5_feed_rate_cum[248]-S5_feed_rate_cum[234])
SPR_534<-Fresh_Slurry_534/mean(S5_Pigs_feed[248:256])
SPRD_534<-SPR_534/(S5_feed_rate_cum[256]-S5_feed_rate_cum[248]) ##Much lower SPRD than previous-->Leak/missanotation
SPR_53<-sum(SPR_531,SPR_532,SPR_533,SPR_534) ## Lower slurry than the expected 500--> Might be leak or missanotation
SPRD_53<-SPR_53/(S5_feed_rate_cum[256]-S5_feed_rate_cum[177]) #Kg/pig/day

Kg_pig_day_Batch1<-c(round(SPRD_511,2),round(SPRD_512,2),
                     round(SPRD_513,2),round(SPRD_514,2),
                     "Total (Kg/pig)",round(SPR_51,2),
                     "Total (Kg/pig/day)",round(SPRD_51,2))
Kg_pig_day_Batch2<-c(round(SPRD_521,2),round(SPRD_522,2),
                     round(SPRD_523,2),round(SPRD_524,2),
                     "Total (Kg/pig)",round(SPR_52,2),
                     "Total (Kg/pig/day)",round(SPRD_52,2))
Kg_pig_day_Batch3<-c(round(SPRD_531,2),round(SPRD_532,2),
                     round(SPRD_533,2),round(SPRD_534,2),
                     "Total (Kg/pig)",round(SPR_53,2),
                     "Total (Kg/pig/day)",round(SPRD_53,2))


df_SPR_S5<-cbind.data.frame(Kg_pig_day_Batch1,Kg_pig_day_Batch2,Kg_pig_day_Batch3)

#Batch 1
SPR_611<-Fresh_Slurry_611/mean(S6_Pigs_feed[1:28])
SPRD_611<-SPR_611/(S6_feed_rate_cum[28]-S6_feed_rate_cum[1])
SPR_612<-Fresh_Slurry_612/mean(S6_Pigs_feed[28:56])
SPRD_612<-SPR_612/(S6_feed_rate_cum[56]-S6_feed_rate_cum[28])

SPR_613<-Fresh_Slurry_613/mean(S6_Pigs_feed[56:70])
SPRD_613<-SPR_613/(S6_feed_rate_cum[70]-S6_feed_rate_cum[56])
SPR_614<-Fresh_Slurry_614/mean(S6_Pigs_feed[70:75])
SPRD_614<-SPR_614/(S6_feed_rate_cum[75]-S6_feed_rate_cum[70])
SPR_61<-sum(SPR_611,SPR_612,SPR_613,SPR_614) #Kg/pig
SPRD_61<-SPR_61/(S6_feed_rate_cum[75]-S6_feed_rate_cum[1]) #Kg/pig/day

#Batch 2

SPR_621<-Fresh_Slurry_621/mean(S6_Pigs_feed[77:99])
SPRD_621<-SPR_621/(S6_feed_rate_cum[99]-S6_feed_rate_cum[77])
SPR_622<-Fresh_Slurry_622/mean(S6_Pigs_feed[99:127])
SPRD_622<-SPR_622/(S6_feed_rate_cum[127]-S6_feed_rate_cum[99])
SPR_623<-Fresh_Slurry_623/mean(S6_Pigs_feed[127:141])
SPRD_623<-SPR_623/(S6_feed_rate_cum[141]-S6_feed_rate_cum[127])

SPR_624<-Fresh_Slurry_624/mean(S6_Pigs_feed[177:199])
SPRD_624<-SPR_624/(S6_feed_rate_cum[199]-S6_feed_rate_cum[177])
SPR_62<-sum(SPR_621,SPR_622,SPR_623,SPR_624) #Kg/pig
SPRD_62<-SPR_62/(S6_feed_rate_cum[199]-S6_feed_rate_cum[77]) #Kg/pig/day

#Batch 3-->

SPR_631<-Fresh_Slurry_631/mean(S6_Pigs_feed[162:184])
SPRD_631<-SPR_631/(S6_feed_rate_cum[184]-S6_feed_rate_cum[162])
SPR_632<-Fresh_Slurry_632/mean(S6_Pigs_feed[184:212])
SPRD_632<-SPR_632/(S6_feed_rate_cum[212]-S6_feed_rate_cum[184])
SPR_633<-Fresh_Slurry_633/mean(S6_Pigs_feed[212:225])
SPRD_633<-SPR_633/(S6_feed_rate_cum[225]-S6_feed_rate_cum[212])
SPR_634<-Fresh_Slurry_634/mean(S6_Pigs_feed[225:247])
SPRD_634<-SPR_634/(S6_feed_rate_cum[247]-S6_feed_rate_cum[225])
SPR_63<-sum(SPR_631,SPR_632,SPR_633,SPR_634) #Kg/pig
SPRD_63<-SPR_63/(S6_feed_rate_cum[247]-S6_feed_rate_cum[162]) #Kg/pig/day

Kg_pig_day_Batch1<-c(round(SPRD_611,2),round(SPRD_612,2),
                     round(SPRD_613,2),round(SPRD_614,2),
                     "Total (Kg/pig)",round(SPR_61,2),
                     "Total (Kg/pig/day)",round(SPRD_61,2))
Kg_pig_day_Batch2<-c(round(SPRD_621,2),round(SPRD_622,2),
                     round(SPRD_623,2),round(SPRD_624,2),
                     "Total (Kg/pig)",round(SPR_62,2),
                     "Total (Kg/pig/day)",round(SPRD_62,2))
Kg_pig_day_Batch3<-c(round(SPRD_631,2),round(SPRD_632,2),
                     round(SPRD_633,2),round(SPRD_634,2),
                     "Total (Kg/pig)",round(SPR_63,2),
                     "Total (Kg/pig/day)",round(SPRD_63,2))


df_SPR_S6<-cbind.data.frame(Kg_pig_day_Batch1,Kg_pig_day_Batch2,Kg_pig_day_Batch3)


### MODEL RUN ###
setwd(".../RData") #Choose own directory
load("optimization_A.RData")
load("optimization.RData")
wthr_pars <- wthr_pars2.0
wthr_pars$rain <- 0

df_man_pars_S5_2.0$conc_fresh$xa_aer <- 0
df_man_pars_S5_2.0$conc_fresh$xa_bac <- 0
arrh_pars <- arrh_pars2.0
arrh_pars$scale_alpha_opt$notVSd <- 1.0

## Use mean concentration values
cols_to_keep <- c("time")
df_man_pars_S5_2.0_DT<-df_man_pars_S5_2.0
cols_to_transform <- setdiff(names(df_man_pars_S5_2.0$conc_fresh), cols_to_keep)

df_man_pars_S5_2.0$conc_fresh[cols_to_transform] <- lapply(df_man_pars_S5_2.0$conc_fresh[cols_to_transform], function(col) mean(col, na.rm = TRUE))
df_man_pars_S5_2.0_DT$conc_fresh[cols_to_transform] <- lapply(df_man_pars_S5_2.0_DT$conc_fresh[cols_to_transform], function(col) sd(col, na.rm = TRUE))

out_S5_2.0<-abm(days=366, man_pars = df_man_pars_S5_2.0, startup = 1, wthr_pars = wthr_pars, 
                add_pars = list(evap = E_m2_day, slurry_mass= df_slurry_water_S5,
                                storage_depth = storage_depth,
                                floor_area = floor_area,
                                area = area,
                                temp_C = df_temp_C_S5))



arrh_pars <- arrh_pars2.0
lnA_S5<-as.numeric(10^cal_S5A$par[1])
E_CH4<-as.numeric(10^cal_S5A$par[2])
qhat_opt_m2_S5<-as.numeric(10^cal_S5$par[1])
scale_alpha_opt_S5<-as.numeric(10^cal_S5$par[2])

arrh_pars$scale_alpha_opt$notVSd <- scale_alpha_opt_S5

out_S5_2.0_new<-abm(days=366, man_pars = df_man_pars_S5_2.0, wthr_pars = wthr_pars ,  
                    arrh_pars = arrh_pars,startup = 1, 
                          add_pars = list(evap = E_m2_day, slurry_mass= df_slurry_water_S5,
                                          storage_depth = storage_depth,
                                          floor_area = floor_area,
                                          area = area,
                                          temp_C = df_temp_C_S5,
                                          qhat_opt.m2 = qhat_opt_m2_S5, scale_alpha_opt.notVSd = scale_alpha_opt_S5,
                                          lnA.VSd_A = lnA_S5, E_CH4.VSd_A = E_CH4))





df_man_pars_S6_2.0$conc_fresh$xa_aer <- 0
df_man_pars_S6_2.0$conc_fresh$xa_bac <- 0

## Use mean concentration values
cols_to_keep <- c("time")
df_man_pars_S6_2.0_DT<-df_man_pars_S6_2.0

cols_to_transform <- setdiff(names(df_man_pars_S6_2.0$conc_fresh), cols_to_keep)

df_man_pars_S6_2.0$conc_fresh[cols_to_transform] <- lapply(df_man_pars_S6_2.0$conc_fresh[cols_to_transform], function(col) mean(col, na.rm = TRUE))
df_man_pars_S6_2.0_DT$conc_fresh[cols_to_transform] <- lapply(df_man_pars_S6_2.0_DT$conc_fresh[cols_to_transform], function(col) sd(col, na.rm = TRUE))


arrh_pars <- arrh_pars2.0
arrh_pars$scale_alpha_opt$notVSd <- 1
out_S6_2.0<-abm(days=days_S6, man_pars = df_man_pars_S6_2.0,startup = 1, wthr_pars = wthr_pars, 
                add_pars = list(evap = E_m2_day, slurry_mass= df_slurry_water_S6,
                                storage_depth = storage_depth,
                                floor_area = floor_area,
                                area = area,
                                temp_C = df_temp_C_S6))



arrh_pars <- arrh_pars2.0
lnA_S6<-as.numeric(10^cal_S6A$par[1])
E_CH4<-as.numeric(10^cal_S6A$par[2])
qhat_opt_m2_S6<-as.numeric(10^cal_S6$par[1])
scale_alpha_opt_S6<-as.numeric(10^cal_S6$par[2])

out_S6_2.0_new<-abm(days=days_S6, man_pars = df_man_pars_S6_2.0, wthr_pars = wthr_pars ,  
                    arrh_pars = arrh_pars,startup = 1, 
                    add_pars = list(evap = E_m2_day, slurry_mass= df_slurry_water_S6,
                                    storage_depth = storage_depth,
                                    floor_area = floor_area,
                                    area = area,
                                    temp_C = df_temp_C_S6,
                                    qhat_opt.m2 = qhat_opt_m2_S6,scale_alpha_opt.notVSd = 1.04245048657282,
                                    lnA.VSd_A = lnA_S6, E_CH4.VSd_A = E_CH4))



### Save the model output
setwd("O:/Tech_BCE/Environmental Engineering/Air Quality Engineering/Pablo García/METEMIS/ABM model/Model output")
save(out_S5_2.0,out_S5_2.0_new,out_S6_2.0,out_S6_2.0_new,
     file="Model_paper_new.RData")

save(df_temp_C_S5,df_temp_C_S6,file="../RData/Slurry_T.RData")

#### TIER 2: Default Danish report methane Arrhenius equation ###




### Constants

# CH4/pig/day using EF
HRT <- 17.9 #Hydraulic retention time (days)
pig_prod_time <- 84 #Average pig production time
VS_pig_day_normtal <- 31.3 / 84 # kg/day/pig, based on normative system of feed digestibility and intake: gives 31.3 kg VS excreted over 84 days. 
EF <- 572.97 # g CH4 / kg VS / year from National inventory report. p 890 or 889
CH4_pig_day_EF <- VS_pig_day_normtal * EF * HRT/365 # g / pig / day, equation 3D-3 page 888

#CH4/pig/day using catalogue equations
CH4_ton_slurry <- 1.81 # finisher pigs drained slatted floor, kg CH4/tonne slurry excreted
slurry_excreted <- 0.5 # tonne / pig
CH4_pig <- slurry_excreted * CH4_ton_slurry # kg CH4/pig produced (that is over 84 days)
CH4_pig_day_climate <- CH4_pig/84*1000 # g CH4/pig/day


## Arrhenius equation
#Constants
VSd<-51/100 # g VSd/gVS
VSnd<-49/100 # g VSnd/gVS
b1<-1
b2<-0.01
lnA<-31.3 #gCH4/KgVS/h
R<-8.314 #J/mol/K
Ea<-81*1000 #J/mol
T_avg<-18.6 + 273 #k Holm, 2015
RT <- R*T_avg
CH4_A<-(VSd*b1*exp(lnA-Ea*(1/(RT)))+VSnd*b2*exp(lnA-Ea*(1/(RT))))*24 #gCH4/day/KgVS

CH4_pig_day_Arr<-CH4_A*VS_pig_day_normtal*HRT #g CH4/pig/day
setwd("...") #Choose own directory
write.table(CH4_pig_day_Arr,"Rfiles/A_default.txt")



### Check height, temperature and emissions ###

ggplot()+geom_point(aes(df_CH4_measured_S5$time[1:3000],(df_CH4_measured_S5$CH4[1:3000]-min(df_CH4_measured_S5$CH4[1:3000]))/(max(df_CH4_measured_S5$CH4[1:3000])-min(df_CH4_measured_S5$CH4[1:3000])),color="Measured CH4"))+
  geom_line(aes(df_slurry_water_S5$time[1:20],(df_slurry_water_S5$slurry_mass[1:20]-min(df_slurry_water_S5$slurry_mass[1:20]))/(max(df_slurry_water_S5$slurry_mass[1:20])-min(df_slurry_water_S5$slurry_mass[1:20])),color="Slurry mass"))+
  geom_line(aes(df_temp_C_S5$time[1:2000],(df_temp_C_S5$temp_C[1:2000]-min(df_temp_C_S5$temp_C[1:2000]))/(max(df_temp_C_S5$temp_C[1:2000])-min(df_temp_C_S5$temp_C[1:2000])),color="Temperature"))+
  geom_line(aes(out_S5_2.0$time[1:80],(out_S5_2.0$CH4_emis_rate[1:80]-min(out_S5_2.0$CH4_emis_rate[1:80]))/(max(out_S5_2.0$CH4_emis_rate[1:80])-min(out_S5_2.0$CH4_emis_rate[1:80])),color="ABM"))+
  geom_line(aes(out_S5_2.0$time[1:80],(out_S5_2.0$CH4_A_emis_rate[1:80]-min(out_S5_2.0$CH4_A_emis_rate[1:80]))/(max(out_S5_2.0$CH4_A_emis_rate[1:80])-min(out_S5_2.0$CH4_A_emis_rate[1:80])),color="Arrhenius"))+
  theme_bw()+xlab("Time")+ylab("Normalized values")+
  theme(panel.grid = element_blank())+
  scale_color_manual("",values=c("green","blue","black","brown","orange","cyan","orange"))


S5_massT<-ggplot()+geom_point(aes(df_CH4_measured_S5$time,(df_CH4_measured_S5$CH4-min(df_CH4_measured_S5$CH4))/(max(df_CH4_measured_S5$CH4)-min(df_CH4_measured_S5$CH4)),color="Measured CH4"))+
  geom_line(aes(df_slurry_water_S5$time,(df_slurry_water_S5$slurry_mass-min(df_slurry_water_S5$slurry_mass))/(max(df_slurry_water_S5$slurry_mass)-min(df_slurry_water_S5$slurry_mass[1:20])),color="Slurry mass"))+
  geom_line(aes(df_temp_C_S5$time,(df_temp_C_S5$temp_C-min(df_temp_C_S5$temp_C))/(max(df_temp_C_S5$temp_C)-min(df_temp_C_S5$temp_C)),color="Temperature"))+
  geom_line(aes(out_S5_2.0$time,(out_S5_2.0$CH4_emis_rate-min(out_S5_2.0$CH4_emis_rate))/(max(out_S5_2.0$CH4_emis_rate)-min(out_S5_2.0$CH4_emis_rate)),color="ABM"))+
  geom_line(aes(out_S5_2.0$time,(out_S5_2.0$CH4_A_emis_rate-min(out_S5_2.0$CH4_A_emis_rate))/(max(out_S5_2.0$CH4_A_emis_rate)-min(out_S5_2.0$CH4_A_emis_rate)),color="Arrhenius"))+
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

S6_massT<-ggplot()+geom_point(aes(df_CH4_measured_S6$time,(df_CH4_measured_S6$CH4-min(df_CH4_measured_S6$CH4))/(max(df_CH4_measured_S6$CH4)-min(df_CH4_measured_S6$CH4)),color="Measured CH4"))+
  geom_line(aes(df_slurry_water_S6$time,(df_slurry_water_S6$slurry_mass-min(df_slurry_water_S6$slurry_mass))/(max(df_slurry_water_S6$slurry_mass)-min(df_slurry_water_S6$slurry_mass[1:20])),color="Slurry mass"))+
  geom_line(aes(df_temp_C_S6$time,(df_temp_C_S6$temp_C-min(df_temp_C_S6$temp_C))/(max(df_temp_C_S6$temp_C)-min(df_temp_C_S6$temp_C)),color="Temperature"))+
  geom_line(aes(out_S6_2.0$time,(out_S6_2.0$CH4_emis_rate-min(out_S6_2.0$CH4_emis_rate))/(max(out_S6_2.0$CH4_emis_rate)-min(out_S6_2.0$CH4_emis_rate)),color="ABM"))+
  geom_line(aes(out_S6_2.0$time,(out_S6_2.0$CH4_A_emis_rate-min(out_S6_2.0$CH4_A_emis_rate))/(max(out_S6_2.0$CH4_A_emis_rate)-min(out_S6_2.0$CH4_A_emis_rate)),color="Arrhenius"))+
  theme_bw()+xlab("Time, days")+ylab("Normalized values")+
  theme(panel.grid = element_blank())+
  scale_color_manual("",values=c("green","blue","black","brown","orange","cyan","orange"))+
  geom_rect(aes(xmin=0, xmax=75, ymin=-Inf, ymax=Inf,fill="Batch 1"), alpha = 0.09, colour = NA)+
  geom_rect(aes(xmin=96, xmax=180, ymin=-Inf, ymax=Inf,fill="Batch 2"), alpha = 0.09, colour = NA)+
  geom_rect(aes(xmin=187, xmax=281, ymin=-Inf, ymax=Inf,fill="Batch 3"), alpha = 0.09, colour = NA)+
  theme(plot.title = element_text(size = 9))+ 
  scale_fill_manual('',values = c('grey1','grey30',"grey60","grey80"))+
  ggtitle("b) Section 2")+
  theme(legend.position = "none")

AmT<-ggplot()+geom_point(aes(df_CH4_measured_S6$time,(df_CH4_measured_S6$CH4-min(df_CH4_measured_S6$CH4))/(max(df_CH4_measured_S6$CH4)-min(df_CH4_measured_S6$CH4)),color="Measured"))+
  geom_line(aes(df_slurry_water_S6$time,(df_slurry_water_S6$slurry_mass-min(df_slurry_water_S6$slurry_mass))/(max(df_slurry_water_S6$slurry_mass)-min(df_slurry_water_S6$slurry_mass[1:20])),color="Slurry mass"))+
  geom_line(aes(df_temp_C_S6$time,(df_temp_C_S6$temp_C-min(df_temp_C_S6$temp_C))/(max(df_temp_C_S6$temp_C)-min(df_temp_C_S6$temp_C)),color="Temperature"))+
  geom_line(aes(out_S6_2.0$time,(out_S6_2.0$CH4_emis_rate-min(out_S6_2.0$CH4_emis_rate))/(max(out_S6_2.0$CH4_emis_rate)-min(out_S6_2.0$CH4_emis_rate)),color="ABM"))+
  geom_line(aes(out_S6_2.0$time,(out_S6_2.0$CH4_A_emis_rate-min(out_S6_2.0$CH4_A_emis_rate))/(max(out_S6_2.0$CH4_A_emis_rate)-min(out_S6_2.0$CH4_A_emis_rate)),color="Arrhenius"))+
  theme_bw()+xlab("Time, days")+ylab("Normalized values")+
  theme(panel.grid = element_blank())+
  scale_color_manual("",values=c("green","blue","black","brown","orange","cyan","orange"))+
  geom_rect(aes(xmin=0, xmax=75, ymin=-Inf, ymax=Inf,fill="Batch 1"), alpha = 0.09, colour = NA)+
  geom_rect(aes(xmin=96, xmax=180, ymin=-Inf, ymax=Inf,fill="Batch 2"), alpha = 0.09, colour = NA)+
  geom_rect(aes(xmin=187, xmax=281, ymin=-Inf, ymax=Inf,fill="Batch 3"), alpha = 0.09, colour = NA)+
  theme(plot.title = element_text(size = 9))+
  scale_fill_manual('',values = c('grey1','grey30',"grey60","grey80"))

setwd("O:/Tech_BCE/Environmental Engineering/Air Quality Engineering/Pablo García/METEMIS/ABM model")
legend <- get_legend(AmT)
group1 <- plot_grid(S5_massT,S6_massT, ncol = 1, align = "hv")
FigS1<-plot_grid(group1,legend,rel_widths = c(1, .5))
ggsave(plot=FigS1,"plots/FigureS3.png", width = 9, height = 5,bg='white')




