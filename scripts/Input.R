library(readxl)
library(dplyr)
library(ggplot2)
library(anytime)
library(lubridate)
library(zoo)
library(anytime)
library(ggplot2)

### Script to generate Figure S1 and the input for ABM_Modelling.R ####

setwd(".../data") # Fill with own local directory
data<-read_excel("Input.xlsx")

Day<-seq(from=0,to=90)

Weight<-(c(30,115))
Time<-c(c(0,90))
p_weight<-c(30,40,50,60,70,80,90,100,110,115)
p_rate<-c(1.23,1.64,1.99,2.3,2.55,2.74,2.86,2.91,2.88,2.83)

m_values<-seq(30,120,by=10) #Mass, Kg
n_values<-c(3.25,3.43,3.41,3.40,3.40,3.38,3.18,2.98,2.78,2.60)

I<-approxfun(Time,Weight)
IntWeight<-I(Day)





###Read the treated data


##Batch1
setwd(".../Batch 1") # Fill with own local directory
C1<-read.table(file="Concentration_A.txt",header=TRUE, sep = "\t")
C2<-read.table(file="Concentration_A2.txt",header=TRUE, sep = "\t")
C3<-read.table(file="Concentration_A3.txt",header=TRUE, sep = "\t")
C4<-read.table(file="Concentration_A4.txt",header=TRUE, sep = "\t")
C5<-read.table(file="Concentration_A5.txt",header=TRUE, sep = "\t")
E1<-read.table(file="Emissions_A.txt",header=TRUE, sep = "\t")
E2<-read.table(file="Emissions_A2.txt",header=TRUE, sep = "\t")
E3<-read.table(file="Emissions_A3.txt",header=TRUE, sep = "\t")
E4<-read.table(file="Emissions_A4.txt",header=TRUE, sep = "\t")
E5<-read.table(file="Emissions_A5.txt",header=TRUE, sep = "\t")
T1<-read.table( file = "TH_A.txt",header=TRUE, sep = "\t")
Flow1<-read.table(file="Flow Batch 1.txt",header=TRUE, sep = "\t")

##Batch 2
setwd(".../Batch 2") # Fill with own local directory
C6<-read.table(file="Concentration_A.txt",header=TRUE, sep = "\t")
C61<-read.table(file="Concentration_A1.txt",header=TRUE, sep = "\t")
C7<-read.table(file="Concentration_A2.txt",header=TRUE, sep = "\t")
C8<-read.table(file="Concentration_A3.txt",header=TRUE, sep = "\t")
C9<-read.table(file="Concentration_A4.txt",header=TRUE, sep = "\t")
C10<-read.table(file="Concentration_A5.txt",header=TRUE, sep = "\t")
E6<-read.table(file="Emissions_A.txt",header=TRUE, sep = "\t")
E61<-read.table(file="Emissions_A1.txt",header=TRUE, sep = "\t")
E7<-read.table(file="Emissions_A2.txt",header=TRUE, sep = "\t")
E8<-read.table(file="Emissions_A3.txt",header=TRUE, sep = "\t")
E9<-read.table(file="Emissions_A4.txt",header=TRUE, sep = "\t")
E10<-read.table(file="Emissions_A5.txt",header=TRUE, sep = "\t")
T2<-read.table( file = "TH_A.txt",header=TRUE, sep = "\t")
Flow2<-read.table(file="Flow Batch 2.txt",header=TRUE, sep = "\t")


##Batch 3
setwd(".../Batch 3") # Fill with own local directory
C11<-read.table(file="Concentration_A.txt",header=TRUE, sep = "\t")
C12<-read.table(file="Concentration_B.txt",header=TRUE, sep = "\t")
C13<-read.table(file="Concentration_C.txt",header=TRUE, sep = "\t")
C14<-read.table(file="Concentration_D.txt",header=TRUE, sep = "\t")
E11<-read.table(file="Emissions_A.txt",header=TRUE, sep = "\t")
E12<-read.table(file="Emissions_B.txt",header=TRUE, sep = "\t")
E13<-read.table(file="Emissions_C.txt",header=TRUE, sep = "\t")
E14<-read.table(file="Emissions_D.txt",header=TRUE, sep = "\t")
T3<-read.table( file = "TH_A.txt",header=TRUE, sep = "\t")
Flow3<-read.table(file="Flow Batch 3.txt",header=TRUE, sep = "\t")




C<-rbind(C1,C2,C3,C4,C5,C61,C6,C7,C8,C9,C10,C11,C12,C13,C14)
E<-rbind(E1,E2,E3,E4,E5,E61,E6,E7,E8,E9,E10,E11,E12,E13,E14)
Temp<-rbind(T1,T2,T3)
Flow_T1<-rbind(Flow1,Flow2,Flow3)

setwd(".../Rfiles") # Fill with own local directory
write.table(C, file = "Concentrations.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(Flow1, file = "Flow_1.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(Flow2, file = "Flow_2.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(Flow3, file = "Flow_3.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(Flow_T1, file = "Flow.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(Temp, file = "Room_T.txt", sep = "\t", row.names = TRUE, col.names = NA)

setwd(".../Temperature-data") # Fill with own local directory
T<-read_excel("Average slurry Temperature.xlsx")

Time_C<-rep(anytime(E$Time),2)
CH4_gd<-c(E$Mean_CH4_S5*24,E$Mean_CH4_S6*24)
Section_E<-c(rep(5,nrow(E)),rep(6,nrow(E)))
Time_T<-c(anytime(T$Time_S5),anytime(T$Time_S6))
Temperature<-c(T$Temperature_S5,T$Temperature_S6)
Section_T<-c(rep(5,nrow(T)),rep(6,nrow(T)))
Time_D<-anytime(data$datetime)
Time_D[c(1:95,138:234)]<-Time_D[c(1:95,138:234)]-hours(2) ##CEST time
Time_D[c(96:137,235:277)]<-Time_D[c(96:137,235:277)]-hours(1) ##CET time

N_pigs<-data$pigs
Kg_pigs<-data$`average pig weight`
Kg_slurry<-data$`slurry mass, kg`
#Kgd_feed<-c(IntRateExcel_51,NaN,IntRateExcel_52,NaN,IntRateExcel_53,
          #  NaN,IntRateExcel_61,NaN,IntRateExcel_62,NaN,IntRateExcel_63)
Section<-data$`section number`
Batch<-data$batch

## Interpolate Weight ##

Weigth51<-c(Kg_pigs[3],Kg_pigs[41])
Weigth52<-c(Kg_pigs[49],Kg_pigs[104])
Weigth53<-c(Kg_pigs[108],Kg_pigs[138])
Weigth61<-c(Kg_pigs[143],Kg_pigs[178])
Weigth62<-c(Kg_pigs[187],Kg_pigs[241])
Weigth63<-c(Kg_pigs[244],Kg_pigs[281])

Time51<-c(Time_D[3],Time_D[41])
Time52<-c(Time_D[49],Time_D[104])
Time53<-c(Time_D[108],Time_D[138])
Time61<-c(Time_D[143],Time_D[178])
Time62<-c(Time_D[187],Time_D[241])
Time63<-c(Time_D[244],Time_D[281])

I51<-approxfun(as.Date(Time51),Weigth51)
I52<-approxfun(as.Date(Time52),Weigth52)
I53<-approxfun(as.Date(Time53),Weigth53)
I61<-approxfun(as.Date(Time61),Weigth61)
I62<-approxfun(as.Date(Time62),Weigth62)
I63<-approxfun(as.Date(Time63),Weigth63)

## Start and End of each batch
Days_51 <- seq(as.Date(Time51[1]), as.Date(Time51[2]), by = "1 day")
Days_52 <- seq(as.Date(Time52[1]), as.Date(Time52[2]), by = "1 day")
Days_53 <- seq(as.Date(Time53[1]), as.Date(Time53[2]), by = "1 day")
Days_61 <- seq(as.Date(Time61[1]), as.Date(Time61[2]), by = "1 day")
Days_62 <- seq(as.Date(Time62[1]), as.Date(Time62[2]), by = "1 day")
Days_63 <- seq(as.Date(Time63[1]), as.Date(Time63[2]), by = "1 day")

##Daily Weight during each batch
DailyWeight_51<-I51(Days_51)
DailyWeight_52<-I52(Days_52)
DailyWeight_53<-I53(Days_53)
DailyWeight_61<-I61(Days_61)
DailyWeight_62<-I62(Days_62)
DailyWeight_63<-I63(Days_63)

## Feed consumption per pig per day

I<-approxfun(p_weight,p_rate)
IntRate_51<-I(DailyWeight_51)
IntRate_52<-I(DailyWeight_52)
IntRate_53<-I(DailyWeight_53)
IntRate_61<-I(DailyWeight_61)
IntRate_62<-I(DailyWeight_62)
IntRate_63<-I(DailyWeight_63)

##Number of pigs each day
match_positions_51 <- match(as.Date(Time_D[3:41]), Days_51)
match_positions_52 <- match(as.Date(Time_D[49:104]), Days_52)
match_positions_53 <- match(as.Date(Time_D[108:138]), Days_53)
match_positions_61 <- match(as.Date(Time_D[143:178]), Days_61)
match_positions_62 <- match(as.Date(Time_D[187:241]), Days_62)
match_positions_63 <- match(as.Date(Time_D[244:281]), Days_63)

Pigs_51 <- rep(NaN, length(Days_51))
Pigs_52 <- rep(NaN, length(Days_52))
Pigs_53 <- rep(NaN, length(Days_53))
Pigs_61 <- rep(NaN, length(Days_61))
Pigs_62 <- rep(NaN, length(Days_62))
Pigs_63 <- rep(NaN, length(Days_63))
Pigs_51[match_positions_51] <- N_pigs[3:41]
Pigs_52[match_positions_52] <- N_pigs[49:104]
Pigs_53[match_positions_53] <- N_pigs[108:138]
Pigs_61[match_positions_61] <- N_pigs[143:178]
Pigs_62[match_positions_62] <- N_pigs[187:241]
Pigs_63[match_positions_63] <- N_pigs[244:281]

Pigs_51 <- na.locf(Pigs_51)
Pigs_52 <- na.locf(Pigs_52)
Pigs_53 <- na.locf(Pigs_53)
Pigs_61 <- na.locf(Pigs_61)
Pigs_62 <- na.locf(Pigs_62)
Pigs_63 <- na.locf(Pigs_63)

## Consider number of pigs for the daily feed intake

Total_IntRate_51<-IntRate_51*Pigs_51
Total_IntRate_52<-IntRate_52*Pigs_52
Total_IntRate_53<-IntRate_53*Pigs_53
Total_IntRate_61<-IntRate_61*Pigs_61
Total_IntRate_62<-IntRate_62*Pigs_62
Total_IntRate_63<-IntRate_63*Pigs_63




Total_feed_51<-208317.3
Scaling_factor_51<-Total_feed_51/sum(Total_IntRate_51)
Total_feed_52<-202279.1
Scaling_factor_52<-Total_feed_52/sum(Total_IntRate_52)
Total_feed_53<-201520
Scaling_factor_53<-Total_feed_53/sum(Total_IntRate_53)
Total_feed_61<-187932
Scaling_factor_61<-Total_feed_61/sum(Total_IntRate_61)
Total_feed_62<-174768
Scaling_factor_62<-Total_feed_62/sum(Total_IntRate_62)
Total_feed_63<-211146.4
Scaling_factor_63<-Total_feed_63/sum(Total_IntRate_63)


RealRate_51<-Total_IntRate_51*Scaling_factor_51
sum(RealRate_51)
RealRate_52<-Total_IntRate_52*Scaling_factor_52
sum(RealRate_52)
RealRate_53<-Total_IntRate_53*Scaling_factor_53
sum(RealRate_53)
RealRate_61<-Total_IntRate_61*Scaling_factor_61
sum(RealRate_61)
RealRate_62<-Total_IntRate_62*Scaling_factor_62
sum(RealRate_62)
RealRate_63<-Total_IntRate_63*Scaling_factor_63
sum(RealRate_63)

Time_Feed<-c(Days_51,Days_52,Days_53,
             Days_61,Days_62,Days_63)
RealRate_Feed<-c(RealRate_51,RealRate_52,RealRate_53,
                 RealRate_61,RealRate_62,RealRate_63)
Section_Feed<-c(rep(5,length(Time_Feed)/2),rep(6,length(Time_Feed)/2))
Batch_Feed<-c(rep(1,length(DailyWeight_51)),rep(2,length(DailyWeight_52)),
              rep(3,length(DailyWeight_53)),rep(1,length(DailyWeight_61)),
              rep(2,length(DailyWeight_62)),rep(3,length(DailyWeight_63)))








###############################################################################
##Fill missing flow
#### Fill in Flow ####
Int_Mean<-approxfun(anytime(Flow2$Flow_All.date.time[1:35000]),
                    Flow2$Flow_All..Airflow.Section_5.[1:35000]) #Airflow at Section 5
Flow_S52<-Int_Mean(anytime(C$M_Time_S5[3500:7000])) #approximate Airflow section 5 rate at measuring time
Flow_S52[1797:2734]<-NaN
Flow_S52[2810:2909]<-NaN
Int_Mean<-approxfun(anytime(Flow2$Flow_All.date.time[1:35000]),
                    Flow2$Flow_All..Airflow.Section_6.[1:35000]) #Airflow at Section 6
Flow_S62<-Int_Mean(anytime(C$M_Time_S6[3500:7000])) #approximate Airflow section 6 rate at measuring time
Flow_S62[1797:2734]<-NaN
Flow_S62[2810:2909]<-NaN

#New data frame with Flow concentration emissions and time
Time_1<-anytime(Flow2$Flow_All.date.time[5150:27898])
Time_2<-anytime(C$M_Time_S5[3500:7531]) #sECTION 5
Time_26<-anytime(C$M_Time_S6[3500:7531]) #sECTION 6
Time_3<-(anytime(E$Time[3500:7531]))
Flow_CO2<-Flow2$Flow_All..Airflow.Section_5.[5150:27898]
Flow_CO26<-Flow2$Flow_All..Airflow.Section_6.[5150:27898]
Temp_CO2<-T2$T_5[5150:27898]
Temp_CO26<-T2$T_6[5150:27898]
CO2_Conc_Diff<-C$M_CO2_S5[3500:7531]-C$M_CO2_BGr[3500:7531]
CH4_Conc_Diff<-C$M_CH4_S5[3500:7531]-C$M_CH4_BGr[3500:7531]
NH3_Conc_Diff<-C$M_NH3_S5[3500:7531]-C$M_NH3_BGr[3500:7531]
N2O_Conc_Diff<-C$M_N2O_S5[3500:7531]-C$M_N2O_BGr[3500:7531]
CO2_Conc_Diff6<-C$M_CO2_S6[3500:7531]-C$M_CO2_BGr[3500:7531]
CH4_Conc_Diff6<-C$M_CH4_S6[3500:7531]-C$M_CH4_BGr[3500:7531]
NH3_Conc_Diff6<-C$M_NH3_S6[3500:7531]-C$M_NH3_BGr[3500:7531]
N2O_Conc_Diff6<-C$M_N2O_S6[3500:7531]-C$M_N2O_BGr[3500:7531]
CO2_Emis<-E$Mean_CO2_S5[3500:7531]
CO2_Emis6<-E$Mean_CO2_S6[3500:7531]
df_Flow_CO2<-cbind.data.frame(Time_1,Flow_CO2,Temp_CO2,Flow_CO26,Temp_CO26)
df_Conc_CO2<-cbind.data.frame(Time_2,CO2_Conc_Diff,CH4_Conc_Diff,NH3_Conc_Diff,N2O_Conc_Diff,
                              Time_26,CO2_Conc_Diff6,CH4_Conc_Diff6,NH3_Conc_Diff6,N2O_Conc_Diff6)
df_Emis_CO2<-cbind.data.frame(Time_3,CO2_Emis,CO2_Emis6)

library(dplyr)
daily_avg_Flow <- df_Flow_CO2 %>%
  group_by(as.Date(Time_1)) %>%
  summarize(DailyAverage_Flow = mean(Flow_CO2, na.rm = TRUE),
            DailyAverage_Temp = mean(Temp_CO2, na.rm = TRUE),
            DailyAverage_Flow6 = mean(Flow_CO26, na.rm = TRUE),
            DailyAverage_Temp6 = mean(Temp_CO26, na.rm = TRUE))

daily_avg_Conc <- df_Conc_CO2 %>%
  group_by(as.Date(Time_2)) %>%
  summarize(DailyAverage_CO2 = mean(CO2_Conc_Diff, na.rm = TRUE),
  DailyAverage_CH4 = mean(CH4_Conc_Diff, na.rm = TRUE),
  DailyAverage_NH3 = mean(NH3_Conc_Diff, na.rm = TRUE),
  DailyAverage_N2O = mean(N2O_Conc_Diff, na.rm = TRUE),
  DailyAverage_CO26 = mean(CO2_Conc_Diff6, na.rm = TRUE),
  DailyAverage_CH46 = mean(CH4_Conc_Diff6, na.rm = TRUE),
  DailyAverage_NH36 = mean(NH3_Conc_Diff6, na.rm = TRUE),
  DailyAverage_N2O6 = mean(N2O_Conc_Diff6, na.rm = TRUE))

daily_avg_Emis <- df_Emis_CO2 %>%
  group_by(as.Date(Time_3)) %>%
  summarize(DailyAverage = mean(CO2_Emis, na.rm = TRUE),
            DailyAverage6 = mean(CO2_Emis6, na.rm = TRUE))


daily_avg_Conc$VR_HPU<-0.2/(daily_avg_Conc$DailyAverage_CO2*1e-6)
daily_avg_Conc$VR_HPU6<-0.2/(daily_avg_Conc$DailyAverage_CO26*1e-6)


##Create Number of pigs per day
start_date <- as.Date("2022-08-07") 
end_date <- as.Date("2022-11-16")    
Dates <- seq(start_date, end_date, by = "1 day")

match_positions <- match(as.Date(Time_D[47:105]), Dates)
match_positions6 <- match(as.Date(Time_D[187:241]), Dates)

N_Fill <- rep(NaN, length(Dates))
N_Fill[match_positions] <- N_pigs[47:105]
N_Fill[1:3]<-0

N_Fill6 <- rep(NaN, length(Dates))
N_Fill6[match_positions6] <- N_pigs[187:241]
N_Fill6[1]<-0
# Use the na.locf function from the zoo package to fill in missing values
library(zoo)
N_Fill <- na.locf(N_Fill)
N_Fill6 <- na.locf(N_Fill6)

IntWeight52<-I52(Dates)
IntWeight62<-I62(Dates)

I<-approxfun(m_values,n_values)
Intn52<-I(IntWeight52)
Intn62<-I(IntWeight62)

Total_Energy<-(5.09*IntWeight52^0.75+(1-(0.47+0.003*IntWeight52))*(Intn52*5.09*IntWeight52^0.75-5.09*IntWeight52^0.75))*N_Fill
Total_Energy6<-(5.09*IntWeight62^0.75+(1-(0.47+0.003*IntWeight62))*(Intn62*5.09*IntWeight62^0.75-5.09*IntWeight62^0.75))*N_Fill6

match_positions <- match(as.Date(daily_avg_Flow$`as.Date(Time_1)`[2:85]), Dates)
T_Fill <- rep(20, length(Dates))
T_Fill[match_positions] <- daily_avg_Flow$DailyAverage_Temp[2:85]

Total_Energy_Temp<-Total_Energy+12*(20-T_Fill)
Total_Energy_Temp6<-Total_Energy6+12*(20-T_Fill)
HPU_Temp<-Total_Energy_Temp/1000
HPU<-Total_Energy/1000
HPU_Temp6<-Total_Energy_Temp6/1000
HPU6<-Total_Energy6/1000

##Missing Picarro data as NaN

new_rows <- data.frame(
  A = c("2022-09-17", "2022-09-18"),
  DailyAverage_CO2 = c(NaN, NaN),
  DailyAverage_CH4 = c(NaN, NaN),
  DailyAverage_NH3 = c(NaN, NaN),
  DailyAverage_N2O = c(NaN, NaN),
  VR_HPU = c(NaN, NaN),
  DailyAverage_CO26 = c(NaN, NaN),
  DailyAverage_CH46 = c(NaN, NaN),
  DailyAverage_NH36 = c(NaN, NaN),
  DailyAverage_N2O6 = c(NaN, NaN),
  VR_HPU6 = c(NaN, NaN)
)
colnames(new_rows)[colnames(new_rows) == "A"] <- colnames(daily_avg_Conc)[1]

# Index where you want to insert the new rows
insert_index <- 41  # Insert at the second position 

# Split the original dataframe at the insert index
df1 <- daily_avg_Conc[seq_len(insert_index - 1),, drop = FALSE]
df2 <- daily_avg_Conc[seq(insert_index, nrow(daily_avg_Conc)),, drop = FALSE]

daily_avg_Conc <- rbind(df1, new_rows, df2)

##Interpolate Cprod

T_Cprod <- c("2022-08-11", "2022-08-30", "2022-08-31", "2022-09-27", "2022-09-28",
             "2022-10-11","2022-10-12","2022-11-16")
T_Cprod6 <- c("2022-08-09", "2022-08-30", "2022-08-31", "2022-09-27", "2022-09-28",
             "2022-10-11","2022-10-12","2022-10-31")
Cprod<-c(0.185,0.2,0.185,0.25,0.22,0.25,0.185,0.25)
Cprod6<-c(0.185,0.2,0.185,0.25,0.22,0.25,0.185,0.25)

I<-approxfun(as.Date(T_Cprod),Cprod)
IntCprod<-I(Dates)
I<-approxfun(as.Date(T_Cprod6),Cprod6)
IntCprod6<-I(Dates)

daily_avg_Conc$VR<-IntCprod*HPU/(daily_avg_Conc$DailyAverage_CO2*1e-6)
daily_avg_Conc$VR6<-IntCprod6*HPU6/(daily_avg_Conc$DailyAverage_CO26*1e-6)

Sys.setlocale("LC_TIME", "English")
VR5<-ggplot()+geom_point(aes(daily_avg_Flow$`as.Date(Time_1)`,daily_avg_Flow$DailyAverage_Flow),shape=1,size=4)+
geom_line(aes(daily_avg_Conc$`as.Date(Time_2)`,daily_avg_Conc$VR),color='blue')+theme_bw()+
  xlab("")+ylab(expression("VR, m"^3*" h"^-1*""))+ggtitle("a) Section 1")

VR6<-ggplot()+geom_point(aes(daily_avg_Flow$`as.Date(Time_1)`[1:70],daily_avg_Flow$DailyAverage_Flow6[1:70]),shape=1,size=4)+
geom_line(aes(daily_avg_Conc$`as.Date(Time_2)`,daily_avg_Conc$VR6),color='green')+theme_bw()+
  xlab("")+ylab(expression("VR, m"^3*" h"^-1*""))+ggtitle("b) Section 2")

FigS2 <- plot_grid(VR5,VR6, ncol = 1, align = "hv")
ggsave(plot=FigS2,".../plots/FigureS1.png", width = 9, height = 5,bg='white')

##Add missing dates with Cprod equation
start_date <- as.POSIXct("2022-10-12 12:00:00") 
end_date <- as.POSIXct("2022-10-25 12:00:00")    
Dates_Flow_Fill <- seq(start_date, end_date, by = "1 day")

df_Flow_Fill <- data.frame(
  X = seq(66,79),
  Flow_All.date.time = Dates_Flow_Fill,
  Flow_All..Airflow.Section_6. = (daily_avg_Conc$VR6[66:79]),
  Flow_All..Airflow.Section_5. = (daily_avg_Conc$VR[66:79])
)

# Index where you want to insert the new rows
insert_index <- 22824  # Insert at the second position (adjust as needed)
Flow2$Flow_All.date.time<-anytime(Flow2$Flow_All.date.time)

# Split the original dataframe at the insert index
df1 <- Flow2[seq_len(insert_index - 1),, drop = FALSE]
df2 <- Flow2[seq(insert_index, nrow(Flow2)),, drop = FALSE]

Flow2 <- rbind(df1, df_Flow_Fill, df2)

##Calculated flow
plot(Flow2$Flow_All.date.time[20000:25000],Flow2$Flow_All..Airflow.Section_5.[20000:25000])
plot(Flow2$Flow_All.date.time[20000:25000],Flow2$Flow_All..Airflow.Section_6.[20000:25000])

##Recalculate Emissions

E<-E[-c(5689:6233),] ##Remove data with wrong flow and emissions calculation

E$Time<-anytime(E$Time)
#Convert Emissions to g/h
Factor_N<-(17.03*1e-6)/(0.082*298) #NH3
Factor_C<-(16.04*1e-3)/(0.082*298) #CH4
Factor_C2<-(44.01*1e-3)/(0.082*298) #CO2
Factor_N2<-(44.013*1e-3)/(0.082*298) #N2O

df_Emissions_Fill <- data.frame(
  X = seq(66,79),
  Time = Dates_Flow_Fill,
  Flow_S5 = daily_avg_Conc$VR[66:79],
  Flow_S6 = daily_avg_Conc$VR[66:79],
  Mean_CH4_S5 = (daily_avg_Conc$VR[66:79]*daily_avg_Conc$DailyAverage_CH4[66:79]*Factor_C),
  Mean_CO2_S5 = (daily_avg_Conc$VR[66:79]*daily_avg_Conc$DailyAverage_CO2[66:79]*Factor_C2),
  Mean_N2O_S5 = (daily_avg_Conc$VR[66:79]*daily_avg_Conc$DailyAverage_N2O[66:79]*Factor_N2),
  Mean_NH3_S5 = (daily_avg_Conc$VR[66:79]*daily_avg_Conc$DailyAverage_NH3[66:79]*Factor_N*100),
  Mean_CH4_S6 = (daily_avg_Conc$VR6[66:79]*daily_avg_Conc$DailyAverage_CH46[66:79]*Factor_C),
  Mean_CO2_S6 = (daily_avg_Conc$VR6[66:79]*daily_avg_Conc$DailyAverage_CO26[66:79]*Factor_C2),
  Mean_N2O_S6 = (daily_avg_Conc$VR6[66:79]*daily_avg_Conc$DailyAverage_N2O6[66:79]*Factor_N2),
  Mean_NH3_S6 = (daily_avg_Conc$VR6[66:79]*daily_avg_Conc$DailyAverage_NH36[66:79]*Factor_N*100))

# Index where you want to insert the new rows
insert_index <- 5689  # Insert at the second position (adjust as needed)

# Split the original dataframe at the insert index
df1 <- E[seq_len(insert_index - 1),, drop = FALSE]
df2 <- E[seq(insert_index, nrow(E)),, drop = FALSE]

E <- rbind(df1, df_Emissions_Fill, df2)



E<-E[-c(5778:5877),] ##Remove data with wrong flow and emissions calculation from 28 to 31 of October


#Convert Emissions to g/h
Factor_N<-(17.03*1e-6)/(0.082*298) #NH3
Factor_C<-(16.04*1e-3)/(0.082*298) #CH4
Factor_C2<-(44.01*1e-3)/(0.082*298) #CO2
Factor_N2<-(44.013*1e-3)/(0.082*298) #N2O

df_Emissions_Fill <- data.frame(
  X = seq(83,84),
  Time = c("2022-10-29 12:00:00 CEST","2022-10-30 12:00:00 CEST"),
  Flow_S5 = daily_avg_Conc$VR[83:84],
  Flow_S6 = daily_avg_Conc$VR[83:84],
  Mean_CH4_S5 = (daily_avg_Conc$VR[83:84]*daily_avg_Conc$DailyAverage_CH4[83:84]*Factor_C),
  Mean_CO2_S5 = (daily_avg_Conc$VR[83:84]*daily_avg_Conc$DailyAverage_CO2[83:84]*Factor_C2),
  Mean_N2O_S5 = (daily_avg_Conc$VR[83:84]*daily_avg_Conc$DailyAverage_N2O[83:84]*Factor_N2),
  Mean_NH3_S5 = (daily_avg_Conc$VR[83:84]*daily_avg_Conc$DailyAverage_NH3[83:84]*Factor_N*100),
  Mean_CH4_S6 = (daily_avg_Conc$VR6[83:84]*daily_avg_Conc$DailyAverage_CH46[83:84]*Factor_C),
  Mean_CO2_S6 = (daily_avg_Conc$VR6[83:84]*daily_avg_Conc$DailyAverage_CO26[83:84]*Factor_C2),
  Mean_N2O_S6 = (daily_avg_Conc$VR6[83:84]*daily_avg_Conc$DailyAverage_N2O6[83:84]*Factor_N2),
  Mean_NH3_S6 = (daily_avg_Conc$VR6[83:84]*daily_avg_Conc$DailyAverage_NH36[83:84]*Factor_N*100))

# Index where you want to insert the new rows
insert_index <- 5778  # Insert at the second position (adjust as needed)

# Split the original dataframe at the insert index
df1 <- E[seq_len(insert_index - 1),, drop = FALSE]
df2 <- E[seq(insert_index, nrow(E)),, drop = FALSE]

E <- rbind(df1, df_Emissions_Fill, df2)


#########################################################################
Time_C<-rep(anytime(E$Time),2)
CH4_gd<-c(E$Mean_CH4_S5*24,E$Mean_CH4_S6*24)
Section_E<-c(rep(5,nrow(E)),rep(6,nrow(E)))



Datetime<-c(Time_C,Time_T,Time_D,Time_Feed)
CH4_g_d<-c(CH4_gd,rep(NaN,length(Time_T)),rep(NaN,length(Time_D)),rep(NaN,length(Time_Feed)))
Section_d<-c(Section_E,rep(NaN,length(Time_T)),rep(NaN,length(Time_D)),rep(NaN,length(Time_Feed)))
Temperature_C<-c(rep(NaN,length(Time_C)),Temperature,rep(NaN,length(Time_D)),rep(NaN,length(Time_Feed)))
Section_C<-c(rep(NaN,length(Time_C)),Section_T,rep(NaN,length(Time_D)),rep(NaN,length(Time_Feed)))
Pigs_Number<-c(rep(NaN,length(Time_C)),rep(NaN,length(Time_T)),N_pigs,rep(NaN,length(Time_Feed)))
Pigs_Mass<-c(rep(NaN,length(Time_C)),rep(NaN,length(Time_T)),Kg_pigs,rep(NaN,length(Time_Feed)))
Slurry_Mass<-c(rep(NaN,length(Time_C)),rep(NaN,length(Time_T)),Kg_slurry,rep(NaN,length(Time_Feed)))
Feed_Rate<-c(rep(NaN,length(Time_C)),rep(NaN,length(Time_T)),rep(NaN,length(Time_D)),RealRate_Feed)
Batch_Rate<-c(rep(NaN,length(Time_C)),rep(NaN,length(Time_T)),rep(NaN,length(Time_D)),Batch_Feed)
Section_Rate<-c(rep(NaN,length(Time_C)),rep(NaN,length(Time_T)),rep(NaN,length(Time_D)),Section_Feed)
Section<-c(rep(NaN,length(Time_C)),rep(NaN,length(Time_T)),Section,rep(NaN,length(Time_Feed)))
Batch<-c(rep(NaN,length(Time_C)),rep(NaN,length(Time_T)),Batch,rep(NaN,length(Time_Feed)))
Comments<-c(rep(NaN,length(Time_C)),rep(NaN,length(Time_T)),data$Comments,rep(NaN,length(Time_Feed)))
df_ABM<-cbind.data.frame(Datetime,CH4_g_d,Temperature_C,Pigs_Number,
                         Pigs_Mass,Slurry_Mass,Feed_Rate,Batch_Rate,Section_Rate,Section,Batch,Section_C,Section_d,Comments)

df_ABM <- df_ABM[order(df_ABM$Datetime), ]
setwd(".../Rfiles")
write.table(df_ABM, file = "df_ABM2.txt", sep = "\t", row.names = TRUE, col.names = NA)

