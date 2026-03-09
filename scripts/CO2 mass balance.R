

### Cover ventilation rate gaps ###

# ---- Load data ----
#VR Batch 2
Flow2<-read.table(here("data","Flow Batch 2.txt"),header=TRUE, sep = "\t")
#Concentration
C<-read.table(here("data/Measured emissions","Concentrations.txt"),header=TRUE, sep = "")
#Room temperature
T2<-read.table(here("data","Room_temp_2.txt"),header=TRUE, sep = "\t")
#Pig weight and number of pigs data
data<-read_excel(here("data","Input.xlsx"))

Time_D<-anytime(data$datetime) #Time
Time_D[c(1:95,138:234)]<-Time_D[c(1:95,138:234)]-hours(2) ##CEST time
Time_D[c(96:137,235:277)]<-Time_D[c(96:137,235:277)]-hours(1) ##CET time
N_pigs<-data$pigs #number of pigs
Kg_pigs<-data$`average pig weight` #Pigs' weight

# ---- Interpolate pigs' weight ----

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



# ---- Syncronize VR with measured CO2 concentration ----
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

# ---- Merge in a data frame ----
Time_1<-anytime(Flow2$Flow_All.date.time[5150:27898])
Time_2<-anytime(C$M_Time_S5[3500:7531]) #sECTION 5
Time_26<-anytime(C$M_Time_S6[3500:7531]) #sECTION 6
Flow_CO2<-Flow2$Flow_All..Airflow.Section_5.[5150:27898]
Flow_CO26<-Flow2$Flow_All..Airflow.Section_6.[5150:27898]
Temp_CO2<-T2$T_5[5150:27898]
Temp_CO26<-T2$T_6[5150:27898]
CO2_Conc_Diff<-C$M_CO2_S5[3500:7531]-C$M_CO2_BGr[3500:7531]
CO2_Conc_Diff6<-C$M_CO2_S6[3500:7531]-C$M_CO2_BGr[3500:7531]
df_Flow_CO2<-cbind.data.frame(Time_1,Flow_CO2,Temp_CO2,Flow_CO26,Temp_CO26)
df_Conc_CO2<-cbind.data.frame(Time_2,CO2_Conc_Diff,
                              Time_26,CO2_Conc_Diff6)

# ---- Daily average ----
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
            DailyAverage_CO26 = mean(CO2_Conc_Diff6, na.rm = TRUE))

# ---- Apply CO2 mass balance equation ----

# Heat Production Unit
daily_avg_Conc$VR_HPU<-0.2/(daily_avg_Conc$DailyAverage_CO2*1e-6)
daily_avg_Conc$VR_HPU6<-0.2/(daily_avg_Conc$DailyAverage_CO26*1e-6)

# Interpolate pigs' weight
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
N_Fill <- na.locf(N_Fill)
N_Fill6 <- na.locf(N_Fill6)

IntWeight52<-I52(Dates)
IntWeight62<-I62(Dates)

# Total energy calculation
##https://www.cigr.org/sites/default/files/documets/CIGR_4TH_WORK_GR.pdf
m_values<-seq(30,120,by=10) #Mass, Kg
n_values<-c(3.25,3.43,3.41,3.40,3.40,3.38,3.18,2.98,2.78,2.60)

I<-approxfun(m_values,n_values)
Intn52<-I(IntWeight52)
Intn62<-I(IntWeight62)

Total_Energy<-(5.09*IntWeight52^0.75+(1-(0.47+0.003*IntWeight52))*(Intn52*5.09*IntWeight52^0.75-5.09*IntWeight52^0.75))*N_Fill
Total_Energy6<-(5.09*IntWeight62^0.75+(1-(0.47+0.003*IntWeight62))*(Intn62*5.09*IntWeight62^0.75-5.09*IntWeight62^0.75))*N_Fill6

# Fill calculated VR
match_positions <- match(as.Date(daily_avg_Flow$`as.Date(Time_1)`[2:85]), Dates)
T_Fill <- rep(20, length(Dates))
T_Fill[match_positions] <- daily_avg_Flow$DailyAverage_Temp[2:85]

Total_Energy_Temp<-Total_Energy+12*(20-T_Fill)
Total_Energy_Temp6<-Total_Energy6+12*(20-T_Fill)
HPU_Temp<-Total_Energy_Temp/1000
HPU<-Total_Energy/1000
HPU_Temp6<-Total_Energy_Temp6/1000
HPU6<-Total_Energy6/1000

new_rows <- data.frame(
  A = c("2022-09-17", "2022-09-18"),
  DailyAverage_CO2 = c(NaN, NaN),
  VR_HPU = c(NaN, NaN),
  DailyAverage_CO26 = c(NaN, NaN),
  VR_HPU6 = c(NaN, NaN)
)
colnames(new_rows)[colnames(new_rows) == "A"] <- colnames(daily_avg_Conc)[1]

# Index where you want to insert the new rows
insert_index <- 41  # Insert at the second position (adjust as needed)

# Split the original dataframe at the insert index
df1 <- daily_avg_Conc[seq_len(insert_index - 1),, drop = FALSE]
df2 <- daily_avg_Conc[seq(insert_index, nrow(daily_avg_Conc)),, drop = FALSE]

daily_avg_Conc <- rbind(df1, new_rows, df2)

#Interpolate Cprod
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
#Calculated Ventilation rate
daily_avg_Conc$VR<-IntCprod*HPU/(daily_avg_Conc$DailyAverage_CO2*1e-6)
daily_avg_Conc$VR6<-IntCprod6*HPU6/(daily_avg_Conc$DailyAverage_CO26*1e-6)

