

### Example of raw CRDS data treatment ###  

# ---- load .dat files ----

if (load_CRDS == TRUE){
list_of_files <- list.files(here("data/May_CRDS_data"),pattern = "*.dat",recursive=TRUE) ##Read data from Picarro
  L<-length(list_of_files)
  listofdfs<-list()
    for(i in 1:L){ 
            Pic_Data<-read.table(here("data/May_CRDS_data",list_of_files[i]),header=TRUE)
      listofdfs[[i]] <- Pic_Data # save  dataframes into the list
      print(i)
    }
  Dat<-bind_rows(listofdfs, .id = "column_label") #Join all elements of the list in one dataframe
  
  #Change format of date and time to be plotted
  Dat$date.time<-paste(Dat$DATE,Dat$TIME)
  Dat$date.time<-as.POSIXct(Dat$date.time,format="%Y-%m-%d %H:%M:%S")
  
  #Isolate Section 5, roof BG, section 6 and trailer BG
  S_5<-Dat[  Dat$MPVPosition== 1 , ] #Select the data where valve is in position 1 (measuring Section 5)
  BG_roof<-Dat[  Dat$MPVPosition== 2 , ] #Select the data where valve is in position 2 (measuring BG at roof)
  S_6<-Dat[  Dat$MPVPosition== 3 , ] #Select the data where valve is in position 3 (measuring Section 6)
  BG_trailer<-Dat[  Dat$MPVPosition== 4 , ] #Select the data where valve is in position 4 (measuring BG at trailer)
  
# ---- Detect points where CRDS changes valve position ----
  #Section 5
  S5_trans<-1
  for (i in 2:nrow(S_5)){
    S5_trans[i]=as.numeric(S_5$date.time[i])-as.numeric(S_5$date.time[i-1])} #Substract time at position i to the time before. Positions that are 12 is where the transition happens
  
  S5_Pos<-which(S5_trans>=500) #Create a vector with the positions where the transition happens (points where timediff is 12 minutes--> more than 5 min as time between consecutive points is less than 1s)
  
  #roof BG
  BGr_trans<-1
  for (i in 2:nrow(BG_roof)){
    BGr_trans[i]=as.numeric(BG_roof$date.time[i])-as.numeric(BG_roof$date.time[i-1])} 
  BGr_Pos<-which(BGr_trans>=50) 
  
  #Section 6
  S6_trans<-1
  for (i in 2:nrow(S_6)){
    S6_trans[i]=as.numeric(S_6$date.time[i])-as.numeric(S_6$date.time[i-1])} 
  
  S6_Pos<-which(S6_trans>=500) 
  
  #trailer BG
  BGt_trans<-1
  for (i in 2:nrow(BG_trailer)){
    BGt_trans[i]=as.numeric(BG_trailer$date.time[i])-as.numeric(BG_trailer$date.time[i-1])} #Substract time at position i to the time before. Positions that are 12 is where the transition happens
  
  BGt_Pos<-which(BGt_trans>=500) #Create a vector with the positions where the transition happens (points where timediff is 12 minutes--> more than 5 min as time between consecutive points is less than 1s)
  
# ---- Take last 5 min data at each position (prevent memory effects) ----
#Section 1  
    listofdfs <- list() 
    
    for(i in 1:length(S5_Pos)){
      
      List_S5<-S_5[(S5_Pos[i]-399):(S5_Pos[i]-10),] #Take all last 5 min data (valve is 10min in each position)
      
      listofdfs[[i]] <- List_S5 # save your dataframes into the list
    }
  List_S5<-listofdfs 
  M_Time_S5<-1
  M_CH4_S5<-1
  M_NH3_S5<-1
  M_CO2_S5<-1
  M_H2O_S5<-1
  M_N2O_S5<-1
  for (i in 1:length(List_S5)){ # Do the average of each element of the list
    M_Time_S5[i]<-mean(List_S5[[i]]$date.time+hours(2)) #Picarro has offset of 2hours behind
    M_CH4_S5[i]<-mean(List_S5[[i]]$CH4_dry)
    M_NH3_S5[i]<-mean(List_S5[[i]]$NH3)
    M_CO2_S5[i]<-mean(List_S5[[i]]$CO2)
    M_N2O_S5[i]<-mean(List_S5[[i]]$N2O_dry)
    M_H2O_S5[i]<-mean(List_S5[[i]]$H2O)}
  M_Time_S5<-anytime(M_Time_S5) #Change the format from numeric to POSIXct
  
#Roof background
    listofdfs <- list() 
    
    for(i in 1:length(BGr_Pos)){
      
      List_BGr<-BG_roof[(BGr_Pos[i]-399):(BGr_Pos[i]-10),]
      
      listofdfs[[i]] <- List_BGr # save your dataframes into the list
    }
  List_BG_roof<-listofdfs 
  List_BG_roof[[20]] <- NULL  
  M_Time_BGr<-1
  M_CH4_BGr<-1
  M_NH3_BGr<-1
  M_CO2_BGr<-1
  M_H2O_BGr<-1
  M_N2O_BGr<-1
  for (i in 1:length(List_BG_roof)){ # Do the average of each element of the list
    M_Time_BGr[i]<-mean(List_BG_roof[[i]]$date.time+hours(2))
    M_CH4_BGr[i]<-mean(List_BG_roof[[i]]$CH4_dry)
    M_NH3_BGr[i]<-mean(List_BG_roof[[i]]$NH3)
    M_CO2_BGr[i]<-mean(List_BG_roof[[i]]$CO2)
    M_N2O_BGr[i]<-mean(List_BG_roof[[i]]$N2O_dry)
    M_H2O_BGr[i]<-mean(List_BG_roof[[i]]$H2O)}
  M_Time_BGr<-anytime(M_Time_BGr) #Change the format from numeric to POSIXct
  
  #Section 6 
    listofdfs <- list() 
    
    for(i in 1:length(S6_Pos)){
      
      List_S6<-S_6[(S6_Pos[i]-399):(S6_Pos[i]-10),]
      
      listofdfs[[i]] <- List_S6 # save your dataframes into the list
      }
  List_S6<-listofdfs
  M_Time_S6<-1
  M_CH4_S6<-1
  M_NH3_S6<-1
  M_CO2_S6<-1
  M_N2O_S6<-1
  M_H2O_S6<-1
  for (i in 1:length(List_S6)){ # Do the average of each element of the list
    M_Time_S6[i]<-mean(List_S6[[i]]$date.time+hours(2))
    M_CH4_S6[i]<-mean(List_S6[[i]]$CH4_dry)
    M_NH3_S6[i]<-mean(List_S6[[i]]$NH3)
    M_N2O_S6[i]<-mean(List_S6[[i]]$N2O_dry)
    M_CO2_S6[i]<-mean(List_S6[[i]]$CO2)
    M_H2O_S6[i]<-mean(List_S6[[i]]$H2O)}
  M_Time_S6<-anytime(M_Time_S6) #Change the format from numeric to POSIXct
  
  #BG trailer (alternative measured background)
    listofdfs <- list() 
    
    for(i in 1:length(BGt_Pos)){
      
      List_BGt<-BG_trailer[(BGt_Pos[i]-399):(BGt_Pos[i]-10),]
      
      listofdfs[[i]] <- List_BGt # save your dataframes into the list
    }
  List_BGt<-listofdfs
  M_Time_BGt<-1
  M_CH4_BGt<-1
  M_NH3_BGt<-1
  M_CO2_BGt<-1
  M_H2O_BGt<-1
  M_N2O_BGt<-1
  for (i in 1:length(List_BGt)){ # Do the average of each element of the list
    M_Time_BGt[i]<-mean(List_BGt[[i]]$date.time+hours(2))
    M_CH4_BGt[i]<-mean(List_BGt[[i]]$CH4_dry)
    M_NH3_BGt[i]<-mean(List_BGt[[i]]$NH3)
    M_CO2_BGt[i]<-mean(List_BGt[[i]]$CO2)
    M_N2O_BGt[i]<-mean(List_BGt[[i]]$N2O_dry)
    M_H2O_BGt[i]<-mean(List_BGt[[i]]$H2O)}
  M_Time_BGt<-anytime(M_Time_BGt) #Change the format from numeric to POSIXct
 }else{
  load(here("data/May_CRDS_data","stuff.RData")}

# ---- Time correction to syncronize background measurement ----
M_Time_S6<-append(M_Time_S6,NaN,after=300)
M_CH4_S6<-append(M_CH4_S6,NaN,after=300)
M_NH3_S6<-append(M_NH3_S6,NaN,after=300)
M_N2O_S6<-append(M_N2O_S6,NaN,after=300)
M_CO2_S6<-append(M_CO2_S6,NaN,after=300)
M_H2O_S6<-append(M_H2O_S6,NaN,after=300)

M_Time_BGt<-append(M_Time_BGt,NaN,after=300)
M_CH4_BGt<-append(M_CH4_BGt,NaN,after=300)
M_NH3_BGt<-append(M_NH3_BGt,NaN,after=300)
M_N2O_BGt<-append(M_N2O_BGt,NaN,after=300)
M_CO2_BGt<-append(M_CO2_BGt,NaN,after=300)
M_H2O_BGt<-append(M_H2O_BGt,NaN,after=300)

# ---- Create interpolation function to subtract the BackGround ----
##Section 5_Mean
Int_S5_Mean<-approxfun(M_Time_S5,M_NH3_S5)
NH3_S5_Mean<-Int_S5_Mean(M_Time_S5) #NH3 from Section 5 
Int_S5_Mean<-approxfun(M_Time_S5,M_N2O_S5)
N2O_S5_Mean<-Int_S5_Mean(M_Time_S5) #N2O from Section 5 
Int_S5_Mean<-approxfun(M_Time_S5,M_CH4_S5)
CH4_S5_Mean<-Int_S5_Mean(M_Time_S5) #CH4 from Section 5
Int_S5_Mean<-approxfun(M_Time_S5,M_CO2_S5)
CO2_S5_Mean<-Int_S5_Mean(M_Time_S5) #CO2 from Section 5
Int_S5_Mean<-approxfun(M_Time_S5,M_H2O_S5)
H2O_S5_Mean<-Int_S5_Mean(M_Time_S5) #H2O from Section 5

##Section 6_Mean
Int_S6_Mean<-approxfun(M_Time_S6,M_NH3_S6)
NH3_S6_Mean<-Int_S6_Mean(M_Time_S6) #NH3 from Section 6 
Int_S6_Mean<-approxfun(M_Time_S6,M_N2O_S6)
N2O_S6_Mean<-Int_S6_Mean(M_Time_S6) #N2O from Section 6 
Int_S6_Mean<-approxfun(M_Time_S6,M_CH4_S6)
CH4_S6_Mean<-Int_S6_Mean(M_Time_S6) #CH4 from Section 6
Int_S6_Mean<-approxfun(M_Time_S6,M_CO2_S6)
CO2_S6_Mean<-Int_S6_Mean(M_Time_S6) #CO2 from Section 6
Int_S6_Mean<-approxfun(M_Time_S6,M_H2O_S6)
H2O_S6_Mean<-Int_S6_Mean(M_Time_S6) #H2O from Section 6

##BG roof_Mean
Int_BGr_Mean<-approxfun(M_Time_BGr,M_NH3_BGr)
NH3_BG5_Mean<-Int_BGr_Mean(M_Time_S5) #NH3 from BG roof
NH3_BG6_Mean<-Int_BGr_Mean(M_Time_S6) #NH3 from BG roof
Int_BGr_Mean<-approxfun(M_Time_BGr,M_N2O_BGr)
N2O_BG5_Mean<-Int_BGr_Mean(M_Time_S5) #N2O from BG roof
N2O_BG6_Mean<-Int_BGr_Mean(M_Time_S6) #N2O from BG roof
Int_BGr_Mean<-approxfun(M_Time_BGr,M_CH4_BGr)
CH4_BG5_Mean<-Int_BGr_Mean(M_Time_S5) #CH4 from BG roof
CH4_BG6_Mean<-Int_BGr_Mean(M_Time_S6) #CH4 from BG roof
Int_BGr_Mean<-approxfun(M_Time_BGr,M_CO2_BGr)
CO2_BG5_Mean<-Int_BGr_Mean(M_Time_S5) #CO2 from BG roof
CO2_BG6_Mean<-Int_BGr_Mean(M_Time_S6) #CO2 from BG roof
Int_BGr_Mean<-approxfun(M_Time_BGr,M_H2O_BGr)
H2O_BG5_Mean<-Int_BGr_Mean(M_Time_S5) #H2O from BG roof
H2O_BG6_Mean<-Int_BGr_Mean(M_Time_S6) #H2O from BG roof

# ---- Calculate real concentration ----
##Section 5 Mean
S5_NH3_Mean<-NH3_S5_Mean-NH3_BG5_Mean
S5_N2O_Mean<-N2O_S5_Mean-N2O_BG5_Mean
S5_CH4_Mean<-CH4_S5_Mean-CH4_BG5_Mean
S5_CO2_Mean<-CO2_S5_Mean-CO2_BG5_Mean
S5_H2O_Mean<-H2O_S5_Mean

##Section 6 Mean
S6_NH3_Mean<-NH3_S6_Mean-NH3_BG6_Mean
S6_N2O_Mean<-N2O_S6_Mean-N2O_BG6_Mean
S6_CH4_Mean<-CH4_S6_Mean-CH4_BG6_Mean
S6_CO2_Mean<-CO2_S6_Mean-CO2_BG6_Mean
S6_H2O_Mean<-H2O_S6_Mean

# ---- Interpolate ventilation rate measurements at CRDS time ----
Int_Mean<-approxfun(Flow_All$date.time,Flow_All$`Airflow Section_5`) #Airflow at Section 5
Flow_S5<-Int_Mean(anytime(M_Time_S5)) #approximate Airflow section 5 rate at measuring time
Int_Mean<-approxfun(Flow_All$date.time,Flow_All$`Airflow Section_6`) #Airflow at Section 6
Flow_S6<-Int_Mean(anytime(M_Time_S6)) #approximate Airflow section 6 rate at measuring time
Int_Mean<-approxfun(Flow_All$date.time,Flow_All$`Temperature_5`) #Temperature section 5
T_5<-Int_Mean(anytime(Flow_All$date.time)) #approximate temperature at section 5
Int_Mean<-approxfun(Flow_All$date.time,Flow_All$`Temperature_6`) #Temperature section 6
T_6<-Int_Mean(anytime(Flow_All$date.time)) #approximate temperature at section 6

# ---- Calculate emissions in g/h ----
#section 1
Factor_N5<-(17.03*1e-6)/(0.082*298) #NH3
Factor_N5<-rep(Factor_N5,length(Flow_S5))
Mean_NH3_S5<-S5_NH3_Mean*Factor_N5*Flow_S5*100
Factor_C5<-(16.04*1e-3)/(0.082*298) #CH4
Factor_C5<-rep(Factor_C5,length(Flow_S5))
Mean_CH4_S5<-S5_CH4_Mean*Factor_C5*Flow_S5
Factor_C25<-(44.01*1e-3)/(0.082*298) #CO2
Factor_C25<-rep(Factor_C25,length(Flow_S5))
Mean_CO2_S5<-S5_CO2_Mean*Factor_C25*Flow_S5
Factor_N25<-(44.013*1e-3)/(0.082*298) #N2O
Factor_N25<-rep(Factor_N25,length(Flow_S5))
Mean_N2O_S5<-S5_N2O_Mean*Factor_N25*Flow_S5

#section 2
Factor_N6<-(17.03*1e-6)/(0.082*298) #NH3
Factor_N6<-rep(Factor_N6,length(Flow_S6))
Mean_NH3_S6<-S6_NH3_Mean*Factor_N6*Flow_S6*100
Factor_C6<-(16.04*1e-3)/(0.082*298) #CH4
Factor_C6<-rep(Factor_C6,length(Flow_S6))
Mean_CH4_S6<-S6_CH4_Mean*Factor_C6*Flow_S6
Factor_C26<-(44.01*1e-3)/(0.082*298) #CO2
Factor_C26<-rep(Factor_C26,length(Flow_S6))
Mean_CO2_S6<-S6_CO2_Mean*Factor_C26*Flow_S6
Factor_N26<-(44.013*1e-3)/(0.082*298) #N2O
Factor_N26<-rep(Factor_N26,length(Flow_S6))
Mean_N2O_S6<-S6_N2O_Mean*Factor_N26*Flow_S6



# ---- merge results in same data frame ----
Time<-1
for (i in 1:length(M_Time_S5)){
  Time[i]<-mean(c(M_Time_S5[i],M_Time_S6[i]))
}

df_Emis<-cbind.data.frame(Time,Flow_S5,Flow_S6,Mean_CH4_S5,Mean_CO2_S5,Mean_N2O_S5,Mean_NH3_S5,Mean_CH4_S6,Mean_CO2_S6,Mean_N2O_S6,Mean_NH3_S6)
df_Conc<-cbind.data.frame(M_Time_S5,M_Time_S6,M_Time_BGr,M_Time_BGt,M_NH3_S5,M_N2O_S5,M_CO2_S5,M_CH4_S5,M_NH3_S6,M_N2O_S6,M_CO2_S6,M_CH4_S6,M_NH3_BGr,M_N2O_BGr,M_CO2_BGr,M_CH4_BGr,M_NH3_BGt,M_N2O_BGt,M_CO2_BGt,M_CH4_BGt,M_H2O_S5,M_H2O_S6,M_H2O_BGr,M_H2O_BGt)

# ---- save data frames ----
write.table(df_Emis, here("Rfiles","Emissions_Example.txt"), sep = "\t", row.names = TRUE, col.names = NA)
write.table(df_Conc, here("Rfiles","Concentration_Example.txt"), sep = "\t", row.names = TRUE, col.names = NA)
