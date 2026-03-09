

### Estimate the Ventilation Rate

# ---- Read files ----

list_of_files_2 <- list.files(path=here("data/Flow"),pattern = "*.txt",recursive=TRUE) ##Read data from Picarro
L_2<-length(list_of_files_2)
listofdfs<-list()

  
  for(i in 1:L_2){ #Loop through the numbers of ID's instead of the ID's
    T_Data<-read.table(here("data/Flow",list_of_files_2[i]),header=FALSE,skip=6,sep="\t")
    listofdfs[[i]] <- T_Data # save your dataframes into the list
  }
Flow_All<-bind_rows(listofdfs, .id = "column_label") #Join all elements of the list in one dataframe

# ---- Rename used columns ----
names(Flow_All)[names(Flow_All) == "V1"] <- "date.time"  ##Change names of used columns
names(Flow_All)[names(Flow_All) == "V31"] <- "DynamicAir_trinl?s_stald6"
names(Flow_All)[names(Flow_All) == "V32"] <- "Spj?ld_trinl?s_stald6"
names(Flow_All)[names(Flow_All) == "V33"] <- "On/off_stald6"
names(Flow_All)[names(Flow_All) == "V23"] <- "DynamicAir_trinl?s_stald5"
names(Flow_All)[names(Flow_All) == "V24"] <- "Spj?ld_trinl?s_stald5"
names(Flow_All)[names(Flow_All) == "V25"] <- "On/off_stald5"
names(Flow_All)[names(Flow_All) == "V21"] <- "Temperature_5"
names(Flow_All)[names(Flow_All) == "V29"] <- "Temperature_6"

# ---- Time correction ----
Flow_All$date.time<-as.POSIXct(Flow_All$date.time,format="%d-%m-%Y %H:%M:%S")
Flow_All$date.time<-Flow_All$date.time-minutes(14)

# ---- As numeric ----
Flow_All$`DynamicAir_trinl?s_stald6` <- gsub(',', '.', Flow_All$`DynamicAir_trinl?s_stald6`) #Replace a "," for "." to turn into numeric
Flow_All$`DynamicAir_trinl?s_stald6` <-as.numeric(Flow_All$`DynamicAir_trinl?s_stald6`)
Flow_All$`DynamicAir_trinl?s_stald5` <- gsub(',', '.', Flow_All$`DynamicAir_trinl?s_stald5`) #Replace a "," for "." to turn into numeric
Flow_All$`DynamicAir_trinl?s_stald5` <-as.numeric(Flow_All$`DynamicAir_trinl?s_stald5`)
Flow_All$`Spj?ld_trinl?s_stald6` <- gsub(',', '.', Flow_All$`Spj?ld_trinl?s_stald6`) #Replace a "," for "." to turn into numeric
Flow_All$`Spj?ld_trinl?s_stald6` <-as.numeric(Flow_All$`Spj?ld_trinl?s_stald6`)
Flow_All$`Spj?ld_trinl?s_stald5` <- gsub(',', '.', Flow_All$`Spj?ld_trinl?s_stald5`) #Replace a "," for "." to turn into numeric
Flow_All$`Spj?ld_trinl?s_stald5` <-as.numeric(Flow_All$`Spj?ld_trinl?s_stald5`)
Flow_All$`On/off_stald6` <- gsub(',', '.', Flow_All$`On/off_stald6`) #Replace a "," for "." to turn into numeric
Flow_All$`On/off_stald6` <-as.numeric(Flow_All$`On/off_stald6`)
Flow_All$`On/off_stald5` <- gsub(',', '.', Flow_All$`On/off_stald5`) #Replace a "," for "." to turn into numeric
Flow_All$`On/off_stald5` <-as.numeric(Flow_All$`On/off_stald5`)
Flow_All$`Temperature_5` <- gsub(',', '.', Flow_All$`Temperature_5`) #Replace a "," for "." to turn into numeric
Flow_All$`Temperature_5` <-as.numeric(Flow_All$`Temperature_5`)
Flow_All$`Temperature_6` <- gsub(',', '.', Flow_All$`Temperature_6`) #Replace a "," for "." to turn into numeric
Flow_All$`Temperature_6` <-as.numeric(Flow_All$`Temperature_6`)

# ---- Apply algorithm to get the flow in m3/h ----
Flow_All$`Tryk_trinl?s_stald6`<-75*Flow_All$`DynamicAir_trinl?s_stald6`-37.5
Flow_All$`Tryk_trinl?s_stald5`<-75*Flow_All$`DynamicAir_trinl?s_stald5`-37.5
Flow_All$`Spj?ld?bning_stald5`<-10.718*Flow_All$`Spj?ld_trinl?s_stald5`
Flow_All$`Spj?ld?bning_stald6`<-10.661*Flow_All$`Spj?ld_trinl?s_stald6`

##Turn negatives into 0##
T_6<-which(Flow_All$`Tryk_trinl?s_stald6`<0)
T_5<-which(Flow_All$`Tryk_trinl?s_stald5`<0)
S_6<-which(Flow_All$`Spj?ld?bning_stald6`<0)
S_5<-which(Flow_All$`Spj?ld?bning_stald5`<0)
Flow_All$`Tryk_trinl?s_stald6`[T_6]<-0
Flow_All$`Tryk_trinl?s_stald5`[T_5]<-0
Flow_All$`Spj?ld?bning_stald6`[S_6]<-0
Flow_All$`Spj?ld?bning_stald5`[S_5]<-0

##Add the extra flow when the extra ventilation is on##
On_5<-which(Flow_All$`On/off_stald5`==0)
Off_5<-which(Flow_All$`On/off_stald5`==25.67)
On_6<-which(Flow_All$`On/off_stald6`==0)
Off_6<-which(Flow_All$`On/off_stald6`==25.5)
Flow_All$`m?levinge6`<-NaN
Flow_All$`m?levinge5`<-NaN
Flow_All$`m?levinge6`[On_6]<-14135
Flow_All$`m?levinge6`[Off_6]<-0
Flow_All$`m?levinge5`[On_5]<- 13448.8
Flow_All$`m?levinge5`[Off_5]<-0
Flow_All$`Airflow Section_6`<-1.07922*sqrt(Flow_All$`Tryk_trinl?s_stald6`)*(Flow_All$`Spj?ld?bning_stald6`^1.49312)+Flow_All$`m?levinge6`
Flow_All$`Airflow Section_5`<-1.07922*sqrt(Flow_All$`Tryk_trinl?s_stald5`)*(Flow_All$`Spj?ld?bning_stald5`^1.49312)+Flow_All$`m?levinge5`

##Create a data frame
df_Flow<-cbind.data.frame(Flow_All$date.time,Flow_All$`Airflow Section_6`,Flow_All$`Airflow Section_5`)
