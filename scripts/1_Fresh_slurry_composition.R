

### FRESH MANURE COMPOSITION URINE/FEACES + FEED ###


# ---- Read data and cod conversion factors ----

Data_Composition<-read_excel(here("data/Slurry", "Composition_R.xlsx"))
Data_Slurry<-read_excel(here("data/Slurry","Slurry_pH_R.xlsx"))
Fibers_Faeces<-read_excel(here("data/Slurry","Fibers_R.xlsx"))
Fat_Faeces<-read_excel(here("data/Slurry","Fat_R.xlsx"))
BMP_data<-read_excel(here("data/Slurry","BMP_R.xlsx"))
Feed_data<-read_excel(here("data/Slurry","Feed_analysis_R.xlsx"))
slurry_mass_S5<-read.table(here("data/Slurry","slurry_mass_S5.txt")) 
slurry_mass_S6<-read.table(here("data/Slurry","slurry_mass_S6.txt")) 
Dat_VFA<-read_excel(here("data/Slurry","VFA_slurry.xlsx"))
Dat_VFA_S5<-Dat_VFA[Dat_VFA$Section == 5,]
Dat_VFA_S5$time<-as.numeric(difftime(Dat_VFA_S5$Date, Dat_VFA_S5$Date[1], units = "days"))
Dat_VFA_S6<-Dat_VFA[Dat_VFA$Section == 6,]
Dat_VFA_S6$time<-as.numeric(difftime(Dat_VFA_S6$Date, Dat_VFA_S6$Date[1], units = "days"))



COD_conv <- c(xa_dead = 0.73, RFd = 0.8444792, iNDF = 0.8444792, starch = 0.8444792, Cfat = 0.3117844, CP = 0.6541602,
              VFA = 0.9383125)
Manure_dens<-1 #Kg/L
Urine_dens<-1 #Kg/L
Faeces_dens<-1 #Kg/L

# ----- Section 1 -----
## ----- Slurry -----
###  pH 
Pos_5S<-which(Data_Slurry$Section==5)
S5_pH_S<-Data_Slurry$pH[Pos_5S]
S5_pH_S_Time<-Data_Slurry$Date[Pos_5S]
S5_pH_S_days<-as.numeric(diff(S5_pH_S_Time)) #days
S5_pH_S_cum<-c(0,cumsum(S5_pH_S_days))
df_S5_pH<-data.frame(time=S5_pH_S_cum,pH = S5_pH_S )

### Total slurry mass (kg, calculated with slurry height records)
Total_Fresh_Slurry_51<-160922
Total_Fresh_Slurry_52<-110967
Total_Fresh_Slurry_53<-124885.2
Total_Fresh_Slurry_61<-130015.3
Total_Fresh_Slurry_62<-93903.05
Total_Fresh_Slurry_63<-139771.1

## ---- Urine ----
Pos_5U<-which(Data_Composition$Section==5 & Data_Composition$Type == "Urine")
S5_Composition_timeU<-Data_Composition$Time[Pos_5U]
S5_pH<-Data_Composition$pH[Pos_5U]
### ---- SO4 
Ratio_S_SO4<-32.06/96.06
S5_SO4_mgL<-Data_Composition$`SO4_mg/L`[Pos_5U] #mg SO4/L urine
S5_SO4<-S5_SO4_mgL/Urine_dens/1000 #g SO4/Kg urine
S5_SO4_S<-S5_SO4*Ratio_S_SO4 #g S/Kg urine
### ---- Urea
S5_TN_U<-Data_Composition$`TN_g/Kg`[Pos_5U] #g Urine-TN/Kg urine
S5_Urea_N<-S5_TN_U*0.75 #g N/Kg urine--> Assuming Urea N is equal to urine N * 0.75 
### ---- VFA 
S5_VFA_mgL_U<-Data_Composition$`VFA_mg/L`[Pos_5U] #mg VFA/L Urine
S5_VFA_U<-S5_VFA_mgL_U/Urine_dens/1000 #g VFA/Kg Urine
S5_VFA_COD_U<-S5_VFA_U/COD_conv[7] #g COD/Kg Urine
### ---- TAN 
S5_TAN_U<-S5_TN_U*(1-0.75) #g Rest-N/Kg Urine --> Fine to assume everything that is not urea is CP or should be TAN??? 
### ---- Ash 
S5_VS_U<-Data_Composition$`VS_%`[Pos_5U] #%-VS/DM 
S5_Ash_U<-100-S5_VS_U #%-Ash/DM 
S5_Ash_g_Kg_U <- ((S5_Ash_U* Data_Composition$`DM_%`[Pos_5U]/100)/100)*1000 # g Ash/ Kg Urine
### ---- VS
#(Assume everything is degradable) and recalculate with Urea
S5_VS_Pct_U<-(S5_VS_U* Data_Composition$`DM_%`[Pos_5U]/100) #%-VS/ Urine--> Kg VS/100Kg Urine
S5_VS_g_Kg_U <-(S5_VS_Pct_U/100)*1000 # g VS/ Kg Urine
Ratio_Urea_Nitrogen<-60.06/(14*2)
S5_Urea_g_Kg<-S5_Urea_N*Ratio_Urea_Nitrogen #g Urea/Kg Urine
S5_VSd_A_U<-S5_VS_g_Kg_U-S5_Urea_g_Kg ## Assume that all VS are degradable after Urea subtraction
S5_VSnd_A_U<-S5_VSd_A_U-S5_VSd_A_U ## No VSnd in Urine

## ---- Faeces ----
Pos_5F<-which(Data_Composition$Section==5 & Data_Composition$Type == "Faeces")

### ---- TAN 
Ratio_N_NH4 <- 14/18
S5_TAN<-Data_Composition$`NH4-N_g/Kg`[Pos_5F] #g NH4/Kg Faeces
S5_TAN_F <-S5_TAN/Ratio_N_NH4 #g N/Kg Faeces
### ---- VFA 
S5_VFA_mgL_F<-Data_Composition$`VFA_mg/L`[Pos_5F] #mg VFA/L Faeces
S5_VFA_F<-S5_VFA_mgL_F/Faeces_dens/1000 #g VFA/Kg Faeces
S5_VFA_COD_F<-S5_VFA_F/COD_conv[7] #g COD/Kg Faeces
### ---- CP 
S5_CP<-(Data_Composition$`TN_g/Kg`[Pos_5F]-S5_TAN)*6.25-0 #g CP/Kg Faeces --> Assuming faeces 0 Urea
S5_CP_COD<-S5_CP/COD_conv[6] # g COD/Kg Faeces
### ---- NDF 
RFd<-29.1 #gCOD/kg (Default)
Pos_5Fibers<-which(Fibers_Faeces$Section == 5)
S5_NDF_F_time<-as.Date(Fibers_Faeces$Date[Pos_5Fibers])
S5_NDF_F<-Fibers_Faeces$`NDF_%`[Pos_5Fibers] #% NDF/DM -->  in Kg NDF / 100 Kg DM
S5_NDF_F_g_Kg<-((S5_NDF_F*Fibers_Faeces$`DM_%`[Pos_5Fibers]/100)/100)*1000 #g NDF/ Kg faeces
S5_NDF_F_COD<-S5_NDF_F_g_Kg/COD_conv[3] #g COD / Kg Faeces
df_Fibers_F<-data.frame(time = S5_NDF_F_time,NDF_F = S5_NDF_F_COD)

### ---- Ash
S5_VS_F<-Data_Composition$`VS_%`[Pos_5F] #%-VS/DM 
S5_Ash_F<-100-S5_VS_F #%-Ash/DM 
S5_Ash_g_Kg_F <- ((S5_Ash_F* Data_Composition$`VS_%`[Pos_5F]/100)/100)*1000 # g Ash/ Kg Faeces

### ---- VS ----
S5_VS_F<-Data_Composition$`VS_%`[Pos_5F] #%-VS/DM 
S5_VS_Pct_F<-(S5_VS_F* Data_Composition$`DM_%`[Pos_5F]/100) #%-VS/ Faeces--> Kg VS/100Kg Faeces
S5_VS_Kg_Kg_F <-((S5_VS_F* Data_Composition$`DM_%`[Pos_5F]/100)/100) # Kg VS/ Kg Faeces
# Theoretical methane potential (units NL CH4/Kg VS)
VFA_Bt <- 373 
Eth_Bt <- 730 
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
#Lipids (Crude Lipids-->CL)
Pos_5Fat<-which(Fat_Faeces$Section == 5)
S5_Cfat_F_time<-as.Date(Fat_Faeces$Date[Pos_5Fat])
S5_CL_F<-Fat_Faeces$`FAT_%`[Pos_5Fat] #% Fat/DM -->  in Kg Fat / 100 Kg DM
S5_Cfat_F_g_Kg<-((S5_CL_F*Fibers_Faeces$`DM_%`[Pos_5Fibers]/100)/100)*1000 #g Fat/ Kg faeces
S5_Cfat_F_COD<-S5_Cfat_F_g_Kg/COD_conv[5] #g COD / Kg Faeces
df_Fat_F<-data.frame(time = S5_Cfat_F_time,Cfat_F = S5_Cfat_F_COD)
S5_CL_PctVS<-(((S5_CL_F[1:2]*10)*(100/S5_VS_F[1:2]))/1000)*100 #% CL/ VS
S5_CL_PctVS[3:4]<-(((S5_CL_F[3:4]*10)*(100/S5_VS_F[3:4]))/1000)*100 #% CL/ VS
S5_CL_PctVS[5:6]<-(((S5_CL_F[5:6]*10)*(100/S5_VS_F[5:6]))/1000)*100 #% CL/ VS
S5_CL_PctVS[7:8]<-(((S5_CL_F[7:8]*10)*(100/S5_VS_F[5:6]))/1000)*100 #% CL/ VS
S5_CL_PctVS[9:10]<-(((S5_CL_F[9:10]*10)*(100/S5_VS_F[5:6]))/1000)*100 #% CL/ VS
# Carbohydrates (remaining percentage)
S5_CH_PctVS <- 100 - S5_Cel_PctVS-S5_HCel_PctVS-S5_ADL_PctVS-S5_CP_PctVS-S5_VFA_PctVS-S5_CL_PctVS
# Theoretical BMP
S5_t_BMP <- S5_CH_PctVS/100*CH_Bt+S5_Cel_PctVS/100*Cel_Bt+S5_HCel_PctVS/100*HCel_Bt+
  S5_ADL_PctVS/100*Lignin_Bt+S5_CP_PctVS/100*CP_Bt+S5_VFA_PctVS/100*VFA_Bt+
  S5_CL_PctVS/100*CL_Bt #NL CH4 / Kg VS
# Measured BMP
Pos_5BMP<-which(BMP_data$Section==5 | BMP_data$Section==56)
S5_BMP<-BMP_data$Mean_BMP[Pos_5BMP] #L CH4/Kg VS
S5_BMP_time<-BMP_data$Date[Pos_5BMP]
## Ratio degradable, non degradable VS (same proportion as BMP_t,BMP)
Ratio_BMP<-S5_BMP[1:2]/S5_t_BMP[1:2]
Ratio_BMP[3:5]<-S5_BMP[3:5]/S5_t_BMP[3]
Ratio_BMP[6:8]<-S5_BMP[6:8]/S5_t_BMP[c(5,7,9)]

# VS degradable non degradable
S5_VSd_A_g_Kg_F<-S5_VS_Kg_Kg_F[1:2]*1000*Ratio_BMP[1:2] #g/Kg faeces
S5_VSd_A_g_Kg_F[3:5]<-S5_VS_Kg_Kg_F[3]*1000*Ratio_BMP[3:5]
S5_VSd_A_g_Kg_F[6:8]<-S5_VS_Kg_Kg_F[5]*1000*Ratio_BMP[6:8]
S5_VSnd_A_g_Kg_F<-S5_VS_Kg_Kg_F[1:2]*1000*(1-Ratio_BMP[1:2]) #g/Kg faeces
S5_VSnd_A_g_Kg_F[3:5]<-S5_VS_Kg_Kg_F[3]*1000*(1-Ratio_BMP[3:5])
S5_VSnd_A_g_Kg_F[6:8]<-S5_VS_Kg_Kg_F[5]*1000*(1-Ratio_BMP[6:8])
df_VS_F<-data.frame(time = as.Date(S5_BMP_time), VSd_A_F = S5_VSd_A_g_Kg_F,
                    VSnd_A_F = S5_VSnd_A_g_Kg_F)

## ---- Feed ----
Feed_data$Date<-c(as.Date("2022-05-17"),as.Date("2022-06-14"),as.Date("2022-07-12"),
                  as.Date("2022-08-16"),as.Date("2022-09-27"),as.Date("2022-10-25"),
                  as.Date("2022-11-15"),as.Date("2022-12-06"),as.Date("2023-01-03"),
                  as.Date("2022-12-13"),as.Date("2023-01-17"),as.Date("2023-01-10"))
Feed_data$Section<-c(56,56,56,56,56,56,6,5,5,6,5,6)
Pos_5<-which(Feed_data$Section==56 | Feed_data$Section==5)
S5_TS_feed<-Feed_data$`TS (%)`[Pos_5] # %DM
S5_time_feed<-Feed_data$Date[Pos_5] 
### ---- Total feed 
Total_feed_51<-208317.3
Total_feed_52<-202279.1
Total_feed_53<-201520
### ---- Starch
S5_Starch<-Feed_data$`Starch_pct in dry matter`[Pos_5] # %/DM
S5_Starch_Pct<-S5_TS_feed*S5_Starch/100 #% in total --> assume in Kg starch/100 Kg feed
S5_Starch_g_kg<- (S5_Starch_Pct/100)*1000 #g starch/Kg feed 
S5_Starch_COD <- S5_Starch_g_kg/COD_conv[4] #g COD/Kg
### ---- Fat
S5_Cfat<-Feed_data$`Fat_pct in dry matter`[Pos_5] # %/DM
S5_Cfat_Pct<-S5_TS_feed*S5_Cfat/100 #% in total --> assume in Kg fat/100 Kg feed
S5_Cfat_g_kg<- (S5_Cfat_Pct/100)*1000 #g fat/Kg slurry 
S5_Cfat_COD <- S5_Cfat_g_kg/COD_conv[5] #g COD/Kg
### ---- TN
S5_TN_feed<-Feed_data$`N_pct in dry matter`[Pos_5] # %/DM
S5_TN_feed_Pct<-S5_TS_feed*S5_TN_feed/100 #% in total --> assume in Kg N/100 Kg feed
S5_TN_feed_g_kg<- (S5_TN_feed_Pct/100)*1000 #g N /Kg feed 
### ---- CP
S5_CP_f<-(S5_TN_feed_g_kg-0)*6.25-0 #g CP/Kg feed --> Assuming feed 0 Urea and 0 TAN
S5_CP_COD_f<-S5_CP_f/COD_conv[6] # g COD/Kg Faeces
### ---- NDF 
S5_NDF_feed<-Feed_data$`ANDF_pct in dry matter`[Pos_5] # %/DM
S5_NDF_feed_Pct<-S5_TS_feed*S5_NDF_feed/100 #% in total 
S5_NDF_feed_g_kg<- (S5_NDF_feed_Pct/100)*1000 #g NDF /Kg feed 
S5_NDF_COD <- S5_NDF_feed_g_kg/COD_conv[3] #g COD/Kg
## ---- Ash
S5_Ash_feed<-Feed_data$`Ash_pct in dry matter`[Pos_5] # %/DM
S5_Ash_feed_Pct<-S5_TS_feed*S5_Ash_feed/100 #% in total --> assume in Kg Ash/ 100 Kg feed
S5_Ash_feed_g_kg<- (S5_Ash_feed_Pct/100)*1000 #g Ash /Kg feed 
### ---- VS
S5_VS_feed<-100-S5_Ash_feed 
S5_VS_feed_Pct<-S5_TS_feed*S5_VS_feed/100 #% in total --> assume in Kg VS/ 100 Kg feed
S5_VS_feed_g_kg<- (S5_VS_feed_Pct/100)*1000 #g VS /Kg feed 

## ---- Create data frames ----
df_Urine<-data.frame(time = as.Date(S5_Composition_timeU),pH_U = S5_pH, SO4_U = S5_SO4_S,
                     Urea_U = S5_Urea_N, VFA_U = S5_VFA_COD_U, TAN_U = S5_TAN_U, 
                     Ash_U = S5_Ash_g_Kg_U, VSd_A_U = S5_VSd_A_U, VSnd_A_U = S5_VSnd_A_U) 
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
# Defaults
Sulfide<-rep(man_pars2.0$conc_fresh$sulfide,length(S5_Composition_timeU))
xa_dead<-rep(man_pars2.0$conc_fresh$xa_dead,length(S5_Composition_timeU))
RFd<-rep(29.1,length(S5_Composition_timeU)) ## From man_pars2.0 instead of 29.1 (Avoid negatives)
VSd<-rep(0,length(S5_Composition_timeU))
dens<-rep(man_pars2.0$dens,length(S5_Composition_timeU))
df_Constants<-data.frame(time = as.Date(S5_Composition_timeU),Sulfide_K = Sulfide, xa_dead_K = xa_dead,
                         RFd_K = RFd, VSd_K = VSd,dens_K = dens)
df_feed<-data.frame(time = as.Date(S5_time_feed),Starch_f = S5_Starch_COD, fat_f = S5_Cfat_COD,
                    TN_f = S5_TN_feed_g_kg, NDF_f = S5_NDF_COD, Ash_f = S5_Ash_feed_g_kg,
                    CP_f = S5_CP_COD_f,VS_f = S5_VS_feed_g_kg)
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

## ---- Create a list-data frame as ABM input (man_pars) ----
### ---- Danish normative system  ----
#Assumptions: 
# 1/ 17% of feed DM is excreeted as faeces DM
# 2/ New slurry is form of urine, faeces
# 3/ The remaing part of 2% of feed and 17 of feed DM is urine.
# 4/ New slurry mass is calculated as the average slurry mass in a period right after they flush to right before
# 5/ Urine, faeces and manure have a density of 1 Kg/L

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

S5_Total_Faeces<-c(rep(Total_Fresh_Slurry_51*Faeces_pct,length(which(df_Composition_S5$Batch==1))),
                   rep(Total_Fresh_Slurry_52*Faeces_pct,length(which(df_Composition_S5$Batch==2))),
                   rep(Total_Fresh_Slurry_53*Faeces_pct,length(which(df_Composition_S5$Batch==3))))
S5_Total_feed<-c(rep(Total_feed_51,length(which(df_Composition_S5$Batch==1))),
                 rep(Total_feed_52,length(which(df_Composition_S5$Batch==2))),
                 rep(Total_feed_53,length(which(df_Composition_S5$Batch==3))))

vs<-unique(S5_VS_Kg_Kg_F)
fs<-unique(S5_Total_Faeces)
fd<-unique(S5_Total_feed)
vsf<-unique(df_Composition_S5$VS_f)
vs[4:5]<-vs[3]
fs[4:5]<-fs[3]
fd[4:5]<-fd[3]

S5_Lig_A_Total_F<-(unique(S5_ADL_F)/100)*((vs)*1000)*(fs) # g VSt in Feaces
S5_VSt_A_F<-(1-unique(S5_ADL_F)/100)*((vs)*1000) # g VSt / kg feces
S5_Lig_A_feed<-S5_Lig_A_Total_F/(fd) #g Lig/Kg feed
S5_VSt_A_feed<-0
S5_VSt_A_feed[1:5]<-(df_Composition_S5$VS_f[1:5])-S5_Lig_A_feed[1] #gVSt / Kg feed
S5_VSt_A_feed[6:12]<-(df_Composition_S5$VS_f[6:12])-S5_Lig_A_feed[2] #gVSt / Kg feed
S5_VSt_A_feed[13:15]<-(df_Composition_S5$VS_f[13:15])-S5_Lig_A_feed[3] #gVSt / Kg feed
S5_VSt_A_feed[16]<-(df_Composition_S5$VS_f[16])-S5_Lig_A_feed[4] #gVSt / Kg feed
S5_VSt_A_feed[17:19]<-(df_Composition_S5$VS_f[17:19])-S5_Lig_A_feed[5] #gVSt / Kg feed


S5_VSnd_A_Total_F<-df_Composition_S5$VSnd_A_F*S5_Total_Faeces # g VSnd
S5_VSnd_A_feed<-S5_VSnd_A_Total_F/S5_Total_feed #g VSnd/Kg feed
S5_VSd_A_feed<-df_Composition_S5$VS_f-S5_VSnd_A_feed #gVSd / Kg feed
df_Composition_S5$VSd_A_f<-S5_VSd_A_feed
df_Composition_S5$VSnd_A_f<-S5_VSnd_A_feed

Final_S5_time<-df_Composition_S5$time
Final_S5_time_days<-as.numeric(diff(Final_S5_time)) #days
Final_S5_time_cum<-c(0,cumsum(Final_S5_time_days))
Final_S5_Sulfide<-df_Composition_S5$Sulfide_K #Default
Final_S5_Urea<-(Fresh_slurry*Faeces_pct_2.0*0+
                  Fresh_slurry*Urine_pct_2.0*df_Composition_S5$Urea_U[1:19]+
                  Fresh_slurry*Feed_pct_2.0*0)/(Fresh_slurry) #g N/Kg new slurry
Final_S5_SO4<-(Fresh_slurry*Faeces_pct_2.0*0+
                 Fresh_slurry*Urine_pct_2.0*df_Composition_S5$SO4_U[1:19]+
                 Fresh_slurry*Feed_pct_2.0*0)/(Fresh_slurry) #g S/Kg new slurry
Final_S5_TAN<-(Fresh_slurry*Faeces_pct_2.0*df_Composition_S5$TAN_F[1:19]+
                 Fresh_slurry*Urine_pct_2.0*df_Composition_S5$TAN_U[1:19]+
                 Fresh_slurry*Feed_pct_2.0*0)/(Fresh_slurry) #g N/Kg new slurry /// Assumptions: Rest of TN from Urine is TAN (1-0.75))
Final_S5_Starch<-(Fresh_slurry*Faeces_pct_2.0*0+
                    Fresh_slurry*Urine_pct_2.0*0+
                    Fresh_slurry*Feed_pct_2.0*df_Composition_S5$Starch_f[1:19])/(Fresh_slurry) #g COD/Kg
Final_S5_VFA<-(Fresh_slurry*Faeces_pct_2.0*df_Composition_S5$VFA_F[1:19]+
                 Fresh_slurry*Urine_pct_2.0*df_Composition_S5$VFA_U[1:19]+
                 Fresh_slurry*Feed_pct_2.0*0)/(Fresh_slurry) #g COD/Kg new slurry
Final_S5_xa_dead <- df_Composition_S5$xa_dead_K #Default
Final_S5_Cfat<-(Fresh_slurry*Faeces_pct_2.0*df_Composition_S5$Cfat_F[1:19]+
                  Fresh_slurry*Urine_pct_2.0*0+
                  Fresh_slurry*Feed_pct_2.0*df_Composition_S5$fat_f[1:19])/(Fresh_slurry) #g COD/Kg new slurry
Final_S5_CP<-(Fresh_slurry*Faeces_pct_2.0*df_Composition_S5$CP_F[1:19]+
                Fresh_slurry*Urine_pct_2.0*0+
                Fresh_slurry*Feed_pct_2.0*df_Composition_S5$CP_f[1:19])/(Fresh_slurry) # g COD/Kg new slurry  /// Assumption: No Urea in faeces, no urea or TAN in feed 
Final_S5_RFd <- df_Composition_S5$RFd_K #Default (From Frederik email)
Final_S5_NDF<-(Fresh_slurry*Faeces_pct_2.0*df_Composition_S5$NDF_F[1:19]+
                 Fresh_slurry*Urine_pct_2.0*0+
                 Fresh_slurry*Feed_pct_2.0*df_Composition_S5$NDF_f[1:19])/(Fresh_slurry) # g COD/Kg new slurry 
Final_S5_iNDF <- Final_S5_NDF-Final_S5_RFd #g COD/Kg new slurry
Final_S5_VSd <- df_Composition_S5$VSd_K #Default
Final_S5_VSd_A<-(Fresh_slurry*Faeces_pct_2.0*df_Composition_S5$VSd_A_F[1:19] +
                   Fresh_slurry*Urine_pct_2.0*df_Composition_S5$VSd_A_U[1:19]+
                   Fresh_slurry*Feed_pct_2.0*df_Composition_S5$VSd_A_f[1:19])/(Fresh_slurry) # g VS/Kg new slurry /// To Ask: fraction of degradable in urine and feed
Final_S5_VSnd_A<-(Fresh_slurry*Faeces_pct_2.0*df_Composition_S5$VSnd_A_F[1:19] +
                    Fresh_slurry*Urine_pct_2.0*df_Composition_S5$VSnd_A_U[1:19]+
                    Fresh_slurry*Feed_pct_2.0*df_Composition_S5$VSnd_A_f[1:19])/(Fresh_slurry) # g VS/Kg new slurry  /// To Ask: fraction of non degradable in urine and feed
Final_S5_ash<-(Fresh_slurry*Faeces_pct_2.0*df_Composition_S5$Ash_F[1:19] +
                 Fresh_slurry*Urine_pct_2.0*df_Composition_S5$Ash_U[1:19]+
                 Fresh_slurry*Feed_pct_2.0*df_Composition_S5$Ash_f[1:19])/(Fresh_slurry) # g ash/Kg new slurry  
Final_S5_pH<-mean(df_Composition_S5$pH_U) # Only considering from urine (no pH measured for faeces) /// Ask Frederik: how to make it change over time
Final_S5_dens <- mean(df_Composition_S5$dens_K) #Default

a1<-rep(S5_VSt_A_F[1],5)
a2<-rep(S5_VSt_A_F[2],7)
a3<-rep(mean(S5_VSt_A_F[3:5]),7)
S5_VSt_A_F<-c(a1,a2,a3)
Final_S5_VSt_A<-(Fresh_slurry*Faeces_pct_2.0*S5_VSt_A_F +
                   Fresh_slurry*Urine_pct_2.0*df_Composition_S5$VSd_A_U[1:19]+
                   Fresh_slurry*Feed_pct_2.0*S5_VSt_A_feed)/(Fresh_slurry) # g VS/Kg new slurry /// To Ask: fraction of degradable in urine and feed

df_man_pars_S5_2.0 <- list(conc_fresh = data.frame(time = Final_S5_time_cum, sulfide = Final_S5_Sulfide, urea = Final_S5_Urea, 
                                                   sulfate = Final_S5_SO4, TAN = Final_S5_TAN, starch = Final_S5_Starch, 
                                                   VFA = Final_S5_VFA, xa_dead = Final_S5_xa_dead, Cfat = Final_S5_Cfat, 
                                                   CP = Final_S5_CP, RFd = Final_S5_RFd, iNDF = Final_S5_iNDF, VSd = Final_S5_VSd, 
                                                   VSd_A = Final_S5_VSd_A, VSnd_A = Final_S5_VSnd_A, ash = Final_S5_ash), 
                           pH = df_S5_pH, dens = Final_S5_dens)
### ---- Alternative model  ----
# Assumptions:
# 1/ Faeces are predicted as: Faeces = −0.523 + 0.00515 cDF + 0.297 DM intake 
# DOI:10.1016/j.anifeedsci.2008.10.008
cDF_51<-(mean(S5_NDF_feed[1:3])/100)*1000 #g NDF/Kg feed DM ##Provisionaly mean, if good use the interpolated values
cDF_52<-(mean(S5_NDF_feed[4:6])/100)*1000 #g NDF/Kg feed DM
cDF_53<-(mean(S5_NDF_feed[7:9])/100)*1000 #g NDF/Kg feed DM
DM_Intake_51<-S5_feed_rate[1:78]*(mean(S5_TS_feed[1:3])/100) #Kg DM feed/day  ## Only testing-->more precise with the real DM rather than mean of the whole batch
DM_Intake_52<-S5_feed_rate[79:176]*(mean(S5_TS_feed[4:6])/100) #Kg DM feed/day  ## Only testing-->more precise with the real DM rather than mean of the whole batch
DM_Intake_53<-S5_feed_rate[177:257]*(mean(S5_TS_feed[7:9])/100) #Kg DM feed/day  ## Only testing-->more precise with the real DM rather than mean of the whole batch
Faeces_daily_51 = -0.105 + 0.00111*cDF_51 + 0.118*DM_Intake_51
Faeces_daily_52 = -0.105 + 0.00111*cDF_52 + 0.118*DM_Intake_52
Faeces_daily_53 = -0.105 + 0.00111*cDF_53 + 0.118*DM_Intake_53

FaecesDM_51<-sum(Faeces_daily_51)
FaecesDM_52<-sum(Faeces_daily_52)
FaecesDM_53<-sum(Faeces_daily_53)

Faeces_51<-FaecesDM_51*(100/S5_DM_F[1])
Faeces_52<-FaecesDM_52*(100/S5_DM_F[3])
Faeces_53<-FaecesDM_53*(100/S5_DM_F[5])

Faeces_pct51<-Faeces_51/Total_Fresh_Slurry_51
Faeces_pct52<-Faeces_52/Total_Fresh_Slurry_52
Faeces_pct53<-Faeces_53/Total_Fresh_Slurry_53

Feed_pct51<-Feed_pct*Total_feed_51/Total_Fresh_Slurry_51
Feed_pct52<-Feed_pct*Total_feed_52/Total_Fresh_Slurry_52
Feed_pct53<-Feed_pct*Total_feed_53/Total_Fresh_Slurry_53

Urine_pct51<- 1-Faeces_pct51-Feed_pct51
Urine_pct52<- 1-Faeces_pct52-Feed_pct52
Urine_pct53<- 1-Faeces_pct53-Feed_pct53

Faeces_pct_3.0<-c(rep(Faeces_pct51,length(which(df_Composition_S5$Batch==1))),
                  rep(Faeces_pct52,length(which(df_Composition_S5$Batch==2))),
                  rep(Faeces_pct53,length(which(df_Composition_S5$Batch==3))))
Urine_pct_3.0<-c(rep(Urine_pct51,length(which(df_Composition_S5$Batch==1))),
                 rep(Urine_pct52,length(which(df_Composition_S5$Batch==2))),
                 rep(Urine_pct53,length(which(df_Composition_S5$Batch==3))))
Feed_pct_3.0<-c(rep(Feed_pct51,length(which(df_Composition_S5$Batch==1))),
                rep(Feed_pct52,length(which(df_Composition_S5$Batch==2))),
                rep(Feed_pct53,length(which(df_Composition_S5$Batch==3))))
Fresh_slurry<-c(rep(Total_Fresh_Slurry_51,length(which(df_Composition_S5$Batch==1))),
                rep(Total_Fresh_Slurry_52,length(which(df_Composition_S5$Batch==2))),
                rep(Total_Fresh_Slurry_53,length(which(df_Composition_S5$Batch==3))))

Final_S5_time<-df_Composition_S5$time
Final_S5_time_days<-as.numeric(diff(Final_S5_time)) #days
Final_S5_time_cum<-c(0,cumsum(Final_S5_time_days))

Final_S5_Sulfide<-df_Composition_S5$Sulfide_K #Default
Final_S5_Urea<-(Fresh_slurry*Faeces_pct_3.0*0+
                  Fresh_slurry*Urine_pct_3.0*df_Composition_S5$Urea_U+
                  Fresh_slurry*Feed_pct_3.0*0)/(Fresh_slurry) #g N/Kg new slurry
Final_S5_SO4<-(Fresh_slurry*Faeces_pct_3.0*0+
                 Fresh_slurry*Urine_pct_3.0*df_Composition_S5$SO4_U+
                 Fresh_slurry*Feed_pct_3.0*0)/(Fresh_slurry) #g S/Kg new slurry
Final_S5_TAN<-(Fresh_slurry*Faeces_pct_3.0*df_Composition_S5$TAN_F+
                 Fresh_slurry*Urine_pct_3.0*df_Composition_S5$TAN_U+
                 Fresh_slurry*Feed_pct_3.0*0)/(Fresh_slurry) #g N/Kg new slurry /// Assumptions: Rest of TN from Urine is TAN (1-0.75))
Final_S5_Starch<-(Fresh_slurry*Faeces_pct_3.0*0+
                    Fresh_slurry*Urine_pct_3.0*0+
                    Fresh_slurry*Feed_pct_3.0*df_Composition_S5$Starch_f)/(Fresh_slurry) #g COD/Kg
Final_S5_VFA<-(Fresh_slurry*Faeces_pct_3.0*df_Composition_S5$VFA_F+
                 Fresh_slurry*Urine_pct_3.0*df_Composition_S5$VFA_U+
                 Fresh_slurry*Feed_pct_3.0*0)/(Fresh_slurry) #g COD/Kg new slurry
Final_S5_xa_dead <- df_Composition_S5$xa_dead_K #Default
Final_S5_Cfat<-(Fresh_slurry*Faeces_pct_3.0*df_Composition_S5$Cfat_F+
                  Fresh_slurry*Urine_pct_3.0*0+
                  Fresh_slurry*Feed_pct_3.0*df_Composition_S5$fat_f)/(Fresh_slurry) #g COD/Kg new slurry
Final_S5_CP<-(Fresh_slurry*Faeces_pct_3.0*df_Composition_S5$CP_F+
                Fresh_slurry*Urine_pct_3.0*0+
                Fresh_slurry*Feed_pct_3.0*df_Composition_S5$CP_f)/(Fresh_slurry) # g COD/Kg new slurry  /// Assumption: No Urea in faeces, no urea or TAN in feed 
Final_S5_RFd <- df_Composition_S5$RFd_K #Default (From Frederik email)
Final_S5_NDF<-(Fresh_slurry*Faeces_pct_3.0*df_Composition_S5$NDF_F+
                 Fresh_slurry*Urine_pct_3.0*0+
                 Fresh_slurry*Feed_pct_3.0*df_Composition_S5$NDF_f)/(Fresh_slurry) # g COD/Kg new slurry 
Final_S5_iNDF <- Final_S5_NDF-Final_S5_RFd #g COD/Kg new slurry
Final_S5_VSd <- df_Composition_S5$VSd_K #Default
Final_S5_VSd_A<-(Fresh_slurry*Faeces_pct_3.0*df_Composition_S5$VSd_A_F +
                   Fresh_slurry*Urine_pct_3.0*df_Composition_S5$VSd_A_U+
                   Fresh_slurry*Feed_pct_3.0*df_Composition_S5$VSd_A_f)/(Fresh_slurry) # g VS/Kg new slurry /// To Ask: fraction of degradable in urine and feed
Final_S5_VSnd_A<-(Fresh_slurry*Faeces_pct_3.0*df_Composition_S5$VSnd_A_F +
                    Fresh_slurry*Urine_pct_3.0*df_Composition_S5$VSnd_A_U+
                    Fresh_slurry*Feed_pct_3.0*df_Composition_S5$VSnd_A_f)/(Fresh_slurry) # g VS/Kg new slurry  /// To Ask: fraction of non degradable in urine and feed
Final_S5_ash<-(Fresh_slurry*Faeces_pct_3.0*df_Composition_S5$Ash_F +
                 Fresh_slurry*Urine_pct_3.0*df_Composition_S5$Ash_U+
                 Fresh_slurry*Feed_pct_3.0*df_Composition_S5$Ash_f)/(Fresh_slurry) # g ash/Kg new slurry  
Final_S5_pH<-mean(df_Composition_S5$pH_U) # Only considering from urine (no pH measured for faeces) /// Ask Frederik: how to make it change over time
Final_S5_dens <- mean(df_Composition_S5$dens_K) #Default

df_man_pars_S5_3.0 <- list(conc_fresh = data.frame(time = Final_S5_time_cum, sulfide = Final_S5_Sulfide, urea = Final_S5_Urea, 
                                                   sulfate = Final_S5_SO4, TAN = Final_S5_TAN, starch = Final_S5_Starch, 
                                                   VFA = Final_S5_VFA, xa_dead = Final_S5_xa_dead, Cfat = Final_S5_Cfat, 
                                                   CP = Final_S5_CP, RFd = Final_S5_RFd, iNDF = Final_S5_iNDF, VSd = Final_S5_VSd, 
                                                   VSd_A = Final_S5_VSd_A, VSnd_A = Final_S5_VSnd_A, ash = Final_S5_ash), 
                           pH = df_S5_pH, dens = Final_S5_dens)

# ----- Section 2 -----
## ----- Slurry -----
###  pH (Values at the start and finish are assume to be the same as next/previous)
Pos_6S<-which(Data_Slurry$Section==6)
S6_pH_S<-Data_Slurry$pH[Pos_6S]
S6_pH_S_Time<-Data_Slurry$Date[Pos_6S]
S6_pH_S_days<-as.numeric(diff(S6_pH_S_Time)) #days
S6_pH_S_cum<-c(0,cumsum(S6_pH_S_days))
df_S6_pH<-data.frame(time=S6_pH_S_cum,pH = S6_pH_S )

## ----- Urine -----
Pos_6U<-which(Data_Composition$Section==6 & Data_Composition$Type == "Urine")
S6_Composition_timeU<-Data_Composition$Time[Pos_6U]

### pH
S6_pH<-Data_Composition$pH[Pos_6U]
###  SO4
Ratio_S_SO4<-32.06/96.06
S6_SO4_mgL<-Data_Composition$`SO4_mg/L`[Pos_6U] #mg SO4/L urine
S6_SO4<-S6_SO4_mgL/Urine_dens/1000 #g SO4/Kg urine
S6_SO4_S<-S6_SO4*Ratio_S_SO4 #g S/Kg urine
###  Urea
S6_TN_U<-Data_Composition$`TN_g/Kg`[Pos_6U] #g Urine-TN/Kg urine
S6_Urea_N<-S6_TN_U*0.75 #g N/Kg urine--> Assuming Urea N is equal to urine N * 0.75 
### VFA
S6_VFA_mgL_U<-Data_Composition$`VFA_mg/L`[Pos_6U] #mg VFA/L Urine
S6_VFA_U<-S6_VFA_mgL_U/Urine_dens/1000 #g VFA/Kg Urine
S6_VFA_COD_U<-S6_VFA_U/COD_conv[7] #g COD/Kg Urine
### TAN
S6_TAN_U<-S6_TN_U*(1-0.75) #g Rest-N/Kg Urine --> Fine to assume everything that is not urea is CP or should be TAN??? 
### Ash
S6_VS_U<-Data_Composition$`VS_%`[Pos_6U] #%-VS/DM 
S6_Ash_U<-100-S6_VS_U #%-Ash/DM 
S6_Ash_g_Kg_U <- ((S6_Ash_U* Data_Composition$`DM_%`[Pos_6U]/100)/100)*1000 # g Ash/ Kg Urine
### VS (Assume everything is degradable)
S6_VS_Pct_U<-(S6_VS_U* Data_Composition$`DM_%`[Pos_6U]/100) #%-VS/ Urine--> Kg VS/100Kg Urine
S6_VS_g_Kg_U <-(S6_VS_Pct_U/100)*1000 # g VS/ Kg Urine
Ratio_Urea_Nitrogen<-60.06/(14*2)
S6_Urea_g_Kg<-S6_Urea_N*Ratio_Urea_Nitrogen #g Urea/Kg Urine
S6_VSd_A_U<-S6_VS_g_Kg_U-S6_Urea_g_Kg ## Assume that all VS are degradable after Urea substraction
S6_VSnd_A_U<-S6_VSd_A_U-S6_VSd_A_U ## No VSnd in Urine

## ----- Faeces -----
Pos_6F<-which(Data_Composition$Section==6 & Data_Composition$Type == "Faeces")

### TAN 
Ratio_N_NH4 <- 14/18
S6_TAN<-Data_Composition$`NH4-N_g/Kg`[Pos_6F] #g NH4-N/Kg Faeces
S6_TAN_F <-S6_TAN/Ratio_N_NH4 #g N/Kg Faeces
### VFA
S6_VFA_mgL_F<-Data_Composition$`VFA_mg/L`[Pos_6F] #mg VFA/L Faeces
S6_VFA_F<-S6_VFA_mgL_F/Faeces_dens/1000 #g VFA/Kg Faeces
S6_VFA_COD_F<-S6_VFA_F/COD_conv[7] #g COD/Kg Faeces
### CP 
S6_CP<-(Data_Composition$`TN_g/Kg`[Pos_6F]-S6_TAN)*6.25-0 #g CP/Kg Faeces --> Assuming faeces 0 Urea
S6_CP_COD<-S6_CP/COD_conv[6] # g COD/Kg Faeces
### NDF 
RFd<-man_pars2.0$conc_fresh$RFd #gCOD/kg 
Pos_6Fibers<-which(Fibers_Faeces$Section == 6)
S6_NDF_F_time<-as.Date(Fibers_Faeces$Date[Pos_6Fibers])
S6_NDF_F<-Fibers_Faeces$`NDF_%`[Pos_6Fibers] #% NDF/DM -->  in Kg NDF / 100 Kg DM
S6_NDF_F_g_Kg<-((S6_NDF_F*Fibers_Faeces$`DM_%`[Pos_6Fibers]/100)/100)*1000 #g NDF/ Kg faeces
S6_NDF_F_COD<-S6_NDF_F_g_Kg/COD_conv[3] #g COD / Kg Faeces
df_Fibers_F_S6<-data.frame(time = S6_NDF_F_time,NDF_F = S6_NDF_F_COD)
### Ash
S6_VS_F<-Data_Composition$`VS_%`[Pos_6F] #%-VS/DM 
S6_Ash_F<-100-S6_VS_F #%-Ash/DM 
S6_Ash_g_Kg_F <- ((S6_Ash_F* Data_Composition$`VS_%`[Pos_6F]/100)/100)*1000 # g Ash/ Kg Faeces
### ----- VS -----
S6_VS_F<-Data_Composition$`VS_%`[Pos_6F] #%-VS/DM 
S6_VS_Pct_F<-(S6_VS_F* Data_Composition$`DM_%`[Pos_6F]/100) #%-VS/ Faeces--> Kg VS/100Kg Faeces
S6_VS_Kg_Kg_F <-((S6_VS_F* Data_Composition$`DM_%`[Pos_6F]/100)/100) # Kg VS/ Kg Faeces
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
#Lignin
S6_ADL_F<-Fibers_Faeces$`ADL_%`[Pos_6Fibers] #% ADL/DM -->  in Kg ADL / 100 Kg DM
S6_ADL_PctVS<-S6_ADL_F[1:2]*(100/S6_VS_F[1:2]) #% ADL/VS
S6_ADL_PctVS[3:4]<-S6_ADL_F[3:4]*(100/S6_VS_F[3:4]) #% ADL/VS
S6_ADL_PctVS[5:10]<-S6_ADL_F[5:10]*(100/S6_VS_F[5]) #% ADL/VS
#Hemicellulose
S6_ADF_F<-Fibers_Faeces$`ADF_%`[Pos_6Fibers] #% ADL/DM -->  in Kg ADF / 100 Kg DM
S6_ADF_PctVS<-S6_ADF_F[1:2]*(100/S6_VS_F[1:2]) #% ADF/VS
S6_ADF_PctVS[3:4]<-S6_ADF_F[3:4]*(100/S6_VS_F[3:4]) #% ADF/VS
S6_ADF_PctVS[5:10]<-S6_ADF_F[5:10]*(100/S6_VS_F[5]) #% ADF/VS
S6_NDF_PctVS<-S6_NDF_F[1:2]*(100/S6_VS_F[1:2]) #% NDF/VS
S6_NDF_PctVS[3:4]<-S6_NDF_F[3:4]*(100/S6_VS_F[3:4]) #% NDF/VS
S6_NDF_PctVS[5:10]<-S6_NDF_F[5:10]*(100/S6_VS_F[5]) #% NDF/VS
S6_HCel_PctVS <- S6_NDF_PctVS - S6_ADF_PctVS #% Hemicellulose/VS
#Cellulose
S6_Cel_PctVS <- S6_ADF_PctVS - S6_ADL_PctVS
#Lipids
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
# Carbohydrates
S6_CH_PctVS <- 100 - S6_Cel_PctVS-S6_HCel_PctVS-S6_ADL_PctVS-S6_CP_PctVS-S6_VFA_PctVS-S6_CL_PctVS
##Theoretical BMP
S6_t_BMP <- S6_CH_PctVS/100*CH_Bt+S6_Cel_PctVS/100*Cel_Bt+S6_HCel_PctVS/100*HCel_Bt+
  S6_ADL_PctVS/100*Lignin_Bt+S6_CP_PctVS/100*CP_Bt+S6_VFA_PctVS/100*VFA_Bt+
  S6_CL_PctVS/100*CL_Bt #NL CH4 / Kg VS
##Measured BMP
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

## ----- Feed -----
S6_TS_feed<-Feed_data$`TS (%)`[Pos_6] # %DM
S6_time_feed<-Feed_data$Date[Pos_6] 

### Starch
S6_Starch<-Feed_data$`Starch_pct in dry matter`[Pos_6] # %/DM
S6_Starch_Pct<-S6_TS_feed*S6_Starch/100 #% in total --> assume in Kg starch/100 Kg feed
S6_Starch_g_kg<- (S6_Starch_Pct/100)*1000 #g starch/Kg feed 
S6_Starch_COD <- S6_Starch_g_kg/COD_conv[4] #g COD/Kg
###  Fat
S6_Cfat<-Feed_data$`Fat_pct in dry matter`[Pos_6] # %/DM
S6_Cfat_Pct<-S6_TS_feed*S6_Cfat/100 #% in total --> assume in Kg fat/100 Kg feed
S6_Cfat_g_kg<- (S6_Cfat_Pct/100)*1000 #g fat/Kg feed 
S6_Cfat_COD <- S6_Cfat_g_kg/COD_conv[5] #g COD/Kg feed
### TN
S6_TN_feed<-Feed_data$`N_pct in dry matter`[Pos_6] # %/DM
S6_TN_feed_Pct<-S6_TS_feed*S6_TN_feed/100 #% in total --> assume in Kg N/100 Kg feed
S6_TN_feed_g_kg<- (S6_TN_feed_Pct/100)*1000 #g N /Kg feed 
### CP
S6_CP_f<-(S6_TN_feed_g_kg-0)*6.25-0 #g CP/Kg feed --> Assuming feed 0 Urea and 0 TAN
S6_CP_COD_f<-S6_CP_f/COD_conv[6] # g COD/Kg Faeces
### NDF 
S6_NDF_feed<-Feed_data$`ANDF_pct in dry matter`[Pos_6] # %/DM
S6_NDF_feed_Pct<-S6_TS_feed*S6_NDF_feed/100 #% in total 
S6_NDF_feed_g_kg<- (S6_NDF_feed_Pct/100)*1000 #g NDF /Kg feed 
S6_NDF_COD <- S6_NDF_feed_g_kg/COD_conv[3] #g COD/Kg
### Ash
S6_Ash_feed<-Feed_data$`Ash_pct in dry matter`[Pos_6] # %/DM
S6_Ash_feed_Pct<-S6_TS_feed*S6_Ash_feed/100 #% in total --> assume in Kg N/ 100 Kg feed
S6_Ash_feed_g_kg<- (S6_Ash_feed_Pct/100)*1000 #g Ash /Kg feed 
### VS
S6_VS_feed<-100-S6_Ash_feed 
S6_VS_feed_Pct<-S6_TS_feed*S6_VS_feed/100 #% in total --> assume in Kg VS/ 100 Kg feed
S6_VS_feed_g_kg<- (S6_VS_feed_Pct/100)*1000 #g VS /Kg feed 

## ---- Create data frames ----
df_Urine_S6<-data.frame(time = as.Date(S6_Composition_timeU),pH_U = S6_pH, SO4_U = S6_SO4_S,
                        Urea_U = S6_Urea_N, VFA_U = S6_VFA_COD_U, TAN_U = S6_TAN_U, 
                        Ash_U = S6_Ash_g_Kg_U, VSd_A_U = S6_VSd_A_U, VSnd_A_U = S6_VSnd_A_U) 

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

df_feed_S6<-data.frame(time = as.Date(S6_time_feed),Starch_f = S6_Starch_COD, fat_f = S6_Cfat_COD,
                       TN_f = S6_TN_feed_g_kg, NDF_f = S6_NDF_COD, Ash_f = S6_Ash_feed_g_kg,
                       CP_f = S6_CP_COD_f,VS_f = S6_VS_feed_g_kg)
Sulfide<-rep(man_pars2.0$conc_fresh$sulfide,length(S6_Composition_timeU))
xa_dead<-rep(man_pars2.0$conc_fresh$xa_dead,length(S6_Composition_timeU))
RFd<-rep(29.1,length(S6_Composition_timeU)) ## From man_pars2.0 instead of 29.1 (Avoid negatives)
VSd<-rep(0,length(S6_Composition_timeU))
dens<-rep(man_pars2.0$dens,length(S6_Composition_timeU))

df_Constants<-data.frame(time = as.Date(S6_Composition_timeU),Sulfide_K = Sulfide, xa_dead_K = xa_dead,
                         RFd_K = RFd, VSd_K = VSd,dens_K = dens)# Data frame for feed

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

vs<-unique(S6_VS_Kg_Kg_F)
fs<-unique(S6_Total_Faeces)
fd<-unique(S6_Total_feed)
vsf<-unique(df_Composition_S6$VS_f)
vs[4:5]<-vs[3]
fs[4:5]<-fs[3]
fd[4:5]<-fd[3]

S6_Lig_A_Total_F<-(unique(S6_ADL_F)/100)*((vs)*1000)*(fs) # g VSt in Feaces
S6_VSt_A_F<-(1-unique(S6_ADL_F)/100)*((vs)*1000) # g VSt / kg feces
S6_Lig_A_feed<-S6_Lig_A_Total_F/(fd) #g Lig/Kg feed
S6_VSt_A_feed<-0
S6_VSt_A_feed[1:5]<-(df_Composition_S6$VS_f[1:5])-S6_Lig_A_feed[1] #gVSt / Kg feed
S6_VSt_A_feed[6:12]<-(df_Composition_S6$VS_f[6:12])-S6_Lig_A_feed[2] #gVSt / Kg feed
S6_VSt_A_feed[13:15]<-(df_Composition_S6$VS_f[13:15])-S6_Lig_A_feed[3] #gVSt / Kg feed
S6_VSt_A_feed[16:17]<-(df_Composition_S6$VS_f[16:17])-S6_Lig_A_feed[4] #gVSt / Kg feed
S6_VSt_A_feed[18:19]<-(df_Composition_S6$VS_f[18:19])-S6_Lig_A_feed[5] #gVSt / Kg feed

## ---- Create a list-data frame as ABM input (man_pars) ----
### ---- Danish normative system  ----
# Assumptions:
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

a1<-rep(S6_VSt_A_F[1],5)
a2<-rep(S6_VSt_A_F[2],7)
a3<-rep(mean(S6_VSt_A_F[3:5]),7)
S6_VSt_A_F<-c(a1,a2,a3)
Final_S6_VSt_A<-(Fresh_slurry*Faeces_pct_2.0*S6_VSt_A_F +
                   Fresh_slurry*Urine_pct_2.0*df_Composition_S6$VSd_A_U+
                   Fresh_slurry*Feed_pct_2.0*S6_VSt_A_feed)/(Fresh_slurry) 

df_man_pars_S6_2.0 <- list(conc_fresh = data.frame(time = Final_S6_time_cum, sulfide = Final_S6_Sulfide, urea = Final_S6_Urea, 
                                                   sulfate = Final_S6_SO4, TAN = Final_S6_TAN, starch = Final_S6_Starch, 
                                                   VFA = Final_S6_VFA, xa_dead = Final_S6_xa_dead, Cfat = Final_S6_Cfat, 
                                                   CP = Final_S6_CP, RFd = Final_S6_RFd, iNDF = Final_S6_iNDF, VSd = Final_S6_VSd, 
                                                   VSd_A = Final_S6_VSd_A, VSnd_A = Final_S6_VSnd_A, ash = Final_S6_ash), 
                           pH = df_S6_pH, dens = Final_S6_dens)

### ---- Alternative model  ----
# Assumptions:
# 1/ Faeces are predicted as: Faeces = −0.523 + 0.00515 cDF + 0.297 DM intake 
# DOI:10.1016/j.anifeedsci.2008.10.008

cDF_61<-(mean(S6_NDF_feed[1:3])/100)*1000 #g NDF/Kg feed DM 
cDF_62<-(mean(S6_NDF_feed[4:6])/100)*1000 #g NDF/Kg feed DM
cDF_63<-(mean(S6_NDF_feed[7:9])/100)*1000 #g NDF/Kg feed DM
DM_Intake_61<-S6_feed_rate[1:78]*(mean(S6_TS_feed[1:3])/100) #Kg DM feed/day  
DM_Intake_62<-S6_feed_rate[79:176]*(mean(S6_TS_feed[4:6])/100) #Kg DM feed/day  
DM_Intake_63<-S6_feed_rate[177:257]*(mean(S6_TS_feed[7:9])/100) #Kg DM feed/day  
Faeces_daily_61 = -0.105 + 0.00111*cDF_61 + 0.118*DM_Intake_61
Faeces_daily_62 = -0.105 + 0.00111*cDF_62 + 0.118*DM_Intake_62
Faeces_daily_63 = -0.105 + 0.00111*cDF_63 + 0.118*DM_Intake_63

FaecesDM_61<-sum(Faeces_daily_61)
FaecesDM_62<-sum(Faeces_daily_62)
FaecesDM_63<-sum(Faeces_daily_63)

Faeces_61<-FaecesDM_61*(100/S6_DM_F[1])
Faeces_62<-FaecesDM_62*(100/S6_DM_F[3])
Faeces_63<-FaecesDM_63*(100/S6_DM_F[5])

Faeces_pct61<-Faeces_61/Total_Fresh_Slurry_61
Faeces_pct62<-Faeces_62/Total_Fresh_Slurry_62
Faeces_pct63<-Faeces_63/Total_Fresh_Slurry_63

Feed_pct61<-Feed_pct*Total_feed_61/Total_Fresh_Slurry_61
Feed_pct62<-Feed_pct*Total_feed_62/Total_Fresh_Slurry_62
Feed_pct63<-Feed_pct*Total_feed_63/Total_Fresh_Slurry_63

Urine_pct61<- 1-Faeces_pct61-Feed_pct61
Urine_pct62<- 1-Faeces_pct62-Feed_pct62
Urine_pct63<- 1-Faeces_pct63-Feed_pct63

Faeces_pct_3.0<-c(rep(Faeces_pct61,length(which(df_Composition_S6$Batch==1))),
                  rep(Faeces_pct62,length(which(df_Composition_S6$Batch==2))),
                  rep(Faeces_pct63,length(which(df_Composition_S6$Batch==3))))
Urine_pct_3.0<-c(rep(Urine_pct61,length(which(df_Composition_S6$Batch==1))),
                 rep(Urine_pct62,length(which(df_Composition_S6$Batch==2))),
                 rep(Urine_pct63,length(which(df_Composition_S6$Batch==3))))
Feed_pct_3.0<-c(rep(Feed_pct61,length(which(df_Composition_S6$Batch==1))),
                rep(Feed_pct62,length(which(df_Composition_S6$Batch==2))),
                rep(Feed_pct63,length(which(df_Composition_S6$Batch==3))))
Fresh_slurry<-c(rep(Total_Fresh_Slurry_61,length(which(df_Composition_S6$Batch==1))),
                rep(Total_Fresh_Slurry_62,length(which(df_Composition_S6$Batch==2))),
                rep(Total_Fresh_Slurry_63,length(which(df_Composition_S6$Batch==3))))

Final_S6_time<-df_Composition_S6$time
Final_S6_time_days<-as.numeric(diff(Final_S6_time)) #days
Final_S6_time_cum<-c(0,cumsum(Final_S6_time_days))

Final_S6_Sulfide<-df_Composition_S6$Sulfide_K #Default
Final_S6_Urea<-(Fresh_slurry*Faeces_pct_3.0*0+
                  Fresh_slurry*Urine_pct_3.0*df_Composition_S6$Urea_U+
                  Fresh_slurry*Feed_pct_3.0*0)/(Fresh_slurry) #g N/Kg new slurry
Final_S6_SO4<-(Fresh_slurry*Faeces_pct_3.0*0+
                 Fresh_slurry*Urine_pct_3.0*df_Composition_S6$SO4_U+
                 Fresh_slurry*Feed_pct_3.0*0)/(Fresh_slurry) #g S/Kg new slurry
Final_S6_TAN<-(Fresh_slurry*Faeces_pct_3.0*df_Composition_S6$TAN_F+
                 Fresh_slurry*Urine_pct_3.0*df_Composition_S6$TAN_U+
                 Fresh_slurry*Feed_pct_3.0*0)/(Fresh_slurry) #g N/Kg new slurry /// Assumptions: Rest of TN from Urine is TAN (1-0.75))
Final_S6_Starch<-(Fresh_slurry*Faeces_pct_3.0*0+
                    Fresh_slurry*Urine_pct_3.0*0+
                    Fresh_slurry*Feed_pct_3.0*df_Composition_S6$Starch_f)/(Fresh_slurry) #g COD/Kg
Final_S6_VFA<-(Fresh_slurry*Faeces_pct_3.0*df_Composition_S6$VFA_F+
                 Fresh_slurry*Urine_pct_3.0*df_Composition_S6$VFA_U+
                 Fresh_slurry*Feed_pct_3.0*0)/(Fresh_slurry) #g COD/Kg new slurry
Final_S6_xa_dead <- df_Composition_S6$xa_dead_K #Default
Final_S6_Cfat<-(Fresh_slurry*Faeces_pct_3.0*df_Composition_S6$Cfat_F+
                  Fresh_slurry*Urine_pct_3.0*0+
                  Fresh_slurry*Feed_pct_3.0*df_Composition_S6$fat_f)/(Fresh_slurry) #g COD/Kg new slurry
Final_S6_CP<-(Fresh_slurry*Faeces_pct_3.0*df_Composition_S6$CP_F+
                Fresh_slurry*Urine_pct_3.0*0+
                Fresh_slurry*Feed_pct_3.0*df_Composition_S6$CP_f)/(Fresh_slurry) # g COD/Kg new slurry  /// Assumption: No Urea in faeces, no urea or TAN in feed 
Final_S6_RFd <- df_Composition_S6$RFd_K #Default (From Frederik email)
Final_S6_NDF<-(Fresh_slurry*Faeces_pct_3.0*df_Composition_S6$NDF_F+
                 Fresh_slurry*Urine_pct_3.0*0+
                 Fresh_slurry*Feed_pct_3.0*df_Composition_S6$NDF_f)/(Fresh_slurry) # g COD/Kg new slurry 
Final_S6_iNDF <- Final_S6_NDF-Final_S6_RFd #g COD/Kg new slurry
Final_S6_VSd <- df_Composition_S6$VSd_K #Default
Final_S6_VSd_A<-(Fresh_slurry*Faeces_pct_3.0*df_Composition_S6$VSd_A_F +
                   Fresh_slurry*Urine_pct_3.0*df_Composition_S6$VSd_A_U+
                   Fresh_slurry*Feed_pct_3.0*df_Composition_S6$VSd_A_f)/(Fresh_slurry) # g VS/Kg new slurry /// To Ask: fraction of degradable in urine and feed
Final_S6_VSnd_A<-(Fresh_slurry*Faeces_pct_3.0*df_Composition_S6$VSnd_A_F +
                    Fresh_slurry*Urine_pct_3.0*df_Composition_S6$VSnd_A_U+
                    Fresh_slurry*Feed_pct_3.0*df_Composition_S6$VSnd_A_f)/(Fresh_slurry) # g VS/Kg new slurry  /// To Ask: fraction of non degradable in urine and feed
Final_S6_ash<-(Fresh_slurry*Faeces_pct_3.0*df_Composition_S6$Ash_F +
                 Fresh_slurry*Urine_pct_3.0*df_Composition_S6$Ash_U+
                 Fresh_slurry*Feed_pct_3.0*df_Composition_S6$Ash_f)/(Fresh_slurry) # g ash/Kg new slurry  
Final_S6_pH<-mean(df_Composition_S6$pH_U) # Only considering from urine (no pH measured for faeces) /// Ask Frederik: how to make it change over time
Final_S6_dens <- mean(df_Composition_S6$dens_K) #Default

df_man_pars_S6_3.0 <- list(conc_fresh = data.frame(time = Final_S6_time_cum, sulfide = Final_S6_Sulfide, urea = Final_S6_Urea, 
                                                   sulfate = Final_S6_SO4, TAN = Final_S6_TAN, starch = Final_S6_Starch, 
                                                   VFA = Final_S6_VFA, xa_dead = Final_S6_xa_dead, Cfat = Final_S6_Cfat, 
                                                   CP = Final_S6_CP, RFd = Final_S6_RFd, iNDF = Final_S6_iNDF, VSd = Final_S6_VSd, 
                                                   VSd_A = Final_S6_VSd_A, VSnd_A = Final_S6_VSnd_A, ash = Final_S6_ash), 
                           pH = df_S6_pH, dens = Final_S6_dens)


# ---- Do average, add extra parameters, make ready for ABM run ----
## ---- Default man_pars input (Danish Normative System) ----
df_man_pars_S6_2.0$conc_fresh$CPs <- df_man_pars_S6_2.0$conc_fresh$CP/2
df_man_pars_S6_2.0$conc_fresh$CPf <- df_man_pars_S6_2.0$conc_fresh$CP/2
man_pars_S6<-list(conc_fresh = data.frame(sulfide = (df_man_pars_S6_2.0$conc_fresh$sulfide),
                                          urea = (df_man_pars_S6_2.0$conc_fresh$urea), 
                                          sulfate = (df_man_pars_S6_2.0$conc_fresh$sulfate), 
                                          TAN = (df_man_pars_S6_2.0$conc_fresh$TAN), 
                                          starch = (df_man_pars_S6_2.0$conc_fresh$starch), 
                                          VFA = (df_man_pars_S6_2.0$conc_fresh$VFA),
                                          xa_dead = (df_man_pars_S6_2.0$conc_fresh$xa_dead),
                                          Cfat = (df_man_pars_S6_2.0$conc_fresh$Cfat), 
                                          CPs = (df_man_pars_S6_2.0$conc_fresh$CPs),
                                          CPf = (df_man_pars_S6_2.0$conc_fresh$CPf),
                                          RFd = (df_man_pars_S6_2.0$conc_fresh$RFd), 
                                          iNDF = (df_man_pars_S6_2.0$conc_fresh$iNDF), 
                                          VSd = (df_man_pars_S6_2.0$conc_fresh$VSd), 
                                          ash = (df_man_pars_S6_2.0$conc_fresh$ash),
                                          VSd_A = (df_man_pars_S6_2.0$conc_fresh$VSd_A), 
                                          VSnd_A = (df_man_pars_S6_2.0$conc_fresh$VSnd_A)), 
                  pH = data.frame(time = df_man_pars_S6_2.0$pH$time, pH = df_man_pars_S6_2.0$pH$pH), 
                  dens = 1000)
man_pars_S6$conc_fresh <- colMeans(man_pars_S6$conc_fresh)
man_pars_S6$conc_fresh <- as.list(man_pars_S6$conc_fresh)

df_man_pars_S5_2.0$conc_fresh$CPs <- df_man_pars_S5_2.0$conc_fresh$CP/2
df_man_pars_S5_2.0$conc_fresh$CPf <- df_man_pars_S5_2.0$conc_fresh$CP/2
man_pars_S5<-list(conc_fresh = data.frame(sulfide = (df_man_pars_S5_2.0$conc_fresh$sulfide),
                                          urea = (df_man_pars_S5_2.0$conc_fresh$urea), 
                                          sulfate = (df_man_pars_S5_2.0$conc_fresh$sulfate), 
                                          TAN = (df_man_pars_S5_2.0$conc_fresh$TAN), 
                                          starch = (df_man_pars_S5_2.0$conc_fresh$starch), 
                                          VFA = (df_man_pars_S5_2.0$conc_fresh$VFA),
                                          xa_dead = (df_man_pars_S5_2.0$conc_fresh$xa_dead),
                                          Cfat = (df_man_pars_S5_2.0$conc_fresh$Cfat), 
                                          CPs = (df_man_pars_S5_2.0$conc_fresh$CPs),
                                          CPf = (df_man_pars_S5_2.0$conc_fresh$CPf),
                                          RFd = (df_man_pars_S5_2.0$conc_fresh$RFd), 
                                          iNDF = (df_man_pars_S5_2.0$conc_fresh$iNDF), 
                                          VSd = (df_man_pars_S5_2.0$conc_fresh$VSd), 
                                          ash = (df_man_pars_S5_2.0$conc_fresh$ash),
                                          VSd_A = (df_man_pars_S5_2.0$conc_fresh$VSd_A), 
                                          VSnd_A = (df_man_pars_S5_2.0$conc_fresh$VSnd_A)), 
                  pH = data.frame(time = df_man_pars_S5_2.0$pH$time, pH = df_man_pars_S5_2.0$pH$pH), 
                  dens = 1000)
man_pars_S5$conc_fresh <- colMeans(man_pars_S5$conc_fresh)
man_pars_S5$conc_fresh <- as.list(man_pars_S5$conc_fresh)

### lnA lignin-based calculation
df_man_pars_S5_2.0_lnA<-df_man_pars_S5_2.0
df_man_pars_S5_2.0_lnA$conc_fresh$VSd_A<-mean(Final_S5_VSt_A)
df_man_pars_S5_2.0_lnA$conc_fresh$VSnd_A<-0
man_pars_S5_lnA<-list(conc_fresh = data.frame(sulfide = (df_man_pars_S5_2.0_lnA$conc_fresh$sulfide),
                                              urea = (df_man_pars_S5_2.0_lnA$conc_fresh$urea), 
                                              sulfate = (df_man_pars_S5_2.0_lnA$conc_fresh$sulfate), 
                                              TAN = (df_man_pars_S5_2.0_lnA$conc_fresh$TAN), 
                                              starch = (df_man_pars_S5_2.0_lnA$conc_fresh$starch), 
                                              VFA = (df_man_pars_S5_2.0_lnA$conc_fresh$VFA),
                                              xa_dead = (df_man_pars_S5_2.0_lnA$conc_fresh$xa_dead),
                                              Cfat = (df_man_pars_S5_2.0_lnA$conc_fresh$Cfat), 
                                              CPs = (df_man_pars_S5_2.0_lnA$conc_fresh$CPs),
                                              CPf = (df_man_pars_S5_2.0_lnA$conc_fresh$CPf),
                                              RFd = (df_man_pars_S5_2.0_lnA$conc_fresh$RFd), 
                                              iNDF = (df_man_pars_S5_2.0_lnA$conc_fresh$iNDF), 
                                              VSd = (df_man_pars_S5_2.0_lnA$conc_fresh$VSd), 
                                              ash = (df_man_pars_S5_2.0_lnA$conc_fresh$ash),
                                              VSd_A = (df_man_pars_S5_2.0_lnA$conc_fresh$VSd_A), 
                                              VSnd_A = (df_man_pars_S5_2.0_lnA$conc_fresh$VSnd_A)), 
                      pH = data.frame(time = df_man_pars_S5_2.0_lnA$pH$time, pH = df_man_pars_S5_2.0_lnA$pH$pH), 
                      dens = 1000)
man_pars_S5_lnA$conc_fresh <- colMeans(man_pars_S5_lnA$conc_fresh)
man_pars_S5_lnA$conc_fresh <- as.list(man_pars_S5_lnA$conc_fresh)

df_man_pars_S6_2.0_lnA<-df_man_pars_S6_2.0
df_man_pars_S6_2.0_lnA$conc_fresh$VSd_A<-mean(Final_S6_VSt_A)
df_man_pars_S6_2.0_lnA$conc_fresh$VSnd_A<-0
man_pars_S6_lnA<-list(conc_fresh = data.frame(sulfide = (df_man_pars_S6_2.0_lnA$conc_fresh$sulfide),
                                              urea = (df_man_pars_S6_2.0_lnA$conc_fresh$urea), 
                                              sulfate = (df_man_pars_S6_2.0_lnA$conc_fresh$sulfate), 
                                              TAN = (df_man_pars_S6_2.0_lnA$conc_fresh$TAN), 
                                              starch = (df_man_pars_S6_2.0_lnA$conc_fresh$starch), 
                                              VFA = (df_man_pars_S6_2.0_lnA$conc_fresh$VFA),
                                              xa_dead = (df_man_pars_S6_2.0_lnA$conc_fresh$xa_dead),
                                              Cfat = (df_man_pars_S6_2.0_lnA$conc_fresh$Cfat), 
                                              CPs = (df_man_pars_S6_2.0_lnA$conc_fresh$CPs),
                                              CPf = (df_man_pars_S6_2.0_lnA$conc_fresh$CPf),
                                              RFd = (df_man_pars_S6_2.0_lnA$conc_fresh$RFd), 
                                              iNDF = (df_man_pars_S6_2.0_lnA$conc_fresh$iNDF), 
                                              VSd = (df_man_pars_S6_2.0_lnA$conc_fresh$VSd), 
                                              ash = (df_man_pars_S6_2.0_lnA$conc_fresh$ash),
                                              VSd_A = (df_man_pars_S6_2.0_lnA$conc_fresh$VSd_A), 
                                              VSnd_A = (df_man_pars_S6_2.0_lnA$conc_fresh$VSnd_A)), 
                      pH = data.frame(time = df_man_pars_S6_2.0_lnA$pH$time, pH = df_man_pars_S6_2.0_lnA$pH$pH), 
                      dens = 1000)

man_pars_S6_lnA$conc_fresh <- colMeans(man_pars_S6_lnA$conc_fresh)
man_pars_S6_lnA$conc_fresh <- as.list(man_pars_S6_lnA$conc_fresh)

## ---- Alternative model man_pars input ----
df_man_pars_S6_3.0$conc_fresh$CPs <- df_man_pars_S6_3.0$conc_fresh$CP/2
df_man_pars_S6_3.0$conc_fresh$CPf <- df_man_pars_S6_3.0$conc_fresh$CP/2
man_pars_S6_model2<-list(conc_fresh = data.frame(sulfide = (df_man_pars_S6_3.0$conc_fresh$sulfide),
                                          urea = (df_man_pars_S6_3.0$conc_fresh$urea), 
                                          sulfate = (df_man_pars_S6_3.0$conc_fresh$sulfate), 
                                          TAN = (df_man_pars_S6_3.0$conc_fresh$TAN), 
                                          starch = (df_man_pars_S6_3.0$conc_fresh$starch), 
                                          VFA = (df_man_pars_S6_3.0$conc_fresh$VFA),
                                          xa_dead = (df_man_pars_S6_3.0$conc_fresh$xa_dead),
                                          Cfat = (df_man_pars_S6_3.0$conc_fresh$Cfat), 
                                          CPs = (df_man_pars_S6_3.0$conc_fresh$CPs),
                                          CPf = (df_man_pars_S6_3.0$conc_fresh$CPf),
                                          RFd = (df_man_pars_S6_3.0$conc_fresh$RFd), 
                                          iNDF = (df_man_pars_S6_3.0$conc_fresh$iNDF), 
                                          VSd = (df_man_pars_S6_3.0$conc_fresh$VSd), 
                                          ash = (df_man_pars_S6_3.0$conc_fresh$ash),
                                          VSd_A = (df_man_pars_S6_3.0$conc_fresh$VSd_A), 
                                          VSnd_A = (df_man_pars_S6_3.0$conc_fresh$VSnd_A)), 
                  pH = data.frame(time = df_man_pars_S6_3.0$pH$time, pH = df_man_pars_S6_3.0$pH$pH), 
                  dens = 1000)
man_pars_S6_model2$conc_fresh <- colMeans(man_pars_S6_model2$conc_fresh)
man_pars_S6_model2$conc_fresh <- as.list(man_pars_S6_model2$conc_fresh)

df_man_pars_S5_3.0$conc_fresh$CPs <- df_man_pars_S5_3.0$conc_fresh$CP/2
df_man_pars_S5_3.0$conc_fresh$CPf <- df_man_pars_S5_3.0$conc_fresh$CP/2
man_pars_S5_model2<-list(conc_fresh = data.frame(sulfide = (df_man_pars_S5_3.0$conc_fresh$sulfide),
                                          urea = (df_man_pars_S5_3.0$conc_fresh$urea), 
                                          sulfate = (df_man_pars_S5_3.0$conc_fresh$sulfate), 
                                          TAN = (df_man_pars_S5_3.0$conc_fresh$TAN), 
                                          starch = (df_man_pars_S5_3.0$conc_fresh$starch), 
                                          VFA = (df_man_pars_S5_3.0$conc_fresh$VFA),
                                          xa_dead = (df_man_pars_S5_3.0$conc_fresh$xa_dead),
                                          Cfat = (df_man_pars_S5_3.0$conc_fresh$Cfat), 
                                          CPs = (df_man_pars_S5_3.0$conc_fresh$CPs),
                                          CPf = (df_man_pars_S5_3.0$conc_fresh$CPf),
                                          RFd = (df_man_pars_S5_3.0$conc_fresh$RFd), 
                                          iNDF = (df_man_pars_S5_3.0$conc_fresh$iNDF), 
                                          VSd = (df_man_pars_S5_3.0$conc_fresh$VSd), 
                                          ash = (df_man_pars_S5_3.0$conc_fresh$ash),
                                          VSd_A = (df_man_pars_S5_3.0$conc_fresh$VSd_A), 
                                          VSnd_A = (df_man_pars_S5_3.0$conc_fresh$VSnd_A)), 
                  pH = data.frame(time = df_man_pars_S5_3.0$pH$time, pH = df_man_pars_S5_3.0$pH$pH), 
                  dens = 1000)
man_pars_S5_model2$conc_fresh <- colMeans(man_pars_S5_model2$conc_fresh)
man_pars_S5_model2$conc_fresh <- as.list(man_pars_S5_model2$conc_fresh)

