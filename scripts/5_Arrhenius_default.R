

#### TIER 2: Default Danish report methane Arrhenius equation ###

# ---- CH4/pig/day using EF ----
HRT <- 17.9 #Hydraulic retention time (days)
pig_prod_time <- 84 #Average pig production time
VS_pig_day_normtal <- 31.3 / 84 # kg/day/pig, based on normative system of feed digestibility and intake: gives 31.3 kg VS excreted over 84 days. 
EF <- 572.97 # g CH4 / kg VS / year from National inventory report. p 890 or 889
CH4_pig_day_EF <- VS_pig_day_normtal * EF * HRT/365 # g / pig / day, equation 3D-3 page 888

# ---- Arrhenius equation ----
VSd<-51/100 # g VSd/gVS
VSnd<-49/100 # g VSnd/gVS
b1<-1
b2<-0.01
lnA<-31.3 #gCH4/KgVS/h
lnA_new<-30.3 #gCH4/KgVSt/h; VS in slurry minus lignin fraction
R<-8.314 #J/mol/K
Ea<-81*1000 #J/mol
T_avg<-18.6 + 273 #k Holm, 2015
RT <- R*T_avg
VSt<-68.54946/1000 #KgVSt/Kg 
CH4_A<-(VSd*b1*exp(lnA-Ea*(1/(RT)))+VSnd*b2*exp(lnA-Ea*(1/(RT))))*24 #gCH4/day/KgVS
CH4_A_lnA<-(VSt*exp(lnA_new-Ea*(1/(RT))))*24 #gCH4/day/KgVS

CH4_pig_day_Arr<-CH4_A*VS_pig_day_normtal*HRT #g CH4/pig/day
A_default<-CH4_pig_day_Arr
write.table(CH4_pig_day_Arr,here("Rfiles","A_default.txt"))
