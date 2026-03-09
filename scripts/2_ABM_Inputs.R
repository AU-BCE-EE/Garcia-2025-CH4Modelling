
### Prepare abm inputs ###

# ---- slurry_mass Section 1 ----
slurry_mass_S5<-read.table(here("data/Slurry","slurry_mass_S5.txt")) 
# ---- slurry_mass Section 2 ----
slurry_mass_S6<-read.table(here("data/Slurry","slurry_mass_S6.txt")) 
# ---- Depth ----
storage_depth<-0.5
# ---- Surface accessible by animals ----
floor_area<-(5.064*2.374)*16
# ---- Total surface area ----
area <- (5.064*2.374)*16+19.174*1
# ---- Slurry temperature Section 1 ----
df_temp_C_S5<-read.table(here("data/Slurry","temp_C_S5.txt")) #daily average
df_S5<-read_excel(here("data/Slurry","Section 1 slurry Temperature.xlsx")) #raw data
# ---- Slurry temperature Section 2 ----
df_temp_C_S6<-read.table(here("data/Slurry","temp_C_S6.txt")) 
df_S6<-read_excel(here("data/Slurry","Section 2 slurry Temperature.xlsx")) #raw data

# ---- Evaporation ----

ER <- function(A, Xs, X,EC) {
  A <-area
  Xs <- 0.62198*pws/(pa - pws)  
  EC  <- 25+18*v
  X<-0.013 #90%RH and 20C  #Kg/Kg
  return(EC*A*(Xs-X)/3600)
}
Temp <- 293.7132
pws <- exp(1)^(77.3450 + 0.0057*Temp - 7235/Temp)/Temp^(8.2)
pa<-101325 #Pa atmospheric pressure
v<-0.02 #m/s #air velocity in slurry surface
E_Calculated<-ER() #Kg/s
E_m2_day<-(E_Calculated*60*60*24)/area

## ---- Rain induced by water spillage ----
Water_per_pig <- 75 #Kg/pig #Normtal
pigs<-263 #Average of all sections and batches
days<-96 #Average number of days per batch

rain <- (Water_per_pig*pigs)/(area*days) #kg/m2/day into the pit per batch equivalent to mm/m2/day
rain50 <- (50*pigs)/(area*days) #kg/m2/day into the pit per batch equivalent to mm/m2/day
rain100 <- (100*pigs)/(area*days) #kg/m2/day into the pit per batch equivalent to mm/m2/day

wthr_pars2 <- ABM::wthr_pars2.0
wthr_pars2$rain<-rain