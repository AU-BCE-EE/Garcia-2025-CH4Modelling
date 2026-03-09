

### Run ABM ###
if (run_abm == TRUE){

# ---- ABM Model ----

arrh_pars <- arrh_pars_pig2.0
arrh_pars$lnA <- 31.3
arrh_pars$E_CH4 <- 81000

#Section 1
out_S5_2.0_Default<-abm(days=366, man_pars = man_pars_S5, wthr_pars = wthr_pars2 ,  
                        arrh_pars = arrh_pars,startup = 1, 
                        add_pars = list(evap = E_m2_day, slurry_mass= slurry_mass_S5,
                                        storage_depth = storage_depth,
                                        floor_area = floor_area,
                                        area = area,
                                        temp_C = df_temp_C_S5))
#Section 2
out_S6_2.0_Default<-abm(days=366, man_pars = man_pars_S6, wthr_pars = wthr_pars2 ,  
                        arrh_pars = arrh_pars,startup = 1, 
                        add_pars = list(evap = E_m2_day, slurry_mass= slurry_mass_S6,
                                        storage_depth = storage_depth,
                                        floor_area = floor_area,
                                        area = area,
                                        temp_C = df_temp_C_S6))

## ---- Sensitivity analysis on ABM and Arrhenius model (only Section 2) ----
#ABM Model with Evaporation 0.5
out_S6_2.0_Default_Evap05<-abm(days=366, man_pars = man_pars_S6, wthr_pars = wthr_pars2 ,  
                               arrh_pars = arrh_pars,startup = 1, 
                               add_pars = list(evap = 0.5, slurry_mass= slurry_mass_S6,
                                               storage_depth = storage_depth,
                                               floor_area = floor_area,
                                               area = area,
                                               temp_C = df_temp_C_S6))

#ABM Model with Evaporation 2
out_S6_2.0_Default_Evap2<-abm(days=366, man_pars = man_pars_S6, wthr_pars = wthr_pars2 ,  
                              arrh_pars = arrh_pars,startup = 1, 
                              add_pars = list(evap = 2, slurry_mass= slurry_mass_S6,
                                              storage_depth = storage_depth,
                                              floor_area = floor_area,
                                              area = area,
                                              temp_C = df_temp_C_S6))

#ABM Model with 50 kg spilled per pig
wthr_pars2$rain<-rain50
out_S6_2.0_Default_Rain50<-abm(days=366, man_pars = man_pars_S6, wthr_pars = wthr_pars2 ,  
                               arrh_pars = arrh_pars,startup = 1, 
                               add_pars = list(evap = E_m2_day, slurry_mass= slurry_mass_S6,
                                               storage_depth = storage_depth,
                                               floor_area = floor_area,
                                               area = area,
                                               temp_C = df_temp_C_S6))

#ABM Model with 100 kg spilled per pig
wthr_pars2$rain<-rain100
out_S6_2.0_Default_Rain100<-abm(days=366, man_pars = man_pars_S6, wthr_pars = wthr_pars2 ,  
                                arrh_pars = arrh_pars,startup = 1, 
                                add_pars = list(evap = E_m2_day, slurry_mass= slurry_mass_S6,
                                                storage_depth = storage_depth,
                                                floor_area = floor_area,
                                                area = area,
                                                temp_C = df_temp_C_S6))

#ABM Model model 2
wthr_pars2$rain<-rain
out_S6_2.0_Model2<-abm(days=366, man_pars = man_pars_S6_model2, wthr_pars = wthr_pars2 ,  
                       arrh_pars = arrh_pars,startup = 1, 
                       add_pars = list(evap = E_m2_day, slurry_mass= slurry_mass_S6,
                                       storage_depth = storage_depth,
                                       floor_area = floor_area,
                                       area = area,
                                       temp_C = df_temp_C_S6))



# ---- optimized ABM Model ----

#ABM Model to test parameters
load("Rfiles/optimization_S5.RData")
load("Rfiles/optimization_S5_A.RData")
load("Rfiles/optimization_S6.RData")
load("Rfiles/optimization_S6_A.RData")

resid_enrich_S5<-10^cal_S5$par[3]
qhat_opt_S5<-10^cal_S5$par[1]
alpha_opt_S5<-10^cal_S5$par[2]
arrh_pars$lnA <-10^cal_S5_A$par[1]

resid_enrich_S6<-10^cal_S6$par[3]
qhat_opt_S6<-10^cal_S6$par[1]
alpha_opt_S6<-10^cal_S6$par[2]
arrh_pars$lnA<-10^cal_S6_A$par[1]

out_S5_2.0_optim<-abm(days=366, man_pars = man_pars_S5, wthr_pars = wthr_pars2 ,  
                      arrh_pars = arrh_pars,startup = 1, 
                      add_pars = list(evap = E_m2_day, resid_enrich = resid_enrich_S5 ,slurry_mass= slurry_mass_S5,
                                      storage_depth = storage_depth,
                                      floor_area = floor_area,
                                      area = area,
                                      temp_C = df_temp_C_S5,
                                      scale.qhat_opt = qhat_opt_S5, 
                                      scale_alpha_opt.notVSd = alpha_opt_S5))

out_S6_2.0_optim<-abm(days=366, man_pars = man_pars_S6, wthr_pars = wthr_pars2 ,  
                      arrh_pars = arrh_pars,startup = 1, 
                      add_pars = list(evap = E_m2_day, resid_enrich = resid_enrich_S6,
                                      slurry_mass= slurry_mass_S6,
                                      storage_depth = storage_depth,
                                      floor_area = floor_area,
                                      area = area,
                                      temp_C = df_temp_C_S6,
                                      scale.qhat_opt = qhat_opt_S6, 
                                      scale_alpha_opt.notVSd = alpha_opt_S6))

# ---- Run alternative arrhenius model with lnA' and VS based on lignin ----
arrh_pars$lnA <- 30.3

out_S5_2.0_lnA<-abm(days=366, man_pars = man_pars_S5_lnA, wthr_pars = wthr_pars2 ,  
                    arrh_pars = arrh_pars,startup = 1, 
                    add_pars = list(evap = E_m2_day, slurry_mass= slurry_mass_S5,
                                    storage_depth = storage_depth,
                                    floor_area = floor_area,
                                    area = area,
                                    temp_C = df_temp_C_S5))

out_S6_2.0_lnA<-abm(days=366, man_pars = man_pars_S6_lnA, wthr_pars = wthr_pars2 ,  
                    arrh_pars = arrh_pars,startup = 1, 
                    add_pars = list(evap = E_m2_day, slurry_mass= slurry_mass_S6,
                                    storage_depth = storage_depth,
                                    floor_area = floor_area,
                                    area = area,
                                    temp_C = df_temp_C_S6))

# ---- Run optimized alternative arrhenius model with lnA' and VS based on lignin ----


load("Rfiles/optimization_S5_lnA.RData")
load("Rfiles/optimization_S6_lnA.RData")

arrh_pars$lnA<-10^cal_S5_lnA$par[1]

out_S5_2.0_lnA_optim<-abm(days=366, man_pars = man_pars_S5_lnA, wthr_pars = wthr_pars2 ,  
                          arrh_pars = arrh_pars,startup = 1, 
                          add_pars = list(evap = E_m2_day, slurry_mass= slurry_mass_S5,
                                          storage_depth = storage_depth,
                                          floor_area = floor_area,
                                          area = area,
                                          temp_C = df_temp_C_S5))


arrh_pars$lnA<-10^cal_S6_lnA$par[1]

out_S6_2.0_lnA_optim<-abm(days=366, man_pars = man_pars_S6_lnA, wthr_pars = wthr_pars2 ,  
                          arrh_pars = arrh_pars,startup = 1, 
                          add_pars = list(evap = E_m2_day, slurry_mass= slurry_mass_S6,
                                          storage_depth = storage_depth,
                                          floor_area = floor_area,
                                          area = area,
                                          temp_C = df_temp_C_S6))

save(out_S5_2.0_Default,out_S6_2.0_Default,
     out_S5_2.0_optim,out_S6_2.0_optim,
     out_S5_2.0_lnA,out_S6_2.0_lnA,
     out_S5_2.0_lnA_optim,out_S6_2.0_lnA_optim,
     out_S6_2.0_Default_Evap05,out_S6_2.0_Default_Evap2,
     out_S6_2.0_Default_Rain50,out_S6_2.0_Default_Rain100,
     out_S6_2.0_Model2,
     file = here("Rfiles","ABM_optim.RData"))

}else{
  load(here("Rfiles", "ABM_optim.RData"))
}

# ---- Prepare data for plots ----

## ---- Section 1 ----
df_Model_S5<-data.frame(time = out_S5_2.0_Default$time , CH4_ABM = out_S5_2.0_Default$CH4_emis_rate,
                        CH4_A = out_S5_2.0_Default$CH4_emis_rate_A,
                        CH4_A_lnA = out_S5_2.0_lnA$CH4_emis_rate_A,
                        CH4_ABM_optim = out_S5_2.0_optim$CH4_emis_rate,
                        CH4_A_optim = out_S5_2.0_optim$CH4_emis_rate_A,
                        CH4_A_lnA_optim = out_S5_2.0_lnA_optim$CH4_emis_rate_A)

df_Measured_S5<-data.frame(time = daily_avg_S5$Group.1, CH4_Total = daily_avg_S5$x,
                           CH4_enteric = daily_Entavg_S5$x)

class(df_Measured_S5)<-c("data.table","data.frame")
class(df_Model_S5)<-c("data.table","data.frame")

dt_merged<-merge(df_Measured_S5,Pigs_S5,by="time",all=TRUE)

dt_CH45<-merge(dt_merged,df_Model_S5,by="time",all=TRUE)
dt_CH45[,CH4_slurry :=CH4_Total-CH4_enteric]
dt_CH45$Batch<-NaN
dt_CH45$Batch[1:77]<-1
dt_CH45$Batch[95:191]<-2
dt_CH45$Batch[197:276]<-3

dt_CH45[,CH4_slurry_pig :=CH4_slurry/mean(Pigs,na.rm=TRUE)]
dt_CH45[,CH4_ABM_pig :=CH4_ABM/mean(Pigs,na.rm=TRUE)]
dt_CH45[,CH4_ABM_optim_pig :=CH4_ABM_optim/mean(Pigs,na.rm=TRUE)]
dt_CH45[,CH4_A_pig :=CH4_A/mean(Pigs,na.rm=TRUE)]
dt_CH45[,CH4_A_optim_pig :=CH4_A_optim/mean(Pigs,na.rm=TRUE)]
dt_CH45[,CH4_A_lnA_pig :=CH4_A_lnA/mean(Pigs,na.rm=TRUE)]
dt_CH45[,CH4_A_lnA_optim_pig :=CH4_A_lnA_optim/mean(Pigs,na.rm=TRUE)]
dt_CH45$A_default<-A_default

Subset_B1<-dt_CH45[dt_CH45$Batch == 1,]
Subset_B1[,CH4_slurry_pig :=CH4_slurry/mean(Pigs)]
Subset_B1[,CH4_ABM_pig :=CH4_ABM/mean(Pigs)]
Subset_B1[,CH4_ABM_optim_pig :=CH4_ABM_optim/mean(Pigs)]
Subset_B1[,CH4_A_pig :=CH4_A/mean(Pigs)]
Subset_B1[,CH4_A_optim_pig :=CH4_A_optim/mean(Pigs)]
Subset_B1[,CH4_A_lnA_pig :=CH4_A_lnA/mean(Pigs)]
Subset_B1[,CH4_A_lnA_optim_pig :=CH4_A_lnA_optim/mean(Pigs)]

Subset_B2<-dt_CH45[dt_CH45$Batch == 2,]
Subset_B2[,CH4_slurry_pig :=CH4_slurry/mean(Pigs)]
Subset_B2[,CH4_ABM_pig :=CH4_ABM/mean(Pigs)]
Subset_B2[,CH4_ABM_optim_pig :=CH4_ABM_optim/mean(Pigs)]
Subset_B2[,CH4_A_pig :=CH4_A/mean(Pigs)]
Subset_B2[,CH4_A_optim_pig :=CH4_A_optim/mean(Pigs)]
Subset_B2[,CH4_A_lnA_pig :=CH4_A_lnA/mean(Pigs)]
Subset_B2[,CH4_A_lnA_optim_pig :=CH4_A_lnA_optim/mean(Pigs)]

Subset_B3<-dt_CH45[dt_CH45$Batch == 3,]
Subset_B3[,CH4_slurry_pig :=CH4_slurry/mean(Pigs)]
Subset_B3[,CH4_ABM_pig :=CH4_ABM/mean(Pigs)]
Subset_B3[,CH4_ABM_optim_pig :=CH4_ABM_optim/mean(Pigs)]
Subset_B3[,CH4_A_pig :=CH4_A/mean(Pigs)]
Subset_B3[,CH4_A_optim_pig :=CH4_A_optim/mean(Pigs)]
Subset_B3[,CH4_A_lnA_pig :=CH4_A_lnA/mean(Pigs)]
Subset_B3[,CH4_A_lnA_optim_pig :=CH4_A_lnA_optim/mean(Pigs)]

dt_plot5<-rbind(Subset_B1,Subset_B2,Subset_B3)

## ---- Section 2 ----
df_Model_S6<-data.frame(time = out_S6_2.0_Default$time , CH4_ABM = out_S6_2.0_Default$CH4_emis_rate,
                        CH4_A = out_S6_2.0_Default$CH4_emis_rate_A,
                        CH4_A_lnA = out_S6_2.0_lnA$CH4_emis_rate_A,
                        CH4_ABM_optim = out_S6_2.0_optim$CH4_emis_rate,
                        CH4_A_optim = out_S6_2.0_optim$CH4_emis_rate_A,
                        CH4_A_lnA_optim = out_S6_2.0_lnA_optim$CH4_emis_rate_A,
                        CH4_ABM_Evap05 = out_S6_2.0_Default_Evap05$CH4_emis_rate,
                        CH4_ABM_Evap2 = out_S6_2.0_Default_Evap2$CH4_emis_rate,
                        CH4_ABM_Rain50 = out_S6_2.0_Default_Rain50$CH4_emis_rate,
                        CH4_ABM_Rain100 = out_S6_2.0_Default_Rain100$CH4_emis_rate,
                        CH4_ABM_Model2 = out_S6_2.0_Model2$CH4_emis_rate,
                        CH4_A_Model2 = out_S6_2.0_Model2$CH4_emis_rate_A)

df_Measured_S6<-data.frame(time = daily_avg_S6$Group.1, CH4_Total = daily_avg_S6$x,
                           CH4_enteric = daily_Entavg_S6$x)

class(df_Measured_S6)<-c("data.table","data.frame")
class(df_Model_S6)<-c("data.table","data.frame")


dt_merged<-merge(df_Measured_S6,Pigs_S6,by="time",all=TRUE)
dt_CH46<-merge(dt_merged,df_Model_S6,by="time",all=TRUE)
dt_CH46[,CH4_slurry :=CH4_Total-CH4_enteric]
dt_CH46$Batch<-NaN
dt_CH46$Batch[1:75]<-1
dt_CH46$Batch[96:180]<-2
dt_CH46$Batch[187:280]<-3

dt_CH46[,CH4_slurry_pig :=CH4_slurry/mean(Pigs,na.rm=TRUE)]
dt_CH46[,CH4_ABM_pig :=CH4_ABM/mean(Pigs,na.rm=TRUE)]
dt_CH46[,CH4_ABM_optim_pig :=CH4_ABM_optim/mean(Pigs,na.rm=TRUE)]
dt_CH46[,CH4_A_pig :=CH4_A/mean(Pigs,na.rm=TRUE)]
dt_CH46[,CH4_A_optim_pig :=CH4_A_optim/mean(Pigs,na.rm=TRUE)]
dt_CH46[,CH4_A_lnA_pig :=CH4_A_lnA/mean(Pigs,na.rm=TRUE)]
dt_CH46[,CH4_A_lnA_optim_pig :=CH4_A_lnA_optim/mean(Pigs,na.rm=TRUE)]
dt_CH46[,CH4_ABM_Evap05_pig :=CH4_ABM_Evap05/mean(Pigs,na.rm=TRUE)]
dt_CH46[,CH4_ABM_Evap2_pig :=CH4_ABM_Evap2/mean(Pigs,na.rm=TRUE)]
dt_CH46[,CH4_ABM_Rain50_pig :=CH4_ABM_Rain50/mean(Pigs,na.rm=TRUE)]
dt_CH46[,CH4_ABM_Rain100_pig :=CH4_ABM_Rain100/mean(Pigs,na.rm=TRUE)]
dt_CH46[,CH4_ABM_Model2_pig :=CH4_ABM_Model2/mean(Pigs,na.rm=TRUE)]
dt_CH46[,CH4_A_Model2_pig :=CH4_A_Model2/mean(Pigs,na.rm=TRUE)]
dt_CH46$A_default<-A_default

Subset_B1<-dt_CH46[dt_CH45$Batch == 1,]
Subset_B1[,CH4_slurry_pig :=CH4_slurry/mean(Pigs,na.rm=TRUE)]
Subset_B1[,CH4_ABM_pig :=CH4_ABM/mean(Pigs,na.rm=TRUE)]
Subset_B1[,CH4_ABM_optim_pig :=CH4_ABM_optim/mean(Pigs,na.rm=TRUE)]
Subset_B1[,CH4_A_pig :=CH4_A/mean(Pigs,na.rm=TRUE)]
Subset_B1[,CH4_A_optim_pig :=CH4_A_optim/mean(Pigs,na.rm=TRUE)]
Subset_B1[,CH4_A_lnA_pig :=CH4_A_lnA/mean(Pigs,na.rm=TRUE)]
Subset_B1[,CH4_A_lnA_optim_pig :=CH4_A_lnA_optim/mean(Pigs,na.rm=TRUE)]
Subset_B1[,CH4_ABM_Evap05_pig :=CH4_ABM_Evap05/mean(Pigs,na.rm=TRUE)]
Subset_B1[,CH4_ABM_Evap2_pig :=CH4_ABM_Evap2/mean(Pigs,na.rm=TRUE)]
Subset_B1[,CH4_ABM_Rain50_pig :=CH4_ABM_Rain50/mean(Pigs,na.rm=TRUE)]
Subset_B1[,CH4_ABM_Rain100_pig :=CH4_ABM_Rain100/mean(Pigs,na.rm=TRUE)]
Subset_B1[,CH4_ABM_Model2_pig :=CH4_ABM_Model2/mean(Pigs,na.rm=TRUE)]
Subset_B1[,CH4_A_Model2_pig :=CH4_A_Model2/mean(Pigs,na.rm=TRUE)]

Subset_B2<-dt_CH46[dt_CH45$Batch == 2,]
Subset_B2[,CH4_slurry_pig :=CH4_slurry/mean(Pigs,na.rm=TRUE)]
Subset_B2[,CH4_ABM_pig :=CH4_ABM/mean(Pigs,na.rm=TRUE)]
Subset_B2[,CH4_ABM_optim_pig :=CH4_ABM_optim/mean(Pigs,na.rm=TRUE)]
Subset_B2[,CH4_A_pig :=CH4_A/mean(Pigs,na.rm=TRUE)]
Subset_B2[,CH4_A_optim_pig :=CH4_A_optim/mean(Pigs,na.rm=TRUE)]
Subset_B2[,CH4_A_lnA_pig :=CH4_A_lnA/mean(Pigs,na.rm=TRUE)]
Subset_B2[,CH4_A_lnA_optim_pig :=CH4_A_lnA_optim/mean(Pigs,na.rm=TRUE)]
Subset_B2[,CH4_ABM_Evap05_pig :=CH4_ABM_Evap05/mean(Pigs,na.rm=TRUE)]
Subset_B2[,CH4_ABM_Evap2_pig :=CH4_ABM_Evap2/mean(Pigs,na.rm=TRUE)]
Subset_B2[,CH4_ABM_Rain50_pig :=CH4_ABM_Rain50/mean(Pigs,na.rm=TRUE)]
Subset_B2[,CH4_ABM_Rain100_pig :=CH4_ABM_Rain100/mean(Pigs,na.rm=TRUE)]
Subset_B2[,CH4_ABM_Model2_pig :=CH4_ABM_Model2/mean(Pigs,na.rm=TRUE)]
Subset_B2[,CH4_A_Model2_pig :=CH4_A_Model2/mean(Pigs,na.rm=TRUE)]

Subset_B3<-dt_CH46[dt_CH45$Batch == 3,]
Subset_B3[,CH4_slurry_pig :=CH4_slurry/mean(Pigs,na.rm=TRUE)]
Subset_B3[,CH4_ABM_pig :=CH4_ABM/mean(Pigs,na.rm=TRUE)]
Subset_B3[,CH4_ABM_optim_pig :=CH4_ABM_optim/mean(Pigs,na.rm=TRUE)]
Subset_B3[,CH4_A_pig :=CH4_A/mean(Pigs,na.rm=TRUE)]
Subset_B3[,CH4_A_optim_pig :=CH4_A_optim/mean(Pigs,na.rm=TRUE)]
Subset_B3[,CH4_A_lnA_pig :=CH4_A_lnA/mean(Pigs,na.rm=TRUE)]
Subset_B3[,CH4_A_lnA_optim_pig :=CH4_A_lnA_optim/mean(Pigs,na.rm=TRUE)]
Subset_B3[,CH4_ABM_Evap05_pig :=CH4_ABM_Evap05/mean(Pigs,na.rm=TRUE)]
Subset_B3[,CH4_ABM_Evap2_pig :=CH4_ABM_Evap2/mean(Pigs,na.rm=TRUE)]
Subset_B3[,CH4_ABM_Rain50_pig :=CH4_ABM_Rain50/mean(Pigs,na.rm=TRUE)]
Subset_B3[,CH4_ABM_Rain100_pig :=CH4_ABM_Rain100/mean(Pigs,na.rm=TRUE)]
Subset_B3[,CH4_ABM_Model2_pig :=CH4_ABM_Model2/mean(Pigs,na.rm=TRUE)]
Subset_B3[,CH4_A_Model2_pig :=CH4_A_Model2/mean(Pigs,na.rm=TRUE)]

dt_plot6<-rbind(Subset_B1,Subset_B2,Subset_B3)

# ---- RMSE ----
RMSE<-function(x_real,x_model){
  s2 = (x_real-x_model)^2
  s2 <- s2[!is.na(s2)]
  s <- sum(sqrt(s2)/length(s2))
  return(s)
}
dt_RMSE_S5 <- dt_plot5[, .(RMSE_ABM = RMSE(CH4_slurry_pig,CH4_ABM_pig),
                           RMSE_ABM_optim = RMSE(CH4_slurry_pig,CH4_ABM_optim_pig),
                           RMSE_A = RMSE(CH4_slurry_pig,CH4_A_pig),
                           RMSE_A_optim = RMSE(CH4_slurry_pig,CH4_A_optim_pig),
                           RMSE_A_lnA = RMSE(CH4_slurry_pig,CH4_A_lnA_pig),
                           RMSE_A_lnA_optim = RMSE(CH4_slurry_pig,CH4_A_lnA_optim_pig)),
                       by = .(Batch)]

dt_RMSE_S6 <- dt_plot6[, .(RMSE_ABM = RMSE(CH4_slurry_pig,CH4_ABM_pig),
                           RMSE_ABM_optim = RMSE(CH4_slurry_pig,CH4_ABM_optim_pig),
                           RMSE_A = RMSE(CH4_slurry_pig,CH4_A_pig),
                           RMSE_A_optim = RMSE(CH4_slurry_pig,CH4_A_optim_pig),
                           RMSE_A_lnA = RMSE(CH4_slurry_pig,CH4_A_lnA_pig),
                           RMSE_A_lnA_optim = RMSE(CH4_slurry_pig,CH4_A_lnA_optim_pig)),
                       by = .(Batch)]

dt_RMSE_S6 <-dt_RMSE_S6[Batch == 1 | Batch == 2 | Batch == 3]

# ---- Predicted and measured CH4 in g pig-1 day-1   ----
## --- Section 1 ----
# Interpolate NaN values using na.approx at CH4_Total and CH4_enteric
dt_plot5$CH4_Total[105:120] <- na.approx(dt_plot5$CH4_Total[105:185])
dt_plot5$CH4_enteric[105:120] <- na.approx(dt_plot5$CH4_enteric[105:185])
dt_plot5$CH4_Total[200:250] <- na.approx(dt_plot5$CH4_Total[200:250])
dt_plot5$CH4_enteric[200:250] <- na.approx(dt_plot5$CH4_enteric[200:250])

dt_plot5[, cum_CH4_Total_Measured := mintegrate(time, CH4_Total, method = 'r', lwr = 0), by = c("Batch")]
dt_plot5[, cum_CH4_Total_Slurry := mintegrate(time, CH4_Total-CH4_enteric, method = 'r', lwr = 0), by = c("Batch")]
dt_plot5[, cum_CH4_Total_ABM := mintegrate(time, CH4_ABM, method = 'r', lwr = 0), by = c("Batch")]
dt_plot5[, cum_CH4_Total_ABM_optim := mintegrate(time, CH4_ABM_optim, method = 'r', lwr = 0), by = c("Batch")]
dt_plot5[, cum_CH4_Total_A := mintegrate(time, CH4_A, method = 'r', lwr = 0), by = c("Batch")]
dt_plot5[, cum_CH4_Total_A_lnA := mintegrate(time, CH4_A_lnA, method = 'r', lwr = 0), by = c("Batch")]
dt_plot5[, cum_CH4_Total_A_optim := mintegrate(time, CH4_A_optim, method = 'r', lwr = 0), by = c("Batch")]
dt_plot5[, cum_CH4_Total_A_lnA_optim := mintegrate(time, CH4_A_lnA_optim, method = 'r', lwr = 0), by = c("Batch")]

dt_CH4T_S5 <- dt_plot5[, .(T_CH4_measured = max(cum_CH4_Total_Measured,na.rm=TRUE),
                           T_CH4_slurry = max(cum_CH4_Total_Slurry,na.rm=TRUE),
                           T_CH4_ABM = max(cum_CH4_Total_ABM,na.rm=TRUE),
                           T_CH4_ABM_optim = max(cum_CH4_Total_ABM_optim,na.rm=TRUE),
                           T_CH4_A = max(cum_CH4_Total_A,na.rm=TRUE),
                           T_CH4_A_lnA = max(cum_CH4_Total_A_lnA,na.rm=TRUE),
                           T_CH4_A_optim = max(cum_CH4_Total_A_optim,na.rm=TRUE),
                           T_CH4_A_lnA_optim = max(cum_CH4_Total_A_lnA_optim,na.rm=TRUE),
                           Days = max(time)-min(time),
                           Pigs = mean(Pigs)),
                       by = .(Batch)]

dt_CH4T_S5[,CH4_pig_day_measured :=T_CH4_measured/Days/Pigs]
dt_CH4T_S5[,CH4_pig_day_slurry :=T_CH4_slurry/Days/Pigs]
dt_CH4T_S5[,CH4_pig_day_ABM :=T_CH4_ABM/Days/Pigs]
dt_CH4T_S5[,CH4_pig_day_ABM_optim :=T_CH4_ABM_optim/Days/Pigs]
dt_CH4T_S5[,CH4_pig_day_A :=T_CH4_A/Days/Pigs]
dt_CH4T_S5[,CH4_pig_day_A_lnA :=T_CH4_A_lnA/Days/Pigs]
dt_CH4T_S5[,CH4_pig_day_A_optim :=T_CH4_A_optim/Days/Pigs]
dt_CH4T_S5[,CH4_pig_day_A_lnA_optim :=T_CH4_A_lnA_optim/Days/Pigs]


## --- Section 2 ----
# Interpolate NaN values using na.approx at CH4_Total and CH4_enteric
dt_plot6$CH4_Total[105:120] <- na.approx(dt_plot6$CH4_Total[105:120])
dt_plot6$CH4_enteric[105:120] <- na.approx(dt_plot6$CH4_enteric[105:120])
dt_plot6$CH4_Total[200:250] <- na.approx(dt_plot6$CH4_Total[200:250])
dt_plot6$CH4_enteric[200:250] <- na.approx(dt_plot6$CH4_enteric[200:250])

dt_plot6[, cum_CH4_Total_Measured := mintegrate(time, CH4_Total, method = 'r', lwr = 0), by = c("Batch")]
dt_plot6[, cum_CH4_Total_Slurry := mintegrate(time, CH4_Total-CH4_enteric, method = 'r', lwr = 0), by = c("Batch")]
dt_plot6[, cum_CH4_Total_ABM := mintegrate(time, CH4_ABM, method = 'r', lwr = 0), by = c("Batch")]
dt_plot6[, cum_CH4_Total_ABM_optim := mintegrate(time, CH4_ABM_optim, method = 'r', lwr = 0), by = c("Batch")]
dt_plot6[, cum_CH4_Total_A := mintegrate(time, CH4_A, method = 'r', lwr = 0), by = c("Batch")]
dt_plot6[, cum_CH4_Total_A_lnA := mintegrate(time, CH4_A_lnA, method = 'r', lwr = 0), by = c("Batch")]
dt_plot6[, cum_CH4_Total_A_optim := mintegrate(time, CH4_A_optim, method = 'r', lwr = 0), by = c("Batch")]
dt_plot6[, cum_CH4_Total_A_lnA_optim := mintegrate(time, CH4_A_lnA_optim, method = 'r', lwr = 0), by = c("Batch")]
dt_plot6[, cum_CH4_Total_ABM_Evap05 := mintegrate(time, CH4_ABM_Evap05, method = 'r', lwr = 0), by = c("Batch")]
dt_plot6[, cum_CH4_Total_ABM_Evap2 := mintegrate(time, CH4_ABM_Evap2, method = 'r', lwr = 0), by = c("Batch")]
dt_plot6[, cum_CH4_Total_ABM_Rain50 := mintegrate(time, CH4_ABM_Rain50, method = 'r', lwr = 0), by = c("Batch")]
dt_plot6[, cum_CH4_Total_ABM_Rain100 := mintegrate(time, CH4_ABM_Rain100, method = 'r', lwr = 0), by = c("Batch")]
dt_plot6[, cum_CH4_Total_ABM_Model2 := mintegrate(time, CH4_ABM_Model2, method = 'r', lwr = 0), by = c("Batch")]
dt_plot6[, cum_CH4_Total_A_Model2 := mintegrate(time, CH4_A_Model2, method = 'r', lwr = 0), by = c("Batch")]

dt_CH4T_S6 <- dt_plot6[, .(T_CH4_measured = max(cum_CH4_Total_Measured,na.rm=TRUE),
                           T_CH4_slurry = max(cum_CH4_Total_Slurry,na.rm=TRUE),
                           T_CH4_ABM = max(cum_CH4_Total_ABM,na.rm=TRUE),
                           T_CH4_ABM_optim = max(cum_CH4_Total_ABM_optim,na.rm=TRUE),
                           T_CH4_A = max(cum_CH4_Total_A,na.rm=TRUE),
                           T_CH4_A_lnA = max(cum_CH4_Total_A_lnA,na.rm=TRUE),
                           T_CH4_A_optim = max(cum_CH4_Total_A_optim,na.rm=TRUE),
                           T_CH4_A_lnA_optim = max(cum_CH4_Total_A_lnA_optim,na.rm=TRUE),
                           T_CH4_ABM_Evap05 = max(cum_CH4_Total_ABM_Evap05,na.rm=TRUE),
                           T_CH4_ABM_Evap2 = max(cum_CH4_Total_ABM_Evap2,na.rm=TRUE),
                           T_CH4_ABM_Rain50 = max(cum_CH4_Total_ABM_Rain50,na.rm=TRUE),
                           T_CH4_ABM_Rain100 = max(cum_CH4_Total_ABM_Rain100,na.rm=TRUE),
                           T_CH4_ABM_Model2 = max(cum_CH4_Total_ABM_Model2,na.rm=TRUE),
                           T_CH4_A_Model2 = max(cum_CH4_Total_A_Model2,na.rm=TRUE),
                           Days = max(time)-min(time),
                           Pigs = mean(Pigs)),
                       by = .(Batch)]

dt_CH4T_S6[,CH4_pig_day_measured :=T_CH4_measured/Days/Pigs]
dt_CH4T_S6[,CH4_pig_day_slurry :=T_CH4_slurry/Days/Pigs]
dt_CH4T_S6[,CH4_pig_day_ABM :=T_CH4_ABM/Days/Pigs]
dt_CH4T_S6[,CH4_pig_day_ABM_optim :=T_CH4_ABM_optim/Days/Pigs]
dt_CH4T_S6[,CH4_pig_day_A :=T_CH4_A/Days/Pigs]
dt_CH4T_S6[,CH4_pig_day_A_lnA :=T_CH4_A_lnA/Days/Pigs]
dt_CH4T_S6[,CH4_pig_day_A_optim :=T_CH4_A_optim/Days/Pigs]
dt_CH4T_S6[,CH4_pig_day_A_lnA_optim :=T_CH4_A_lnA_optim/Days/Pigs]
dt_CH4T_S6[,CH4_pig_day_ABM_Evap05 :=T_CH4_ABM_Evap05/Days/Pigs]
dt_CH4T_S6[,CH4_pig_day_ABM_Evap2 :=T_CH4_ABM_Evap2/Days/Pigs]
dt_CH4T_S6[,CH4_pig_day_ABM_Rain50 :=T_CH4_ABM_Rain50/Days/Pigs]
dt_CH4T_S6[,CH4_pig_day_ABM_Rain100 :=T_CH4_ABM_Rain100/Days/Pigs]
dt_CH4T_S6[,CH4_pig_day_ABM_Model2 :=T_CH4_ABM_Model2/Days/Pigs]
dt_CH4T_S6[,CH4_pig_day_A_Model2 :=T_CH4_A_Model2/Days/Pigs]
dt_CH4T_S6 <- dt_CH4T_S6[!is.nan(Batch)]

# ---- Save resulting files ----
write.table(dt_plot5,here("Rfiles","data5.txt"))
write.table(dt_plot6,here("Rfiles","data6.txt"))
write.table(man_pars_S6_lnA$conc_fresh,here("Rfiles","conc_fresh_S6_lnA.txt"))
write.table(man_pars_S6$conc_fresh,here("Rfiles","conc_fresh_S6.txt"))
write.table(man_pars_S5_lnA$conc_fresh,here("Rfiles","conc_fresh_S5_lnA.txt"))
write.table(man_pars_S5$conc_fresh,here("Rfiles","conc_fresh_S5.txt"))
write.table(man_pars_S6$pH,here("Rfiles","pH_S6.txt"))
write.table(man_pars_S5$pH,here("Rfiles","pH_S5.txt"))
write.table(man_pars_S5$dens,here("Rfiles","dens.txt"))

