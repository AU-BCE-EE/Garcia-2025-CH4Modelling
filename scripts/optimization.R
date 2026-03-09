

### Optimization script


# ---- what to optimize ----
if (optim == TRUE){

# ---- load files to optimize  ----
dat_S5<-read.table(here("Rfiles","data5.txt")) 
dat_S6<-read.table(here("Rfiles","data6.txt")) 

conc_fresh_S6_lnA<-as.list(read.table(here("Rfiles","conc_fresh_S6_lnA.txt"))) 
conc_fresh_S6<-as.list(read.table(here("Rfiles","conc_fresh_S6.txt"))) 
conc_fresh_S5_lnA<-as.list(read.table(here("Rfiles","conc_fresh_S5_lnA.txt"))) 
conc_fresh_S5<-as.list(read.table(here("Rfiles","conc_fresh_S5.txt")))

pH_S6<-read.table(here("Rfiles","pH_S6.txt")) 
pH_S5<-read.table(here("Rfiles","pH_S5.txt"))

dens<-read.table(here("Rfiles","dens.txt")) 

slurry_water_S5<-read.table(here("data/Slurry","slurry_mass_S5.txt")) 
slurry_water_S6<-read.table(here("data/Slurry","slurry_mass_S6.txt")) 

df_temp_C_S5<-read.table(here("data/Slurry","temp_C_S5.txt")) 
df_temp_C_S6<-read.table(here("data/Slurry","temp_C_S6.txt"))

mng_pars = list(slurry_prod_rate = 5700, 
                slurry_mass = 39000,     
                storage_depth = 0.6,     
                resid_depth = 0.05,      
                floor_area = 650,        
                area = 715,              
                empty_int = 42,          
                temp_C = 20,
                wash_water = 75000,            
                wash_int = NA,
                rest_d = 5,
                cover = 'none',
                resid_enrich = 0.9,
                slopes = c(urea = NA, slurry_prod_rate = NA),
                graze = c(start = 'May', duration = 0, hours_day = 0),
                scale = c(ks_coefficient = 1, qhat_opt = 1, 
                          xa_fresh = 1, yield = 1, alpha_opt = 1))

mng_pars_S6 <- mng_pars_S5 <- mng_pars

man_pars_S5 <- list(conc_fresh = conc_fresh_S5, pH = pH_S5, dens = 1000)
man_pars_S6 <- list(conc_fresh = conc_fresh_S6, pH = pH_S6, dens = 1000)

man_pars_S5_lnA <- list(conc_fresh = conc_fresh_S5_lnA, pH = pH_S5,
                        dens = 1000)
man_pars_S6_lnA <- list(conc_fresh = conc_fresh_S6_lnA, pH = pH_S6,
                        dens = 1000)


mng_pars_S5$slurry_mass <- slurry_water_S5
mng_pars_S6$slurry_mass <- slurry_water_S6

mng_pars_S5$temp_C <- df_temp_C_S5
mng_pars_S6$temp_C <- df_temp_C_S6

Water_per_pig <- 75 #Kg/pig #Normtal
pigs<-263 #Average of all sections and batches
days<-96 #Average number of days per batch
area <- (5.064*2.374)*16+19.174*1

rain <- (Water_per_pig*pigs)/(area*days) #kg/m2/day into the pit per batch equivalent to mm/m2/day
wthr_pars2 <- ABM::wthr_pars2.0
wthr_pars2$rain<-rain

grp_pars = ABM::grp_pars2.0


evap_pars = list(evap = 1.325838)


arrh_pars = ABM::arrh_pars_pig2.0


# ---- New parameters  ----
new_pars_abm <- c(scale.qhat_opt = 0.5,
                  scale_alpha_opt.notVSd = 1.5,
                  resid_enrich = 1)

new_pars_A <- c(31.3)

pars.cal <- log10(new_pars_abm)
pars.cal_A <- log10(new_pars_A)

# ---- Functions to run abm in optimization function  ----
resCalc <- function(par, to, dat, man_pars, mng_pars, evap_pars, wthr_pars, grp_pars){
  
  obs <- data.frame(time = dat$time[!is.na(dat$CH4_slurry)], 
                    CH4_emis_rate = dat$CH4_slurry[!is.na(dat$CH4_slurry)])
  
  times <- obs$time[!duplicated(obs$time)]
  
  p <- 10^par
  
  out <- abm(days = max(times, na.rm = T), times = times, mng_pars = mng_pars, man_pars = man_pars, 
             evap_pars = evap_pars, wthr_pars = wthr_pars, grp_pars = grp_pars, add_pars = p, rates_calc = 'instant', 
             approx_method = c(temp = 'linear', pH = 'linear', slurry_mass = 'early'))
  
  pred <- out[!duplicated(out$time), to]
  obs <- obs[, to]
  res <- sum(sqrt(abs(pred - obs))/length(pred))
  
  print(res)
  print(p)
  
  # initialize if not yet set
  if (!exists(".min_res", envir = .GlobalEnv)) {
    .GlobalEnv$.min_res <- Inf
    .GlobalEnv$.best_pars <- NULL
  }
  
  # update running minimum
  if (res < .GlobalEnv$.min_res) {
    .GlobalEnv$.min_res <- res
    .GlobalEnv$.best_pars <- p
  }
  
  # print current and best
  cat(
    "Current res:", res, 
    "| Min res so far:", .GlobalEnv$.min_res, 
    "| Params:", paste(p, collapse = ", "), 
    "| Best params:", paste(.GlobalEnv$.best_pars, collapse = ", "), "\n"
  )
  
  return(res)
  
}

resCalc_A <- function(par, to, dat, man_pars, mng_pars, evap_pars, wthr_pars){
  
  obs <- data.frame(time = dat$time[!is.na(dat$CH4_slurry)], 
                    CH4_emis_rate_A = dat$CH4_slurry[!is.na(dat$CH4_slurry)])
  
  times <- obs$time[!duplicated(obs$time)]
  
  p <- 10^par
  
  arrh_pars <- arrh_pars_pig2.0
  arrh_pars$E_CH4<-81000
  arrh_pars$lnA<-p
  
  out <- abm(days = max(times, na.rm = T), times = times, mng_pars = mng_pars, man_pars = man_pars, arrh_pars = arrh_pars,
             evap_pars = evap_pars, wthr_pars = wthr_pars, grp_pars = grp_pars, add_pars = p, rates_calc = 'instant', 
             approx_method = c(temp = 'linear', pH = 'linear', slurry_mass = 'early'))
  
  pred <- out[!duplicated(out$time), to]
  obs <- obs[, to]
  res <- sum(sqrt(abs(pred - obs))/length(pred))
  
  print(res)
  print(p)
  
  return(res)  
}

  cal_S5 <- optim(par = pars.cal, 
                  fn = resCalc,
                  to = 'CH4_emis_rate',
                  dat = dat_S5,
                  man_pars = man_pars_S5,
                  mng_pars = mng_pars_S5,
                  wthr_pars = wthr_pars2,
                  evap_pars = evap_pars,
                  grp_pars = grp_pars,
                  method = 'L-BFGS-B',
                  lower = log10(c(0.5,0.5,0.9)),
                  upper = log10(c(1.5,1.5,2)))
  save(cal_S5,file=here("Rfiles","optimization_S5.RData"))

    cal_S6 <- optim(par = pars.cal, 
                  fn = resCalc,
                  to = 'CH4_emis_rate',
                  dat = dat_S6,
                  man_pars = man_pars_S6,
                  mng_pars = mng_pars_S6,
                  wthr_pars = wthr_pars2,
                  evap_pars = evap_pars,
                  grp_pars = grp_pars,
                  method = 'L-BFGS-B',
                  lower = log10(c(0.3,0.3,0.5)),
                  upper = log10(c(2,2,2)))
  save(cal_S6,file=here("Rfiles","optimization_S6.RData"))


cal_S5_A <- optim(par = pars.cal_A, 
                  fn = resCalc_A,
                  to = 'CH4_emis_rate_A',
                  dat = dat_S5,
                  man_pars = man_pars_S5,
                  mng_pars = mng_pars_S5,
                  wthr_pars = wthr_pars2,
                  evap_pars = evap_pars,
                  method = 'L-BFGS-B',
                  lower = log10(27),
                  upper = log10(33))
save(cal_S5_A,file=here("Rfiles","optimization_S5_A.RData"))

cal_S6_A <- optim(par = pars.cal_A, 
                  fn = resCalc_A,
                  to = 'CH4_emis_rate_A',
                  dat = dat_S6,
                  man_pars = man_pars_S6,
                  mng_pars = mng_pars_S6,
                  wthr_pars = wthr_pars2,
                  evap_pars = evap_pars,
                  method = 'L-BFGS-B',
                  lower = log10(27),
                  upper = log10(33))
save(cal_S6_A,file=here("Rfiles","optimization_S6_A.RData"))

cal_S5_lnA <- optim(par = pars.cal_A, 
                    fn = resCalc_A,
                    to = 'CH4_A_emis_rate',
                    dat = dat_S5,
                    man_pars = man_pars_S5_lnA,
                    mng_pars = mng_pars_S5,
                    wthr_pars = wthr_pars2,
                    evap_pars = evap_pars,
                    method = 'L-BFGS-B',
                    lower = log10(27),
                    upper = log10(33))
save(cal_S5_lnA,file=here("Rfiles","optimization_S5_lnA.RData"))

cal_S6_lnA <- optim(par = pars.cal_A, 
                    fn = resCalc_A,
                    to = 'CH4_A_emis_rate',
                    dat = dat_S6,
                    man_pars = man_pars_S6_lnA,
                    mng_pars = mng_pars_S6,
                    wthr_pars = wthr_pars2,
                    evap_pars = evap_pars,
                    method = 'L-BFGS-B',
                    lower = log10(27),
                    upper = log10(33))
save(cal_S6_lnA,file=here("Rfiles","optimization_S6_lnA.RData"))
}