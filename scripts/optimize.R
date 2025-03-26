rm(list=ls())
setwd("...") #Choose own directory
library(ABM)
library(data.table)

dat <- fread('../data/dat.csv')
conc_fresh <- fread('../data/conc_fresh.csv')
wash_water_S5 <- fread('../data/wash_water_S5.csv')
wash_water_S6 <- fread('../data/wash_water_S6.csv')
gfdgfdg
mng_pars_S5 = list(slurry_prod_rate = 5700,  
                   slurry_mass = 39000,      
                   storage_depth = 0.5,      
                   resid_depth = 0.05,        
                   floor_area = 192.351,          
                   area = 211.525,               
                   empty_int = 42,  
                   temp_C = 20,
                   wash_water = 75000,            
                   wash_int = NA,
                   rest_d = 5,
                   cover = 'none',
                   resid_enrich = 0.9,
                   slopes = c(urea = NA, slurry_prod_rate = NA),
                   graze = c(start = 'May', duration = 0, hours_day = 0),
                   scale = c(ks_coefficient = 1, qhat_opt = 1, xa_fresh = 1, yield = 1, alpha_opt = 1))

mng_pars_S5$rest_d <- 0
mng_pars_S5$slurry_prod_rate <- 0
mng_pars_S5$resid_depth <- 0.05
mng_pars_S5$empty_int <- 28
mng_pars_S5$cover = "none"

mng_pars_S6 <- mng_pars_S5

dat_S5 <- dat[SectionID == 5]
dat_S6 <- dat[SectionID == 6]

slurry_mass_dat_S5 <- data.frame(time = dat_S5$time[!is.na(dat_S5$slurry_mass)], 
                                 slurry_mass = dat_S5$slurry_mass[!is.na(dat_S5$slurry_mass)], 
                                 wash_water = wash_water_S5$wash_water)
slurry_mass_dat_S6 <- data.frame(time = dat_S6$time[!is.na(dat_S6$slurry_mass)], 
                                 slurry_mass = dat_S6$slurry_mass[!is.na(dat_S6$slurry_mass)],
                                 wash_water = wash_water_S6$wash_water)
temp_C_dat_S5 <- data.frame(time = dat_S5$time[!is.na(dat_S5$temperature_manure)], 
                            temp_C = dat_S5$temperature_manure[!is.na(dat_S5$temperature_manure)])
temp_C_dat_S6 <- data.frame(time = dat_S6$time[!is.na(dat_S6$temperature_manure)], 
                            temp_C = dat_S6$temperature_manure[!is.na(dat_S6$temperature_manure)])

conc_fresh[, time := NULL]
conc_fresh_S5 <- as.list(conc_fresh[!duplicated(conc_fresh) & SectionID == 5][, SectionID := NULL])
conc_fresh_S6 <- as.list(conc_fresh[!duplicated(conc_fresh) & SectionID == 6][, SectionID := NULL])

man_pars_S5 <- list(conc_fresh = conc_fresh_S5, dens = 1000, 
                    pH = data.frame(time = dat_S5$time[!is.na(dat_S5$pH)], pH = dat_S5$pH[!is.na(dat_S5$pH)]))

man_pars_S6 <- list(conc_fresh = conc_fresh_S6, dens = 1000, 
                    pH = data.frame(time = dat_S6$time[!is.na(dat_S6$pH)], pH = dat_S6$pH[!is.na(dat_S6$pH)]))

mng_pars_S5$slurry_mass <- slurry_mass_dat_S5
mng_pars_S6$slurry_mass <- slurry_mass_dat_S6
mng_pars_S5$temp_C <- temp_C_dat_S5
mng_pars_S6$temp_C <- temp_C_dat_S6

wthr_pars <- wthr_pars2.0
wthr_pars$rain <- 0

new_pars <- c(qhat_opt.m2 = grp_pars2.0$qhat_opt[['m2']],
              scale_alpha_opt.notVSd = arrh_pars2.0$scale_alpha_opt$notVSd)

new_pars_A <- c(lnA.VSd_A = arrh_pars2.0$lnA[['VSd_A']],
                E_CH4.VSd_A = arrh_pars2.0$E_CH4[['VSd_A']])

pars.cal <- log10(new_pars)
pars.cal_A <- log10(new_pars_A)

resCalc <- function(par, to, dat, man_pars, mng_pars, evap_pars, wthr_pars){
  
  obs <- data.frame(time = dat$time[!is.na(dat$CH4_emission_manure)], 
                    CH4_emis_rate = dat$CH4_emission_manure[!is.na(dat$CH4_emission_manure)])
  
  times <- obs$time[!duplicated(obs$time)]
  
  p <- 10^par
  
  out <- abm(days = max(times, na.rm = T), times = times, mng_pars = mng_pars, man_pars = man_pars, 
             evap_pars = evap_pars, wthr_pars = wthr_pars, add_pars = p, rates_calc = 'instant', 
             approx_method = c(temp = 'linear', pH = 'linear', slurry_mass = 'early'))
  
  pred <- out[!duplicated(out$time), to]
  obs <- obs[, to]
  res <- sum(sqrt(abs(pred - obs))/length(pred))
  
  print(res)
  print(p)
  
  return(res)
  
}

resCalc_A <- function(par, to, dat, man_pars, mng_pars, evap_pars, wthr_pars){
  
  obs <- data.frame(time = dat$time[!is.na(dat$CH4_emission_manure)], 
                    CH4_A_emis_rate = dat$CH4_emission_manure[!is.na(dat$CH4_emission_manure)])
  
  times <- obs$time[!duplicated(obs$time)]
  
  p <- 10^par
  
  out <- abm(days = max(times, na.rm = T), times = times, mng_pars = mng_pars, man_pars = man_pars, 
             evap_pars = evap_pars, wthr_pars = wthr_pars, add_pars = p, rates_calc = 'instant', 
             approx_method = c(temp = 'linear', pH = 'linear', slurry_mass = 'early'))
  
  pred <- out[!duplicated(out$time), to]
  obs <- obs[, to]
  res <- sum(sqrt(abs(pred - obs))/length(pred))
  
  print(res)
  print(p)
  
  return(res)
  
}

sdffds

cal_S5 <- optim(par = pars.cal, 
        fn = resCalc,
        to = 'CH4_emis_rate',
        dat = dat_S5,
        man_pars = man_pars_S5,
        mng_pars = mng_pars_S5,
        wthr_pars = wthr_pars,
        evap_pars = list(evap = 1.3258),
        method = 'L-BFGS-B',
        lower = log10(c(1, 0.5)),
        upper = log10(c(10, 10)))

cal_S6 <- optim(par = pars.cal,
               fn = resCalc,
               to = 'CH4_emis_rate',
               dat = dat_S6,
               man_pars = man_pars_S6,
               mng_pars = mng_pars_S6,
               evap_pars = list(evap = 1.3258),
               wthr_pars = wthr_pars,
               method = 'L-BFGS-B',
               lower = log10(c(1, 0.5)),
               upper = log10(c(5.2, 2)))

save(cal_S5,cal_S6,file="optimization.RData")


cal_S5A <- optim(par = pars.cal_A, 
                fn = resCalc_A,
                to = 'CH4_A_emis_rate',
                dat = dat_S5,
                man_pars = man_pars_S5,
                mng_pars = mng_pars_S5,
                wthr_pars = wthr_pars,
                evap_pars = list(evap = 1.3258),
                method = 'L-BFGS-B',
                lower = log10(c(10, 7500)),
                upper = log10(c(35, 90000)))

cal_S6A <- optim(par = pars.cal_A,
                fn = resCalc_A,
                to = 'CH4_A_emis_rate',
                dat = dat_S6,
                man_pars = man_pars_S6,
                mng_pars = mng_pars_S6,
                evap_pars = list(evap = 1.3258),
                wthr_pars = wthr_pars,
                method = 'L-BFGS-B',
                lower = log10(c(10, 8000)),
                upper = log10(c(32, 80500)))

save(cal_S5A,cal_S6A,file="optimization_A.RData")
10^cal_S5A$par
10^cal_S6A$par
