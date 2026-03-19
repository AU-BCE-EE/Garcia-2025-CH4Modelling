
## To run the script:
#clone  "https://github.com/AU-BCE-EE/Garcia-2025-CH4Modelling" to your own local folder


# ---- clear files ----
rm(list=ls())


# ---- Choose ----
## ---- run abm() function? (check "6_Run_ABM.R") ----
run_abm <- FALSE 
#TRUE: run ABM 
#FALSE: do not run ABM (save time)

## ---- run optimization? (check "optimization.R") ----
optim <- FALSE 
#TRUE: run optimization script (takes few hours)
#FALSE: load saved optimization files

## ---- load CRDS data treatment example? (check "raw_CRDS_data.R") ----
load_CRDS <- FALSE 
#TRUE: run raw data treatment example script (needs to be TRUE to generate FigS1)
#FALSE: load saved  files (save time)


# ---- Install same abm version as in manuscript ----
devtools::install_github("AU-BCE-EE/ABM", ref = "dev_Arrhenius", vignette = FALSE)

# ---- Used packages (if installation needed run install.packages("name of package")) ----

library(here)
library(readxl)
library(anytime)
library(ABM)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(data.table)
library(zoo)
library(ampvis2)
library(scales)
library(cowplot)
library(pheatmap)




# ---- Load scripts ----
source(here("scripts","mintegrate.R")) #Function to calculate cumulative CH4 emissions
source(here("scripts","optimization.R")) #Function to run the optimization of the ABM and Arrhenius models
source(here("scripts","Ventilation rate.R")) #Function to calculate the ventilation rate and missing gaps
source(here("scripts","raw_CRDS_data.R")) #Function to read raw CRDS data (request full data if needed)

source(here("scripts", "1_Fresh_slurry_composition.R")) # create man_pars list with farm data
source(here("scripts","2_ABM_Inputs.R")) # ABM Inputs: temperature, slurry mass, evaporation rate,...
source(here("scripts","3_CH4_Emissions.R")) # Measured total CH4 Emissions and Enteric CH4 emissions
source(here("scripts","4_CH4_Potential_Rate.R")) # CH4 rate from incubation test
source(here("scripts","5_Arrhenius_default.R")) # CH4 emissions as estimated in the Danish NIR
source(here("scripts","6_Run_ABM.R")) # Run ABM() function to estimate CH4 emissions
source(here("scripts","7_Microbial_analysis.R")) # Prepare microbial data (DNA, Protein, qPCR)
source(here("scripts","8_Figures.R")) # Prepare microbial data (DNA, Protein, qPCR)

# --- Table 3 (model performance) ----
#RMSE
dt_RMSE_S5
dt_RMSE_S6

#Emissions
dt_CH4T_S5
dt_CH4T_S6

