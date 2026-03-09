*Note to users of this template*: see the guidance here for setting up a paper repo: <https://github.com/AU-BCE-EE/GitHub-guidance/blob/main/paper-repos.md>.
Delete this text and any other notes or example text that isn't relevant.

# Garcia-2025-CH4-Modelling
Repo for CH4 modelling validation in a commercial scale pig farm. Run main.R to generate the Figures contained in the `plots` folder

# Maintainer
Pablo García Pérez (<https://au.dk/pg@bce.au.dk>) and Frederik Dalby (<https://au.dk/fd@bce.au.dk>) 
# Submitted paper
Garcia, P., Hansen, M. J., Feilberg, A., Nielsen, J. L., Kleikamp, H. B. C., Gertsen, S. O., Dalby, F. R. Evaluation of ABM and Arrhenius models for methane estimation on a Danish pig house with insights into methanogenic presence and activity. Submitted for Agricultural Systems.  
# Overview
This repo contains the data and the scripts for the data processing to generate the figures for the submitted paper. The scripts run in R and require add-on packages.

# Directory information
## `data`
Contains subfolders with data about the incubation test to calculate the methane production rate in the subfolder `CH4 rate`, metadata about the DNA analysis of methanogens and bacterial communities in `DNA`, ventilation rate raw txt files in `Flow`, an example of CRDS raw .dat files in `May_CRDS_Data`, already treated measured emissions in `Measured emissions` (more raw data available upon request), protein annotation to determine activity in `Proteins`, slurry compositioinal analysis in `Slurry`, and additional files needed for data analysis.  
## `logs`
R version information.   
## `plots`
All plots in main manuscript and supplementary material. 
## `Rfiles`
All files generated with the used scripts.
## `scripts`
All R files with specific parts of the datatreatment required to generate the plots. Check each one individually for additional information.

# Links to published paper






