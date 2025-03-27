*Note to users of this template*: see the guidance here for setting up a paper repo: <https://github.com/AU-BCE-EE/GitHub-guidance/blob/main/paper-repos.md>.
Delete this text and any other notes or example text that isn't relevant.

# Garcia-2024-CH4-Modelling
 Repo for paper on CH4 modelling validation, including ABM and Arrhenius models.


# Maintainer
Pablo García Pérez (<https://au.dk/pg@bce.au.dk>) and Frederik Dalby (<https://au.dk/fd@bce.au.dk>) 
# Submitted paper
Garcia, P., Hansen, M. J., Feilberg, A., Nielsen, J. L., Kleikamp, H. B. C., Gertsen, S. O., Dalby, F. R. Evaluation of ABM and Arrhenius models for methane estimation on a Danish pig house with insights into methanogenic presence and activity. Submitted for Agricultural Systems.  
# Overview
This repo contains the data and the scripts for the data processing to generate the figures for the submitted paper. The scripts run in R and require add-on packages.

# Directory information
## `Batch 1`
Contains data on CH4 emissions, CH4 concentration, room temperature, and relative humidity from the first batch. Additionaly, a subfolder `CH4 rate` with data on the slurry incubation to determine the CH4 rate and `Section 5` and `Section 6` containing data of the slurry temperature.  
## `Batch 2`
Contains data on CH4 emissions, CH4 concentration, room temperature, and relative humidity from the second batch. Additionaly, a subfolder `CH4 rate` with data on the slurry incubation to determine the CH4 rate and `Section 5` and `Section 6` containing data of the slurry temperature.   
## `Batch 3`
Contains data on CH4 emissions, CH4 concentration, room temperature, and relative humidity from the third batch. Additionaly, a subfolder `CH4 rate` with data on the slurry incubation to determine the CH4 rate and `Section 5` and `Section 6` containing data of the slurry temperature. 
## `Batch 4`
Contains data on the average CH4 emissions 2 hours before and 2 hours after discharging, the slurry volume, and the number of pigs.
## `data`
Contains information on the slurry, feces, urine, and feed composition. Additionally, the subfolder `DNA` contains data for the DNA analysis for the relative abundance and the qPCR for the absolute abundance of methanogens, and the subfolder `Proteins` contains data for the relative activity based on proteomic analyses.
## `plots`
Folder to store the generated Figures from the submitted manuscript
## `data`
Contains the processed data necessary to generate the figures. Includes the default CH4 emissions with the current Danish Arrhenius model, the CH4 emissions predicted by the ABM and modified Arrhenius model, the measured emissions per day and per pig together with slurry temperature, slurry production, predicted pig mass, predicted enteric emissions, feed consumption, ventilation rate, and optimized parameters.  
## `Temperature_data`
Contains the average measured temeprature from slurry from each section
## `scripts`
Contains the R files to process the data. 

**Imput.R**: estimates the weight of the pigs at each day of the batch, the feed consumption per pig per day, the number of pigs at each day, fills missing ventilation rate data for cases where it was not registered based on the CO2 mass balance equation, generates Figure S1, converts emissions to gram per hour, creates and store a txt file called "df_ABM2.txt" that contains all the information.

**ABM_Modelling.R**: Reads information contained in "df_ABM2.txt" and makes it compatible with the abm() function to run both ABM and Arrhenius model, sincronize times, polish outliers, include the washing intervals, calculates enteric methane emissions, adds the enteric methane emissions to "df_ABM2.txt" in another file called "df_ABM3.txt", calculates the parameters used as inputs for both models, covering feces, urine, slurry, and feed composition ensuring the model runs with the proper units, runs the abm() function with both default and optimized parameters, save the model output as "Model_paper_new.RData", calculates and save the CH4 emissions as estimated in the Danish National Inventory Report as "A_default.txt", generates Figure S3.

**CH4 rate.R**: Calculates the CH4 rate from the collected slurry samples based on the measured CH4 concentration from the GC and the measured airflow from the AMPTS II. Calculates the CH4 rate in units of mg of CH4 per kg of VS per day to compare with the literature and in units of g CH4 per pig per day to compare with the measured emissions.

**Temperature.R**: Reads the slurry temperature data from the three temperature sensors per section and generates Figure S2.

**Figures.R**: Generates Figure 1 and 2 from the main manuscript, calculates the RMSE of the ABM and Arrhenius model, calculates the average cumulative CH4 per pig per day from the in-situ measurements, from the ABM, and from the Arrhenius, generates Figure S4 and S5.

**optimize.R**: Optimize key parameters for a better fitting of the measured emissions.

**mintegrate.R**: funciton to cacluate cumulative emissions.

# Links to published paper






