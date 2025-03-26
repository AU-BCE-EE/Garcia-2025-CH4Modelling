library(dplyr)
library(ggplot2)
library(anytime)
library(lubridate)
library("readxl")
library(writexl)

setwd("...") #Choose own directory



##Read the data
  CH4_rate <- function(Per, file) {
    file_path <- switch(
      as.character(file),
      "1" = "data/CH4 rate/report_2022-06-27_day.xlsx",
      "2" = "data/CH4 rate/report_2022-07-21_day.xlsx",
      "3" = "data/CH4 rate/report_2022-07-21_day.xlsx",
      "4" = "data/CH4 rate/report_2022-08-08_day.xlsx",
      "5" = "data/CH4 rate/report_2022-10-10_day.xlsx",
      "6" = "data/CH4 rate/report_2022-11-01_day.xlsx",
      "7" = "data/CH4 rate/report_2022-12-20_day.xlsx",
      "8" = "data/CH4 rate/report_2023-01-10_day.xlsx",
      "9" = "data/CH4 rate/report_2023-02-02_day_S5.2.xlsx",
                  stop("Invalid data value. Please specify correct number.")
    )
    data <- read_excel(file_path, skip = 15)
    
    # Convert to data frame 
    data <- as.data.frame(data)

##Change names
if (file == 1){
names(data)[names(data) == "4-1 Volume [Nml]...2"] <- "Volume_6.1"
names(data)[names(data) == "4-1 Volume [Nml]...3"] <- "Volume_5.3"
names(data)[names(data) == "4-3 Volume [Nml]"] <- "Volume_5.2"
names(data)[names(data) == "3-1 Volume [Nml]"] <- "Volume_6.2"
names(data)[names(data) == "3-2 Volume [Nml]"] <- "Volume_5.1"
names(data)[names(data) == "3-3 Volume [Nml]"] <- "Volume_6.3"
names(data)[names(data) == "4-1 Flow [Nml/day]...17"] <- "Rate_6.1"
names(data)[names(data) == "4-2 Flow [Nml/day]...18"] <- "Rate_5.3"
names(data)[names(data) == "4-3 Flow [Nml/day]"] <- "Rate_5.2"
names(data)[names(data) == "3-1 Flow [Nml/day]"] <- "Rate_6.2"
names(data)[names(data) == "3-2 Flow [Nml/day]"] <- "Rate_5.1"
names(data)[names(data) == "3-3 Flow [Nml/day]"] <- "Rate_6.3"
} else if (file == 2){
  names(data)[names(data) == "4-1 Volume [Nml]...2"] <- "Volume_5.1"
  names(data)[names(data) == "1-1 Volume [Nml]"] <- "Volume_5.2"
  names(data)[names(data) == "4-3 Volume [Nml]"] <- "Volume_5.3"
  names(data)[names(data) == "3-1 Volume [Nml]"] <- "Volume_6.1"
  names(data)[names(data) == "3-2 Volume [Nml]"] <- "Volume_6.2"
  names(data)[names(data) == "3-3 Volume [Nml]"] <- "Volume_6.3"
  names(data)[names(data) == "4-1 Flow [Nml/day]...17"] <- "Rate_5.1"
  names(data)[names(data) == "1-1 Flow [Nml/day]"] <- "Rate_5.2"
  names(data)[names(data) == "4-3 Flow [Nml/day]"] <- "Rate_5.3"
  names(data)[names(data) == "3-1 Flow [Nml/day]"] <- "Rate_6.1"
  names(data)[names(data) == "3-2 Flow [Nml/day]"] <- "Rate_6.2"
  names(data)[names(data) == "3-3 Flow [Nml/day]"] <- "Rate_6.3"
  
} else if (file == 3){
  names(data)[names(data) == "1-2 Volume [Nml]"] <- "Volume_5.1"
  names(data)[names(data) == "1-3 Volume [Nml]"] <- "Volume_5.2"
  names(data)[names(data) == "1-3 new  Volume [Nml]"] <- "Volume_5.3"
  names(data)[names(data) == "4-1 Volume [Nml]...12"] <- "Volume_6.1"
  names(data)[names(data) == "4-2 Volume [Nml]...13"] <- "Volume_6.2"
  names(data)[names(data) == "4-1-no N Volume [Nml]"] <- "Volume_6.3"
  names(data)[names(data) == "1-2 Flow [Nml/day]"] <- "Rate_5.1"
  names(data)[names(data) == "1-3 Flow [Nml/day]"] <- "Rate_5.2"
  names(data)[names(data) == "1-3 new  Flow [Nml/day]"] <- "Rate_5.3"
  names(data)[names(data) == "4-1 Flow [Nml/day]...27"] <- "Rate_6.1"
  names(data)[names(data) == "4-2 Flow [Nml/day]...28"] <- "Rate_6.2"
  names(data)[names(data) == "4-1-no N Flow [Nml/day]"] <- "Rate_6.3"
} else if (file == 4|file ==5|file ==6){
    names(data)[names(data) == "4-1 Volume [Nml]...2"] <- "Volume_5.1"
    names(data)[names(data) == "1-1 Volume [Nml]"] <- "Volume_6.3"
    names(data)[names(data) == "4-3 Volume [Nml]"] <- "Volume_5.2"
    names(data)[names(data) == "3-1 Volume [Nml]"] <- "Volume_5.3"
    names(data)[names(data) == "3-2 Volume [Nml]"] <- "Volume_6.1"
    names(data)[names(data) == "3-3 Volume [Nml]"] <- "Volume_6.2"
    names(data)[names(data) == "4-1 Flow [Nml/day]...17"] <- "Rate_5.1"
    names(data)[names(data) == "1-1 Flow [Nml/day]"] <- "Rate_6.3"
    names(data)[names(data) == "4-3 Flow [Nml/day]"] <- "Rate_5.2"
    names(data)[names(data) == "3-1 Flow [Nml/day]"] <- "Rate_5.3"
    names(data)[names(data) == "3-2 Flow [Nml/day]"] <- "Rate_6.1"
    names(data)[names(data) == "3-3 Flow [Nml/day]"] <- "Rate_6.2"
} else if (file == 7){
    names(data)[names(data) == "m6.1 Volume [Nml]"] <- "Volume_6.1"
    names(data)[names(data) == "m6.2 Volume [Nml]"] <- "Volume_6.2"
    names(data)[names(data) == "m6.3 Volume [Nml]"] <- "Volume_6.3"
    names(data)[names(data) == "m6.1 Flow [Nml/day]"] <- "Rate_6.1"
    names(data)[names(data) == "m6.2 Flow [Nml/day]"] <- "Rate_6.2"
    names(data)[names(data) == "m6.3 Flow [Nml/day]"] <- "Rate_6.3"
} else if (file == 8|file == 9){
  names(data)[names(data) == "m6.1 Volume [Nml]"] <- "Volume_6.1"
  names(data)[names(data) == "m6.2 Volume [Nml]"] <- "Volume_6.2"
  names(data)[names(data) == "m6.3 Volume [Nml]"] <- "Volume_6.3"
  names(data)[names(data) == "m6.1 Flow [Nml/day]"] <- "Rate_6.1"
  names(data)[names(data) == "m6.2 Flow [Nml/day]"] <- "Rate_6.2"
  names(data)[names(data) == "m6.3 Flow [Nml/day]"] <- "Rate_6.3"
  names(data)[names(data) == "m5.1 Volume [Nml]"] <- "Volume_5.1"
  names(data)[names(data) == "m5.2 Volume [Nml]"] <- "Volume_5.2"
  names(data)[names(data) == "m5.3 Volume [Nml]"] <- "Volume_5.3"
  names(data)[names(data) == "m5.1 Flow [Nml/day]"] <- "Rate_5.1"
  names(data)[names(data) == "m5.2 Flow [Nml/day]"] <- "Rate_5.2"
  names(data)[names(data) == "m5.3 Flow [Nml/day]"] <- "Rate_5.3"
}
#CH4 ratio, assuming all gas produced is CO2 + CH4

  Ratio_CH4_6<-Per$GC_CH4_6/(Per$GC_CH4_6+Per$GC_CO2_6) #% of CH4 section 6
  Ratio_CH4_5<-Per$GC_CH4_5/(Per$GC_CH4_5+Per$GC_CO2_5) #% of CH4 section 5
  
  ## Interpolate the ratio at each day of the AMPTS working
  if (all(is.na(Ratio_CH4_5))) {
    # Skip interpolation
    warning("skipping interpolation of Section 5.")
    CH4_5_day<-rep(NaN,length(data$Day))
  } else {
    # Proceed with the interpolation
  Int_CH4<-approxfun(Per$Time_GC,Ratio_CH4_5)
  CH4_5_day<-Int_CH4(Per$Time_GC[1]+days(data$Day)) #CH4 Percentage from section 5 
  }
  Int_CH4<-approxfun(Per$Time_GC,Ratio_CH4_6)
  CH4_6_day<-Int_CH4(Per$Time_GC[1]+days(data$Day)) #CH4 Percentage from section 6 
  
  ## Multiply by the daily registered flow of the AMPTS
  fun_empty <- function(col) {
    if (length(col) == 0) {
      return(NaN)
    } else {
      return(col)
    }
  }
  CH4_rate_5.1<-fun_empty(data$Rate_5.1)*CH4_5_day
  CH4_rate_5.2<-fun_empty(data$Rate_5.2)*CH4_5_day
  CH4_rate_5.3<-fun_empty(data$Rate_5.3)*CH4_5_day
  CH4_rate_6.1<-fun_empty(data$Rate_6.1)*CH4_6_day
  CH4_rate_6.2<-fun_empty(data$Rate_6.2)*CH4_6_day
  CH4_rate_6.3<-fun_empty(data$Rate_6.3)*CH4_6_day
  CH4_volume_5.1<-fun_empty(data$Volume_5.1)*CH4_5_day
  CH4_volume_5.2<-fun_empty(data$Volume_5.2)*CH4_5_day

  CH4_volume_5.3<-fun_empty(data$Volume_5.3)*CH4_5_day
  CH4_volume_6.1<-fun_empty(data$Volume_6.1)*CH4_6_day
  CH4_volume_6.2<-fun_empty(data$Volume_6.2)*CH4_6_day
  CH4_volume_6.3<-fun_empty(data$Volume_6.3)*CH4_6_day
  df<-cbind.data.frame(data$Day,CH4_rate_5.1,CH4_rate_5.2,CH4_rate_5.3,
                       CH4_rate_6.1,CH4_rate_6.2,CH4_rate_6.3,
                       CH4_volume_5.1,CH4_volume_5.2,CH4_volume_5.3,
                       CH4_volume_6.1,CH4_volume_6.2,CH4_volume_6.3)
  return(df)
}




##GC points CH4-CO2% ratio
## Batch 1.1
Time_GC<-c(anytime("2022-05-31 18:00"),anytime("2022-06-01 14:00"),anytime("2022-06-09 13:00"),anytime("2022-06-16 15:40"),anytime("2022-06-23 14:17"))
GC_CO2_6<-c(0.01,12.86651,23.08894,14.26047,19.0001)
GC_CH4_6<-c(0,3.58975,21.35938,10.07769,13.66497)
GC_CO2_5<-c(01,12.95003,29.70341,15.93526,23.11955)
GC_CH4_5<-c(0,4.0252,14.947,8.33966,16.49749)
Per_B1.1<-cbind.data.frame(Time_GC,GC_CO2_6,GC_CH4_6,GC_CO2_5,GC_CH4_5)

## Batch 1.2
Time_GC<-c(anytime("2022-06-29 11:30"),anytime("2022-06-30 14:30"),
           anytime("2022-07-07 15:30"),anytime("2022-07-11 15:20"),anytime("2022-07-14 15:15"))
GC_CO2_6<-c(1.78271,11.26834,11.70188,8.3422,8.0)
GC_CH4_6<-c(0.846,13.01187,17.07820,13.05961,11)
GC_CO2_5<-c(2.26719,11.56293,12.47953,11.249,10.72820)
GC_CH4_5<-c(0.76234,13.54941,12.45159,9.8406,9.61081)
Per_B1.2<-cbind.data.frame(Time_GC,GC_CO2_6,GC_CH4_6,GC_CO2_5,GC_CH4_5)

## Batch 1.3
Time_GC<-c(anytime("2022-07-12 15:30"),anytime("2022-07-13 14:00"),
           anytime("2022-07-14 15:00"),anytime("2022-07-18 11:00"))
GC_CO2_6<-c(1.66,5.224,3.8656,5.27657)
GC_CH4_6<-c(6.398,1.92948,2.27474,14.71877)
GC_CO2_5<-c(0.5,4.5768,4.98112,5.99593)
GC_CH4_5<-c(0,1.01212,1.86376,10.29078)
Per_B1.3<-cbind.data.frame(Time_GC,GC_CO2_6,GC_CH4_6,GC_CO2_5,GC_CH4_5)

## Batch 2.1
Time_GC<-c(anytime("2022-08-30 18:00"),anytime("2022-08-31 14:40"),
           anytime("2022-09-05 14:00"),anytime("2022-09-12 13:40"))
GC_CO2_6<-c(0.726,2.22241,3.33,10.23056)
GC_CH4_6<-c(4.961,6.59062,6.53,22.99335)
GC_CO2_5<-c(1.10069,2.60554,3.05,9.21954)
GC_CH4_5<-c(7.94687,13.5803,9.38,20.12883)
Per_B2.1<-cbind.data.frame(Time_GC,GC_CO2_6,GC_CH4_6,GC_CO2_5,GC_CH4_5)

## Batch 2.2
Time_GC<-c(anytime("2022-09-27 16:10"),anytime("2022-09-28 14:00"),
           anytime("2022-09-29 15:00"),anytime("2022-10-03 11:00"),
           anytime("2022-10-06 15:30"))
GC_CO2_6<-c(1,8.50247,10.7258,13.18533,13.81099) #assume CH4% at time 0 is 0
GC_CH4_6<-c(0,2.04814,4.91063,15.74646,17.74339)
GC_CO2_5<-c(1,10.09473,14.291,17.87817,17.71294)
GC_CH4_5<-c(0,3.25516,6.79795,17.36876,16.99371)
Per_B2.2<-cbind.data.frame(Time_GC,GC_CO2_6,GC_CH4_6,GC_CO2_5,GC_CH4_5)

## Batch 2.3
Time_GC<-c(anytime("2022-10-11 15:00"),anytime("2022-10-12 14:30"),
           anytime("2022-10-13 15:40"),anytime("2022-10-17 11:00"),
           anytime("2022-10-20 15:00"),anytime("2022-10-27 12:30"))
GC_CO2_6<-c(1,5.55455,6.28119,8.01807,12.13807,9.14755) #assume CH4% at time 0 is 0
GC_CH4_6<-c(0,1.19723,2.53514,7.54611,13.10814,18.0594)
GC_CO2_5<-c(1,7.52815,8.84212,11.81126,9.29570,20.23340)
GC_CH4_5<-c(0,1.90831,3.77085,10.02434,11.9615,19.36434)
Per_B2.3<-cbind.data.frame(Time_GC,GC_CO2_6,GC_CH4_6,GC_CO2_5,GC_CH4_5)

# Batch 3.1.6
Time_GC<-c(anytime("2022-11-29 14:10"),anytime("2022-12-01 14:00"),
           anytime("2022-12-06 13:40"),anytime("2022-12-09 8:00"),
           anytime("2022-12-12 10:00"),anytime("2022-12-16 11:00"))
GC_CO2_6<-c(1,11.79,21.83125,21.61259,19.61627,16.58783) #assume CH4% at time 0 is 0
GC_CH4_6<-c(0,5.15,14.23396,15.78052,14.27426,12.85894)
GC_CO2_5<-c(NaN,NaN,NaN,NaN,NaN,NaN)
GC_CH4_5<-c(NaN,NaN,NaN,NaN,NaN,NaN)
Per_B3.1.6<-cbind.data.frame(Time_GC,GC_CO2_6,GC_CH4_6,GC_CO2_5,GC_CH4_5)

# Batch 3.2.6 & 3.1.5
Time_GC<-c(anytime("2022-12-20 14:00"),anytime("2022-12-22 17:00"),
           anytime("2022-12-27 14:40"),anytime("2022-12-29 15:00"),
           anytime("2023-01-02 12:20"),anytime("2023-01-10 13:30"))
GC_CO2_6<-c(NaN,NaN,1,7.14488,19.63230,16.58783) #assume CH4% at time 0 is 0
GC_CH4_6<-c(NaN,NaN,0,4.79897,17.47921,12.85894)
GC_CO2_5<-c(1,10.42986,4.44874,NaN,NaN,NaN)
GC_CH4_5<-c(0,11.88105,10,NaN,NaN,NaN)
Per_B3.2.6_3.1.5<-cbind.data.frame(Time_GC,GC_CO2_6,GC_CH4_6,GC_CO2_5,GC_CH4_5)

# Batch 3.3.6 & 3.2.5
Time_GC<-c(anytime("2023-01-10 14:00"),anytime("2023-01-12 16:30"),
           anytime("2023-01-17 15:00"),anytime("2023-01-19 16:00"),
           anytime("2023-01-23 16:45"),anytime("2023-02-02 15:20"))
GC_CO2_6<-c(1,5.41208,7.43516,7.11375,NaN,NaN) #assume CH4% at time 0 is 0
GC_CH4_6<-c(0,6.39168,14.43423,13.33303,NaN,NaN)
GC_CO2_5<-c(NaN,NaN,1,14.53255,25.09244,NaN)
GC_CH4_5<-c(NaN,NaN,0,9.55511,18.56507,NaN)
Per_B3.3.6_3.2.5<-cbind.data.frame(Time_GC,GC_CO2_6,GC_CH4_6,GC_CO2_5,GC_CH4_5)

#Plot the results
Ratios<-list(Per_B1.1,Per_B1.2,Per_B1.3,
             Per_B2.1,Per_B2.2,Per_B2.3,Per_B3.1.6,Per_B3.2.6_3.1.5,Per_B3.3.6_3.2.5)
Data_sets<-c(1,2,3,4,5,6,7,8,9) 
# 1<- Batch 1 first collection
# 2<- Batch 1 second collection
# 3<- Batch 1 third collection
# 4<- Batch 2 first collection
# 5<- Batch 2 second collection
# 6<- Batch 2 third collection
# 7<- Batch 3 first collection Section 6
# 8<- Batch 3 first collection Section 5, second collection section 6
# 9<- Batch 3 second collection Section 5, third collection section 6

plots<-list()
for (i in 7:9){ #Specify the files to read
  plots[[i]]<-CH4_rate(Ratios[[i]],Data_sets[i])
  
rate_5_1_1<-ggplot()+geom_point(aes(x=plots[[i]]$`data$Day` ,y=plots[[i]]$CH4_rate_5.1,color="Replicate 5.1"),size=2,shape=0)+
  geom_line(aes(x=plots[[i]]$`data$Day`,y=plots[[i]]$CH4_rate_5.1,color="Replicate 5.1"))+
  geom_point(aes(x=plots[[i]]$`data$Day`,y=plots[[i]]$CH4_rate_5.2,color="Replicate 5.2"),size=2,shape=0)+
  geom_line(aes(x=plots[[i]]$`data$Day`,y=plots[[i]]$CH4_rate_5.2,color="Replicate 5.2"))+
  geom_point(aes(x=plots[[i]]$`data$Day`,y=plots[[i]]$CH4_rate_5.3,color="Replicate 5.3"),size=2,shape=0)+
  geom_line(aes(x=plots[[i]]$`data$Day`,y=plots[[i]]$CH4_rate_5.3,color="Replicate 5.3"))+
  xlab("Time (days)")+
  scale_y_continuous(name = expression(paste("CH"[4]* " rate, NmL/day")))+
  theme_bw()+theme(panel.grid=element_blank())+
  theme(panel.border =element_rect(colour = "grey", fill=NA))+
  theme(axis.line = element_line(colour = "black"))+
  theme(panel.spacing.x=unit(2, "lines"))+
  theme(legend.text = element_text(size=15))+
  theme(legend.title = element_text(size=15))+
  guides(shape = guide_legend(override.aes = list(size = 5)))+
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=23.3))
  
  volume_5_1_1<-ggplot()+geom_point(aes(x=plots[[i]]$`data$Day`,y=plots[[i]]$CH4_volume_5.1,color="Replicate 5.1"),size=2,shape=0)+
    geom_line(aes(x=plots[[i]]$`data$Day`,y=plots[[i]]$CH4_volume_5.1,color="Replicate 5.1"))+
    geom_point(aes(x=plots[[i]]$`data$Day`,y=plots[[i]]$CH4_volume_5.2,color="Replicate 5.2"),size=2,shape=0)+
    geom_line(aes(x=plots[[i]]$`data$Day`,y=plots[[i]]$CH4_volume_5.2,color="Replicate 5.2"))+
    geom_point(aes(x=plots[[i]]$`data$Day`,y=plots[[i]]$CH4_volume_5.3,color="Replicate 5.3"),size=2,shape=0)+
    geom_line(aes(x=plots[[i]]$`data$Day`,y=plots[[i]]$CH4_volume_5.3,color="Replicate 5.3"))+
    xlab("Time (days)")+
    scale_y_continuous(name = expression(paste("CH"[4]* " volume, NmL")))+
    theme_bw()+theme(panel.grid=element_blank())+
    theme(panel.border =element_rect(colour = "grey", fill=NA))+
    theme(axis.line = element_line(colour = "black"))+
    theme(panel.spacing.x=unit(2, "lines"))+
    theme(legend.text = element_text(size=15))+
    theme(legend.title = element_text(size=15))+
    guides(shape = guide_legend(override.aes = list(size = 5)))+
    theme(axis.text=element_text(size=20),
          axis.title=element_text(size=23.3))
  
  rate_6_1_1<-ggplot()+geom_point(aes(x=plots[[i]]$`data$Day` ,y=plots[[i]]$CH4_rate_6.1,color="Replicate 6.1"),size=2,shape=0)+
    geom_line(aes(x=plots[[i]]$`data$Day`,y=plots[[i]]$CH4_rate_6.1,color="Replicate 6.1"))+
    geom_point(aes(x=plots[[i]]$`data$Day`,y=plots[[i]]$CH4_rate_6.2,color="Replicate 6.2"),size=2,shape=0)+
    geom_line(aes(x=plots[[i]]$`data$Day`,y=plots[[i]]$CH4_rate_6.2,color="Replicate 6.2"))+
    geom_point(aes(x=plots[[i]]$`data$Day`,y=plots[[i]]$CH4_rate_6.3,color="Replicate 6.3"),size=2,shape=0)+
    geom_line(aes(x=plots[[i]]$`data$Day`,y=plots[[i]]$CH4_rate_6.3,color="Replicate 6.3"))+
    xlab("Time (days)")+
    scale_y_continuous(name = expression(paste("CH"[4]* " rate, NmL/day")))+
    theme_bw()+theme(panel.grid=element_blank())+
    theme(panel.border =element_rect(colour = "grey", fill=NA))+
    theme(axis.line = element_line(colour = "black"))+
    theme(panel.spacing.x=unit(2, "lines"))+
    theme(legend.text = element_text(size=15))+
    theme(legend.title = element_text(size=15))+
    guides(shape = guide_legend(override.aes = list(size = 5)))+
    theme(axis.text=element_text(size=20),
          axis.title=element_text(size=23.3))
  
  volume_6_1_1<-ggplot()+geom_point(aes(x=plots[[i]]$`data$Day`,y=plots[[i]]$CH4_volume_6.1,color="Replicate 6.1"),size=2,shape=0)+
    geom_line(aes(x=plots[[i]]$`data$Day`,y=plots[[i]]$CH4_volume_6.1,color="Replicate 6.1"))+
    geom_point(aes(x=plots[[i]]$`data$Day`,y=plots[[i]]$CH4_volume_6.2,color="Replicate 6.2"),size=2,shape=0)+
    geom_line(aes(x=plots[[i]]$`data$Day`,y=plots[[i]]$CH4_volume_6.2,color="Replicate 6.2"))+
    geom_point(aes(x=plots[[i]]$`data$Day`,y=plots[[i]]$CH4_volume_6.3,color="Replicate 6.3"),size=2,shape=0)+
    geom_line(aes(x=plots[[i]]$`data$Day`,y=plots[[i]]$CH4_volume_6.3,color="Replicate 6.3"))+
    xlab("Time (days)")+
    scale_y_continuous(name = expression(paste("CH"[4]* " volume, NmL")))+
    theme_bw()+theme(panel.grid=element_blank())+
    theme(panel.border =element_rect(colour = "grey", fill=NA))+
    theme(axis.line = element_line(colour = "black"))+
    theme(panel.spacing.x=unit(2, "lines"))+
    theme(legend.text = element_text(size=15))+
    theme(legend.title = element_text(size=15))+
    guides(shape = guide_legend(override.aes = list(size = 5)))+
    theme(axis.text=element_text(size=20),
          axis.title=element_text(size=23.3))
  
  plot_list <- list(
    list(rate_5_1_1, volume_5_1_1, rate_6_1_1, volume_6_1_1)
  )
  
  # Define the file paths for the first, second, and third samples
  path_list <- list(
    "Batch 1/CH4 rate/First samples",
    "Batch 1/CH4 rate/Second samples",
    "Batch 1/CH4 rate/Third samples",
    "Batch 2/CH4 rate/First samples",
    "Batch 2/CH4 rate/Second samples",
    "Batch 2/CH4 rate/Third samples",
    "Batch 3/CH4 rate/First samples",
    "Batch 3/CH4 rate/Second samples",
    "Batch 3/CH4 rate/Third samples")
  
  # File names for the plots
  file_names <- c("Rate_S5.png", "Volume_S5.png", "Rate_S6.png", "Volume_S6.png")
  


    folder_path <- path_list[[i]]
         
    
    # Default range for j and folder path
    j_range <- 1:length(plot_list[[1]])
    folder_path_current <- folder_path
    
    # Special conditions for i == 7 and i == 8
    if (i == 7) {
      j_range <- 3:4
    } else if (i == 8) {
      j_range <- 1:4  # For all plots 1-4
      folder_path_current <- path_list[[i-1]]  # Change folder for 1:2 plots
    }
    
    # Loop through the range of j and save the plots
    for (j in j_range) {
      # For i == 8, use a different folder path for j == 1 or 2
      if ((i == 8 || i == 9) && j <= 2) {
        folder_path_current <- path_list[[i-1]]
      }
      
      ggsave(plot = plot_list[[1]][[j]],
             filename = file.path(folder_path_current, file_names[j]),
             width = 12, height = 5)
      print(paste("plot", i, "saved"))
    }
    
  ## Calculate CH4 rate in units of mg CH4/(Kg VS day) ##
  d_CH4<-0.657 #mg/mL
  VS<-read_excel("data/VS_slurry.xlsx")
  ## Collected slurry mass for each sample
  M_5.1<-c(408.65,391.47,384.77,394.19,383.26,400.88,NaN,394.27,400.93) 
  M_5.2<-c(389.42,419.6,389.97,394.97,397.35,405.43,NaN,397.16,400.26) 
  M_5.3<-c(410.36,398.37,398.95,393.69,383.73,399.71,NaN,393.41,400.17) 
  M_6.1<-c(400.04,392.56,377.63,405.11,406.03,409.16,413.54,399.74,400.9) 
  M_6.2<-c(391.59,403.08,403.31,401.18,418.11,400.83,404.15,406.41,401.79) 
  M_6.3<-c(738.59-347.61,406.1,391.03,403.37,385.67,399.43,409.49,400.10,403.06) 
  
  ## VS data
  ## Unit change
  d5 <- 0
  d6 <- 0
  d9<-0
  d8<-0
  if (i == 8){
    d5<-1
    d8<-1
  }
  if (i == 9){
    d9<-1
  }

  rates<-list()
  for (k in c("5.1", "5.2", "5.3","6.1", "6.2", "6.3")) {

    slurry_mass <- get(paste0("M_", k))[i]

    # Calculate the 24-hour rate
    if (k == "5.1" | k == "5.2" | k == "5.3"){
        rates[[paste0("Rate_", k, "_24")]] <- plots[[i-d5-d9]][[paste0("CH4_rate_", k)]][2] * d_CH4 / ((mean(VS$`VS_%`[c(i-d5+d9,i+d5-d9,i+d9)])/100) * (slurry_mass / 1000))
    } else {
      rates[[paste0("Rate_", k, "_24")]] <- plots[[i-d6]][[paste0("CH4_rate_", k)]][2] * d_CH4 / ((mean(VS$`VS_%`[c(i+13+2*d8,i+13+d5+d8+d9,i+13+2*d5+2*d9)])/100) * (slurry_mass / 1000))
          }
    # Calculate the maximum rate
    if (k == "5.1" | k == "5.2" | k == "5.3"){
    rates[[paste0("Rate_", k, "_max")]] <- max(plots[[i-d5-d9]][[paste0("CH4_rate_", k)]], na.rm = TRUE) * d_CH4 / 
      ((mean(VS$`VS_%`[c(i-d5+d9,i+d5-d9,i+d9)])/100) * (slurry_mass/ 1000))
    } else {
      rates[[paste0("Rate_", k, "_max")]] <- max(plots[[i-d6]][[paste0("CH4_rate_", k)]], na.rm = TRUE) * d_CH4 /
      ((mean(VS$`VS_%`[c(i+13+2*d8,i+13+d5+d8+d9,i+13+2*d5+2*d9)])/100) * (slurry_mass/ 1000))
               }
  }  
  rates_df <- as.data.frame(t(unlist(rates)))
  rates_df$Units<-"mg CH4/(Kg VS day)"
  # Write the data frame to a text file
  write.table(rates_df, file = file.path(folder_path_current, "Rates.txt"), row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)  
  print(paste("table", i, "saved"))

  ## Calculate CH4 rate in units of g CH4/(pig day) ##
  #Pigs
  Pigs_5<-c(302,271,268,284,272,270,NaN,279,271,270)
  Pigs_6<-c(302,265,181,282,265,272,294,272,270)
  kgS_5<-c(42304.9952,59226.99328,42304.9952,25382.99712,52881.244,
           33843.99616,NaN,67687.99232,42304.9952,44420.24496)
  kgS_6<-c(43362.62008,61342.24304,31728.7464,23267.74736,50765.99424,
           31728.7464,40189.74544,57111.74352,54996.49376)
  
  rates2<-list()
  for (l in c("5.1", "5.2", "5.3","6.1", "6.2", "6.3")) {
    
    # Calculate the 24-hour rate
    if (l == "5.1" | l == "5.2" | l == "5.3"){
      rates2[[paste0("Rate_", l, "_24")]] <- ((plots[[i-d5-d9]][[paste0("CH4_rate_", l)]][2] * d_CH4)/1000) / (Pigs_5[i] * (slurry_mass/ 1000))
    } else{
      rates2[[paste0("Rate_", l, "_24")]] <- ((plots[[i-d6]][[paste0("CH4_rate_", l)]][2] * d_CH4)/1000) / (Pigs_6[i] * (slurry_mass/ 1000))
          }
    # Calculate the maximum rate
    if (l == "5.1" | l == "5.2" | l == "5.3"){
      rates2[[paste0("Rate_", l, "_max")]] <- ((max(plots[[i]][[paste0("CH4_rate_", l)]], na.rm = TRUE) * d_CH4)/1000) / (Pigs_5[i] * (slurry_mass/ 1000))
    }  else{
      rates2[[paste0("Rate_", l, "_max")]] <- ((max(plots[[i]][[paste0("CH4_rate_", l)]], na.rm = TRUE) * d_CH4)/1000) / (Pigs_6[i] * (slurry_mass/ 1000))
    }
  }
  rates2_df <- as.data.frame(t(unlist(rates2)))
  rates2_df[1:6]<-rates2_df[1:6]*kgS_5[i]
  rates2_df[7:12]<-rates2_df[7:12]*kgS_6[i]
  rates2_df$Units<-"g CH4/(pig day)"
  # Write the data frame to a text file
  write.table(rates2_df, file = file.path(folder_path, "Rates2.txt"), row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)  
  print(paste("table2", i, "saved")) 
  }
  
  
  
  
  
  
  
  
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
   
   
   