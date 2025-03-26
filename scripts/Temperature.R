#### Read temperature data from METEMIS project ###

library(readxl)
library(dplyr)
library(ggplot2)
library(anytime)
library(lubridate)
library("readxl")
library(writexl)

### BATCH 1 ###


## Section 5 data
      setwd(".../Batch 1") #Choose own directory
  
      list_of_files<- list.files(path="Section 5",pattern = "*.xlsx",recursive=TRUE) ##Read data from Picarro
      L<-length(list_of_files)
      
      setwd(".../Batch 1/Section 5") #Choose own directory
      
      S5_1<-list()
      ###Read all PTR-MS files 
      
      for(i in 1:L){ #Loop through the numbers of ID's instead of the ID's
        
        
        
        S5_1[[i]]<-read_excel(list_of_files[i])}
      
      for(i in 1:L){
        S5_1[[i]]<-as.data.frame(S5_1[[i]])
        
      }
      S5_1<-bind_rows(S5_1, .id = "column_label") #Join all elements of the list in one dataframe
      S5_1<-S5_1[order(S5_1$`Date-Time (CEST)`),]
      S5_1$Group<-as.character(S5_1$Group)
      
## Section 6 data
      setwd(".../Batch 1") #Choose own directory
      
      
      list_of_files<- list.files(path="Section 6",pattern = "*.xlsx",recursive=TRUE) ##Read data from Picarro
      L<-length(list_of_files)
      
      setwd(".../Section 6") #Choose own directory
      
      S6_1<-list()
      ###Read all PTR-MS files 
      
      for(i in 1:L){ #Loop through the numbers of ID's instead of the ID's
        
        
        
        S6_1[[i]]<-read_excel(list_of_files[i])}
      
      for(i in 1:L){
        S6_1[[i]]<-as.data.frame(S6_1[[i]])
        
      }
      ### Small correction on Sensor 6.2 I was not able to press the buttom and the sensor was laying in Foulum at this time
     S6_1[[15]]<- S6_1[[15]][-(2014:2451),]
      ############################################
     
      S6_1<-bind_rows(S6_1, .id = "column_label") #Join all elements of the list in one dataframe
      S6_1<-S6_1[order(S6_1$`Date-Time (CEST)`),]
      S6_1$Group<-as.character(S6_1$Group)
      
      
      
      ### Batch 2 ###
      
     
      ## Section 5 data
      setwd(".../Batch 2") #Choose own directory
      
      list_of_files<- list.files(path="Section 5",pattern = "*.xlsx",recursive=TRUE) ##Read data from Picarro
      L<-length(list_of_files)
      
      setwd(".../Batch 2/Section 5")
      
      S5_2<-list()
      ###Read all PTR-MS files 
      
      for(i in 1:L){ #Loop through the numbers of ID's instead of the ID's
        
        
        
        S5_2[[i]]<-read_excel(list_of_files[i])}
      
      for(i in 1:L){
        S5_2[[i]]<-as.data.frame(S5_2[[i]])
        
      }
      S5_2<-bind_rows(S5_2, .id = "column_label") #Join all elements of the list in one dataframe
      S5_2<-S5_2[order(S5_2$`Date-Time (CEST)`),]
      S5_2$Group<-as.character(S5_2$Group)
      
      ## Change of time adjustment
      Time<-c(S5_2$`Date-Time (CEST)`,S5_2$`Date-Time (CET)`)
      Time<-na.omit(Time)
      S5_2$Time<-Time
      #####################
      
      ## Section 6 data
      setwd(".../Batch 2") #Choose own directory
      
      
      list_of_files<- list.files(path="Section 6",pattern = "*.xlsx",recursive=TRUE) ##Read data from Picarro
      L<-length(list_of_files)
      
      setwd(".../Batch 2/Section 6") #Choose own directory
      
      S6_2<-list()
      ###Read all PTR-MS files 
      
      for(i in 1:L){ #Loop through the numbers of ID's instead of the ID's
        
        
        
        S6_2[[i]]<-read_excel(list_of_files[i])}
      
      for(i in 1:L){
        S6_2[[i]]<-as.data.frame(S6_2[[i]])
        
      }
      S6_2<-bind_rows(S6_2, .id = "column_label") #Join all elements of the list in one dataframe
      S6_2<-S6_2[order(S6_2$`Date-Time (CEST)`),]
      S6_2$Group<-as.character(S6_2$Group)
      
      ## Change of time adjustment
      Time<-c(S6_2$`Date-Time (CEST)`,S6_2$`Date-Time (CET)`)
      Time<-na.omit(Time)
          S6_2$Time<-Time
      #####################
      
            ### Batch 3
      
      ## Section 5 data
      setwd(".../Batch 3")
      
      list_of_files<- list.files(path="Section 5",pattern = "*.xlsx",recursive=TRUE) ##Read data from Picarro
      L<-length(list_of_files)
      
      setwd(".../Batch 3/Section 5")
      
      S5_3<-list()
      ###Read all PTR-MS files 
      
      for(i in 1:L){ #Loop through the numbers of ID's instead of the ID's
        
        
        
        S5_3[[i]]<-read_excel(list_of_files[i])}
      
      for(i in 1:L){
        S5_3[[i]]<-as.data.frame(S5_3[[i]])
        
      }
      S5_3<-bind_rows(S5_3, .id = "column_label") #Join all elements of the list in one dataframe
      S5_3<-S5_3[order(S5_3$`Date-Time (CET)`),]
      S5_3$Group<-as.character(S5_3$Group)
      

      #####################
      
      ## Section 6 data
      setwd(".../Batch 3")
      
      
      list_of_files<- list.files(path="Section 6",pattern = "*.xlsx",recursive=TRUE) ##Read data from Picarro
      L<-length(list_of_files)
      
      setwd(".../Batch 3/Section 6")
      
      S6_3<-list()
      ###Read all PTR-MS files 
      
      for(i in 1:L){ #Loop through the numbers of ID's instead of the ID's
        
        
        
        S6_3[[i]]<-read_excel(list_of_files[i])}
      
      for(i in 1:L){
        S6_3[[i]]<-as.data.frame(S6_3[[i]])
        
      }
      S6_3<-bind_rows(S6_3, .id = "column_label") #Join all elements of the list in one dataframe
      S6_3<-S6_3[order(S6_3$`Date-Time (CET)`),]
      S6_3$Group<-as.character(S6_3$Group)
      
      ## Change of time adjustment
     
      #####################
      
      ### Plot temperature data
      
  
###### Temperature data frame ##########

      Time_S5<-c(S5_1$`Date-Time (CEST)`,S5_2$Time,S5_3$`Date-Time (CET)`)
      Temperature_S5<-c(S5_1$`Ch: 1 - Temperature   (°C)`,S5_2$`Ch: 1 - Temperature   (°C )`,S5_3$`Ch: 1 - Temperature   (°C )`)
      Sensor_Position_S5<-c(S5_1$Group,S5_2$Group,S5_3$Group)
      
      Time_S6<-c(S6_1$`Date-Time (CEST)`,S6_2$Time,S6_3$`Date-Time (CET)`)
      Temperature_S6<-c(S6_1$`Ch: 1 - Temperature   (°C)`,S6_2$`Ch: 1 - Temperature   (°C )`,S6_3$`Ch: 1 - Temperature   (°C )`)
      Sensor_Position_S6<-c(S6_1$Group,S6_2$Group,S6_3$Group)
      
      df_S5<-cbind.data.frame(Time_S5,Temperature_S5,Sensor_Position_S5)
      df_S6<-cbind.data.frame(Time_S6,Temperature_S6,Sensor_Position_S6)

      
      library(dplyr)
      library(lubridate)
      
      
      
      T_S5<-df_S5 %>% 
        mutate(Time_S5 = floor_date(Time_S5, "hour")) %>%
        group_by(Time_S5) %>%
        summarize(Temperature_S5 = mean(Temperature_S5))
      
      T_S6<-df_S6 %>% 
        mutate(Time_S6 = floor_date(Time_S6, "hour")) %>%
        group_by(Time_S6) %>%
        summarize(Temperature_S6 = mean(Temperature_S6))

      
      Sys.setlocale("LC_TIME", "en_US.UTF-8")      
      
T5<-ggplot()+geom_line(aes(df_S5$Time_S5[which(df_S5$Sensor_Position_S5=="5.1")],df_S5$Temperature_S5[which(df_S5$Sensor_Position_S5=="5.1")],color="5.1"))+
  geom_line(aes(df_S5$Time_S5[which(df_S5$Sensor_Position_S5=="5.2")],df_S5$Temperature_S5[which(df_S5$Sensor_Position_S5=="5.2")],color="5.2"))+
  geom_line(aes(df_S5$Time_S5[which(df_S5$Sensor_Position_S5=="5.3")],df_S5$Temperature_S5[which(df_S5$Sensor_Position_S5=="5.3")],color="5.3"))+
  geom_line(aes(T_S5$Time_S5,T_S5$Temperature_S5),color="black")+
  xlab("")+ylab("Temperature, °C")+theme_bw()+ggtitle("a) Section 1")+
  xlim(c(min(df_S5$Time_S5[which(df_S5$Sensor_Position_S5=="5.1")]),
         max(df_S5$Time_S5[which(df_S5$Sensor_Position_S5=="5.1")])))+
  theme(legend.title = element_blank())


T6<-ggplot()+geom_line(aes(df_S6$Time_S6[which(df_S6$Sensor_Position_S6=="6.1")],df_S6$Temperature_S6[which(df_S6$Sensor_Position_S6=="6.1")],color="6.1"))+
  geom_line(aes(df_S6$Time_S6[which(df_S6$Sensor_Position_S6=="6.2")],df_S6$Temperature_S6[which(df_S6$Sensor_Position_S6=="6.2")],color="6.2"))+
  geom_line(aes(df_S6$Time_S6[which(df_S6$Sensor_Position_S6=="6.3")],df_S6$Temperature_S6[which(df_S6$Sensor_Position_S6=="6.3")],color="6.3"))+
  geom_line(aes(T_S6$Time_S6,T_S6$Temperature_S6),color="black")+
  xlab("")+ylab("Temperature, °C")+theme_bw()+ggtitle("b) Section 2")+
  theme(legend.title = element_blank())

FigS1 <- plot_grid(T5,T6, ncol = 1, align = "hv")
ggsave(plot=FigS1,"plots/FigureS2.png", width = 8, height = 9,bg='white')

