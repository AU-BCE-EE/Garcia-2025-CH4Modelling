

### Microbial analysis data ###

# --- Load data ----
#DNA
metadata <- read_excel(here("data/DNA","metadataPablo.xlsx"))
otutable_V1V8 <- read_excel(here("data/DNA","otutable_pablo_v1-8.xlsx"))
otutable_mcra <- read_excel(here("data/DNA","otutable_paclo_mcra.xlsx"))
dat_qPCR <- read_excel(here("data/DNA","qPCR.xlsx"))

#Proteins
dat_prot<-read.table(here("data/Proteins","Tax_counts3.tsv"),header=TRUE, sep = "\t")


# --- create ampvis object (DNA) ----
# All microbes
d_V1V8 <- amp_load(otutable = otutable_V1V8,
                   metadata = metadata)
# Methanogens
d_mcra <- amp_load(otutable = otutable_mcra,
                   metadata = metadata)

# --- Prepare DNA data to plot ----
df_mcra<-amp_export_long(d_mcra)

Mcra<-tapply(df_mcra$count,list(df_mcra$Genus,df_mcra$sample_type),sum,na.rm=TRUE)
Rel_Mcra<-as.data.frame(Mcra)
Rel_Mcra$Genus<-c("Unidentified","Candidatus_Methanomethylophilus","Methanobrevibacter",
                  "Methanocorpusculum","Methanoculleus","Methanomassiliicoccales","Methanosaeta",
                  "Methanosphaera","Methanospirillum","organism_methanogenic")
class(Rel_Mcra)<-c("data.table","data.frame")

datlong <- melt(Rel_Mcra, id.vars = c("Genus"))

datlong[,sum :=sum(value),by=c("variable")]
datlong[,relative :=value/sum*100,by=c("variable")]

datlong$variable <- factor(datlong$variable, levels = c("Time0_Solid_Before","Time0_Before","Time0_After", "Time0_Solid_After",  
                                                        "Time1_Before","Time1_After", "Time1_Liquid", 
                                                        "Time2_Before","Time2_After", 
                                                        "Time3_Before","Time3_After"))
datlong$Genus <- factor(datlong$Genus, levels = c("Unidentified","Candidatus_Methanomethylophilus", "organism_methanogenic",  
                                                  "Methanospirillum","Methanomassiliicoccales", "Methanoculleus", 
                                                  "Methanocorpusculum","Methanosaeta", 
                                                  "Methanobrevibacter","Methanosphaera"))

datlong$widths<-1
datlong$widths[which(datlong$variable=="Time0_Solid_Before")]<-1
datlong$widths[which(datlong$variable=="Time0_Before")]<-2
datlong$widths[which(datlong$variable=="Time0_After")]<-3.5
datlong$widths[which(datlong$variable=="Time0_Solid_After")]<-4.5
datlong$widths[which(datlong$variable=="Time1_Before")]<-6.5
datlong$widths[which(datlong$variable=="Time1_After")]<-8
datlong$widths[which(datlong$variable=="Time1_Liquid")]<-9
datlong$widths[which(datlong$variable=="Time2_Before")]<-11
datlong$widths[which(datlong$variable=="Time2_After")]<-12.5
datlong$widths[which(datlong$variable=="Time3_Before")]<-14.5
datlong$widths[which(datlong$variable=="Time3_After")]<-16

class(dat_qPCR)<-c("data.table","data.frame")
dat_qPCR$widths<-1
dat_qPCR$widths[which(dat_qPCR$SampleID=="Time0_Solid_Before")]<-1
dat_qPCR$widths[which(dat_qPCR$SampleID=="Time0_Before")]<-2
dat_qPCR$widths[which(dat_qPCR$SampleID=="Time0_After")]<-3.5
dat_qPCR$widths[which(dat_qPCR$SampleID=="Time0_Solid_After")]<-4.5
dat_qPCR$widths[which(dat_qPCR$SampleID=="Time1_Before")]<-6.5
dat_qPCR$widths[which(dat_qPCR$SampleID=="Time1_After")]<-8
dat_qPCR$widths[which(dat_qPCR$SampleID=="Time1_Liquid_After")]<-9
dat_qPCR$widths[which(dat_qPCR$SampleID=="Time2_Before")]<-11
dat_qPCR$widths[which(dat_qPCR$SampleID=="Time2_After")]<-12.5
dat_qPCR$widths[which(dat_qPCR$SampleID=="Time3_Before")]<-14.5
dat_qPCR$widths[which(dat_qPCR$SampleID=="Time3_After")]<-16

##Add CH4 emissions
Batch4$Breaks<-c(1.5,4,6.5,8.5,11,12.5,14.5,16)
##Scaling secondary axis
range_y1 <- range(dat_qPCR$`Cell count mcrA`/0.1)
range_y2 <- range(Batch4$Mean_CH4/Batch4$Volume)
scale_factor <- max(range_y1) / max(range_y2)

# --- Prepare Protein data to plot ----
Genus<-rep(dat_prot$genus,11)
Tax_counts<-c(dat_prot$PG1_HK_JLN_REMESH,dat_prot$PG2_HK_JLN_REMESH,dat_prot$PG3_HK_JLN_REMESH,
              dat_prot$PG4_HK_JLN_REMESH,dat_prot$PG5_HK_JLN_REMESH,dat_prot$PG6_HK_JLN_REMESH,
              dat_prot$PG7_HK_JLN_REMESH,dat_prot$PG8_HK_JLN_REMESH,dat_prot$PG9_HK_JLN_REMESH,
              dat_prot$PG10_HK_JLN_REMESH,dat_prot$PG11_HK_JLN_REMESH)
SampleID<-c(rep("Time0_Before",nrow(dat_prot)),rep("Time0_Solid_Before",nrow(dat_prot)),rep("Time0_After",nrow(dat_prot)),
            rep("Time0_Solid_After",nrow(dat_prot)),rep("Time1_Before",nrow(dat_prot)),rep("Time1_After",nrow(dat_prot)),
            rep("Time1_Liquid_After",nrow(dat_prot)),rep("Time2_Before",nrow(dat_prot)),rep("Time2_After",nrow(dat_prot)),
            rep("Time3_Before",nrow(dat_prot)),rep("Time3_After",nrow(dat_prot)))

df_prots<-cbind.data.frame(Genus,
                           Tax_counts,SampleID)
matches <- grepl("Methano", df_prots$Genus)
df_Methanogens<-df_prots[matches,]
class(df_Methanogens)<-c("data.table","data.frame")

df_Methanogens[,sum :=sum(Tax_counts,na.rm=TRUE),by=c("SampleID")]
df_Methanogens[,relative :=Tax_counts/sum*100,by=c("SampleID")]

df_Methanogens$SampleID <- factor(df_Methanogens$SampleID, levels = c("Time0_Solid_Before","Time0_Before","Time0_After", "Time0_Solid_After",  
                                                                      "Time1_Before","Time1_After", "Time1_Liquid_After", 
                                                                      "Time2_Before","Time2_After", 
                                                                      "Time3_Before","Time3_After"))

## ---- Select more abundants ----
Abundance <- df_Methanogens[, .(sum = sum(relative,na.rm=TRUE)),
                            by = .( Genus)]
A_order<-Abundance[order(-sum), ]
Most_abundant_genus<-A_order$Genus[1:9]
Most_abundant_genus<-c(Most_abundant_genus,"Methanosphaera","Methanoplanus","Methanothrix")
df_Activity<-df_Methanogens[df_Methanogens$Genus %in% Most_abundant_genus, ]

Abundance_per_sample <- df_Methanogens[, .(sum = sum(relative,na.rm=TRUE)),
                                       by = .( SampleID,Genus)]
Abundance_per_sample[Abundance_per_sample$Genus == "Methanosphaera",]

Abundance_per_sample[401:440,][order(-sum)]



sum_relative <- df_Activity[, sum(relative,na.rm=TRUE), by = SampleID]

diff_relative <- 100 - sum_relative$V1

# Create a new data.table with the rows to be added
new_rows <- data.table(Genus = "Rest",
                       Tax_counts = NA,
                       SampleID = sum_relative$SampleID,
                       sum = diff_relative,
                       relative = diff_relative)

# Bind the new rows to the original data.table
df_Activity <- rbind(df_Activity, new_rows)

df_Activity$widths<-1
df_Activity$widths[which(df_Activity$SampleID=="Time0_Solid_Before")]<-1
df_Activity$widths[which(df_Activity$SampleID=="Time0_Before")]<-2
df_Activity$widths[which(df_Activity$SampleID=="Time0_After")]<-3.5
df_Activity$widths[which(df_Activity$SampleID=="Time0_Solid_After")]<-4.5
df_Activity$widths[which(df_Activity$SampleID=="Time1_Before")]<-6.5
df_Activity$widths[which(df_Activity$SampleID=="Time1_After")]<-8
df_Activity$widths[which(df_Activity$SampleID=="Time1_Liquid_After")]<-9
df_Activity$widths[which(df_Activity$SampleID=="Time2_Before")]<-11
df_Activity$widths[which(df_Activity$SampleID=="Time2_After")]<-12.5
df_Activity$widths[which(df_Activity$SampleID=="Time3_Before")]<-14.5
df_Activity$widths[which(df_Activity$SampleID=="Time3_After")]<-16

##Plot
df_Activity$Genus <- factor(df_Activity$Genus, levels = c("Rest","Methanothrix", "Methanosphaera","Methanoplanus", "Methanocaldococcus",  
                                                          "Methanobacterium","Methanospirillum", "Methanofollis", 
                                                          "Methanohalophilus","Methanolobus", 
                                                          "Methanobrevibacter","Methanosarcina","Methanoculleus"))
