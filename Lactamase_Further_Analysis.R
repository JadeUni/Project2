######## Random Forest ML Algorithm - Project 2 ##########

############### Set the working directory ################
getwd()
setwd("C:/Users/jadea/Documents/Project 2/Datasets")
workingdir = "."
getwd() # This is just to check the change worked. 

################## Load in R Packages ###################
# Load R Packages
library(ggplot2)
library(reshape2)
library(dplyr) #Reformat Datasets
## To install its: install.packages("Package_Name")

################# Load in datasets #######################
Potential_New_Resistance <- readRDS("Potential_New_Resistance_Lactamase.rds")

################## Separating the data sets ##################

############ Resistant Predicted as Susceptible ##############
# Examples in the testing dataset where resistant bacteria were predicted to be susceptible
ResistanceToSusceptible <- Potential_New_Resistance[-which(Potential_New_Resistance$match == "Yes"), ]
ResistanceToSusceptible <- ResistanceToSusceptible[-which(ResistanceToSusceptible$AMR == "Susceptible"), ]
ResistanceToSusceptible <- ResistanceToSusceptible %>% select(Reference.No., X250, X100, X50, X25, X10, X5, X0)

##########Save datasets - Resistant Predicted as Susceptible ####################
write.csv(x=ResistanceToSusceptible, file="ResistanceToSusceptible_Lactamase.csv")
DataAnalysis2 <- read.csv("ResToSusLactamase_SNPDistances2.csv")

# Examples in the testing dataset where susceptible bacteria were predicted to be resistant
SusceptibleToResistant <- Potential_New_Resistance[-which(Potential_New_Resistance$match == "Yes"), ]
SusceptibleToResistant <- SusceptibleToResistant[-which(SusceptibleToResistant$AMR == "Resistant"), ]

##########Save datasets - Susceptible Predicted as Resistant ####################
write.csv(x=SusceptibleToResistant, file="SusceptibleToResistance_Lactamase.csv")
DataAnalysis_SusToRes <- read.csv("SusToResLactamase_SNPDistances.csv")

# Examples in the testing dataset where the bacteria was accurately predicted as resistant
Resistant <- Potential_New_Resistance[-which(Potential_New_Resistance$match == "No"), ]
Resistant <- Resistant[-which(Resistant$AMR == "Susceptible"), ]

##########Save datasets - Resistant ####################
write.csv(x=Resistant, file="Resistant_Lactamase.csv")
DataAnalysis_Res <- read.csv("Res_Lactamase_SNPDistances.csv")

# Examples in the testing dataset where the bacteria was accurately predicted as susceptible
Susceptible <- Potential_New_Resistance[-which(Potential_New_Resistance$match == "No"), ]
Susceptible <- Susceptible[-which(Susceptible$AMR == "Resistant"), ]

##########Save datasets - Susceptible ####################
write.csv(x=Susceptible, file="Susceptible_Lactamase.csv")
DataAnalysis_Sus <- read.csv("Sus_Lactamase_SNPDistances.csv")

############## Plotting the cluster graphs ##################
####### Resistance to Susceptible - FQ ##################
DataAnalysis2$SNP.Threshold <- as.character(DataAnalysis2$SNP.Threshold)
DataAnalysis2$SNP.Threshold <- factor(DataAnalysis2$SNP.Threshold, levels = unique(DataAnalysis2$SNP.Threshold))

ggplot(DataAnalysis2, aes(x = SNP.Threshold, y = Distance)) +
  geom_point() +
  #ylim(0, 1300) +
  scale_y_continuous(breaks = seq(1, 1301, by=50)) +
  theme_bw() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(colour = "grey60", linetype = "dashed"),
    axis.text.x = element_text(angle = 60, hjust = 1)
  )

####### Susceptible To Resistant ##################
DataAnalysis_SusToRes$SNP.Threshold <- as.character(DataAnalysis_SusToRes$SNP.Threshold)
DataAnalysis_SusToRes$SNP.Threshold <- factor(DataAnalysis_SusToRes$SNP.Threshold, levels = unique(DataAnalysis_SusToRes$SNP.Threshold))

ggplot(DataAnalysis_SusToRes, aes(x = SNP.Threshold, y = Distance)) +
  geom_point() +
  #ylim(0, 1300) +
  scale_y_continuous(breaks = seq(1, 1301, by=50)) +
  theme_bw() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(colour = "grey60", linetype = "dashed"),
    axis.text.x = element_text(angle = 60, hjust = 1)
  )

####### Resistant ##################
DataAnalysis_Res$SNP.Threshold <- as.character(DataAnalysis_Res$SNP.Threshold)
DataAnalysis_Res$SNP.Threshold <- factor(DataAnalysis_Res$SNP.Threshold, levels = unique(DataAnalysis_Res$SNP.Threshold))

ggplot(DataAnalysis_Res, aes(x = SNP.Threshold, y = Distance)) +
  geom_point() +
  #ylim(0, 1300) +
  scale_y_continuous(breaks = seq(1, 1301, by=50)) +
  theme_bw() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(colour = "grey60", linetype = "dashed"),
    axis.text.x = element_text(angle = 60, hjust = 1)
  )

####### Susceptible ##################
DataAnalysis_Sus$SNP.Threshold <- as.character(DataAnalysis_Sus$SNP.Threshold)
DataAnalysis_Sus$SNP.Threshold <- factor(DataAnalysis_Sus$SNP.Threshold, levels = unique(DataAnalysis_Sus$SNP.Threshold))

ggplot(DataAnalysis_Sus, aes(x = SNP.Threshold, y = Distance)) +
  geom_point() +
  #ylim(0, 1300) +
  scale_y_continuous(breaks = seq(1, 1301, by=50)) +
  theme_bw() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(colour = "grey60", linetype = "dashed"),
    axis.text.x = element_text(angle = 60, hjust = 1)
  )

####### Combined Graph ##################
final <- read.csv("Clustering_Lactamase_Final.csv")

final$SNP.Threshold <- as.character(final$SNP.Threshold)
final$SNP.Threshold <- factor(final$SNP.Threshold, levels = unique(final$SNP.Threshold))

ggplot(final, aes(x = SNP.Threshold, y = Distance, color = Key)) +
  geom_point() +
  #ylim(0, 1300) +
  scale_y_continuous(breaks = seq(1, 1401, by=50)) +
  theme_bw() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(colour = "grey60", linetype = "dashed"),
    axis.text.x = element_text(angle = 60, hjust = 1)
  )