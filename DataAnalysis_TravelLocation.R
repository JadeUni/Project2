############### Set the working directory ################
getwd()
setwd("C:/Users/jadea/Documents/Project 2")
workingdir = "."
getwd() # This is just to check the change worked.

#packages
library(dplyr)
library(tidyverse)
library(reshape2)
library(lessR)
library(plotly)
library(ggplot2)
library(ggpubr)


#################################################################################

#Import data 
df <- read.csv(file = "TyphiData_Location.csv")
df <- as.data.frame(df)


############# AMR per given exact travel location ###############
Salmonella_Typhi_mean <- df
Salmonella_Typhi_mean <- Salmonella_Typhi_mean %>%
  rowwise() %>%
  mutate(total = sum(c_across(12:22))) %>% # add column total which sums up AMR
  group_by(Travel) %>%
  summarise(mean=mean(total)) # find mean of AMR by travel location

Salmonella_Typhi_mean <- Salmonella_Typhi_mean %>%
  mutate(percentage=mean/sum(mean)*100) #Convert to %

Salmonella_Typhi_graphic <- ggplot(Salmonella_Typhi_mean, aes(x=Travel, y=percentage, fill=Travel)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle(" ")


#Adding the title and the axis
annotate_figure(Salmonella_Typhi_graphic, 
                top=text_grob("Salmonella typhi Antimicrobial Resistance Percentage Per Travel Location"), 
                bottom=text_grob(" "), 
                left = text_grob("Level of resistance (%)",rot = 90, vjust = 1))

############# AMR per travel location (Continent) ###############
Salmonella_Typhi_mean_Continent <- df
Salmonella_Typhi_mean_Continent<- Salmonella_Typhi_mean_Continent %>%
  rowwise() %>%
  mutate(total = sum(c_across(12:22))) %>% # add column total which sums up AMR
  group_by(Continent) %>%
  summarise(mean=mean(total)) # find mean of AMR by travel location

Salmonella_Typhi_mean_Continent <- Salmonella_Typhi_mean_Continent %>%
  mutate(percentage=mean/sum(mean)*100) #Convert to %

Salmonella_Typhi_Continent_graphic <- ggplot(Salmonella_Typhi_mean_Continent, aes(x=Continent, y=percentage, fill=Continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle(" ")


#Adding the title and the axis
annotate_figure(Salmonella_Typhi_Continent_graphic, 
                top=text_grob(" "), 
                bottom=text_grob(" "), 
                left = text_grob("Level of resistance (%)",rot = 90, vjust = 1))
