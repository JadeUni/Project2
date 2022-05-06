############### Set the working directory ################
getwd()
setwd("C:/Users/jadea/Documents/Project 2")
workingdir = "."
getwd() # This is just to check the change worked.

# load library
library(tm)
library(Hmisc)

#Import data 
df <- read.csv(file = "TyphiData.csv")
year_data <- as.data.frame(df$Year)
travel_data <- as.data.frame(df$Travel)

# Summary statistics 
describe(year_data) 
describe(travel_data)

############ Evolution of AMR across time for every antimicrobial class ############

# Install packages
library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyverse)

# lactamases
lactamasesmean <- df %>%
  group_by(Year) %>%
  summarise(mean=mean(`Î².Lactamases.Presence`, exclude.NA=T)/1*100) # get mean and then percentage - 1 Abx class

lactamasesgraphic <- ggplot(lactamasesmean, aes(Year,mean)) + 
  geom_point() + 
  ggtitle("Lactamases") + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5)) + 
  geom_smooth() + 
  ylim(0,100) + 
  xlab("") + 
  ylab("")

# Carbapenemase
Carbapenemasemean <- df %>%
  group_by(Year)%>%
  summarise(mean=mean(Carbapenemase.Presence, exclude.NA=T)/1*100)

Carbapenemasegraphic <- ggplot(Carbapenemasemean, aes(Year,mean)) + 
  geom_point() + 
  ggtitle("Carbapenemase") + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5)) + 
  labs("Carbapenemase") + 
  geom_smooth() + 
  ylim(0,100) +
  xlab("") + 
  ylab("")

# Aminoglycoside
Aminoglycosidemean <- df %>%
  group_by(Year)%>%
  summarise(mean=mean(Aminoglycosides.Presence, exclude.NA=T)/1*100)

Aminoglycosidegraphic <- ggplot(Aminoglycosidemean, aes(Year,mean)) + 
  geom_point() + 
  ggtitle("Aminoglycoside") + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5)) + 
  labs("Aminoglycoside") + 
  geom_smooth() + 
  ylim(0,100) +
  xlab("") + 
  ylab("")

# Fluoroquinolone
Fluoroquinolonemean <- df %>%
  group_by(Year)%>%
  summarise(mean=mean(Fluoroquinolones.Presence, exclude.NA=T)/1*100)

Fluoroquinolonegraphic <- ggplot(Fluoroquinolonemean, aes(Year,mean)) + 
  geom_point() + 
  ggtitle("Fluoroquinolone") + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5)) + 
  labs("Fluoroquinolone") + 
  geom_smooth() + 
  ylim(0,100) +
  xlab("") + 
  ylab("")

# Macrolide
Macrolidemean <- df %>%
  group_by(Year)%>%
  summarise(mean=mean(Macrolides.Presence, exclude.NA=T)/1*100)

Macrolidegraphic <- ggplot(Macrolidemean, aes(Year,mean)) + 
  geom_point() + 
  ggtitle("Macrolide") + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5)) + 
  labs("Macrolide") + 
  geom_smooth() + 
  ylim(0,100) +
  xlab("") + 
  ylab("")

# Trimethoprim
Trimethoprimmean <- df %>%
  group_by(Year)%>%
  summarise(mean=mean(Trimethoprim.Presence, exclude.NA=T)/1*100)

Trimethoprimgraphic <- ggplot(Trimethoprimmean, aes(Year,mean)) + 
  geom_point() + 
  ggtitle("Trimethoprim") + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5)) + 
  labs("Trimethoprim") + 
  geom_smooth() + 
  ylim(0,100) +
  xlab("") + 
  ylab("")

# Fosfomycin
Fosfomycinmean <- df %>%
  group_by(Year)%>%
  summarise(mean=mean(Fosfomycin.Presence, exclude.NA=T)/1*100)

Fosfomycingraphic <- ggplot(Fosfomycinmean, aes(Year,mean)) + 
  geom_point() + 
  ggtitle("Fosfomycin") + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5)) + 
  labs("Fosfomycin") + 
  geom_smooth() + 
  ylim(0,100) +
  xlab("") + 
  ylab("")

# Tetracycline
Tetracyclinemean <- df %>%
  group_by(Year)%>%
  summarise(mean=mean(Tetracyclines.Presence, exclude.NA=T)/1*100)

Tetracyclinegraphic <- ggplot(Tetracyclinemean, aes(Year,mean)) + 
  geom_point() + 
  ggtitle("Tetracycline") + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5)) + 
  labs("Tetracycline") + 
  geom_smooth() + 
  ylim(0,100) +
  xlab("") + 
  ylab("")

# Sulphonamide
Sulphonamidemean <- df %>%
  group_by(Year)%>%
  summarise(mean=mean(Sulphonamides.Presence, exclude.NA=T)/1*100)

Sulphonamidegraphic <- ggplot(Sulphonamidemean, aes(Year,mean)) + 
  geom_point() + 
  ggtitle("Sulphonamide") + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5)) + 
  labs("Sulphonamide") + 
  geom_smooth() + 
  ylim(0,100) +
  xlab("") + 
  ylab("")

# Chloramphenicol
Chloramphenicolmean <- df %>%
  group_by(Year)%>%
  summarise(mean=mean(Chloramphenicol.Presence, exclude.NA=T)/1*100)

Chloramphenicolgraphic <- ggplot(Chloramphenicolmean, aes(Year,mean)) + 
  geom_point() + 
  ggtitle("Chloramphenicol") + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5)) + 
  labs("Chloramphenicol") + 
  geom_smooth() + 
  ylim(0,100) +
  xlab("") + 
  ylab("")

# Rifamycin
Rifamycinmean <- df %>%
  group_by(Year)%>%
  summarise(mean=mean(Rifamycins.Presence, exclude.NA=T)/1*100)

Rifamycingraphic <- ggplot(Rifamycinmean, aes(Year,mean)) + 
  geom_point() + 
  ggtitle("Rifamycin") + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5)) + 
  labs("Rifamycin") + 
  geom_smooth() + 
  ylim(0,100) +
  xlab("") + 
  ylab("")

#Joining the graphs - excluded disinfectants as cannot fit
AMRclassestime<-ggarrange(Aminoglycosidegraphic, Carbapenemasegraphic, Chloramphenicolgraphic,
                          Fluoroquinolonegraphic, Fosfomycingraphic, lactamasesgraphic, Macrolidegraphic,
                          Rifamycingraphic, Sulphonamidegraphic, Tetracyclinegraphic, Trimethoprimgraphic)

#Adding the title and the axis
annotate_figure(AMRclassestime, 
                top=text_grob(" "), 
                bottom=text_grob("Year"), 
                left = text_grob("Level of resistance (%)",rot = 90, vjust = 1))
