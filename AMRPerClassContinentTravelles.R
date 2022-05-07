# Continent by antimicrobial class

# Lactamases
lactamasesmean <- df %>%
  group_by(Continent)%>%
  summarise(mean=mean(`Î².Lactamases.Presence`, exclude.NA=T)/1*100) # get mean and then percentage - 1 Abx class

lactamasesmean <- lactamasesmean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

lactamasegraphic <- ggplot(lactamasesmean, aes(x=Continent, y=percentage, fill=Continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Lactamase")

# Carbapenemase
Carbapenemasemean <- df %>%
  group_by(Continent) %>%
  summarise(mean=mean(Carbapenemase.Presence, exclude.NA=T)/1*100) # get mean and then percentage - 1 Abx class

Carbapenemasemean <- Carbapenemasemean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

Carbapenemasegraphic <- ggplot(Carbapenemasemean, aes(x=Continent, y=percentage, fill=Continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Carbapenemase")

# Aminoglycosides
Aminoglycosidemean <- df %>%
  group_by(Continent) %>%
  summarise(mean=mean(Aminoglycosides.Presence, exclude.NA=T)/1*100) # get mean and then percentage - 1 Abx class

Aminoglycosidemean <- Aminoglycosidemean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

Aminoglycosidegraphic <- ggplot(Aminoglycosidemean, aes(x=Continent, y=percentage, fill=Continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Aminoglycoside")

#Flouroquinolone
Fluoroquinolonemean <- df %>%
  group_by(Continent) %>%
  summarise(mean=mean(Fluoroquinolones.Presence, exclude.NA=T)/1*100) # get mean and then percentage - 1 Abx class

Fluoroquinolonemean <- Fluoroquinolonemean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

Fluoroquinolonegraphic <- ggplot(Fluoroquinolonemean, aes(x=Continent, y=percentage, fill=Continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Fluoroquinolone")

#Macrolide
Macrolidemean <- df %>%
  group_by(Continent) %>%
  summarise(mean=mean(Macrolides.Presence, exclude.NA=T)/1*100) # get mean and then percentage - 1 Abx class

Macrolidemean <- Macrolidemean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

Macrolidegraphic <- ggplot(Macrolidemean, aes(x=Continent, y=percentage, fill=Continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Macrolide")

#Trimethoprim
Trimethoprimmean <- df %>%
  group_by(Continent) %>%
  summarise(mean=mean(Trimethoprim.Presence, exclude.NA=T)/1*100) # get mean and then percentage - 1 Abx class

Trimethoprimmean <- Trimethoprimmean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

Trimethoprimgraphic <- ggplot(Trimethoprimmean, aes(x=Continent, y=percentage, fill=Continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Trimethoprim")

#Fosfomycin
Fosfomycinmean <- df %>%
  group_by(Continent) %>%
  summarise(mean=mean(Fosfomycin.Presence, exclude.NA=T)/1*100) # get mean and then percentage - 1 Abx class

Fosfomycinmean <- Fosfomycinmean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

Fosfomycingraphic <- ggplot(Fosfomycinmean, aes(x=Continent, y=percentage, fill=Continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Fosfomycin")

#Tetracycline
Tetracyclinemean <- df %>%
  group_by(Continent) %>%
  summarise(mean=mean(Tetracyclines.Presence, exclude.NA=T)/1*100) # get mean and then percentage - 1 Abx class

Tetracyclinemean <- Tetracyclinemean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

Tetracyclinegraphic <- ggplot(Tetracyclinemean, aes(x=Continent, y=percentage, fill=Continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Tetracycline")

#Sulphonamide
Sulphonamidemean <- df %>%
  group_by(Continent) %>%
  summarise(mean=mean(Sulphonamides.Presence, exclude.NA=T)/1*100) # get mean and then percentage - 1 Abx class

Sulphonamidemean <- Sulphonamidemean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

Sulphonamidegraphic <- ggplot(Sulphonamidemean, aes(x=Continent, y=percentage, fill=Continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Sulphonamide")

#Chloramphenicol
Chloramphenicolmean <- df %>%
  group_by(Continent) %>%
  summarise(mean=mean(Chloramphenicol.Presence, exclude.NA=T)/1*100) # get mean and then percentage - 1 Abx class

Chloramphenicolmean <- Chloramphenicolmean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

Chloramphenicolgraphic <- ggplot(Chloramphenicolmean, aes(x=Continent, y=percentage, fill=Continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Chloramphenicol")

#Rifamycin
Rifamycinmean <- df %>%
  group_by(Continent) %>%
  summarise(mean=mean(Rifamycins.Presence, exclude.NA=T)/1*100) # get mean and then percentage - 1 Abx class

Rifamycinmean <- Rifamycinmean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

Rifamycingraphic <- ggplot(Rifamycinmean, aes(x=Continent, y=percentage, fill=Continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Rifamycin")

#Joining the graphs - excluded disinfectants as cannot fit
Continent_AMR_Class<-ggarrange(lactamasegraphic, Aminoglycosidegraphic, Fluoroquinolonegraphic, 
                               Trimethoprimgraphic, Fosfomycingraphic, Tetracyclinegraphic,
                               Sulphonamidegraphic, Chloramphenicolgraphic)

#Adding the title and the axis
annotate_figure(Continent_AMR_Class, 
                top=text_grob("Percentage Level of Resistance Against the Different Antibiotic Classes Per Continent Travelled From"), 
                #bottom=text_grob("Continent"), 
                left = text_grob("Level of resistance (%)",rot = 90, vjust = 1))
