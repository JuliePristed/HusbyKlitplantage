## Data exploration and model preparation
## Husby Klitplantage, Masters thesis
## Script by Julie Pristed
## Start: January 2021

#install.packages('multcompView')
#install.packages("spdep", dependencies=TRUE)
#install.packages('readr')
#install.packages('usdm')
#install.packages('car')
#install.packages("MASS", dependencies=TRUE)
#install.packages("stargazer")
#install.packages("MuMIn")

rm(list=ls()) #clear environment
showTmpFiles() #show temporary files
removeTmpFiles(h = 0) #clear all temporary files

# Working Directory
# setwd("/Users/JuliePristed/Documents/Dokumenter/AU/A. Kandidat/A. Speciale/Data")

# Softcoding working directory
Dir.Base <- getwd() # to find the project folder
Dir.Data <- file.path(Dir.Base, "Miljødata.csv") # index the data folder

# Loading packages
library(dplyr) # Data manipulation
library(DescTools) # Computing Dunn's post hoc test 
library(nlme) # Model constructions
library(ggplot2) # Plot layout options
library(multcompView) # Grouping according to anova results
library(car) # Variance inflation measurement
library(spdep) # Spatial autocorrelation analysis - have not done this yet
library(MASS) # Model selection
library(stargazer) # structuring model output
library(MuMIn) # Calculating R squared for mixed effect models

# Loading datasets
Environment <- read.csv("Miljødata.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
Environment$Habitat.vurdering <- as.factor(Environment$Habitat.vurdering)
str(Environment)

Resultater_alle_index <- read.csv("Resultater_alle_index.csv")

# Creating subsets 
# Data subset without NA's ####
Environment_NA <- Environment %>% 
  dplyr::select(Plot.ID,
                Artsindex,
                Strukturindex,
                Naturtilstandsindex,
                Habitat.code,
                Habitat.vurdering,
                Zone,
                Zone_col,
                Zone_orto,
                SuccessionTime,
                Successionstid_orto,
                Afstand.hav,
                SRI,
                Retning.klasse,
                Hældning,
                Elevation,
                Lys,
                Kroneåbning15,
                Kronehøjde15,
                Ved,
                Førnelag,
                Organisk.lag,
                Blandingslag,
                OrganiskM) %>%
  na.omit(Environment_NA)
str(Environment_NA)

# Creating data subset without NA's for each dry habitat type ####
Environment_NA2100 <- dplyr::filter(Environment_NA, Habitat.vurdering == "2100")
str(Environment_NA2100)
Environment_NA2120 <- filter(Environment_NA, Habitat.vurdering == "2120")
Environment_NA2130 <- filter(Environment_NA, Habitat.vurdering == "2130")
Environment_NA2140 <- filter(Environment_NA, Habitat.vurdering == "2140")
Environment_NA2170 <- filter(Environment_NA, Habitat.vurdering == "2170")

# Creating data subsets for each zone ####
#(will come in handy in a minute)
Environment_zone2014 <- dplyr::filter(Environment, Zone_orto == "2014")
str(Environment_zone2014)

Environment_zone2017 <- filter(Environment, Zone_orto == "2017")
Environment_zone2018 <- filter(Environment, Zone_orto == "2018")
Environment_zone2019 <- filter(Environment, Zone_orto == "2019")
Environment_zoneNaturligVegetation <- filter(Environment, Zone_orto == "Naturlig Vegetation")
Environment_zonePlantage <- filter(Environment, Zone_orto == "Plantage")

# Creating data subset containing only cleared areas ####
Environment_zoneClearing <- Environment %>%
  dplyr::filter(Zone_orto != "Plantage") %>%
  dplyr::filter(Zone_orto != "Naturlig Vegetation")


# Creating data subsets for each habitat type ####
Environment_2100 <- dplyr::filter(Environment, Habitat.vurdering == "2100")
str(Environment_2100)
Environment_2120 <- filter(Environment, Habitat.vurdering == "2120")
Environment_2130 <- filter(Environment, Habitat.vurdering == "2130")
Environment_2140 <- filter(Environment, Habitat.vurdering == "2140")
Environment_2170 <- filter(Environment, Habitat.vurdering == "2170")
Environment_2190 <- filter(Environment, Habitat.vurdering == "2190")

Environment_2190NA <- Environment %>% 
  dplyr::select(Plot.ID,
                Artsindex,
                Strukturindex,
                Naturtilstandsindex,
                Habitat.code,
                Habitat.vurdering,
                Zone,
                Zone_col,
                Zone_orto,
                SuccessionTime,
                Successionstid_orto,
                Afstand.hav,
                SRI,
                Retning.klasse,
                Hældning,
                Elevation,
                Lys,
                Kroneåbning15,
                Kronehøjde15,
                Ved,
                Vandprøve.pH,
                Vandflade,
                Førnelag,
                Organisk.lag,
                Blandingslag,
                OrganiskM) %>%
  na.omit(Environment_2190NA)
Environment_2190NA <- filter(Environment_2190NA, Habitat.vurdering == "2190")
str(Environment_2190NA)

# Creating data subsets without plots close to roads ####
# Filtering plots <5m from roads away
Environment5road <- Environment %>%
  filter(X5mRoad != "Yes")

  # Filtering plots <10m from roads away
Environment10road <- Environment %>%
  filter(X10mRoad != "Yes")

# Creating data subset containing only numeric variables for analysis ####
Env.num <- Environment %>%
  dplyr::select(Plot.ID, Artsindex, Strukturindex, Naturtilstandsindex, # Choosing variables
         SuccessionTime,
         Afstand.hav,
         SRI,
         Hældning,
         Elevation,
         Førnelag,
         Organisk.lag,
         OrganiskM) %>%
  dplyr::group_by(Plot.ID) %>%   # defining data from plots ids
  na.omit(Env.num)               # getting rid of NA's
str(Env.num)


# Creating matrix without natural vegetation ####
clearing_plantationMatrix <- Environment %>% 
  dplyr::select(Plot.ID,
         Artsindex,
         Habitat.code,
         SuccessionTime,
         Afstand.hav,
         Retning.klasse,
         SRI,
         Hældning,
         Elevation,
         Lys,
         OrganiskM,
         Ved) %>%
  filter(Environment$Successionstid_orto != "25") %>% # Filtering away plots that were cleared 26 years ago, i.e. Natural
  group_by(Plot.ID)                       # Grouping by plot id

head(clearing_plantationMatrix)
#========================================================================================================
#      Normality
#========================================================================================================
# Histograms and Shapiro-Wilk test using Species Index ####
(artsindex_hist <- ggplot(Environment, aes(x = Artsindex))                     # Defining variable in esthetics
                + geom_histogram()   )                                         # Choosing histogram 
shapiro.test(Environment$Artsindex) # Test value less than 0.05 = significant problems with normality
# Normality OK
# Beautifying
(artsindex_hist <- ggplot(Environment, aes(x = Artsindex)) +                
    geom_histogram(binwidth = 0.02, colour = "grey", fill = "darkgrey") +     # Changing the binwidth (datawise) and colours
    geom_vline(aes(xintercept = mean(Artsindex)),                             # Adding a line for mean Naturtilstandsindex
               colour = "red", linetype = "dashed", size=1) +                 # Changing the look of the line
    theme_classic() +                                                         # Changing the theme to get rid of the grey background
    ylab("Count\n# Plots") +                                                  # Changing the text of the y axis label
    xlab("\nArtsindex")  +                                                    # \n adds a blank line between axis and text
    theme(axis.text = element_text(size = 12),                                # Changing font size of axis labels and title
          axis.title.x = element_text(size = 14, face = "plain"),             # face="plain" is the default, can be changed it to italic, bold, etc. 
          panel.grid = element_blank(),                                       # Removing the grey grid lines
          plot.margin = unit(c(1,1,1,1), units = , "cm")))                    # Putting a 1 cm margin around the plot


# Checking each zone seperately
# 2014
(artsindex_hist2014 <- ggplot(Environment_zone2014, aes(x = Artsindex)) +                
    geom_histogram(binwidth = 0.01, colour = "grey", fill = "darkgrey") +    
    geom_vline(aes(xintercept = mean(Artsindex)),                            
               colour = "red", linetype = "dashed", size=1)) 
shapiro.test(Environment_zone2014$Artsindex) # Normality OK


#2017
(artsindex_hist2017 <- ggplot(Environment_zone2017, aes(x = Artsindex)) +                
    geom_histogram(binwidth = 0.02, colour = "grey", fill = "darkgrey") +    
    geom_vline(aes(xintercept = mean(Artsindex)),                            
               colour = "red", linetype = "dashed", size=1)) 
shapiro.test(Environment_zone2017$Artsindex) # Problems with Normality

hist(sqrt(Environment_zone2017$Artsindex), breaks = 20)
shapiro.test(sqrt(Environment_zone2017$Artsindex)) # Problems become worse...

#2018
(artsindex_hist2018 <- ggplot(Environment_zone2018, aes(x = Artsindex)) +                
    geom_histogram(binwidth = 0.02, colour = "grey", fill = "darkgrey") +    
    geom_vline(aes(xintercept = mean(Artsindex)),                            
               colour = "red", linetype = "dashed", size=1)) 
shapiro.test(Environment_zone2018$Artsindex) # Normality OK

#2019
(artsindex_hist2019 <- ggplot(Environment_zone2019, aes(x = Artsindex)) +                
    geom_histogram(binwidth = 0.04, colour = "grey", fill = "darkgrey") +   
    geom_vline(aes(xintercept = mean(Artsindex)),                             
               colour = "red", linetype = "dashed", size=1)) 
shapiro.test(Environment_zone2019$Artsindex) # Normality OK

#Natural vegetation
(artsindex_hist_nat <- ggplot(Environment_zoneNaturligVegetation, aes(x = Artsindex)) +                
    geom_histogram(binwidth = 0.02, colour = "grey", fill = "darkgrey") +    
    geom_vline(aes(xintercept = mean(Artsindex)),                             
               colour = "red", linetype = "dashed", size=1)) 
shapiro.test(Environment_zoneNaturligVegetation$Artsindex) # Normality OK

#Plantation
(artsindex_hist_plan <- ggplot(Environment_zonePlantage, aes(x = Artsindex)) +                
    geom_histogram(binwidth = 0.02, colour = "grey", fill = "darkgrey") +    
    geom_vline(aes(xintercept = mean(Artsindex)),                             
               colour = "red", linetype = "dashed", size=1))
shapiro.test(Environment_zonePlantage$Artsindex) # Problems with Normality

# Histograms and Shapiro-Wilk test using Structure Index ####
(Strukturindex_hist <- ggplot(Environment, aes(x = Strukturindex))            # Defining variable in esthetics
 + geom_histogram()   )                                                       # Choosing histogram 
shapiro.test(Environment$Strukturindex) # Normality OK

# Beautifying
(Strukturindex_hist <- ggplot(Environment, aes(x = Strukturindex)) +                
    geom_histogram(binwidth = 0.02, colour = "grey", fill = "darkgrey") +     # Changing the binwidth (datawise) and colours
    geom_vline(aes(xintercept = mean(Strukturindex)),                             # Adding a line for mean Naturtilstandsindex
               colour = "red", linetype = "dashed", size=1) +                 # Changing the look of the line
    theme_classic() +                                                         # Changing the theme to get rid of the grey background
    ylab("Count\n# Plots") +                                                  # Changing the text of the y axis label
    xlab("\nStrukturindex")  +                                                    # \n adds a blank line between axis and text
    theme(axis.text = element_text(size = 12),                                # Changing font size of axis labels and title
          axis.title.x = element_text(size = 14, face = "plain"),             # face="plain" is the default, can be changed it to italic, bold, etc. 
          panel.grid = element_blank(),                                       # Removing the grey grid lines
          plot.margin = unit(c(1,1,1,1), units = , "cm")))                    # Putting a 1 cm margin around the plot

# Checking each zone seperately
# 2014
(Strukturindex_hist2014 <- ggplot(Environment_zone2014, aes(x = Strukturindex)) +                
   geom_histogram(binwidth = 0.02, colour = "grey", fill = "darkgrey") +    
   geom_vline(aes(xintercept = mean(Strukturindex)),                            
              colour = "red", linetype = "dashed", size=1)) 
shapiro.test(Environment_zone2012$Strukturindex) # Normality OK

#2017
(Strukturindex_hist2017 <- ggplot(Environment_zone2017, aes(x = Strukturindex)) +                
    geom_histogram(binwidth = 0.02, colour = "grey", fill = "darkgrey") +    
    geom_vline(aes(xintercept = mean(Strukturindex)),                            
               colour = "red", linetype = "dashed", size=1)) 
shapiro.test(Environment_zone2017$Strukturindex) # Normality OK

#2018
(Strukturindex_hist2018 <- ggplot(Environment_zone2018, aes(x = Strukturindex)) +                
    geom_histogram(binwidth = 0.02, colour = "grey", fill = "darkgrey") +    
    geom_vline(aes(xintercept = mean(Strukturindex)),                            
               colour = "red", linetype = "dashed", size=1)) 
shapiro.test(Environment_zone2018$Strukturindex) # Normality OK

#2019
(Strukturindex_hist2019 <- ggplot(Environment_zone2019, aes(x = Strukturindex)) +                
    geom_histogram(binwidth = 0.02, colour = "grey", fill = "darkgrey") +   
    geom_vline(aes(xintercept = mean(Strukturindex)),                             
               colour = "red", linetype = "dashed", size=1)) 
shapiro.test(Environment_zone2019$Strukturindex) # Normality OK

#Natural 
(Strukturindex_hist_nat <- ggplot(Environment_zoneNaturligVegetation, aes(x = Strukturindex)) +                
    geom_histogram(binwidth = 0.02, colour = "grey", fill = "darkgrey") +    
    geom_vline(aes(xintercept = mean(Strukturindex)),                             
               colour = "red", linetype = "dashed", size=1)) 
shapiro.test(Environment_zoneNaturligVegetation$Strukturindex) # Normality OK

#Plantation
(Strukturindex_hist_plan.n <- ggplot(Environment_zonePlantage, aes(x = Strukturindex)) +                
    geom_histogram(binwidth = 0.02, colour = "grey", fill = "darkgrey") +    
    geom_vline(aes(xintercept = mean(Strukturindex)),                             
               colour = "red", linetype = "dashed", size=1))
shapiro.test(Environment_zonePlantage$Strukturindex) # Normality OK 


# Histograms and Shapiro-Wilk test using Nature condition Index ####
(Naturtilstandsindex_hist <- ggplot(Environment, aes(x = Naturtilstandsindex))            # Defining variable in esthetics
 + geom_histogram()   )                                                                   # Choosing histogram 
shapiro.test(Environment$Naturtilstandsindex) # Normality OK

# Beautifying
(Naturtilstandsindex_hist <- ggplot(Environment, aes(x = Naturtilstandsindex)) +                
    geom_histogram(binwidth = 0.02, colour = "grey", fill = "darkgrey") +     # Changing the binwidth (datawise) and colours
    geom_vline(aes(xintercept = mean(Naturtilstandsindex)),                             # Adding a line for mean Naturtilstandsindex
               colour = "red", linetype = "dashed", size=1) +                 # Changing the look of the line
    theme_classic() +                                                         # Changing the theme to get rid of the grey background
    ylab("Count\n# Plots") +                                                  # Changing the text of the y axis label
    xlab("\nNaturtilstandsindex")  +                                                    # \n adds a blank line between axis and text
    theme(axis.text = element_text(size = 12),                                # Changing font size of axis labels and title
          axis.title.x = element_text(size = 14, face = "plain"),             # face="plain" is the default, can be changed it to italic, bold, etc. 
          panel.grid = element_blank(),                                       # Removing the grey grid lines
          plot.margin = unit(c(1,1,1,1), units = , "cm")))                    # Putting a 1 cm margin around the plot

# Checking each zone seperately
# 2014
(Naturtilstandsindex_hist2014 <- ggplot(Environment_zone2014, aes(x = Naturtilstandsindex)) +                
   geom_histogram(binwidth = 0.02, colour = "grey", fill = "darkgrey") +    
   geom_vline(aes(xintercept = mean(Naturtilstandsindex)),                            
              colour = "red", linetype = "dashed", size=1)) 
shapiro.test(Environment_zone2014$Naturtilstandsindex) # Normality OK

#2017
(Naturtilstandsindex_hist2017 <- ggplot(Environment_zone2017, aes(x = Naturtilstandsindex)) +                
    geom_histogram(binwidth = 0.02, colour = "grey", fill = "darkgrey") +    
    geom_vline(aes(xintercept = mean(Naturtilstandsindex)),                            
               colour = "red", linetype = "dashed", size=1)) 
shapiro.test(Environment_zone2017$Naturtilstandsindex) # Normality OK

#2018
(Naturtilstandsindex_hist2018 <- ggplot(Environment_zone2018, aes(x = Naturtilstandsindex)) +                
    geom_histogram(binwidth = 0.02, colour = "grey", fill = "darkgrey") +    
    geom_vline(aes(xintercept = mean(Naturtilstandsindex)),                            
               colour = "red", linetype = "dashed", size=1)) 
shapiro.test(Environment_zone2018$Naturtilstandsindex) # Normality OK

#2019
(Naturtilstandsindex_hist2019 <- ggplot(Environment_zone2019, aes(x = Naturtilstandsindex)) +                
    geom_histogram(binwidth = 0.02, colour = "grey", fill = "darkgrey") +   
    geom_vline(aes(xintercept = mean(Naturtilstandsindex)),                             
               colour = "red", linetype = "dashed", size=1)) 
shapiro.test(Environment_zone2019$Naturtilstandsindex) # Normality OK

#Natural vegetation
(Naturtilstandsindex_hist_nat <- ggplot(Environment_zoneNaturligVegetation, aes(x = Naturtilstandsindex)) +                
    geom_histogram(binwidth = 0.02, colour = "grey", fill = "darkgrey") +    
    geom_vline(aes(xintercept = mean(Naturtilstandsindex)),                             
               colour = "red", linetype = "dashed", size=1)) 
shapiro.test(Environment_zoneNaturligVegetation$Naturtilstandsindex) # Normality OK

#Plantation
(Naturtilstandsindex_hist_plan <- ggplot(Environment_zonePlantage, aes(x = Naturtilstandsindex)) +                
    geom_histogram(binwidth = 0.02, colour = "grey", fill = "darkgrey") +    
    geom_vline(aes(xintercept = mean(Naturtilstandsindex)),                             
               colour = "red", linetype = "dashed", size=1))
shapiro.test(Environment_zonePlantage$Naturtilstandsindex) # Problems with Normality

# Histograms and Shapiro-Wilk test using Species index and habitat types ####
# 2100
(Artsindex_hist_2100 <- ggplot(Environment_2100, aes(x = Artsindex)) +                
   geom_histogram(binwidth = 0.02, colour = "grey", fill = "darkgrey") +    
   geom_vline(aes(xintercept = mean(Artsindex)),                             
              colour = "red", linetype = "dashed", size=1)) 
shapiro.test(Environment_2100$Artsindex) # Normality OK

# 2120
(Artsindex_hist_2120 <- ggplot(Environment_2120, aes(x = Artsindex)) +                
    geom_histogram(binwidth = 0.02, colour = "grey", fill = "darkgrey") +    
    geom_vline(aes(xintercept = mean(Artsindex)),                             
               colour = "red", linetype = "dashed", size=1)) 
shapiro.test(Environment_2120$Artsindex) # Normality OK

# 2130
(Artsindex_hist_2130 <- ggplot(Environment_2130, aes(x = Artsindex)) +                
    geom_histogram(binwidth = 0.02, colour = "grey", fill = "darkgrey") +    
    geom_vline(aes(xintercept = mean(Artsindex)),                             
               colour = "red", linetype = "dashed", size=1)) 
shapiro.test(Environment_2130$Artsindex) # Normality OK

# 2140
(Artsindex_hist_2140 <- ggplot(Environment_2140, aes(x = Artsindex)) +                
    geom_histogram(binwidth = 0.02, colour = "grey", fill = "darkgrey") +    
    geom_vline(aes(xintercept = mean(Artsindex)),                             
               colour = "red", linetype = "dashed", size=1)) 
shapiro.test(Environment_2140$Artsindex) # Normality OK

# 2170
(Artsindex_hist_2170 <- ggplot(Environment_2170, aes(x = Artsindex)) +                
    geom_histogram(binwidth = 0.02, colour = "grey", fill = "darkgrey") +    
    geom_vline(aes(xintercept = mean(Artsindex)),                             
               colour = "red", linetype = "dashed", size=1)) 
shapiro.test(Environment_2170$Artsindex) # Normality OK

# 2190
(Artsindex_hist_2190 <- ggplot(Environment_2190, aes(x = Artsindex)) +                
    geom_histogram(binwidth = 0.02, colour = "grey", fill = "darkgrey") +    
    geom_vline(aes(xintercept = mean(Artsindex)),                             
               colour = "red", linetype = "dashed", size=1)) 
shapiro.test(Environment_2190$Artsindex) # Normality OK

# Histograms and Shapiro-Wilk test using Structure index and habitat types ####
# 2100
(Strukturindex_hist_2100 <- ggplot(Environment_2100, aes(x = Strukturindex)) +                
   geom_histogram(binwidth = 0.02, colour = "grey", fill = "darkgrey") +    
   geom_vline(aes(xintercept = mean(Strukturindex)),                             
              colour = "red", linetype = "dashed", size=1)) 
shapiro.test(Environment_2100$Strukturindex) # Normality OK

# 2120
(Strukturindex_hist_2120 <- ggplot(Environment_2120, aes(x = Strukturindex)) +                
    geom_histogram(binwidth = 0.02, colour = "grey", fill = "darkgrey") +    
    geom_vline(aes(xintercept = mean(Strukturindex)),                             
               colour = "red", linetype = "dashed", size=1)) 
shapiro.test(Environment_2120$Strukturindex) # problems with normality

# 2130
(Strukturindex_hist_2130 <- ggplot(Environment_2130, aes(x = Strukturindex)) +                
    geom_histogram(binwidth = 0.02, colour = "grey", fill = "darkgrey") +    
    geom_vline(aes(xintercept = mean(Strukturindex)),                             
               colour = "red", linetype = "dashed", size=1)) 
shapiro.test(Environment_2130$Strukturindex) # Normality OK

# 2140
(Strukturindex_hist_2140 <- ggplot(Environment_2140, aes(x = Strukturindex)) +                
    geom_histogram(binwidth = 0.02, colour = "grey", fill = "darkgrey") +    
    geom_vline(aes(xintercept = mean(Strukturindex)),                             
               colour = "red", linetype = "dashed", size=1)) 
shapiro.test(Environment_2140$Strukturindex) # Normality OK

# 2170
(Strukturindex_hist_2170 <- ggplot(Environment_2170, aes(x = Strukturindex)) +                
    geom_histogram(binwidth = 0.02, colour = "grey", fill = "darkgrey") +    
    geom_vline(aes(xintercept = mean(Strukturindex)),                             
               colour = "red", linetype = "dashed", size=1)) 
shapiro.test(Environment_2170$Strukturindex) # Normality OK

# 2190
(Strukturindex_hist_2190 <- ggplot(Environment_2190, aes(x = Strukturindex)) +                
    geom_histogram(binwidth = 0.02, colour = "grey", fill = "darkgrey") +    
    geom_vline(aes(xintercept = mean(Strukturindex)),                             
               colour = "red", linetype = "dashed", size=1)) 
shapiro.test(Environment_2190$Strukturindex) # Normality OK

# Histograms and Shapiro-Wilk test using Nature index and habitat types ####
# 2100
(Naturtilstandsindex_hist_2100 <- ggplot(Environment_2100, aes(x = Naturtilstandsindex)) +                
    geom_histogram(binwidth = 0.02, colour = "grey", fill = "darkgrey") +    
    geom_vline(aes(xintercept = mean(Naturtilstandsindex)),                             
               colour = "red", linetype = "dashed", size=1)) 
shapiro.test(Environment_2100$Naturtilstandsindex) # Problems with Normality

# 2120
(Naturtilstandsindex_hist_2120 <- ggplot(Environment_2120, aes(x = Naturtilstandsindex)) +                
    geom_histogram(binwidth = 0.02, colour = "grey", fill = "darkgrey") +    
    geom_vline(aes(xintercept = mean(Naturtilstandsindex)),                             
               colour = "red", linetype = "dashed", size=1)) 
shapiro.test(Environment_2120$Naturtilstandsindex) # Normality OK

# 2130
(Naturtilstandsindex_hist_2130 <- ggplot(Environment_2130, aes(x = Naturtilstandsindex)) +                
    geom_histogram(binwidth = 0.02, colour = "grey", fill = "darkgrey") +    
    geom_vline(aes(xintercept = mean(Naturtilstandsindex)),                             
               colour = "red", linetype = "dashed", size=1)) 
shapiro.test(Environment_2130$Naturtilstandsindex) # Normality OK

# 2140
(Naturtilstandsindex_hist_2140 <- ggplot(Environment_2140, aes(x = Naturtilstandsindex)) +                
    geom_histogram(binwidth = 0.02, colour = "grey", fill = "darkgrey") +    
    geom_vline(aes(xintercept = mean(Naturtilstandsindex)),                             
               colour = "red", linetype = "dashed", size=1)) 
shapiro.test(Environment_2140$Naturtilstandsindex) # Normality OK

# 2170
(Naturtilstandsindex_hist_2170 <- ggplot(Environment_2170, aes(x = Naturtilstandsindex)) +                
    geom_histogram(binwidth = 0.02, colour = "grey", fill = "darkgrey") +    
    geom_vline(aes(xintercept = mean(Naturtilstandsindex)),                             
               colour = "red", linetype = "dashed", size=1)) 
shapiro.test(Environment_2170$Naturtilstandsindex) # Normality OK

# 2190
(Naturtilstandsindex_hist_2190 <- ggplot(Environment_2190, aes(x = Naturtilstandsindex)) +                
    geom_histogram(binwidth = 0.02, colour = "grey", fill = "darkgrey") +    
    geom_vline(aes(xintercept = mean(Naturtilstandsindex)),                             
               colour = "red", linetype = "dashed", size=1)) 
shapiro.test(Environment_2190$Naturtilstandsindex) # Normality OK

#========================================================================================================
#      Variance among Zones
#========================================================================================================
# Checking for outliers using boxplots ####
# Species index
Species_boxplot_allobs <- ggplot(Environment,
                         aes( y = Artsindex)) + 
  geom_boxplot()
plot(Species_boxplot_allobs) # No outliers

# Structure Index
Structure_boxplot_allobs <- ggplot(Environment,
                                 aes( y = Strukturindex)) + 
  geom_boxplot()
plot(Structure_boxplot_allobs) # a few outliers

# Nature condition Index
Nature_boxplot_allobs <- ggplot(Environment,
                                   aes( y = Naturtilstandsindex)) + 
  geom_boxplot()
plot(Nature_boxplot_allobs) # a few outliers

#--------------------------------------------------------------------------------------------------------
#      Variance in Species index by Zone from Naturstyrelsen - Not using this in the final report
#--------------------------------------------------------------------------------------------------------
# Boxplot: species index by zone ####
# I have used the tutorials from https://www.r-graph-gallery.com
species_boxplot <- ggplot(Environment,
                          aes(x = Zone, y = Artsindex)) + 
  geom_boxplot()
plot(species_boxplot)

# Anova on species index ~ zone 
artsindex_zones <- lm(Artsindex ~ Zone, data = Environment) # Testing if there is difference among zones 
artsindex_anova <- aov(artsindex_zones)
summary(artsindex_anova)

# Tukey test on anova - showing individual relationships between zones
tukey_artsindex <- TukeyHSD(artsindex_anova) # Grouping zones based on these results
plot(tukey_artsindex)
summary(tukey_artsindex)
print(tukey_artsindex)

# Species index by grouped zones based on Tukeys test
# https://www.r-graph-gallery.com/84-tukey-test.html

# Generating lables for zone groups
generate_label_df <- function(TUKEY, variable){
  
  # Extract labels and factor levels from Tukey post-hoc
  Tukey.levels <- TUKEY[[variable]][,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  
  # Putting labels in same order as in boxplot
  Tukey.labels$Zone = rownames(Tukey.labels)
  Tukey.labels = Tukey.labels[order(Tukey.labels$Zone) , ]
  return(Tukey.labels)
}

# Applying the function on my dataset
LABELS <- generate_label_df(tukey_artsindex, "Zone")
as.numeric(as.factor(LABELS[,1]))

# Defining colours
my_colours <- c("White", "grey")

# Drawing boxplot with my new levels
a <- boxplot(Environment$Artsindex ~ Environment$Zone, ylim = c(min(Environment$Artsindex),
                                                                1.1*max(Environment$Artsindex)),
             col = my_colours[as.numeric(as.factor(LABELS[,1]))],
             xlab = "Zone", ylab = "Artsindex", main ="")

# I want to write the letter over each box. Over is how high I want to write it.
over <- 0.1*max( a$stats[nrow(a$stats),] )

#Add the labels
text( c(1:nlevels(Environment$Zone)) , a$stats[nrow(a$stats),]+over , LABELS[,1]  , col= "Black" )
levels(Environment$Zone)

# Species index by grouped zones
species_boxplot2 <- ggplot(Environment,
                           aes(x = Zone_col, y = Artsindex)) + # Using grouped zones based on anova (see further down)
  geom_boxplot()
plot(species_boxplot2)


# Boxplot: species index without 5m road plots, grouped zones
ggplot(Environment5road,
       aes(x = Zone_col, y = Artsindex)) + 
  geom_boxplot()

# Boxplot with species index without 10m road plots, grouped zones
ggplot(Environment10road,
       aes(x = Zone_col, y = Artsindex)) + 
  geom_boxplot()

# Evaluation of Species Index from Zone and Habitat type ####
# Species index explained by zone (as proxy for succession time)
Species.zone <- lm(Artsindex ~ Zone_orto,
                   data = Environment)
summary(Species.zone)
plot(Species.zone)
# Running Anova on categorical model
aov_species_zone <- aov(Species.zone)
summary(aov_species_zone)

# Species index explained by habitat type 
Species.hab <- lm(Artsindex ~ Habitat.code,
                  data = Environment)
summary(Species.hab)
plot(Species.hab)
# Running Anova on categorical model
aov_species_hab <- aov(Species.hab)
summary(aov_species_hab)

# Species index explained by time and habitat type 
Species.hab.zone <- lm(Artsindex ~ Zone_col 
                       + Habitat.code
                       + Zone_col * Habitat.code,
                       data = Environment)
summary(Species.hab.zone)
plot(Species.hab.zone)
vif_lmNhz <- car::vif(Species.hab.zone)
vif_lmhz
# Running Anova on categorical model
aov_species_hab_zone <- aov(Species.hab.zone)
summary(aov_species_hab_zone)

#____________________________________________________________________________________________________----
# Boxplot: Variance in structure index by Zone ####
structure_boxplot <- ggplot(Environment,
                            aes(x = Zone, y = Strukturindex)) + 
  geom_boxplot()
plot(structure_boxplot)

# Anova on structure index ~ zone 
Strukturindex_zones <- lm(Strukturindex ~ Zone, data = Environment)            # Testing if there is difference among zones 
Strukturindex_anova <- aov(Strukturindex_zones)
summary(Strukturindex_anova)

# Tukey test on anova - showing individual relationships between zones
tukey_Strukturindex <- TukeyHSD(Strukturindex_anova)                           # Grouping zones based on these results
plot(tukey_Strukturindex)
summary(tukey_Strukturindex)
print(tukey_Strukturindex)

# Generating Plot with grouped zones 
# Generating lables for zone groups
generate_label_df <- function(TUKEY, variable){
  
  # Extract labels and factor levels from Tukey post-hoc
  Tukey.levels <- TUKEY[[variable]][,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  
  # Putting labels in same order as in boxplot
  Tukey.labels$Zone = rownames(Tukey.labels)
  Tukey.labels = Tukey.labels[order(Tukey.labels$Zone) , ]
  return(Tukey.labels)
}

# Applying the function on my dataset
LABELS <- generate_label_df(tukey_Strukturindex, "Zone")
as.numeric(as.factor(LABELS[,1]))

# Defining colours
my_colours_struktur <- c("lightblue","cyan","blue", "darkblue")

# Drawing boxplot with my new levels
a <- boxplot(Environment$Strukturindex ~ Environment$Zone, ylim = c(min(Environment$Strukturindex),
                                                                1.1*max(Environment$Strukturindex)),
             col = my_colours_struktur[as.numeric(as.factor(LABELS[,1]))],
             xlab = "Zone", ylab = "Strukturindex", main ="")

# I want to write the letter over each box. Over is how high I want to write it.
over <- 0.1*max( a$stats[nrow(a$stats),] )

#Add the labels
text( c(1:nlevels(Environment$Zone)) , a$stats[nrow(a$stats),]+over , LABELS[,1]  , col= "Black" )
levels(Environment$Zone)

# Evaluation of Structure Index from Zone and Habitat type (using collective zones from previous) ####
# Structure index explained by zone (as proxy for succession time)
Structure.zone <- lm(Strukturindex ~ Zone_col,
                   data = Environment)
summary(Structure.zone)
plot(Structure.zone)
# Running Anova on categorical model
aov_structure_zone <- aov(Structure.zone)
summary(aov_structure_zone)

# Structure index explained by habitat type 
Structure.hab <- lm(Strukturindex ~ Habitat.code,
                  data = Environment)
summary(Structure.hab)
plot(Structure.hab)
# Running Anova on categorical model
aov_Structure_hab <- aov(Structure.hab)
summary(aov_Structure_hab)

# Structure index explained by time and habitat type 
Structure.hab.zone <- lm(Strukturindex ~ Zone_col 
                       + Habitat.code
                       + Zone_col * Habitat.code,
                       data = Environment)
summary(Structure.hab.zone)
plot(Structure.hab.zone)
vif_lmNhz <- car::vif(Structure.hab.zone)
vif_lmhz
# Running Anova on categorical model
aov_Structure_hab_zone <- aov(Structure.hab.zone)
summary(aov_Structure_hab_zone)

#____________________________________________________________________________________________________----
# Boxplot: Variance in Nature condition index by Zone (grouped from previous) ####
# Structure index using grouped zones
nature_boxplot <- ggplot(Environment,
                            aes(x = Zone_col, y = Naturtilstandsindex)) + 
  geom_boxplot()
plot(nature_boxplot)

# Checking if there is significant difference between the zones and habitat types
# Effect of zone (proxy for succession time)
Natur.zone <- lm(Naturtilstandsindex ~ Zone_col,
                     data = Environment)
summary(Natur.zone)
plot(Natur.cor.zone)
# Testing difference between categories
aov_nature_zone <- aov(Natur.zone)
summary(aov_nature_zone)

# Effect of habitat type
nature_hab_boxplot <- ggplot(Environment,
                         aes(x = Habitat.code, y = Naturtilstandsindex)) + 
  geom_boxplot()
plot(nature_hab_boxplot)

Natur.hab <- lm(Naturtilstandsindex ~ Habitat.code,
                     data = Environment)
summary(Natur.hab)
plot(Natur.hab)

# Running anova
aov_nature_hab <- aov(Natur.hab)
summary(aov_nature_hab)

# Effect of both
Natur.cor.time <- lm(Naturtilstandsindex ~ Zone_col 
                     + Habitat.code
                     + Zone_col * Habitat.code,
                     data = Environment)
summary(Natur.cor.time)
plot(Natur.cor.time)
aov_nat_hab_zone <- aov(Natur.cor.time)
summary(aov_nat_hab_zone)

# Running Anova on categorical model
aov_natureindex_time <- aov(Natur.cor.time)
summary(aov_natureindex_time)

# Boxplot with structure index without 5m road plots, grouped zones
ggplot(Environment5road,
       aes(x = Zone_col, y = Naturtilstandsindex)) + 
  geom_boxplot()

# Boxplot with structure index without 10m road plots, grouped zones
ggplot(Environment10road,
       aes(x = Zone_col, y = Naturtilstandsindex)) + 
  geom_boxplot()

# Generating Plot with grouped zones #### 
# Anova on Nature condition index ~ zone 
naturindex_zones <- lm(Naturtilstandsindex ~ Zone, data = Environment)            # Testing if there is difference among zones 
naturindex_anova <- aov(naturindex_zones)
summary(naturindex_anova)

# Tukey test on anova - showing individual relationships between zones
tukey_Naturtilstandsindex <- TukeyHSD(naturindex_anova)                           # Grouping zones based on these results
plot(tukey_Naturtilstandsindex)
summary(tukey_Naturtilstandsindex)
print(tukey_Naturtilstandsindex)
# Generating lables for zone groups
generate_label_df <- function(TUKEY, variable){
  
  # Extract labels and factor levels from Tukey post-hoc
  Tukey.levels <- TUKEY[[variable]][,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  
  # Putting labels in same order as in boxplot
  Tukey.labels$Zone = rownames(Tukey.labels)
  Tukey.labels = Tukey.labels[order(Tukey.labels$Zone) , ]
  return(Tukey.labels)
}

# Applying the function on my dataset
LABELS <- generate_label_df(tukey_Naturtilstandsindex, "Zone")
as.numeric(as.factor(LABELS[,1]))

# Defining colours
my_colours_natur <- c("lightgreen", "darkgreen")

# Drawing boxplot with my new levels
a <- boxplot(Environment$Naturtilstandsindex ~ Environment$Zone, ylim = c(min(Environment$Naturtilstandsindex),
                                                                    1.1*max(Environment$Naturtilstandsindex)),
             col = my_colours_natur[as.numeric(as.factor(LABELS[,1]))],
             xlab = "Zone", ylab = "Naturtilstandsindex", main ="")

# I want to write the letter over each box. Over is how high I want to write it.
over <- 0.1*max( a$stats[nrow(a$stats),] )

#Add the labels
text( c(1:nlevels(Environment$Zone)) , a$stats[nrow(a$stats),]+over , LABELS[,1]  , col= "Black" )
levels(Environment$Zone)


# Boxplot with Nature index, grouped zones
nature_boxplot <- ggplot(Environment,
                         aes(x = Zone_col, y = Naturtilstandsindex)) + 
  geom_boxplot()
plot(nature_boxplot)

# Boxplot with Nature index without 5m road plots, grouped zones 
ggplot(Environment5road,                                                # Using data subsets without plots close to roads
       aes(x = Zone_col, y = Naturtilstandsindex)) +              
  geom_boxplot()

# Boxplot with Nature index without 10m road plots, grouped zones
ggplot(Environment10road,
       aes(x = Zone_col, y = Naturtilstandsindex)) + 
  geom_boxplot()


# Boxplot: Species index by habitat type
specieshab_boxplot <- ggplot(Environment,
                          aes(x = Habitat.code, y = Artsindex)) + 
  geom_boxplot()
plot(specieshab_boxplot)

#--------------------------------------------------------------------------------------------------------
#      Variance in Species index by Zone from Orto Photos - This is what I am using for the final report
#--------------------------------------------------------------------------------------------------------
# Boxplot: Variance in species index by zone ####

#I have used the tutorials from https://www.r-graph-gallery.com
species_boxplot <- ggplot(Environment,
                          aes(x = Zone_orto, y = Artsindex)) + 
  geom_boxplot()
plot(species_boxplot)

summary(Environment$Artsindex)
mean(Environment$Artsindex)
mean(Environment_zone2014$Artsindex)
mean(Environment_zone2018$Artsindex)
mean(Environment_zone2019$Artsindex)
mean(Environment_zonePlantage$Artsindex)

mean(Environment_zone2017$Artsindex)
mean(Environment_zoneNaturligVegetation$Artsindex)

# Anova on species index ~ Zone_orto 
artsindex_Zone_ortos <- lm(Artsindex ~ Zone_orto, data = Environment)            # Testing if there is difference among Zone_ortos 
summary(artsindex_Zone_ortos)
artsindex_anova <- aov(artsindex_Zone_ortos)
summary(artsindex_anova)

# Tukey test on anova - showing individual relationships between Zone_ortos
tukey_artsindex <- TukeyHSD(artsindex_anova)                           # Grouping Zone_ortos based on these results
plot(tukey_artsindex)
summary(tukey_artsindex)
print(tukey_artsindex)

# Computing Kruskal-Wallis test - we cannot use anova when data are not normally distributed
# Resource: http://www.r-tutor.com/elementary-statistics/non-parametric-methods/kruskal-wallis-test
ArtsindexKW <- kruskal.test(Artsindex ~ Zone_orto, data = Environment)
ArtsindexKW # there is significant numbers of star species among the zones

# Running Dunn's test 
# Resource: http://www.r-tutor.com/elementary-statistics/non-parametric-methods/mann-whitney-wilcoxon-test
# https://rdrr.io/cran/DescTools/man/DunnTest.html
# https://stackoverflow.com/questions/58472408/how-to-apply-dunn-test-for-dataframes-in-r
# https://rcompanion.org/handbook/F_08.html
ArtsindexDunn <- DunnTest(Artsindex ~ Zone_orto, Environment)
ArtsindexDunn # Shows the same results as anova + tukey

# Species index by grouped Zone_ortos based on Tukeys test
# https://www.r-graph-gallery.com/84-tukey-test.html

# Generating lables for Zone_orto groups
generate_label_df <- function(TUKEY, variable){
  
  # Extract labels and factor levels from Tukey post-hoc
  Tukey.levels <- TUKEY[[variable]][,4] # using 4th column in tukey output
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  
  # Putting labels in same order as in boxplot
  Tukey.labels$Zone_orto = rownames(Tukey.labels)
  Tukey.labels = Tukey.labels[order(Tukey.labels$Zone_orto) , ]
  return(Tukey.labels)
}

# Applying the function on my dataset
LABELS <- generate_label_df(tukey_artsindex, "Zone_orto")
as.numeric(as.factor(LABELS[,1])) # refering to 1st column in LABELS data frame from function

# Defining colours
my_colours <- c("yellow", "darkblue", "lightblue", "orange", "darkorchid1", "green")

# Drawing boxplot with my new levels
a <- boxplot(Environment$Artsindex ~ Environment$Zone_orto, ylim = c(min(Environment$Artsindex), # defining y limits
                                                                1.1*max(Environment$Artsindex)), # 1.1 * max means there will be a little space above my boxes
             col = my_colours,
             xlab = "Rydningszone", ylab = "Artsindeks", main ="") # I am choosing no title (main)

# I want to write the letter over each box. Over is how high I want to write it.
over <- 0.1*max( a$stats[nrow(a$stats),] )

#Add the labels
text( c(1:nlevels(Environment$Zone_orto)) , a$stats[nrow(a$stats),]+over , LABELS[,1]  , col= "Black" )
levels(Environment$Zone_orto)

# Species index by grouped Zone_ortos - I am not using this in my final report
species_boxplot2 <- ggplot(Environment,
                           aes(x = Zone_orto_col, y = Artsindex)) + # Using grouped Zone_ortos based on anova (see further down)
  geom_boxplot()
plot(species_boxplot2)


# Boxplot: species index without 5m road plots, grouped Zone_ortos
ggplot(Environment5road,
       aes(x = Zone_orto_col, y = Artsindex)) + 
  geom_boxplot()

# Boxplot with species index without 10m road plots, grouped Zone_ortos
ggplot(Environment10road,
       aes(x = Zone_orto_col, y = Artsindex)) + 
  geom_boxplot()
#____________________________________________________________________________________________________----
# Boxplot: Variance in structure index by Zone_orto ####
structure_boxplot <- ggplot(Environment,
                            aes(x = Zone_orto, y = Strukturindex)) + 
  geom_boxplot()
plot(structure_boxplot)

mean(Environment$Strukturindex)
mean(Environment_zone2014$Strukturindex)
mean(Environment_zone2017$Strukturindex)
mean(Environment_zone2018$Strukturindex)
mean(Environment_zone2019$Strukturindex)
mean(Environment_zoneNaturligVegetation$Strukturindex)
mean(Environment_zonePlantage$Strukturindex)

# Anova on structure index ~ Zone_orto 
Strukturindex_Zone_ortos <- lm(Strukturindex ~ Zone_orto, data = Environment)            # Testing if there is difference among Zone_ortos 
Strukturindex_anova <- aov(Strukturindex_Zone_ortos)
summary(Strukturindex_anova)

# Tukey test on anova - showing individual relationships between Zone_ortos
tukey_Strukturindex <- TukeyHSD(Strukturindex_anova)                           # Grouping Zone_ortos based on these results
plot(tukey_Strukturindex)
summary(tukey_Strukturindex)
print(tukey_Strukturindex)

# Generating Plot with grouped Zone_ortos 
# Generating lables for Zone_orto groups
generate_label_df <- function(TUKEY, variable){
  
  # Extract labels and factor levels from Tukey post-hoc
  Tukey.levels <- TUKEY[[variable]][,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  
  # Putting labels in same order as in boxplot
  Tukey.labels$Zone_orto = rownames(Tukey.labels)
  Tukey.labels = Tukey.labels[order(Tukey.labels$Zone_orto) , ]
  return(Tukey.labels)
}

# Applying the function on my dataset
LABELS <- generate_label_df(tukey_Strukturindex, "Zone_orto")
as.numeric(as.factor(LABELS[,1]))

# Defining colours
my_colours <- c("yellow", "darkblue", "lightblue", "orange", "darkorchid1", "green")

# Drawing boxplot with my new levels
a <- boxplot(Environment$Strukturindex ~ Environment$Zone_orto, ylim = c(min(Environment$Strukturindex),
                                                                    1.1*max(Environment$Strukturindex)),
             col = my_colours,
             xlab = "Rydningszone", ylab = "Strukturindeks", main ="")

# I want to write the letter over each box. Over is how high I want to write it.
over <- 0.1*max( a$stats[nrow(a$stats),] )

#Add the labels
text( c(1:nlevels(Environment$Zone_orto)) , a$stats[nrow(a$stats),]+over , LABELS[,1]  , col= "Black" )
levels(Environment$Zone_orto)

#____________________________________________________________________________________________________----
# Boxplot: Variance in Nature condition index by Zone_orto ####
nature_boxplot <- ggplot(Environment,
                         aes(x = Zone_orto, y = Naturtilstandsindex)) + 
  geom_boxplot()
plot(nature_boxplot)

mean(Environment$Naturtilstandsindex)
mean(Environment_zoneNaturligVegetation$Naturtilstandsindex)
mean(Environment_zone2017$Naturtilstandsindex)

mean(Environment_zonePlantage$Naturtilstandsindex)
mean(Environment_zone2014$Naturtilstandsindex)
mean(Environment_zone2018$Naturtilstandsindex)
mean(Environment_zone2019$Naturtilstandsindex)

# Boxplot with condition index without 5m road plots Zone_ortos
ggplot(Environment5road,
       aes(x = Zone_orto, y = Naturtilstandsindex)) + 
  geom_boxplot()

# Boxplot with condition index without 10m road plots, Zone_ortos
ggplot(Environment10road,
       aes(x = Zone_orto, y = Naturtilstandsindex)) + 
  geom_boxplot()

# Generating Plot with Zone_ortos 
# Anova on Nature condition index ~ Zone_orto 
naturindex_Zone_ortos <- lm(Naturtilstandsindex ~ Zone_orto, data = Environment)            # Testing if there is difference among Zone_ortos 
naturindex_anova <- aov(naturindex_Zone_ortos)
summary(naturindex_anova)

# Tukey test on anova - showing individual relationships between Zone_ortos
tukey_Naturtilstandsindex <- TukeyHSD(naturindex_anova)                           # Grouping Zone_ortos based on these results
plot(tukey_Naturtilstandsindex)
summary(tukey_Naturtilstandsindex)
print(tukey_Naturtilstandsindex)

# Kruskal-Wallis test
NaturtilstandsindexKW <- kruskal.test(Naturtilstandsindex ~ Zone_orto, data = Environment)
NaturtilstandsindexKW # there is significant numbers of star species among the zones
# Running Dunn's test 
NaturtilstandsindexDunn <- DunnTest(Naturtilstandsindex ~ Zone_orto, Environment)
NaturtilstandsindexDunn # Shows the same results as anova + tukey

# Generating lables for Zone_orto groups
generate_label_df <- function(TUKEY, variable){
  
  # Extract labels and factor levels from Tukey post-hoc
  Tukey.levels <- TUKEY[[variable]][,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  
  # Putting labels in same order as in boxplot
  Tukey.labels$Zone_orto = rownames(Tukey.labels)
  Tukey.labels = Tukey.labels[order(Tukey.labels$Zone_orto) , ]
  return(Tukey.labels)
}

# Applying the function on my dataset
LABELS <- generate_label_df(tukey_Naturtilstandsindex, "Zone_orto")
as.numeric(as.factor(LABELS[,1]))

# Defining colours
my_colours <- c("yellow", "darkblue", "lightblue", "orange", "darkorchid1", "green")

# Drawing boxplot with my new levels
a <- boxplot(Environment$Naturtilstandsindex ~ Environment$Zone_orto, ylim = c(min(Environment$Naturtilstandsindex),
                                                                          1.1*max(Environment$Naturtilstandsindex)),
             col = my_colours,
             xlab = "Rydningszone", ylab = "Naturtilstandsindeks", main ="")

# I want to write the letter over each box. Over is how high I want to write it.
over <- 0.1*max( a$stats[nrow(a$stats),] )

#Add the labels
text( c(1:nlevels(Environment$Zone_orto)) , a$stats[nrow(a$stats),]+over , LABELS[,1]  , col= "Black" )
levels(Environment$Zone_orto)



#========================================================================================================
#      Variance among Habitat types
#========================================================================================================
# Species index by habitat type ####
species_habitat_boxplot <- ggplot(Environment,
                          aes(x = Habitat.vurdering, y = Artsindex)) + 
  geom_boxplot()
plot(species_habitat_boxplot)

mean(Environment_2100$Artsindex)
mean(Environment_2120$Artsindex)
mean(Environment_2130$Artsindex)
mean(Environment_2140$Artsindex)
mean(Environment_2170$Artsindex)
mean(Environment_2190$Artsindex)

# Anova on species index ~ zone 
artsindex_habitattype <- lm(Artsindex ~ Habitat.vurdering, data = Environment) # Testing if there is difference among zones 
art_hab_anova <- aov(artsindex_habitattype)
summary(art_hab_anova)

# Tukey test on anova - showing individual relationships between zones
tukey_art_hab <- TukeyHSD(art_hab_anova) # Grouping zones based on these results
plot(tukey_art_hab)
summary(tukey_art_hab)
print(tukey_art_hab)

# Species index by haibtat type based on Tukeys test
# https://www.r-graph-gallery.com/84-tukey-test.html

# Generating lables for zone groups
generate_label_df <- function(TUKEY, variable){
  
  # Extract labels and factor levels from Tukey post-hoc
  Tukey.levels <- TUKEY[[variable]][,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  
  # Putting labels in same order as in boxplot
  Tukey.labels$Habitat.vurdering = rownames(Tukey.labels)
  Tukey.labels = Tukey.labels[order(Tukey.labels$Habitat.vurdering) , ]
  return(Tukey.labels)
}

# Applying the function on my dataset
LABELS <- generate_label_df(tukey_art_hab, "Habitat.vurdering")
as.factor(LABELS[,1])

# Defining colours
my_colours_hab <- c("#DCE319FF", "#3CBB75FF","#287D8EFF")

# Drawing boxplot with my new levels
a <- boxplot(Environment$Artsindex ~ as.factor(Environment$Habitat.vurdering), ylim = c(min(Environment$Artsindex),
                                                                1.1*max(Environment$Artsindex)),
             col = "grey",
             xlab = "Habitat type", ylab = "Artsindeks", main ="")

# I want to write the letter over each box. Over is how high I want to write it.
over <- 0.1*max( a$stats[nrow(a$stats),] )

#Add the labels
text( c(1:nlevels(as.factor(Environment$Habitat.vurdering))) , a$stats[nrow(a$stats),]+over , LABELS[,1]  , col= "Black" )
levels(Environment$Habitat.vurdering)


# Structure index by habitat type ####
struktur_habitat_boxplot <- ggplot(Environment,
                                 aes(x = Habitat.vurdering, y = Strukturindex)) + 
  geom_boxplot()
plot(struktur_habitat_boxplot)

mean(Environment_2100$Strukturindex)
mean(Environment_2120$Strukturindex)
mean(Environment_2130$Strukturindex)
mean(Environment_2140$Strukturindex)
mean(Environment_2170$Strukturindex)
mean(Environment_2190$Strukturindex)


# Anova on structure index ~ habitat 
strukturindex_habitattype <- lm(Strukturindex ~ Habitat.vurdering, data = Environment) # Testing if there is difference among zones 
struktur_hab_anova <- aov(strukturindex_habitattype)
summary(struktur_hab_anova)

# Tukey test on anova - showing individual relationships between zones
tukey_struktur_hab <- TukeyHSD(struktur_hab_anova) # Grouping zones based on these results
plot(tukey_struktur_hab)
summary(tukey_struktur_hab)

# Kruskal-Wallis test
StrukturhabKW <- kruskal.test(Strukturindex ~ Habitat.vurdering, data = Environment)
StrukturhabKW # there is significant numbers of star species among the zones
# Running Dunn's test 
StrukturhabDunn <- DunnTest(Strukturindex ~ Habitat.vurdering, Environment)
StrukturhabDunn # Shows the same results as anova + tukey
?DunnTest

# Species index by haibtat type based on Tukeys test
# https://www.r-graph-gallery.com/84-tukey-test.html

# Generating lables for zone groups
generate_label_df <- function(TUKEY, variable){
  
  # Extract labels and factor levels from Tukey post-hoc
  Tukey.levels <- TUKEY[[variable]][,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  
  # Putting labels in same order as in boxplot
  Tukey.labels$Habitat.vurdering = rownames(Tukey.labels)
  Tukey.labels = Tukey.labels[order(Tukey.labels$Habitat.vurdering) , ]
  return(Tukey.labels)
}

# Applying the function on my dataset
LABELS <- generate_label_df(tukey_struktur_hab, "Habitat.vurdering")
as.factor(LABELS[,1])

# Defining colours
my_colours_hab <- c("#FDE725FF", "#95D840FF", "#287D8EFF", "#3CBB75FF", "#39568CFF")

# Drawing boxplot with my new levels
a <- boxplot(Environment$Strukturindex ~ as.factor(Environment$Habitat.vurdering), ylim = c(min(Environment$Strukturindex),
                                                                                                  1.1*max(Environment$Strukturindex)),
             col = "grey",
             xlab = "Habitat type", ylab = "Strukturindeks", main ="")

# I want to write the letter over each box. Over is how high I want to write it.
over <- 0.1*max( a$stats[nrow(a$stats),] )

#Add the labels
text( c(1:nlevels(as.factor(Environment$Habitat.vurdering))) , a$stats[nrow(a$stats),]+over , LABELS[,1]  , col= "Black" )
levels(Environment$Habitat.vurdering)



# Nature condition index by habitat type ####
nature_habitat_boxplot <- ggplot(Environment,
                                  aes(x = Habitat.vurdering, y = Naturtilstandsindex)) + 
  geom_boxplot()
plot(nature_habitat_boxplot)

mean(Environment_2100$Naturtilstandsindex)
mean(Environment_2120$Naturtilstandsindex)
mean(Environment_2130$Naturtilstandsindex)
mean(Environment_2140$Naturtilstandsindex)
mean(Environment_2170$Naturtilstandsindex)
mean(Environment_2190$Naturtilstandsindex)

# Anova on nature condition index ~ habitat 
naturindex_habitattype <- lm(Naturtilstandsindex ~ Habitat.vurdering, data = Environment) # Testing if there is difference among zones 
natur_hab_anova <- aov(naturindex_habitattype)
summary(natur_hab_anova)

# Tukey test on anova - showing individual relationships between zones
tukey_natur_hab <- TukeyHSD(natur_hab_anova) # Grouping zones based on these results
plot(tukey_natur_hab)
summary(tukey_natur_hab)

# Kruskal-Wallis test
NaturhabKW <- kruskal.test(Naturtilstandsindex ~ Habitat.vurdering, data = Environment)
NaturhabKW # there is significant numbers of star species among the zones
# Running Dunn's test 
NaturhabDunn <- DunnTest(Naturtilstandsindex ~ Habitat.vurdering, Environment)
NaturhabDunn # Shows different results than the anova + tukey

# Species index by haibtat type based on Tukeys test
# https://www.r-graph-gallery.com/84-tukey-test.html

# Generating lables for zone groups - I have chosen to do this by hand, since I cannot get the label function to work with dunns test
generate_label_df <- function(Dunn, variable){
  
  # Extract labels and factor levels from Tukey post-hoc
  Dunn.levels <- Dunn[[variable]][,3]
  Dunn.labels <- data.frame(multcompLetters(Dunn.levels)['Letters'])
  
  # Putting labels in same order as in boxplot
  Dunn.labels$Habitat.vurdering = rownames(Dunn.labels)
  Dunn.labels = Dunn.labels[order(Dunn.labels$Habitat.vurdering) , ]
  return(Dunn.labels)
}

# Applying the function on my dataset
LABELS <- generate_label_df(NaturhabDunn, "Habitat.vurdering")
as.factor(LABELS[,1])

# Defining colours
my_colours_hab <- c("#FDE725FF", "#95D840FF", "#287D8EFF", "#3CBB75FF", "#39568CFF")

# Drawing boxplot with my new levels
a <- boxplot(Environment$Naturtilstandsindex ~ as.factor(Environment$Habitat.vurdering), ylim = c(min(Environment$Naturtilstandsindex),
                                                                                        1.1*max(Environment$Naturtilstandsindex)),
             col = "grey",
             xlab = "Habitat type", ylab = "Naturtilstandsindeks", main ="")

# I want to write the letter over each box. Over is how high I want to write it.
over <- 0.1*max( a$stats[nrow(a$stats),] )

#Add the labels
text( c(1:nlevels(as.factor(Environment$Habitat.vurdering))) , a$stats[nrow(a$stats),]+over , LABELS[,1]  , col= "Black" )
levels(Environment$Habitat.vurdering)



#========================================================================================================
#      Coliniarity
#========================================================================================================
# Plotting variables agianst each other ####
plot(Env.num)                                                                          # Plotting all variables in dataframe against oneaother
pairs(~Artsindex + Strukturindex + Naturtilstandsindex + Habitat.vurdering + SuccessionTime + Afstand.hav +   # Plotting specific variables
      SRI + Hældning + Elevation + Lys + Ved + OrganiskM, data=Environment,
      lower.panel = panel.smooth)

# Correlation Matrix ####
# Pearsons correlation assumes normality                                               # Spearman correlation does not assume linearity
cor.env <- cor(Env.num, method = "spearman")                                           # Correlation values > 0.9 
cor.env                                                                                # should be considered alarming

# Analysing the correlation matrix using eigensystem analysis ####
eigen(cor(Env.num))$values  # If values are very different from oneanother, this should be investigated
# Condition Number: Ratio of max to min Eigen values of the correlation matrix
max(eigen(cor.env)$values)/min(eigen(cor.env)$values) # If the ratio is the order of 100 or more, then the problem with coliniarity is significant

# Regressing the explanatory variables from the others ####
# Distance to sea
x.cor <- lm(Afstand.hav ~ Zone
                    + Retning.klasse 
                    + SRI 
                    + Hældning 
                    + Elevation 
                    + Lys 
                    + OrganiskM 
                    + Ved,
                    data = Environment)
car::vif(x.cor)

x.cor1 <- lm(SRI ~ Afstand.hav
            + Retning.klasse 
            +  Zone
            + Hældning 
            + Elevation 
            + Lys 
            + OrganiskM 
            + Ved,
            data = Environment)
vif(x.cor1)

x.cor2 <- lm(Hældning ~
            + SRI 
            + Retning.klasse
            + Elevation 
            + Lys 
            + OrganiskM 
            + Ved
            + Afstand.hav
            + Zone,
            data = Environment)
vif(x.cor2)
# I will continue to analyse coliniarity within my models while fitting, using the car::vif function.
# Modelling charachter variables and additional relationsships, that may be related ####
shapiro.test(Environment$Afstand.hav) # Normality not ok - hence use of ranked cor measure
dist_habitat <- lm(Afstand.hav ~ Habitat.vurdering, data = Environment)
plot(Afstand.hav ~ Habitat.vurdering, data = Environment, 
     xlab = "Habitat type", ylab = "Afstand til havet (m)")
summary(dist_habitat)
kruskal.test(Afstand.hav ~ Habitat.vurdering, data = Environment)
DunnTest(Afstand.hav ~ Habitat.vurdering, Environment)

dist.time <- lm(Afstand.hav ~ Successionstid_orto, data = Environment)
plot(Afstand.hav ~ Successionstid_orto, data = Environment, 
     xlab = "Successionstid (år)", ylab = "Afstand til havet (m)")
summary(dist.time)
cor(Environment$Successionstid_orto, Environment$Afstand.hav, method = "spearman") # Spearman Rank correlation because distance to sea is not normally distributed

cor(Environment$Successionstid_orto, Environment$Ved, method = "spearman")
cor(Environment_2190$Successionstid_orto, Environment_2190$Ved, method = "spearman")
cor(Environment$Afstand.hav, Environment$Ved, method = "spearman")

dist.time1 <- lm(Afstand.hav ~ Successionstid_orto, data = Environment_zoneClearing)
plot(Afstand.hav ~ Successionstid_orto, data = Environment_zoneClearing, 
     xlab = "Successionstid i ryddede zoner (år)", ylab = "Afstand til havet (m)")
summary(dist.time1)
cor(Environment_zoneClearing$Successionstid_orto, Environment_zoneClearing$Afstand.hav, method = "spearman")

Artsind.org <- nlme::gls(Artsindex ~ OrganiskM, data = Environment)
plot(Artsindex ~ OrganiskM, data = Environment, 
     xlab = "Organisk materiale i jorden (cm)", ylab = "Artsindex")
abline(Artsind.org, Environment)
summary(Artsind.org)

dist.org <- gls(OrganiskM ~ Afstand.hav, data = Environment)
plot(OrganiskM ~ Afstand.hav, data = Environment, 
     xlab = "Distance to ocean (m)", ylab = "Organisk indhold i jorden (cm)")
abline(dist.org, Environment)
summary(dist.org)

# Correlation between prior plantation characteristics and present abiotic variables
cor(Environment$Kroneåbning15, Environment$OrganiskM, method = "spearman")
cor(Environment$Kronehøjde15, Environment$OrganiskM, method = "spearman")

cor(Environment$Kroneåbning15, Environment$Ved, method = "spearman")
cor(Environment$Kronehøjde15, Environment$Ved, method = "spearman")

cor(Environment$Kroneåbning15, Environment$Ved, method = "spearman")
cor(Environment$Kronehøjde15, Environment$Ved, method = "spearman")

#========================================================================================================
#      Fitting models
#========================================================================================================
# Model selection using generalized linear model ####
# Species index correlated variables ####
Artsindex.complex <- (nlme::gls(Artsindex ~ Habitat.code
                    + Afstand.hav 
                    + SuccessionTime
                    + Retning.klasse
                    + SRI 
                    + Hældning 
                    + Lys 
                    + Førnelag
                    + Organisk.lag
                    + OrganiskM,
                    Environment, method = "ML",
                    na.action = "na.omit"))            # dropping missing values
summary(Artsindex.complex)
layout(matrix(c(1,2,3,4),2,2))                         # Defining diagnostics plot layout (2 and 2 matrix)
plot(Artsindex.complex)                                # Calling standardized residuals plot

# Model selection
stepAIC(Artsindex.complex, method= "ML", direction = "backward")

# Model chosen from model selection
Artsindex.minimal <- (nlme::gls(Artsindex ~ Afstand.hav 
                                + SRI 
                                + Hældning 
                                + Lys
                                + OrganiskM,
                                Environment, method = "ML",
                                na.action = "na.omit")) # dropping missing values
summary(Artsindex.minimal)
layout(matrix(c(1,2,3,4),2,2)) # Defining diagnostics plot layout (2 and 2 matrix)
plot(Artsindex.complex) # Calling standardized residuals plot

# Checking variance inflation factor for each variable in the model
vif1 <- car::vif(Artsindex.minimal) # If variance inflation is 1 there is none at all (worry if >5)
mean(vif1) # is the mean more than 1?
# with 2.1 I wouldn't worry too much

# Structure index correlated variables ####
Strukturindex.complex <- nlme::gls(Strukturindex ~ Habitat.code
                                + Afstand.hav 
                                + SuccessionTime
                                + Retning.klasse
                                + SRI 
                                + Hældning 
                                + Lys 
                                + Førnelag
                                + Organisk.lag
                                + OrganiskM,
                                Environment, method = "ML",
                                na.action = "na.omit") # dropping missing values
summary(Strukturindex.complex)
layout(matrix(c(1,2,3,4),2,2)) # Defining diagnostics plot layout (2 and 2 matrix)
plot(Strukturindex.complex) # Calling standardized residuals plot

# Model selection
stepAIC(Strukturindex.complex, method= "ML", direction = "backward") # This modelselection does not work. It does not choose the best fit

# Model chosen from model selection
Strukturindex.minimal <- (nlme::gls(Strukturindex ~ Habitat.code
                                + Afstand.hav 
                                + SuccessionTime
                                + SRI 
                                + Lys
                                + Førnelag 
                                + Organisk.lag
                                + OrganiskM,
                                Environment, method = "ML",
                                na.action = "na.omit")) # dropping missing values
summary(Strukturindex.minimal)
layout(matrix(c(1,2,3,4),2,2)) # Defining diagnostics plot layout (2 and 2 matrix)
plot(Strukturindex.complex) # Calling standardized residuals plot

# Checking variance inflation factor for each variable in the model
vif2 <- car::vif(Strukturindex.minimal) # If variance inflation is 1 there is none at all (worry if >5)
print(vif2)
mean(vif2) # is the mean more than 1?
# Mean vif = 2.4 

# Structure index correlated variables 
Naturindex.complex <- nlme::gls(Naturtilstandsindex ~ Habitat.code
                                   + Afstand.hav 
                                   + SuccessionTime
                                   + Retning.klasse
                                   + SRI 
                                   + Hældning 
                                   + Lys 
                                   + Førnelag
                                   + Organisk.lag
                                   + OrganiskM,
                                   Environment, method = "ML",
                                   na.action = "na.omit") # dropping missing values
summary(Naturindex.complex)
layout(matrix(c(1,2,3,4),2,2)) # Defining diagnostics plot layout (2 and 2 matrix)
plot(Naturindex.complex) # Calling standardized residuals plot

# Model selection
stepAIC(Naturindex.complex, method= "ML", direction = "backward")

# Model chosen from model selection
Naturindex.minimal <- (nlme::gls(Naturtilstandsindex ~ Habitat.code
                                    + Afstand.hav 
                                    + SRI 
                                    + Hældning
                                    + OrganiskM,
                                    Environment, method = "ML",
                                    na.action = "na.omit")) # dropping missing values
summary(Naturindex.minimal)
layout(matrix(c(1,2,3,4),2,2)) # Defining diagnostics plot layout (2 and 2 matrix)
plot(Naturindex.complex) # Calling standardized residuals plot

# Checking variance inflation factor for each variable in the model
vif3 <- car::vif(Naturindex.minimal) # If variance inflation is 1 there is none at all (worry if >5)
vif3
mean(vif3) # is the mean more than 1?
# Mean vif = 2.5

# Model selection on habitat type ####
# 2190, chosen because important variables have a lot of NA's, and mix effect models do not deal with that very well
# Nature condition index correlated variables (random = Zone)
Naturtilstandsindex.2190.mix1 <- nlme::gls( Naturtilstandsindex ~ Afstand.hav   
                                            + Successionstid_orto
                                            + Hældning 
                                            + Elevation 
                                            + Lys
                                            + Kroneåbning15
                                            + Kronehøjde15
                                            + OrganiskM 
                                            + Ved
                                            + Vandprøve.pH
                                            + Vandflade,
                                            Environment_2190NA,
                                            na.action = "na.omit",
                                            method = "ML")
?gls
summary(Naturtilstandsindex.2190.mix1)
layout(matrix(c(1,2,3,4),2,2))
plot(Naturtilstandsindex.2190.mix1)
car::vif(Naturtilstandsindex.2190.mix1)
# Model selection
stepAIC(Naturtilstandsindex.2190.mix1, method= "ML", direction = "backward")
#--------------------------------------------------------------------------------------------------------
# Mixed effect model - What I ended up using to deal with zone layout
# Species index ####
# Artsindex correlated variables (random = Zone)
Artsindex.complex.mix1 <- nlme::lme( Artsindex ~ Afstand.hav   # The mixed effect model, that I need to fix
                         + Successionstid_orto
                         + Hældning 
                         + Elevation 
                         + Lys
                         + SRI
                         + Kroneåbning15
                         + Kronehøjde15
                         + OrganiskM 
                         + Ved,
                         Environment_NA,
                         na.action = "na.omit",
                         random = ~ 1|Zone_orto,
                         method = "ML")
summary(Artsindex.complex.mix1)
layout(matrix(c(1,2,3,4),2,2))
plot(Artsindex.complex.mix1)

# Model selection
stepAIC(Artsindex.complex.mix1, method= "ML", direction = "backward")

# Reduced model
Artsindex.reduced.mix1 <- lme(Artsindex ~ Afstand.hav + Hældning + Elevation + SRI + Kroneåbning15,
                              Environment_NA,
                              random = ~ 1|Zone_orto,
                              method = "ML")
summary(Artsindex.reduced.mix1)
# variance inflation
car::vif(Artsindex.reduced.mix1)

# Nicefying output
stargazer(Artsindex.reduced.mix1, type = "text",
          digits = 4,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separate = "lakh")

library(MuMIn) # Calculating R squared for mixed effect models

MuMIn::r.squaredGLMM(Artsindex.reduced.mix1)


# Artsindex correlated variables (random = habitat type)
Artsindex.complex.mix2 <- nlme::lme( Artsindex ~ Afstand.hav   # The mixed effect model, that I need to fix
                                     + Successionstid_orto
                                     + Hældning 
                                     + Elevation 
                                     + Lys
                                     + Kroneåbning15
                                     + Kronehøjde15
                                     + OrganiskM 
                                     + Ved,
                                     Environment_NA,
                                     na.action = "na.omit",
                                     random = ~ 1|Habitat.vurdering,
                                     method = "ML")
summary(Artsindex.complex.mix2)
layout(matrix(c(1,2,3,4),2,2))
plot(Artsindex.complex.mix2)

# Model selection
stepAIC(Artsindex.complex.mix2, method= "ML", direction = "backward")

# Reduced model
Artsindex.reduced.mix2 <- lme(Artsindex ~ Afstand.hav + Successionstid_orto + Kronehøjde15 +      OrganiskM,
                              Environment_NA,
                              random = ~ 1|Habitat.vurdering,
                              method = "ML")
summary(Artsindex.reduced.mix2)

# Structure index ####
# Strukturindex correlated variables (random = Zone)
Strukturindex.complex.mix1 <- nlme::lme( Strukturindex ~ Afstand.hav   # The mixed effect model, that I need to fix
                                    + Successionstid_orto
                                    + Hældning 
                                    + Elevation 
                                    + Lys
                                    + SRI
                                    + Kroneåbning15
                                    + Kronehøjde15
                                    + OrganiskM 
                                    + Ved,
                                    Environment_NA,
                                    na.action = "na.omit",
                                    random = ~ 1|Zone_orto,
                                    method = "ML")
summary(Strukturindex.complex.mix1)
layout(matrix(c(1,2,3,4),2,2))
plot(Strukturindex.complex.mix1)

# Model selection
stepAIC(Strukturindex.complex.mix1, method= "ML", direction = "backward")

# Reduced model
Strukturindex.reduced.mix1 <- lme(Strukturindex ~ Successionstid_orto + SRI + Kronehøjde15 , 
                                  Environment_NA,
                                  na.action = "na.omit",
                                  random = ~ 1|Zone_orto,
                                  method = "ML")
summary(Strukturindex.reduced.mix1)

# Nicefying output
stargazer(Strukturindex.reduced.mix1, type = "text",
          digits = 4,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separate = "lakh")
MuMIn::r.squaredGLMM(Strukturindex.reduced.mix1)

# Strukturindex correlated variables (random = Habitat type)
Strukturindex.complex.mix2 <- nlme::lme( Strukturindex ~ Afstand.hav   # The mixed effect model, that I need to fix
                                         + Successionstid_orto
                                         + Hældning 
                                         + Elevation 
                                         + Lys
                                         + Kroneåbning15
                                         + Kronehøjde15
                                         + OrganiskM 
                                         + Ved,
                                         Environment_NA,
                                         na.action = "na.omit",
                                         random = ~ 1|Habitat.vurdering,
                                         method = "ML")
summary(Strukturindex.complex.mix2)
layout(matrix(c(1,2,3,4),2,2))
plot(Strukturindex.complex.mix2)

# Model selection
stepAIC(Strukturindex.complex.mix2, method= "ML", direction = "backward")
stepAIC(Strukturindex.complex.mix2, method= "ML", direction = "forward")


# Nature condition index ####
# Nature condition index correlated variables (random = Zone)
Naturtilstandsindex.complex.mix1 <- nlme::lme( Naturtilstandsindex ~ Afstand.hav   
                                         + Successionstid_orto
                                         + Hældning 
                                         + Elevation 
                                         + Lys
                                         + SRI
                                         + Kroneåbning15
                                         + Kronehøjde15
                                         + OrganiskM 
                                         + Ved,
                                         Environment_NA,
                                         na.action = "na.omit",
                                         random = ~ 1|Zone_orto,
                                         method = "ML")
summary(Naturtilstandsindex.complex.mix1)
layout(matrix(c(1,2,3,4),2,2))
plot(Naturtilstandsindex.complex.mix1)

# Model selection
stepAIC(Naturtilstandsindex.complex.mix1, method= "ML", direction = "backward")
stepAIC(Naturtilstandsindex.complex.mix1, method= "ML", direction = "forward")

# Reduced model
Naturtilstandsindex.reduced.mix1 <- lme(Naturtilstandsindex ~ Afstand.hav + Successionstid_orto + Hældning +      Lys + SRI + Kronehøjde15 + OrganiskM ,
                                        Environment_NA,
                                        na.action = "na.omit",
                                        random = ~ 1|Zone_orto,
                                        method = "ML")
summary(Naturtilstandsindex.reduced.mix1)

# Nicefying output
stargazer(Naturtilstandsindex.reduced.mix1, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separate = "lakh")
MuMIn::r.squaredGLMM(Naturtilstandsindex.reduced.mix1)


# Nature condition index correlated variables (random = Habitat type)
Naturtilstandsindex.complex.mix2 <- nlme::lme( Naturtilstandsindex ~ Afstand.hav   
                                         + Successionstid_orto
                                         + Hældning 
                                         + Elevation 
                                         + Lys
                                         + Kroneåbning15
                                         + Kronehøjde15
                                         + OrganiskM 
                                         + Ved,
                                         Environment_NA,
                                         na.action = "na.omit",
                                         random = ~ 1|Habitat.vurdering,
                                         method = "ML")
summary(Naturtilstandsindex.complex.mix2)
layout(matrix(c(1,2,3,4),2,2))
plot(Naturtilstandsindex.complex.mix2)

# Model selection
stepAIC(Naturtilstandsindex.complex.mix2, method= "ML", direction = "backward")
stepAIC(Naturtilstandsindex.complex.mix2, method= "ML", direction = "forward")

# Nature condition index in habitat types ####
# 2120
Naturtilstandsindex.2120.mix1 <- nlme::lme( Naturtilstandsindex ~ Afstand.hav   
                                               + Successionstid_orto
                                               + Hældning 
                                               + Elevation 
                                               + Lys
                                               + SRI
                                               + Kroneåbning15
                                               + Kronehøjde15
                                               + OrganiskM 
                                               + Ved,
                                               Environment_NA2120,
                                               na.action = "na.omit",
                                               random = ~ 1|Zone_orto,
                                               method = "ML")
summary(Naturtilstandsindex.2120.mix1)
layout(matrix(c(1,2,3,4),2,2))
plot(Naturtilstandsindex.2120.mix1)

# Model selection
stepAIC(Naturtilstandsindex.2120.mix1, method= "ML", direction = "backward")

# 2130
Naturtilstandsindex.2130.mix1 <- nlme::lme( Naturtilstandsindex ~ Afstand.hav   
                                            + Successionstid_orto
                                            + Hældning 
                                            + Elevation 
                                            + Lys
                                            + SRI
                                            + Kroneåbning15
                                            + Kronehøjde15
                                            + OrganiskM 
                                            + Ved,
                                            Environment_NA2130,
                                            na.action = "na.omit",
                                            random = ~ 1|Zone_orto,
                                            method = "ML")
summary(Naturtilstandsindex.2130.mix1)
layout(matrix(c(1,2,3,4),2,2))
plot(Naturtilstandsindex.2130.mix1)

# Model selection
stepAIC(Naturtilstandsindex.2130.mix1, method= "ML", direction = "backward")

Naturtilstandsindex.2130.reduced1 <- nlme::lme(Naturtilstandsindex ~ SRI + Kronehøjde15 , 
                                               Environment_NA2130,
                                               na.action = "na.omit",
                                               random = ~ 1|Zone_orto,
                                               method = "ML")
summary(Naturtilstandsindex.2130.reduced1)

# Nicefying output
stargazer(Naturtilstandsindex.2130.reduced1, type = "text",
          digits = 4,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separate = "lakh")
MuMIn::r.squaredGLMM(Naturtilstandsindex.2130.reduced1)

# 2140
Naturtilstandsindex.2140.mix1 <- nlme::lme( Naturtilstandsindex ~ Afstand.hav   
                                            + Successionstid_orto
                                            + Hældning 
                                            + Elevation 
                                            + Lys
                                            + SRI
                                            + Kroneåbning15
                                            + Kronehøjde15
                                            + OrganiskM 
                                            + Ved,
                                            Environment_NA2140,
                                            na.action = "na.omit",
                                            random = ~ 1|Zone_orto,
                                            method = "ML")
summary(Naturtilstandsindex.2140.mix1)
layout(matrix(c(1,2,3,4),2,2))
plot(Naturtilstandsindex.2140.mix1)

# Model selection
stepAIC(Naturtilstandsindex.2140.mix1, method= "ML", direction = "backward")

Naturtilstandsindex.2140.reduced1 <- nlme::lme(Naturtilstandsindex ~ Afstand.hav + Elevation + Lys + Kroneåbning15 +      Kronehøjde15 + OrganiskM , 
                                               Environment_NA2140,
                                               na.action = "na.omit",
                                               random = ~ 1|Zone_orto,
                                               method = "ML")
summary(Naturtilstandsindex.2140.reduced1)

# Nicefying output
stargazer(Naturtilstandsindex.2140.reduced1, type = "text",
          digits = 4,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separate = "lakh")
MuMIn::r.squaredGLMM(Naturtilstandsindex.2140.reduced1)

# 2190
# Nature condition index correlated variables (random = Zone)
Naturtilstandsindex.2190.mix1 <- nlme::lme( Naturtilstandsindex ~ Afstand.hav   
                                            + Successionstid_orto 
                                            + Kroneåbning15
                                            + Kronehøjde15
                                            + OrganiskM 
                                            + Ved
                                            + Vandprøve.pH
                                            + Vandflade,
                                            Environment_2190NA,
                                            na.action = "na.omit",
                                            random = ~ 1|Zone_orto,
                                            method = "ML")
summary(Naturtilstandsindex.2190.mix1)
layout(matrix(c(1,2,3,4),2,2))
plot(Naturtilstandsindex.2190.mix1)
car::vif(Naturtilstandsindex.2190.mix1)

# Model selection
stepAIC(Naturtilstandsindex.2190.mix1, method= "ML", direction = "backward")

Naturtilstandsindex.2190.reduced1 <- nlme::lme(Naturtilstandsindex ~ Afstand.hav + Successionstid_orto + Ved + SRI, 
                                               Environment_2190NA,
                                               na.action = "na.omit",
                                               random = ~ 1|Zone_orto,
                                               method = "ML")
summary(Naturtilstandsindex.2190.reduced1)

# Nicefying output
stargazer(Naturtilstandsindex.2190.reduced1, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separate = "lakh")
MuMIn::r.squaredGLMM(Naturtilstandsindex.2190.reduced1)

# 2190 with water samples and organic matter only
Naturtilstandsindex.2190.water <- nlme::lme( Naturtilstandsindex ~ OrganiskM 
                                            + Ved
                                            + Vandprøve.pH
                                            + Vandflade,
                                            Environment_2190NA,
                                            na.action = "na.omit",
                                            random = ~ 1|Zone_orto,
                                            method = "ML")
summary(Naturtilstandsindex.2190.water)
layout(matrix(c(1,2,3,4),2,2))
plot(Naturtilstandsindex.2190.water)
car::vif(Naturtilstandsindex.2190.water)
# Nicefying output
stargazer(Naturtilstandsindex.2190.water, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separate = "lakh")

# 2190 with water samples only
Naturtilstandsindex.2190.water.reduced <- nlme::lme( Naturtilstandsindex ~ Vandprøve.pH
                                             + Vandflade,
                                             Environment_2190NA,
                                             na.action = "na.omit",
                                             random = ~ 1|Zone_orto,
                                             method = "ML")
summary(Naturtilstandsindex.2190.water.reduced)
layout(matrix(c(1,2,3,4),2,2))
plot(Naturtilstandsindex.2190.water.reduced)
car::vif(Naturtilstandsindex.2190.water.reduced)
# Nicefying output
stargazer(Naturtilstandsindex.2190.water.reduced, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separate = "lakh")
MuMIn::r.squaredGLMM(Naturtilstandsindex.2190.water.reduced)

#--------------------------------------------------------------------------------------------------------
# Linear models: 
# Species Index ####
Artsindex.cor <- lm(Artsindex ~ Habitat.vurdering 
                         + Successionstid_orto 
                         + Afstand.hav 
                         + Retning.klasse 
                         + Hældning
                         + SRI
                         + Elevation 
                         + Lys 
                         + OrganiskM
                         + Kroneåbning15
                         + Kronehøjde15
                         + Ved,
                         data = Environment)
summary(Artsindex.cor)
vif_lmA1 <- car::vif(Artsindex.cor)
vif_lmA1

# Manual model selection
Artsindex.cor.reduced <- lm(Artsindex ~ Afstand.hav # Reducing one variable at a time starting with least significant
                    + SRI
                    + Hældning
                    + Lys 
                    + OrganiskM,
                    data = Environment)
summary(Artsindex.cor.reduced)
vif_lmA2 <- car::vif(Artsindex.cor.reduced)
vif_lmA2


# Same analysis without plots <5m from roads
Artsindex5road.cor <- lm(Artsindex ~ Habitat.code 
                    + SuccessionTime 
                    + Afstand.hav 
                    + Retning.klasse 
                    + SRI
                    + Hældning 
                    + Elevation 
                    + Lys 
                    + OrganiskM 
                    + Ved,
                    data = Environment5road)
summary(Artsindex5road.cor)

vif_lmA5r1 <- car::vif(Artsindex5road.cor)
vif_lmA5r1

# Manual model selection
Artsindex5road.cor.reduced <- lm(Artsindex ~ Afstand.hav # Reducing one variable at a time starting with least significant
                         + Retning.klasse
                         + Hældning
                         + Lys 
                         + OrganiskM,
                         data = Environment5road)
summary(Artsindex5road.cor.reduced)
vif_lmA5r2 <- car::vif(Artsindex5road.cor.reduced)
vif_lmA5r2

# Fitting model without the natural vegetation
Artsindex.clearings <- lm(Artsindex ~ Habitat.code
                          + SuccessionTime 
                          + Afstand.hav 
                          + Retning.klasse
                          + SRI 
                          + Hældning 
                          + Elevation 
                          + Lys 
                          + OrganiskM 
                          + Kroneåbning15
                          + Kronehøjde15
                          + Ved,
                          data = clearingMatrix)
summary(Artsindex.clearings)
vif_lmAc1 <- car::vif(Artsindex.clearings)
vif_lmAc1

# Manual model selection
Artsindex.clearings.reduced <- lm(Artsindex ~ Afstand.hav
                                  + SRI 
                                  + Hældning
                                  + Lys 
                                  + OrganiskM,
                                 data = clearingMatrix)
summary(Artsindex.clearings.reduced)
plot(Artsindex.clearings.reduced)
vif_lmAc2 <- car::vif(Artsindex.clearings.reduced)
vif_lmAc2

# Regular linear model species index Log transformed
Artsindexlog.cor <- lm(log(Artsindex) ~ Habitat.code
                       + SuccessionTime 
                       + Afstand.hav 
                       + Retning.klasse
                       + SRI 
                       + Hældning 
                       + Elevation 
                       + Lys 
                       + OrganiskM
                       + Kroneåbning15
                       + Kronehøjde15
                       + Ved,
                       data = Environment)
summary(Artsindexlog.cor)
plot(Artsindexlog.cor)
vif_lmlogA1 <- car::vif(Artsindexlog.cor)
vif_lmlogA1

# Model selection species index Log transformed
Artsindexlog.cor.reduced <- lm(log(Artsindex) ~ Afstand.hav # Reducing one variable at a time starting with least significant
                       + SRI 
                       + Hældning
                       + Lys,
                       data = Environment)
summary(Artsindexlog.cor.reduced)
plot(Artsindexlog.cor.reduced)
vif_lmlogA2 <- car::vif(Artsindexlog.cor.reduced)
vif_lmlogA2

# Without plots <10m from roads
Artsindex10road.cor <- lm(Artsindex ~ Habitat.code 
                         + SuccessionTime 
                         + Afstand.hav 
                         + Retning.klasse
                         + SRI 
                         + Hældning 
                         + Elevation 
                         + Lys 
                         + OrganiskM 
                         + Kroneåbning15
                         + Kronehøjde15
                         + Ved,
                         data = Environment10road)
summary(Artsindex10road.cor)
vif_lmA10r1 <- car::vif(Artsindex5road.cor)
vif_lmA10r1

# Manual model selection
Artsindex10road.cor.reduced <- lm(Artsindex ~ Afstand.hav # Reducing one variable at a time starting with least significant
                                  + SRI 
                                  + Hældning
                                  + Lys,
                                 data = Environment5road)
summary(Artsindex10road.cor.reduced)
vif_lmA10r2 <- car::vif(Artsindex10road.cor.reduced)
vif_lmA10r2

# Structure index ####
# Strukturindex correlated variables 
Struktur.cor <- lm(Strukturindex ~ Habitat.code 
                    + SuccessionTime 
                    + Afstand.hav 
                    + Retning.klasse
                    + SRI 
                    + Hældning 
                    + Elevation 
                    + Lys 
                    + OrganiskM
                    + Kroneåbning15
                    + Kronehøjde15 
                    + Ved,
                    data = Environment)
summary(Struktur.cor)
layout(matrix(c(1,2,3,4),2,2))
plot(Struktur.cor)
vif_lmS1 <- car::vif(Struktur.cor)
vif_lmS1

# Manual model selection
Struktur.cor.reduced <- lm(Strukturindex ~ Afstand.hav
                           + Lys,
                                  data = Environment)
summary(Struktur.cor.reduced)
vif_lmS2 <- car::vif(Struktur.cor.reduced)
vif_lmS2

# Without plots <5m from roads
Struktur5road.cor <- lm(Strukturindex ~ Habitat.code 
                   + SuccessionTime 
                   + Afstand.hav 
                   + Retning.klasse 
                   + SRI 
                   + Hældning 
                   + Elevation 
                   + Lys 
                   + OrganiskM 
                   + Kroneåbning15
                   + Kronehøjde15
                   + Ved,
                   data = Environment5road)
summary(Struktur5road.cor)

# Manual model selection
Struktur5road.cor.reduced <- lm(Strukturindex ~ Afstand.hav
                                + Lys,
                           data = Environment)
summary(Struktur5road.cor.reduced)
vif_lmS5r2 <- car::vif(Struktur5road.cor.reduced)
vif_lmS5r2

# Without plots <10m from roads
Struktur10road.cor <- lm(Strukturindex ~ Habitat.code 
                        + SuccessionTime 
                        + Afstand.hav 
                        + Retning.klasse 
                        + SRI
                        + Hældning 
                        + Elevation 
                        + Lys 
                        + OrganiskM 
                        + Kroneåbning15
                        + Kronehøjde15
                        + Ved,
                        data = Environment10road)
summary(Struktur10road.cor)

# Nature condition index ####
# Naturtilstandsindex correlated variables 
Natur.cor <- lm(Naturtilstandsindex ~ Habitat.code 
                   + SuccessionTime 
                   + Afstand.hav
                   + Retning.klasse 
                   + SRI 
                   + Hældning 
                   + Elevation 
                   + Lys 
                   + OrganiskM 
                   + Ved
                   + Kroneåbning15
                   + Kronehøjde15,
                   data = Environment)
summary(Natur.cor)
plot(Natur.cor)
vif_lmN1 <- car::vif(Natur.cor)
vif_lmN1

# Manual model selection
Natur.cor.reduced <- lm(Naturtilstandsindex ~ Afstand.hav
                        + SRI 
                        + Hældning
                        + Lys 
                        + OrganiskM,
                           data = Environment)
summary(Natur.cor.reduced)
vif_lmN2 <- car::vif(Natur.cor.reduced)
vif_lmN2

# Nature index by succession time ####
# Succession within the cleared areas
Nature.time <- lm(Naturtilstandsindex ~ Successionstid_orto,
                        data = Environment_zoneClearing)
summary(Nature.time)
plot(Naturtilstandsindex ~ Successionstid_orto,
     data = Environment_zoneClearing)
plot(Nature.time)

# How long time will it take to reach the desired Nature Condition Index? 
solve(0.0056808,(mean(Environment_zoneNaturligVegetation$Naturtilstandsindex)-mean(Environment_zonePlantage$Naturtilstandsindex)))

# Making Nature condition Index ~ Succession time plot look nicer
(Nature.timegg <- ggplot(Environment, aes(x = Successionstid_orto, y = Naturtilstandsindex)) +
  geom_point() +
  geom_smooth(data = Environment, 
              method = lm, 
              formula = Naturtilstandsindex ~ Successionstid_orto,
              color = "red",
              se = TRUE) +
  stat_smooth(data = Environment,
              method = lm,
              se = TRUE,
              level = 0.95) +
  theme_classic() +
    xlab("Successionstid (år)") +
    ylab("Naturtilstandsindeks"))

# Making Nature condition Index ~ Succession time (in cleared areas) plot look nicer
(Nature.timegg <- ggplot(Environment_zoneClearing, aes(x = Successionstid_orto, y = Naturtilstandsindex)) +
    geom_point() +
    geom_smooth(data = Environment_zoneClearing, 
                method = lm, 
                formula = Naturtilstandsindex ~ Successionstid_orto,
                color = "red",
                se = TRUE) +
    stat_smooth(data = Environment_zoneClearing,
                method = lm,
                se = TRUE,
                level = 0.95) +
    theme_classic() +
    xlab("Successionstid (år)") +
    ylab("Naturtilstandsindeks"))

# Nature index explained by time and habitat type (repetition)
Natur.hab.zone <- lm(Naturtilstandsindex ~ Zone_orto 
                + Habitat.vurdering
                + Zone_orto * Habitat.vurdering,
                data = Environment)
summary(Natur.hab.zone)
plot(Natur.hab.zone)

# Running Anova on categorical model
Natur.hab.zone.anova <- aov(Natur.hab.zone)
summary(Natur.hab.zone.anova)

# Nature condition with time in each habitat type ####
# 2100 not normally distributed
# 2120 
Nature.time2120 <- lm(Naturtilstandsindex ~ Successionstid_orto,
                  data = Environment_2120)
summary(Nature.time2120) # No result because there are only 3 points in this subset, and they are same age
plot(Naturtilstandsindex ~ Successionstid_orto,
     data = Environment_2120)
plot(Nature.time2120)

# 2130 
Nature.time2130 <- lm(Naturtilstandsindex ~ Successionstid_orto,
                      data = Environment_2130)
summary(Nature.time2130) # Insignificant effect of age (almost significant)
plot(Naturtilstandsindex ~ Successionstid_orto,
     data = Environment_2130, xlab = "Successionstid (år)", ylab = "Naturtilstandsindeks")
abline(Nature.time2130, col = "darkgrey")
plot(Nature.time2130)

# 2140 
Nature.time2140 <- lm(Naturtilstandsindex ~ Successionstid_orto,
                      data = Environment_2140)
summary(Nature.time2140) # Significant effect of age
plot(Naturtilstandsindex ~ Successionstid_orto,
     data = Environment_2140, xlab = "Successionstid (år)", ylab = "Naturtilstandsindeks")
abline(Nature.time2140, col = "darkgrey")
plot(Nature.time2140)

# 2170 
Nature.time2170 <- lm(Naturtilstandsindex ~ Successionstid_orto,
                      data = Environment_2170)
summary(Nature.time2170) # Insignificant effect of age because there is only 4 observations
plot(Naturtilstandsindex ~ Successionstid_orto,
     data = Environment_2170)
abline(Nature.time2170, col = "green")
plot(Nature.time2170)

# 2190 
Nature.time2190 <- lm(Naturtilstandsindex ~ Successionstid_orto,
                      data = Environment_2190)
summary(Nature.time2190) # Insignificant effect of age
plot(Naturtilstandsindex ~ Successionstid_orto,
     data = Environment_2190, xlab = "Successionstid (år)", ylab = "Naturtilstandsindeks")
abline(Nature.time2190, col = "darkgrey")
plot(Nature.time2190)

shapiro.test(Environment_2190$Naturtilstandsindex)

# When will we reach the goal? ####

# How long time will it take to reach the Nature condition index of the Natural zone?
solve(0.0056808,(mean(Environment_zoneNaturligVegetation$Naturtilstandsindex)-mean(Environment_zone2019$Naturtilstandsindex)))

# How long time will it take to reach the Nature condition index of "High Quality" (0.8)?
solve(0.0056808,(0.8-mean(Environment_zone2019$Naturtilstandsindex)))

# Linear models of Nature condition without plots close to roads ####
# Without plots <5m from roads
Natur5road.cor <- lm(Naturtilstandsindex ~ Habitat.code 
                + SuccessionTime 
                + Afstand.hav
                + Retning.klasse 
                + SRI 
                + Hældning 
                + Elevation 
                + Lys 
                + OrganiskM 
                + Ved,
                data = Environment5road)
summary(Natur5road.cor)

# Without plots <10m from roads
Natur10road.cor <- lm(Naturtilstandsindex ~ Habitat.code 
                     + SuccessionTime 
                     + Afstand.hav
                     + Retning.klasse 
                     + SRI 
                     + Hældning 
                     + Elevation 
                     + Lys 
                     + OrganiskM 
                     + Ved,
                     data = Environment10road)
summary(Natur10road.cor)

# End of script -----------------------------------------------------------------------------------------