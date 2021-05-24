# Species Indicator Analysis

# Julie Pristed
# Speciale
# March 2021

rm(list=ls()) #clear environment
showTmpFiles() #show temporary files
removeTmpFiles(h = 0) #clear all temporary files

## DATA INPUT ##

# Working directory
# setwd("/Users/JuliePristed/Documents/Dokumenter/AU/A. Kandidat/A. Speciale/Data")

# Softcoding working directory
Dir.Base <- getwd() # to find the project folder
Dir.Data <- file.path(Dir.Base, "Abundans_data.csv") # index the data folder


# Loading data
Abundance_data <- read.csv('Abundans_data.csv', header = TRUE, sep = ",")

#========================================================================================================
#  Species Indicator Analysis
#========================================================================================================
# Method based on Cáceres, Legendre and Moretti, 2010: https://onlinelibrary.wiley.com/doi/abs/10.1111/j.1600-0706.2010.18334.x 

# Resources: 
# https://jkzorz.github.io/2019/07/02/Indicator-species-analysis.html
# https://cran.r-project.org/web/packages/indicspecies/vignettes/indicspeciesTutorial.pdf

# Installing packages
#install.packages("indicspecies")

# Loading Packages ####
library(indicspecies) # species indicator analysis

# Specifying data characteristics in data frame
abund = Abundance_data[,4:ncol(Abundance_data)] # my abundance data starts in column 4
zone = Abundance_data$Zone_orto
habitat = as.factor(Abundance_data$Habitat.vurdering)

# Creating data subsets for each habitat type ####
Abundance_2100 <- dplyr::filter(Abundance_data, Habitat.vurdering == "2100")
str(Abundance_2100) # zone = 1 level
Abundance_2120 <- filter(Abundance_data, Habitat.vurdering == "2120") # zone = 1 level
Abundance_2130 <- filter(Abundance_data, Habitat.vurdering == "2130") 
Abundance_2140 <- filter(Abundance_data, Habitat.vurdering == "2140")
Abundance_2170 <- filter(Abundance_data, Habitat.vurdering == "2170") # zone = 1 level
Abundance_2190 <- filter(Abundance_data, Habitat.vurdering == "2190")

# Sorting abundance variable into the data subsets
abund2100 = Abundance_2100[,4:ncol(Abundance_2100)] # my abundance data starts in column 4
abund2120 = Abundance_2120[,4:ncol(Abundance_2120)] # my abundance data starts in column 4
abund2130 = Abundance_2130[,4:ncol(Abundance_2130)] # my abundance data starts in column 4
abund2140 = Abundance_2140[,4:ncol(Abundance_2140)] # my abundance data starts in column 4
abund2170 = Abundance_2170[,4:ncol(Abundance_2170)] # my abundance data starts in column 4
abund2190 = Abundance_2190[,4:ncol(Abundance_2190)] # my abundance data starts in column 4

# Creating zone variable subsets
zone2130 = Abundance_2130$Zone_orto
zone2140 = Abundance_2140$Zone_orto
zone2190 = Abundance_2190$Zone_orto

# Indicator species in zones
inv_zone = indicspecies::multipatt(abund, zone, func = "r.g", control = how(nperm = 9999)) # using point biserial correlation voefficient and 9999 permutations
summary(inv_zone)

# Indicator species in habitat types
inv_habitat = indicspecies::multipatt(abund, habitat, func = "r.g", control = how(nperm = 9999)) # using point biserial correlation voefficient and 9999 permutations
summary(inv_habitat)

# Indicator species in zones in each habitat type subset
# 2130
inv_zone2130 = indicspecies::multipatt(abund2130, zone2130, func = "r.g", control = how(nperm = 9999)) # using point biserial correlation voefficient and 9999 permutations
summary(inv_zone2130)

#2140
inv_zone2140 = indicspecies::multipatt(abund2140, zone2140, func = "r.g", control = how(nperm = 9999)) # using point biserial correlation voefficient and 9999 permutations
summary(inv_zone2140)

#2190
inv_zone2190 = indicspecies::multipatt(abund2190, zone2190, func = "r.g", control = how(nperm = 9999)) # using point biserial correlation voefficient and 9999 permutations
summary(inv_zone2190)
