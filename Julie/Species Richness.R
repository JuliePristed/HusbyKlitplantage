# Species Richness Analysis
# Testing composition of sensitive species among zones and habitats
# Julie Pristed
# April 2021

rm(list=ls()) #clear environment

#install.packages("DescTools")

# Working Directory
# setwd("/Users/JuliePristed/Documents/Dokumenter/AU/A. Kandidat/A. Speciale/Data")

# Softcoding working directory
Dir.Base <- getwd() # to find the project folder
Dir.Data <- file.path(Dir.Base, "UnikkeArter.csv") # index the data folder
Dir.Data <- file.path(Dir.Base, "ArtsrigdomR.csv") # index the data folder

# Loading datasets
Arter <- read.csv("UnikkeArter.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
str(Arter)
levels(Arter$Zone)

ArterStat <- read.csv("ArtsrigdomR.csv", header = TRUE, sep = ",")
ArterStat$Zone_orto <- as.factor(ArterStat$Zone_orto)
str(ArterStat)
levels(ArterStat$Zone_orto)

# Loading packages
library(dplyr) # Data manipulation
library(DescTools) # Computing Dunn's post hoc test 

# Zone subsets
ArterNaturlig <- filter(Arter, Zone == "Naturlig Vegetation")
Arter2014 <- filter(Arter, Zone == "2014")
Arter2017 <- filter(Arter, Zone == "2017")
Arter2018 <- filter(Arter, Zone == "2018")
Arter2019 <- filter(Arter, Zone == "2019")
ArterPlantage <- filter(Arter, Zone == "Plantage")

ArterRydning <- filter(Arter, Zone != "Naturlig Vegetation") 
ArterRydning <- filter(ArterRydning, Zone != "Plantage")

# Habitat subsets
Arter2100 <- filter(Arter, Naturtype == "2100")
Arter2120 <- filter(Arter, Naturtype == "2120")
Arter2130 <- filter(Arter, Naturtype == "2130")
Arter2140 <- filter(Arter, Naturtype == "2140")
Arter2170 <- filter(Arter, Naturtype == "2170")
Arter2190 <- filter(Arter, Naturtype == "2190")

#----------------------------------------------------------------------------------------
# Counting unique species
#----------------------------------------------------------------------------------------
# Total richness
length(unique(Arter$ArtLatin))
Bidragsart <-filter(Arter, Bidragsart == 1)
length(unique(Bidragsart$ArtLatin))

Stjernearter <- filter(Arter, Artsscore > 3)
length(unique(Stjernearter$ArtLatin))
Tostjernearter <-filter(Arter, Artsscore >5)
length(unique(Tostjernearter$ArtLatin))

 #----------------------------------------------------------------------------------------
# Counting unique species in zones
#----------------------------------------------------------------------------------------
# Natural Vegetation
length(unique(ArterNaturlig$ArtLatin))
BidragsartNaturlig <-filter(ArterNaturlig, Bidragsart == 1)
length(unique(BidragsartNaturlig$ArtLatin))

# 2014
length(unique(Arter2014$ArtLatin))
Bidragsart2014 <-filter(Arter2014, Bidragsart == 1)
length(unique(Bidragsart2014$ArtLatin))

# 2017
length(unique(Arter2017$ArtLatin))
Bidragsart2017 <-filter(Arter2017, Bidragsart == 1)
length(unique(Bidragsart2017$ArtLatin))

# 2018
length(unique(Arter2018$ArtLatin))
Bidragsart2018 <-filter(Arter2018, Bidragsart == 1)
length(unique(Bidragsart2018$ArtLatin))

# 2019
length(unique(Arter2019$ArtLatin))
Bidragsart2019 <-filter(Arter2019, Bidragsart == 1)
length(unique(Bidragsart2019$ArtLatin))

# Plantation Vegetation
length(unique(ArterPlantage$ArtLatin))
BidragsartPlantage <-filter(ArterPlantage, Bidragsart == 1)
length(unique(BidragsartPlantage$ArtLatin))

#----------------------------------------------------------------------------------------
# Counting unique STAR species in zones
#----------------------------------------------------------------------------------------
# Natural Vegetation
StjernearterNaturlig <- filter(ArterNaturlig, Artsscore > 3)
length(unique(StjernearterNaturlig$ArtLatin))
TostjernearterNaturlig <-filter(ArterNaturlig, Artsscore >5)
length(unique(TostjernearterNaturlig$ArtLatin))

# 2014
Stjernearter2014 <- filter(Arter2014, Artsscore > 3)
length(unique(Stjernearter2014$ArtLatin))
Tostjernearter2014 <-filter(Arter2014, Artsscore >5)
length(unique(Tostjernearter2014$ArtLatin))

# 2017
Stjernearter2017 <- filter(Arter2017, Artsscore > 3)
length(unique(Stjernearter2017$ArtLatin))
Tostjernearter2017 <-filter(Arter2017, Artsscore >5)
length(unique(Tostjernearter2017$ArtLatin))

# 2018
Stjernearter2018 <- filter(Arter2018, Artsscore > 3)
length(unique(Stjernearter2018$ArtLatin))
Tostjernearter2018 <-filter(Arter2018, Artsscore >5)
length(unique(Tostjernearter2018$ArtLatin))

# 2019
Stjernearter2019 <- filter(Arter2019, Artsscore > 3)
length(unique(Stjernearter2019$ArtLatin))
Tostjernearter2019 <-filter(Arter2019, Artsscore >5)
length(unique(Tostjernearter2019$ArtLatin))

# Plantation Vegetation
StjernearterPlantage <- filter(ArterPlantage, Artsscore > 3)
length(unique(StjernearterPlantage$ArtLatin))
TostjernearterPlantage <-filter(ArterPlantage, Artsscore >5)
length(unique(TostjernearterPlantage$ArtLatin))

# Cleared zones
StjernearterRydning <- filter(ArterRydning, Artsscore > 3)
length(unique(StjernearterRydning$ArtLatin))
TostjernearterRydning <-filter(ArterRydning, Artsscore >5)
length(unique(TostjernearterRydning$ArtLatin))

#----------------------------------------------------------------------------------------
# Counting unique species in habitats
#----------------------------------------------------------------------------------------
# 2100
length(unique(Arter2100$ArtLatin))
Bidragsart2100 <-filter(Arter2100, Bidragsart == 1)
length(unique(Bidragsart2100$ArtLatin))

# 2120
length(unique(Arter2120$ArtLatin))
Bidragsart2120 <-filter(Arter2120, Bidragsart == 1)
length(unique(Bidragsart2120$ArtLatin))

# 2130
length(unique(Arter2130$ArtLatin))
Bidragsart2130 <-filter(Arter2130, Bidragsart == 1)
length(unique(Bidragsart2130$ArtLatin))

# 2140
length(unique(Arter2140$ArtLatin))
Bidragsart2140 <-filter(Arter2140, Bidragsart == 1)
length(unique(Bidragsart2140$ArtLatin))

# 2170
length(unique(Arter2170$ArtLatin))
Bidragsart2170 <-filter(Arter2170, Bidragsart == 1)
length(unique(Bidragsart2170$ArtLatin))

# 2190
length(unique(Arter2190$ArtLatin))
Bidragsart2190 <-filter(Arter2190, Bidragsart == 1)
length(unique(Bidragsart2190$ArtLatin))

#----------------------------------------------------------------------------------------
# Counting unique STAR species in habitats
#----------------------------------------------------------------------------------------

# 2100
Stjernearter2100 <- filter(Arter2100, Artsscore > 3)
length(unique(Stjernearter2100$ArtLatin))
Tostjernearter2100 <-filter(Arter2100, Artsscore >5)
length(unique(Tostjernearter2100$ArtLatin))

# 2120
Stjernearter2120 <- filter(Arter2120, Artsscore > 3)
length(unique(Stjernearter2120$ArtLatin))
Tostjernearter2120 <-filter(Arter2120, Artsscore >5)
length(unique(Tostjernearter2120$ArtLatin))

# 2130
Stjernearter2130 <- filter(Arter2130, Artsscore > 3)
length(unique(Stjernearter2130$ArtLatin))
Tostjernearter2130 <-filter(Arter2130, Artsscore >5)
length(unique(Tostjernearter2130$ArtLatin))

# 2140
Stjernearter2140 <- filter(Arter2140, Artsscore > 3)
length(unique(Stjernearter2140$ArtLatin))
Tostjernearter2140 <-filter(Arter2140, Artsscore >5)
length(unique(Tostjernearter2140$ArtLatin))

# 2170
Stjernearter2170 <- filter(Arter2170, Artsscore > 3)
length(unique(Stjernearter2170$ArtLatin))
Tostjernearter2170 <-filter(Arter2170, Artsscore >5)
length(unique(Tostjernearter2170$ArtLatin))

# 2190
Stjernearter2190 <- filter(Arter2190, Artsscore > 3)
length(unique(Stjernearter2190$ArtLatin))
Tostjernearter2190 <-filter(Arter2190, Artsscore >5)
length(unique(Tostjernearter2190$ArtLatin))


#----------------------------------------------------------------------------------------
# Analysis of variance
#----------------------------------------------------------------------------------------

# Testing normality
hist(ArterStat$Antal.arter)
shapiro.test(ArterStat$Antal.arter) # normality not OK
hist(log10(ArterStat$Antal.arter))
shapiro.test(log10(ArterStat$Antal.arter)) # didnt help


hist(ArterStat$Antal.bidragsarter)
shapiro.test(ArterStat$Antal.bidragsarter)
hist(log10(ArterStat$Antal.bidragsarter))
shapiro.test(log10(ArterStat$Antal.bidragsarter))

hist(ArterStat$Antal.stjernearter)
shapiro.test(ArterStat$Antal.stjernearter)
hist(log10(ArterStat$Antal.stjernearter))
shapiro.test(log10(ArterStat$Antal.stjernearter))

hist(ArterStat$Antal.tostjernearter)
shapiro.test(ArterStat$Antal.tostjernearter)
hist(log10(ArterStat$Antal.tostjernearter))
shapiro.test(log10(ArterStat$Antal.tostjernearter))

# Computing Kruskal-Wallis test - we cannot use anova when data are not normally distributed
# Resource: http://www.r-tutor.com/elementary-statistics/non-parametric-methods/kruskal-wallis-test

# star species in zones
starKW <- kruskal.test(Antal.stjernearter ~ Zone_orto, data = ArterStat)
starKW # there is significant numbers of star species among the zones
star2KW <- kruskal.test(Antal.tostjernearter ~ Zone_orto, data = ArterStat)
star2KW

# star species in habitats
starKWhab <- kruskal.test(Antal.stjernearter ~ Habitat.vurdering, data = ArterStat)
starKWhab
star2KWhab <- kruskal.test(Antal.tostjernearter ~ Habitat.vurdering, data = ArterStat)
star2KWhab


# Running Dunn's test 
# Resource: http://www.r-tutor.com/elementary-statistics/non-parametric-methods/mann-whitney-wilcoxon-test
# https://rdrr.io/cran/DescTools/man/DunnTest.html
# https://stackoverflow.com/questions/58472408/how-to-apply-dunn-test-for-dataframes-in-r
# https://rcompanion.org/handbook/F_08.html

# Star species in zones
starDunn <- DunnTest(Antal.stjernearter ~ Zone_orto, ArterStat)
starDunn
star2Dunn <- DunnTest(Antal.tostjernearter ~ Zone_orto, ArterStat)
star2Dunn

# Star species in habitats
starDunnhab <- DunnTest(Antal.stjernearter ~ Habitat.vurdering, ArterStat)
starDunnhab
star2Dunnhab <- DunnTest(Antal.tostjernearter ~ Habitat.vurdering, ArterStat)
star2Dunnhab

# End of script ---------------