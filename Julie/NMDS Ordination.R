# Julies Speciale 2020/2021
# Ordination

rm(list=ls()) #clear environment
showTmpFiles() #show temporary files
removeTmpFiles(h = 0) #clear all temporary files


# Downloading Packages ####
#install.packages("dplyr")
#install.packages("vegan", dependencies=TRUE)
#install.packages("fuzzySim")
#install.packages("tidyr")
#install.packages("tidylog")
#install.packages("ggplot2")

# Loading Packages ####
library(fuzzySim) # Data manipulation
library(dplyr) # Data manipulation
library(tidyr) # Data manipulation
library(tidylog) # Tracking data manipulation
library(vegan) # Ordination tools
library(ggplot2) # Visualisation tools

# Working Directory
# setwd("/Users/JuliePristed/Documents/Dokumenter/AU/A. Kandidat/A. Speciale/Data")

# Softcoding working directory
Dir.Base <- getwd() # to find the project folder
Dir.Data <- file.path(Dir.Base, "HusbySpecies.csv") # index the data folder
Dir.Data <- file.path(Dir.Base, "HusbyActivities.csv") # index the data folder
Dir.Data <- file.path(Dir.Base, "Resultater_alle_index.csv") # index the data folder
Dir.Data <- file.path(Dir.Base, "Miljødata.csv") # index the data folder

# Loading data ####
Species <- read.csv('HusbySpecies.csv', header = TRUE, sep = ",")
Activities <- read.csv("HusbyActivities.csv", header = TRUE, sep = ",")
Index <- read.csv("Resultater_alle_index.csv", header = TRUE, sep = ",")
Environment <- read.csv("Miljødata.csv", header = TRUE, sep = ",")
Environment$Habitat.vurdering <- as.factor(Environment$Habitat.vurdering)

#========================================================================================================
#  Creating matrixes
#========================================================================================================
# Creating presence/absensce Matrix ####
PresenceMatrix <- fuzzySim::splist2presabs(Species,sites.col = "Plot.ID", sp.col = "ArtLatin") 
rownames(PresenceMatrix) <- PresenceMatrix[ , 1]  #Ændre rownames til navnet på plottet - så jeg har 1 objekt pr. plot[row,column] - hvis man ikke skriver noget, tager den alt indhold
PresenceMatrix <- PresenceMatrix[ , -1] #Ændre matrixen, så jeg smider alt indhold i første række ud

# Creating abundance matrix from pinpoint data ####
AbundanceMatrix <- Species %>% 
  dplyr::select(ArtDansk, HyppighedPin, Plot.ID) %>%
  dplyr::filter(ArtDansk != "") %>% #Filtrerer rækker uden navn fra
  dplyr::group_by(Plot.ID) %>%  #Putter arter i plots
  filter(!duplicated(ArtDansk)) %>% #Fjerner dublikerede arter (registreret flere gange)
  tidyr::spread(key = ArtDansk, value = HyppighedPin) %>% 
  replace(is.na(.),  0)
# Change rownames to name on plot - 1 objekt pr. plot 
rownames(AbundanceMatrix) <- AbundanceMatrix[ , 1] #[row,column] - hvis man ikke skriver noget, tager den alt indhold

#Ændre matrixen, så jeg smider alt indhold i første række ud
AbundanceMatrix <- AbundanceMatrix[ , -1]
# Species %>% dplyr::count(Plot.ID, ArtLatin) %>% dplyr::pull(n)

# Creating functional group data set from cover ####
FunctionalMatrix <- Activities %>% 
  dplyr::select(Plot.ID,
         Dækning.Træer.Under1m, 
         Dækning.Træer.Over1m, 
         Dækning.Træer.Samlet, 
         Dækning.Dværgbuske, 
         Dækning.Bredbladede.Urter, 
         Dækning.Græsser,
         Dækning.Halvgræsser,
         Dækning.Mosser,
         Dækning.Laver) 

rownames(FunctionalMatrix) <- FunctionalMatrix[ , 1] #Naming rows after 1. column
FunctionalMatrix <- FunctionalMatrix[ , -1] # Losing the first column with plot ID

# Creating index matrix ####
IndexMatrix <- Index %>% 
  dplyr::select(Plot.ID,
         Artsindex,
         Strukturindex,
         Naturtilstandsindex) 

rownames(IndexMatrix) <- IndexMatrix[ , 1] #Naming rows after 1. column
IndexMatrix <- IndexMatrix[ , -1] # Losing the first column with plot ID

# Creating environmental matrix ####
EnvMatrix <- Environment %>% 
  dplyr::select(Plot.ID,
         Successionstid_orto,
         Afstand.hav,
         SRI,
         Hældning, 
         Elevation,
         Lys,
         Kroneåbning15,
         Kronehøjde15,
         Ved,
         OrganiskM)

rownames(EnvMatrix) <- EnvMatrix[ , 1] #Naming rows after 1. column
EnvMatrix <- EnvMatrix[ , -1] # Losing the first column with plot ID

# Hældning dataset without NA
Env_Hældning <- Environment %>%
  filter(Environment$Hældning != "NA")

# Elevation dataset without NA
Env_elevation <- Environment %>%
  filter(Environment$Elevation != "NA")
#========================================================================================================
# Evaluating Distribution in variables - only important in relation to ordisurf - which I ended up not doing
#========================================================================================================
# Succession time ####
Time_hist <- ggplot(Environment, aes(x = Successionstid_orto)) +                
   geom_histogram(binwidth = 1, colour = "grey", fill = "darkgrey") +    
   geom_vline(aes(xintercept = mean(Successionstid_orto)),                            
              colour = "red", linetype = "dashed", size=1)
plot(Time_hist)
shapiro.test(Environment$Successionstid_orto) # Normality not ok

logTime_hist <- ggplot(Environment, aes(x = log(Successionstid_orto))) +                
  geom_histogram(binwidth = 0.2, colour = "grey", fill = "darkgrey") +    
  geom_vline(aes(xintercept = mean(log(Successionstid_orto))),                            
             colour = "red", linetype = "dashed", size=1)
plot(logTime_hist)
shapiro.test(Environment$Successionstid_orto) # Normality not ok

hist(sqrt(Environment$Successionstid_orto), breaks = 20)
shapiro.test(sqrt(Environment$Successionstid_orto)) # Becomes remotely better, but still way significant problems


# Distance to sea ####
Dist_hist <- ggplot(Environment, aes(x = Afstand.hav)) +                
  geom_histogram(binwidth = 5, colour = "grey", fill = "darkgrey") +    
  geom_vline(aes(xintercept = mean(Afstand.hav)),                            
             colour = "red", linetype = "dashed", size=1)
plot(Dist_hist)
shapiro.test(Environment$Afstand.hav) # Normality not ok
shapiro.test(sqrt(Environment$Afstand.hav)) # slightly better

# SRI ####
SRI_hist <- ggplot(Environment, aes(x = SRI)) +                
  geom_histogram(binwidth = 0.05, colour = "grey", fill = "darkgrey") +    
  geom_vline(aes(xintercept = mean(SRI)),                            
             colour = "red", linetype = "dashed", size=1)
plot(SRI_hist)
shapiro.test(Environment$SRI) # Normality not ok

hist(log10(Environment$SRI))
shapiro.test(log10(Environment$SRI)) #not improved

# Hældning ####
Hældning_hist <- ggplot(Env_Hældning, aes(x = Hældning)) + # Using dataset without NA's               
  geom_histogram(binwidth = 2, colour = "grey", fill = "darkgrey") +    
  geom_vline(aes(xintercept = mean(Hældning)),                            
             colour = "red", linetype = "dashed", size=1)
plot(Hældning_hist)
shapiro.test(Env_Hældning$Hældning) # Normality not ok
hist(log(Env_Hældning$Hældning)) # Looking way better
shapiro.test(log(Env_Hældning$Hældning)) # Better but normality still not ok

# Elevation ####
Elevation_hist <- ggplot(Env_elevation, aes(x = Elevation)) + # Using dataset without NA's               
  geom_histogram(binwidth = 2, colour = "grey", fill = "darkgrey") +    
  geom_vline(aes(xintercept = mean(Elevation)),                            
             colour = "red", linetype = "dashed", size=1)
plot(Elevation_hist)
shapiro.test(Env_elevation$Elevation) # Normality not ok
hist(log(Env_elevation$Elevation)) # looking way better
shapiro.test(log(Env_elevation$Elevation)) # cannot be computed...

# Canopy Openness 2015 ####
Kroneåbning15_hist <- ggplot(Environment, aes(x = Kroneåbning15)) +              
  geom_histogram(binwidth = 0.5, colour = "grey", fill = "darkgrey") +    
  geom_vline(aes(xintercept = mean(Kroneåbning15)),                            
             colour = "red", linetype = "dashed", size=1)
plot(Kroneåbning15_hist)
shapiro.test(Environment$Kroneåbning15) # Normality not ok
hist(log10(Environment$Kroneåbning15), breaks = 20) # This does not help..
shapiro.test(log10(Environment$Kroneåbning15))

# Canopy Height 2015 ####
Kronehøjde15_hist <- ggplot(Environment, aes(x = Kronehøjde15)) +                
  geom_histogram(binwidth = 10, colour = "grey", fill = "darkgrey") +    
  geom_vline(aes(xintercept = mean(Kronehøjde15)),                            
             colour = "red", linetype = "dashed", size=1)
plot(Kronehøjde15_hist)
shapiro.test(Environment$Kronehøjde15) # Normality not ok
hist(sqrt(Environment$Kronehøjde15)) # looks better
shapiro.test(sqrt(Environment$Kronehøjde15)) # Normality still not ok, but way better

# Dead Wook Cover ####
Ved_hist <- ggplot(Environment, aes(x = Ved)) +                
  geom_histogram(binwidth = 2, colour = "grey", fill = "darkgrey") +    
  geom_vline(aes(xintercept = mean(Ved)),                            
             colour = "red", linetype = "dashed", size=1)
plot(Ved_hist)
shapiro.test(Environment$Ved) # Normality not ok
hist(log(Environment$Ved), breaks = 10) # Looks much better
shapiro.test(log(Environment$Ved)) # Cannot calculate..

# Organic Matter ####
OrganiskM_hist <- ggplot(Environment, aes(x = OrganiskM)) +                
  geom_histogram(binwidth = 1, colour = "grey", fill = "darkgrey") +    
  geom_vline(aes(xintercept = mean(OrganiskM)),                            
             colour = "red", linetype = "dashed", size=1)
plot(OrganiskM_hist)
shapiro.test(Environment$OrganiskM) # Normality not ok
hist(log(Environment$OrganiskM)) # Looks better
shapiro.test(log(Environment$OrganiskM)) # Cannot calculate
#========================================================================================================
#  Calculation Ordinations
#========================================================================================================
# Resources I have used: ####
# https://jonlefcheck.net/2012/10/24/nmds-tutorial-in-r/
# https://rpubs.com/CPEL/NMDS

#--------------------------------------------------------------------------------------------------------
#  Species Presence
#--------------------------------------------------------------------------------------------------------

set.seed(17) # 17 appears to be the least random number between 1 and 20 :-)
# Meta NMDS Species presence ####
NMDSSpecies <- vegan::metaMDS(PresenceMatrix, distance = "jaccard", binary = TRUE, # Using Jaccard dissimilarity
                       k = 2, # Number of dimensions
                       trymax = 2000, # Number of tries
                       engine = c("monoMDS"), # MonoMDS seems to be the best
                       autotransform = TRUE,
                       wascores = TRUE, 
                       expand = TRUE, 
                       trace = 1, 
                       plot = FALSE)

layout(matrix(c(1),1,1))       # Only one plot at once
vegan::stressplot(NMDSSpecies) # Evaluating stress
plot(NMDSSpecies) # Viewing the NMDS plot

stress.species <- vegan::goodness(NMDSSpecies) # 
mean(stress.species)

# Plotting NMDS using ordiplot ####
vegan::ordiplot(NMDSSpecies, type = "n") # drawing ordination space
orditorp(NMDSSpecies, display = "species", col = "blue", air= 0.2, cex = 0.4) #plotting species inside ordination space
orditorp(NMDSSpecies, display = "sites", col = "red", air = 0.01) #plotting sites (plots) inside ordination space
?orditorp
View(NMDSSpecies)


# Assigning plots to (old) zones ####
zones= c(rep("2012-2014", 27),
         rep("2016", 27), 
         rep("2017", 27), 
         rep("2018", 20), 
         rep("2019", 23), 
         rep("Natural South", 23), 
         rep("Plantation South", 27), 
         rep("Natural North", 23), 
         rep("Plantation North", 21)) # Assigning plots to clearing zones

ordiplot(NMDSSpecies, type = "n") # drawing ordination space
ordihull(NMDSSpecies, groups = zones, # drawing in the clearing zones as polygons
         draw = "polygon", 
         col = "grey90", 
         label = TRUE) # adding labels


# Assigning the clearing zones (old) colours ####
colors= c(rep("dark red",27),
          rep("red", 27),
          rep("orange",27),
          rep("yellow", 20),
          rep("brown", 23),
          rep("dark green", 23),
          rep("dark blue", 27),
          rep("light green", 23),
          rep("light blue", 21))

# Creating polygons for each zone
ordiplot(NMDSSpecies, type = "n")

# Zone 2012-2014 ####
vegan::ordihull(NMDSSpecies$point[grep("2012-2014",zones),], 
         draw = "polygon",
         groups=zones[zones=="2012-2014"],
         col=colors[grep("dark red",colors)],
         label=T)
# Zone 2016 ####
ordihull(NMDSSpecies$point[grep("2016",zones),], 
         draw = "polygon",
         groups=zones[zones=="2016"],
         col=colors[grep("red",colors)],
         label=T)
# Zone 2017 ####
ordihull(NMDSSpecies$point[grep("2017",zones),], 
         draw = "polygon",
         groups=zones[zones=="2017"],
         col=colors[grep("orange",colors)],
         label=T)
# Zone 2018 ####
ordihull(NMDSSpecies$point[grep("2018",zones),], 
         draw = "polygon",
         groups=zones[zones=="2018"],
         col=colors[grep("yellow",colors)],
         label=T)
# Zone 2019 ####
ordihull(NMDSSpecies$point[grep("2019",zones),], 
         draw = "polygon",
         groups=zones[zones=="2019"],
         col=colors[grep("brown",colors)],
         label=T)
# Zone Natural North ####
ordihull(NMDSSpecies$point[grep("Natural North",zones),], 
         draw = "polygon",
         groups=zones[zones=="Natural North"],
         col=colors[grep("light green",colors)],
         label=T)
# Zone Natural South ####
ordihull(NMDSSpecies$point[grep("Natural South",zones),], 
         draw = "polygon",
         groups=zones[zones=="Natural South"],
         col=colors[grep("dark green",colors)],
         label=T)
# Zone Plantation North ####
ordihull(NMDSSpecies$point[grep("Plantation North",zones),], 
         draw = "polygon",
         groups=zones[zones=="Plantation North"],
         col=colors[grep("light blue",colors)],
         label=T)
# Zone Plantation South ####
ordihull(NMDSSpecies$point[grep("Plantation South",zones),], 
         draw = "polygon",
         groups=zones[zones=="Plantation South"],
         col=colors[grep("dark blue",colors)],
         label=T)


vegan::orditorp(example_NMDS,display="species",col="red",air=0.01)
orditorp(example_NMDS,display="sites",col=c(rep("green",5),
                                            rep("blue",5)),air=0.01,cex=1.25)


...



#--------------------------------------------------------------------------------------------------------
#  Species Abundance
#--------------------------------------------------------------------------------------------------------
set.seed(17)
# Calculation Meta NMDS ####
NMDSAbundance <- vegan::metaMDS(AbundanceMatrix, distance = "bray", # Using Bray-Curtis dissimilarity
k = 2, # Number of dimensions
trymax = 2000) # Number of tries

save(NMDSAbundance, file = )
vegan::stressplot(NMDSAbundance) # Evaluating stress
plot(NMDSAbundance) # Viewing the NMDS plot

# Drawing ordination with species and plot labels
ordiplot(NMDSAbundance, type = "n") # drawing ordination space
orditorp(NMDSAbundance, display = "species", col = "blue", air= 0.01, cex = 0.4) #plotting species inside ordination space
orditorp(NMDSAbundance, display = "sites", col = "red", air = 0.01, cex = 0.3) #plotting sites (plots) inside ordination space

# Drawing abundance ordiantion using (old) zone polygons ####
vegan::ordiplot(NMDSAbundance, type = "n") # drawing ordination space

# Zone 2012-2014 ####
ordihull(NMDSAbundance$point[grep("2012-2014",zones),], 
         draw = "polygon",
         groups=zones[zones=="2012-2014"],
         col=colors[grep("dark red",colors)],
         label=T)
# Zone 2016 ####
ordihull(NMDSAbundance$point[grep("2016",zones),], 
         draw = "polygon",
         groups=zones[zones=="2016"],
         col=colors[grep("red",colors)],
         label=T)
# Zone 2017 ####
ordihull(NMDSAbundance$point[grep("2017",zones),], 
         draw = "polygon",
         groups=zones[zones=="2017"],
         col=colors[grep("orange",colors)],
         label=T)
# Zone 2018 ####
ordihull(NMDSAbundance$point[grep("2018",zones),], 
         draw = "polygon",
         groups=zones[zones=="2018"],
         col=colors[grep("yellow",colors)],
         label=T)
# Zone 2019 ####
ordihull(NMDSAbundance$point[grep("2019",zones),], 
         draw = "polygon",
         groups=zones[zones=="2019"],
         col=colors[grep("brown",colors)],
         label=T)
# Zone Natural North ####
ordihull(NMDSAbundance$point[grep("Natural North",zones),], 
         draw = "polygon",
         groups=zones[zones=="Natural North"],
         col=colors[grep("light green",colors)],
         label=T)
# Zone Natural South ####
ordihull(NMDSAbundance$point[grep("Natural South",zones),], 
         draw = "polygon",
         groups=zones[zones=="Natural South"],
         col=colors[grep("dark green",colors)],
         label=T)
# Zone Plantation North ####
ordihull(NMDSAbundance$point[grep("Plantation North",zones),], 
         draw = "polygon",
         groups=zones[zones=="Plantation North"],
         col=colors[grep("light blue",colors)],
         label=T)
# Zone Plantation South ####
ordihull(NMDSAbundance$point[grep("Plantation South",zones),], 
         draw = "polygon",
         groups=zones[zones=="Plantation South"],
         col=colors[grep("dark blue",colors)],
         label=T)





#--------------------------------------------------------------------------------------------------------
#  Functional groups Cover
#--------------------------------------------------------------------------------------------------------
set.seed(17)
# Calculation Meta NMDS ####
NMDSFunctional <- vegan::metaMDS(FunctionalMatrix, distance = "bray", # Using Bray-Curtis dissimilarity
                              k = 2, # Number of dimensions
                              trymax = 2000, # Number of tries
                              engine = c("monoMDS", "isoMDS"), # MonoMDS seems to be the best
                              autotransform = TRUE,
                              wascores = TRUE, 
                              expand = TRUE, 
                              trace = 1, 
                              plot = FALSE)

layout(matrix(c(1),1,1))       # Only one plot at once
vegan::stressplot(NMDSFunctional) # Evaluating stress
plot(NMDSFunctional) # Viewing the NMDS plot

# Drawing ordination with species and plot labels
ordiplot(NMDSFunctional, type = "n") # drawing ordination space
orditorp(NMDSFunctional, display = "species", col = "blue", air= 0.01, cex = 0.8) #plotting species inside ordination space
orditorp(NMDSFunctional, display = "sites", col = "red", air = 0.01, cex = 0.3) #plotting sites (plots) inside ordination space


#--------------------------------------------------------------------------------------------------------
#  Index values
#--------------------------------------------------------------------------------------------------------
# NMDS INDEX ####
# Regner for Artsscoreindex, Strukturindex og Naturtilstandsindex

# Creating Index NMDS 
NMDSIndex <- metaMDS(IndexMatrix, distance = "bray", # Using Bray-Curtis dissimilarity
                     k = 2, # Number of dimensions
                     trymax = 200, # Number of tries
                     engine = c("monoMDS"), # MonoMDS seems to be the best
                     autotransform = TRUE,
                     wascores = TRUE, 
                     expand = TRUE, 
                     trace = 1, 
                     plot = FALSE)
stressplot(NMDSIndex) # Evaluating stress
goodness(NMDSIndex) # statistics on my NMDS
plot(NMDSIndex)




#--------------------------------------------------------------------------------------------------------
# Comparing ordinations to oneanother ####
# Comparing species presence and abundance ordinations
species.comp <- vegan::procrustes(NMDSSpecies,NMDSAbundance)
plot(species.comp)

# Viewing the two ordinations side by side with the comparison plot
# 2x2 plot matrix layout
layout(matrix(c(1,2,3,4),2,2))
plot(NMDSSpecies) # Viewing the NMDS plot
plot(NMDSAbundance) # Viewing the NMDS plot

# Getting rid of matrix layout
layout(matrix(c(1),1,1))
#========================================================================================================
# Evaluating environmental gradients
#========================================================================================================
# Resource: https://jkzorz.github.io/2020/04/04/NMDS-extras.html

# Adding environment to presence ordination ####
EnvSpecies = envfit(NMDSSpecies, EnvMatrix, permutations = 999, strata = NULL, na.rm = TRUE)
EnvSpecies # Calling environmental fit

plot(NMDSSpecies)
plot(EnvSpecies, col = "black")

# Extracting info from nmds to be used in ggplot
species.scores = as.data.frame(scores(NMDSSpecies)) # Creating data frame containing coordinates from nmds
species.scores$Zone = Environment$Zone # Adding zone from Environment data frame
species.scores$Zone_col = Environment$Zone_col # Adding zone_col from Environment data frame
species.scores$Zone_orto = Environment$Zone_orto # Adding zone_orto from Environment data frame
species.scores$Tilstandsklasse = Index$Tilstandsklasse # Adding nature condition from Index data frame


# Extracting continuous variables for environment vectors
EnvSpecies_coord_cont = as.data.frame(scores(EnvSpecies, "vectors")) * ordiArrowMul(EnvSpecies) # extracting continuous variables as vectors

# Adding environment to abundance ordination ####
EnvAbundance = envfit(NMDSAbundance, EnvMatrix, permutations = 999, strata = NULL, na.rm = TRUE)
EnvAbundance

plot(NMDSAbundance)
plot(EnvAbundance, col = "black")

# Extracting info from nmds to be used in ggplot
abundance.scores = as.data.frame(scores(NMDSAbundance)) # Creating data frame containing coordinates from nmds
abundance.scores$Zone = Environment$Zone # Adding zone from Environment data frame
abundance.scores$Zone_col = Environment$Zone_col # Adding zone_col from Environment data frame
abundance.scores$Zone_orto = Environment$Zone_orto # Adding zone_orto from Environment data frame
abundance.scores$Tilstandsklasse = Index$Tilstandsklasse # Adding nature condition from Index data frame

# Extracting continuous variables for environment vectors
EnvAbundance_coord_cont = as.data.frame(scores(EnvAbundance, "vectors")) * ordiArrowMul(EnvAbundance) # extracting continuous variables as vectors

# Adding environment to functional ordination ####
EnvFunctional = envfit(NMDSFunctional, EnvMatrix, permutations = 999, strata = NULL, na.rm = TRUE)
EnvFunctional

plot(NMDSFunctional)
plot(EnvFunctional, col = "black")

# Extracting info from nmds to be used in ggplot
functional.scores = as.data.frame(scores(NMDSFunctional)) # Creating data frame containing coordinates from nmds
functional.scores$Zone = Environment$Zone # Adding zone from Environment data frame
functional.scores$Zone_col = Environment$Zone_col # Adding zone_col from Environment data frame
functional.scores$Zone_orto = Environment$Zone_orto # Adding zone_orto from Environment data frame
functional.scores$Tilstandsklasse = Index$Tilstandsklasse # Adding nature condition from Index data frame

# Extracting continuous variables for environment vectors
EnvFunctional_coord_cont = as.data.frame(scores(EnvFunctional, "vectors")) * ordiArrowMul(EnvFunctional) # extracting continuous variables as vectors

# Adding environment to Index ordination ####
EnvIndex = envfit(NMDSIndex, EnvMatrix, permutations = 999, strata = NULL, na.rm = TRUE)
EnvIndex

plot(NMDSIndex)
plot(EnvIndex)

# Extracting info from nmds to be used in ggplot
index.scores = as.data.frame(scores(NMDSIndex)) # Creating data frame containing coordinates from nmds
index.scores$Zone = Environment$Zone # Adding zone from Environment data frame
index.scores$Zone_col = Environment$Zone_col # Adding zone_col from Environment data frame
index.scores$Zone_orto = Environment$Zone_orto # Adding zone_orto from Environment data frame
index.scores$Tilstandsklasse = Index$Tilstandsklasse # Adding nature condition from Index data frame

# Extracting continuous variables for environment vectors
EnvIndex_coord_cont = as.data.frame(scores(EnvIndex, "vectors")) * ordiArrowMul(EnvIndex) # extracting continuous variables as vectors
View(EnvIndex_coord_cont)
#--------------------------------------------------------------------------------------------------------
# Evaluating environmental gradients as surf diagrams - I didn't use these
# Presence data ####

# Succession time
species_time_surf <- vegan::ordisurf(NMDSSpecies,EnvMatrix$Successionstid_orto, family = "gaussian")
summary(species_time_surf)

# Distance to sea
species_dist_surf <- ordisurf(NMDSSpecies,EnvMatrix$Afstand.hav, family = "gaussian")
summary(species_dist_surf)

# SRI
species_sri_surf <- ordisurf(NMDSSpecies,log(EnvMatrix$SRI), family = "gaussian")
summary(species_sri_surf)

# Hældning
species_Hældning_surf <- ordisurf(NMDSSpecies,EnvMatrix$Hældning, family = "gaussian")
summary(species_Hældning_surf)

# Elevation
species_elevation_surf <- ordisurf(NMDSSpecies,EnvMatrix$Elevation, family = "gaussian")
summary(species_elevation_surf)

# Openness
species_openness_surf <- ordisurf(NMDSSpecies,EnvMatrix$Kroneåbning15, family = "gaussian", add = TRUE)
esummary(species_openness_surf)

# Canopy Height
species_canopyh_surf <- ordisurf(NMDSSpecies,EnvMatrix$Kronehøjde15, family = "gaussian")
summary(species_canopyh_surf)

# Ved
species_Ved_surf <- ordisurf(NMDSSpecies,EnvMatrix$Ved, family = "gaussian")
summary(species_Ved_surf)

# OrganiskM
species_organic_surf <- ordisurf(NMDSSpecies,EnvMatrix$OrganiskM, family = "gaussian")
summary(species_organic_surf)


# Abundance data ####
# Succession time
abundance_time_surf <- ordisurf(NMDSAbundance,EnvMatrix$Successionstid_orto, family = "gaussian")
summary(abundance_time_surf)

# Distance to sea
abundance_dist_surf <- ordisurf(NMDSAbundance,EnvMatrix$Afstand.hav, family = "gaussian")
summary(abundance_dist_surf)

# SRI
abundance_sri_surf <- ordisurf(NMDSAbundance,EnvMatrix$SRI, family = "gaussian", add = TRUE, cex = 0.3)
summary(abundance_sri_surf)

# Hældning
abundance_Hældning_surf <- ordisurf(NMDSAbundance,EnvMatrix$Hældning, family = "gaussian")
summary(abundance_Hældning_surf)

# Elevation
abundance_elevation_surf <- ordisurf(NMDSAbundance,EnvMatrix$Elevation, family = "gaussian")
summary(abundance_elevation_surf)

# Openness
abundance_openness_surf <- ordisurf(NMDSAbundance,EnvMatrix$Kroneåbning15, family = "gaussian", add = TRUE)
esummary(abundance_openness_surf)

# Canopy Height
abundance_canopyh_surf <- ordisurf(NMDSAbundance,EnvMatrix$Kronehøjde15, family = "gaussian")
summary(abundance_canopyh_surf)

# Ved
abundance_Ved_surf <- ordisurf(NMDSAbundance,EnvMatrix$Ved, family = "gaussian")
summary(abundance_Ved_surf)

# OrganiskM
abundance_organic_surf <- ordisurf(NMDSAbundance,EnvMatrix$OrganiskM, family = "gaussian")
summary(abundance_organic_surf)

#--------------------------------------------------------------------------------------------------------
# Extracting scores ####
# Resource: https://chrischizinski.github.io/rstats/ordisurf/?fbclid=IwAR1Wu5TESljC5deqD6nhNOqMgmV3n3KV84DHrxdFfS6sC2vKWZSY5H6XvDA
#========================================================================================================
#  Visualisation of ordinations
#========================================================================================================
# Resources I have used ####
# https://jkzorz.github.io/2019/06/06/NMDS.html and 
# https://jkzorz.github.io/2020/04/04/NMDS-extras.html
#--------------------------------------------------------------------------------------------------------
# Presence Data

# Plotting Species Presence ordination with zone shapes using ggplot ####
Species_gg = ggplot(data = species.scores, # using data from species presence ordination
                    aes(x = NMDS1, y = NMDS2)) + # defining axis in esthetics
  geom_point(data = species.scores, # Using scatterplot geom
             aes(shape = Zone_orto)) +
  scale_shape_manual(values = c(15,16,17,18,5,4)) +
  theme(axis.title = element_text(size = 10, face = "bold", colour = "grey30"), # adjusting apearence of plot
        panel.background = element_blank(), 
        panel.border = element_rect(fill = NA, colour = "grey30"), 
        axis.text = element_text(), 
        legend.key = element_blank(), 
        legend.title = element_text(size = 10, face = "bold", colour = "grey30"), 
        legend.text = element_text(size = 9, colour = "grey30")) +
  labs(shape = "Zone")

Species_gg # calling plot

# Plotting Species Presence ordination with zone shapes and environment segments using ggplot ####
Species_env_gg = ggplot(data = species.scores, # using data from species presence ordination
                    aes(x = NMDS1, y = NMDS2)) + # defining axis in esthetics
  geom_point(data = species.scores, # Using scatterplot geom
             aes(shape = Zone_orto)) +
  scale_shape_manual(values = c(15,16,17,18,5,4)) +
  geom_segment(aes(x = 0, y = 0, 
                   xend = NMDS1, 
                   yend = NMDS2), 
               data = EnvSpecies_coord_cont, size =1, alpha = 0.5, colour = "grey30") + # adding environmental gradients using vectors
  geom_text(data = EnvSpecies_coord_cont, # Defining appearence of environment vectors
            aes(x = NMDS1, y = NMDS2), 
            colour = "grey30", 
            fontface = "bold", 
            label = row.names(EnvSpecies_coord_cont)) +
  theme(axis.title = element_text(size = 10, face = "bold", colour = "grey30"), # adjusting apearence of plot
        panel.background = element_blank(), 
        panel.border = element_rect(fill = NA, colour = "grey30"), 
        axis.text = element_text(),
        legend.key = element_blank(),
        legend.title = element_text(size = 10, face = "bold", colour = "grey30"), 
        legend.text = element_text(size = 9, colour = "grey30")) +
  labs(shape = "Zone")

Species_env_gg # calling plot

# Plotting Species Presence NMDS with zone shapes, environment vectors and condition colouring
Species_env_con_gg <- ggplot(data = species.scores, 
                        aes(x = NMDS1, y = NMDS2)) + 
  geom_point(data = species.scores,
             aes(shape = Zone_orto, # defining shapes from new zones by Line
               colour = Tilstandsklasse), size = 3, alpha = 0.5) + # defining colours from Novana tilstandsklasser
  scale_colour_manual(values = c("darkgreen", "limegreen", "yellow2", "darkorange", "red1")) + # manually selecting colours
  scale_shape_manual(values = c(15,16,17,18,5,4)) + # manuaaly selecting shapes (codes)
  geom_segment(aes(x = 0, y = 0, 
                   xend = NMDS1, 
                   yend = NMDS2), 
               data = EnvSpecies_coord_cont, size =1, alpha = 0.5, colour = "grey30") + # adding environmental gradients using vectors
  geom_text(data = EnvSpecies_coord_cont, # Defining appearence of environment vectors
            aes(x = NMDS1, y = NMDS2), 
            colour = "grey30", 
            fontface = "bold", 
            label = row.names(EnvSpecies_coord_cont)) + # Adding labels
  theme(axis.title = element_text(size = 10, face = "bold", colour = "grey30"), 
        panel.background = element_blank(), 
        panel.border = element_rect(fill = NA, colour = "grey30"), 
        axis.text = element_text(), 
        legend.key = element_blank(), 
        legend.title = element_text(size = 10, face = "bold", colour = "grey30"), 
        legend.text = element_text(size = 9, colour = "grey30")) + 
  labs(colour = "Tilstandsklasse", # Lining up legend with colours and shapes of figure
       shape = "Zone")
Species_env_con_gg # Calling final plot


#--------------------------------------------------------------------------------------------------------
# Abundance Data 

# Plotting Species Abundance ordination with zone shapes using ggplot ####
Abundance_gg = ggplot(data = abundance.scores, # using data from species presence ordination
                    aes(x = NMDS1, y = NMDS2)) + # defining axis in esthetics
  geom_point(data = abundance.scores, # Using scatterplot geom
             aes(shape = Zone_orto)) +
  scale_shape_manual(values = c(15,16,17,18,5,4)) +
  theme(axis.title = element_text(size = 10, face = "bold", colour = "grey30"), # adjusting apearence of plot
        panel.background = element_blank(), 
        panel.border = element_rect(fill = NA, colour = "grey30"), 
        axis.text = element_text(), 
        legend.key = element_blank(), 
        legend.title = element_text(size = 10, face = "bold", colour = "grey30"), 
        legend.text = element_text(size = 9, colour = "grey30")) +
  labs(shape = "Zone")

Abundance_gg # calling plot

# Plotting Species Abundance NMDS with environmental variables in it ####
Abundance_env_gg <- ggplot(data = abundance.scores, 
                         aes(x = NMDS1, y = NMDS2)) + 
  geom_point(data = abundance.scores, 
             aes(shape = Zone_orto,
                 colour = Tilstandsklasse), size = 3, alpha = 0.5) + 
  scale_colour_manual(values = c("darkgreen", "limegreen", "yellow2", "darkorange", "red1")) +
  scale_shape_manual(values = c(15,16,17,18,5,4)) +
  geom_segment(aes(x = 0, y = 0, 
                   xend = NMDS1, 
                   yend = NMDS2), 
               data = EnvAbundance_coord_cont, size =1, alpha = 0.5, colour = "grey30") +
  geom_text(data = EnvAbundance_coord_cont, 
            aes(x = NMDS1, y = NMDS2), 
            colour = "grey30", 
            fontface = "bold", 
            label = row.names(EnvAbundance_coord_cont)) + 
  theme(axis.title = element_text(size = 10, face = "bold", colour = "grey30"), 
        panel.background = element_blank(), 
        panel.border = element_rect(fill = NA, colour = "grey30"), 
        axis.text = element_text(),
        legend.key = element_blank(), 
        legend.title = element_text(size = 10, face = "bold", colour = "grey30"), 
        legend.text = element_text(size = 9, colour = "grey30")) + 
  labs(colour = "Tilstandsklasse",
       shape = "Zone")
Abundance_env_gg


#--------------------------------------------------------------------------------------------------------
# Functional Groups Data

# Plotting Functional group NMDS with environmental variables in it ####
Functional_env_gg <- ggplot(data = functional.scores, 
                           aes(x = NMDS1, y = NMDS2)) + 
  geom_point(data = functional.scores, 
             aes(shape = Zone_orto,
                 colour = Tilstandsklasse), size = 3, alpha = 0.5) + 
  scale_colour_manual(values = c("darkgreen", "limegreen", "yellow2", "darkorange", "red1")) +
  scale_shape_manual(values = c(15,16,17,18,5,4)) +
  geom_segment(aes(x = 0, y = 0, 
                   xend = NMDS1, 
                   yend = NMDS2), 
               data = EnvFunctional_coord_cont, size =1, alpha = 0.5, colour = "grey30") +
  geom_text(data = EnvFunctional_coord_cont, 
            aes(x = NMDS1, y = NMDS2), 
            colour = "grey30", 
            fontface = "bold", 
            label = row.names(EnvFunctional_coord_cont)) + 
  theme(axis.title = element_text(size = 10, face = "bold", colour = "grey30"), 
        panel.background = element_blank(), 
        panel.border = element_rect(fill = NA, colour = "grey30"), 
        axis.text = element_text(),
        legend.key = element_blank(), 
        legend.title = element_text(size = 10, face = "bold", colour = "grey30"), 
        legend.text = element_text(size = 9, colour = "grey30")) + 
  labs(colour = "Tilstandsklasse",
       shape = "Zone")

Functional_env_gg

#--------------------------------------------------------------------------------------------------------
# Index Data

# Plotting Index NMDS with environmental variables in it ####
Index_env_gg <- ggplot(data = index.scores, 
                            aes(x = NMDS1, y = NMDS2)) + 
  geom_point(data = index.scores, 
             aes(shape = Zone_orto,
               colour = Tilstandsklasse), size = 3, alpha = 0.5) + 
  scale_colour_manual(values = c("darkgreen", "limegreen", "yellow2", "darkorange", "red1")) +
  scale_shape_manual(values = c(15,16,17,18,5,4)) +
  geom_segment(aes(x = 0, y = 0, 
                   xend = NMDS1, 
                   yend = NMDS2), 
               data = EnvIndex_coord_cont, size =1, alpha = 0.5, colour = "grey30") +
  geom_text(data = EnvIndex_coord_cont, 
            aes(x = NMDS1, y = NMDS2), 
            colour = "grey30", 
            fontface = "bold", 
            label = row.names(EnvIndex_coord_cont)) + 
  theme(axis.title = element_text(size = 10, face = "bold", colour = "grey30"), 
        panel.background = element_blank(), 
        panel.border = element_rect(fill = NA, colour = "grey30"), 
        axis.text = element_text(),
        legend.key = element_blank(), 
        legend.title = element_text(size = 10, face = "bold", colour = "grey30"), 
        legend.text = element_text(size = 9, colour = "grey30")) + 
  labs(colour = "Tilstandsklasse",
       shape = "Zone")

Index_env_gg


# End of script --------------------------------------------------------------------------------