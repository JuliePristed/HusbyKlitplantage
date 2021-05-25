#  Building Boosted Regression Trees with Rpart and caret #

# Line Larsdatter
# May 2021
# line@larsdatter.dk

#--------------------------------------------#
#### LOAD NEEDDED PACKAGES ####
#--------------------------------------------#

library(rpart) # for building trees
library(rpart.plot)# for plotting regression trees
library(caret) # classification and regression training
library(gbm)
library(dismo)

#--------------------------------------------#
#### SET WORKING DIRECTORY ####
#--------------------------------------------#

setwd("C:/Users/Line/Desktop/DataR")

#--------------------------------------------#
#### CHECK AND CORRECT IMPORTED DATA ####
#--------------------------------------------#

# ----------- OVERALL DATA ----------- #

envi <- read.csv(file= "Environment_samlet_model_ny.csv", header = TRUE, sep = ";", dec = ",") # load and rename datasheet
envi <- na.omit(envi)

write.csv(envi, "C:/Users/Line/Desktop/DataR/envi_nona.csv")

names(envi)[1] <- "id" # rename first column
names(envi) # show names of all coloumns

envi$Tilstandsklasse <- as.factor(envi$Tilstandsklasse)
envi$Type <- as.factor(envi$Type)
envi$Aspekt.klasse <- as.factor(envi$Aspekt.klasse)

str(envi) # check datatype of all columns
#View(envi)

# -------- TEST AND TRAINING SETS --------- #

# 2014

envi.no2014 <- subset(envi, envi$Ryddet !="2014", drop = FALSE)
envi.2014 <- subset(envi, envi$Ryddet == "2014", drop = FALSE)

# 2017

envi.no2017 <- subset(envi, envi$Ryddet !="2017", drop = FALSE)
envi.2017 <- subset(envi, envi$Ryddet == "2017", drop = FALSE)

# 2018

envi.no2018 <- subset(envi, envi$Ryddet !="2018", drop = FALSE)
envi.2018 <- subset(envi, envi$Ryddet == "2018", drop = FALSE)

# 2019

envi.no2019 <- subset(envi, envi$Ryddet !="2019", drop = FALSE)
envi.2019 <- subset(envi, envi$Ryddet == "2019", drop = FALSE)

# NatVeg

envi.no.natveg <- subset(envi, envi$Ryddet !="NatVeg", drop = FALSE)
envi.natveg <- subset(envi, envi$Ryddet == "NatVeg", drop = FALSE)

# Plantation

envi.no.plantation <- subset(envi, envi$Ryddet !="Plantation", drop = FALSE)
envi.plantation <- subset(envi, envi$Ryddet == "Plantation", drop = FALSE)

#--------------------------------------------#
#### BOOSTED REGRESSION TREES ####
#--------------------------------------------#

# In trees created by rpart( ), move to the LEFT branch when the stated condition is true
# Leave one out cross validation, by zone. Zone-name indicates test zone.

####------- Model with ALL variables ------ ####

# ----------- 2014 ----------- #

brt.2014.all <- rpart(Tilstandsklasse ~   # function for boosted regression tree, testing "type" against all these other factors
                    EVI
                  + Aspekt.klasse
                  + Haeldning
                  + Elevation
                  + Organisk.lag
                  + Vegetationshojde.20
                  + Dodt.ved
                  + Successionstid
                  + Afstand.til.kyst
                  + TWI
                  + Kroneaabning.15
                  + Vegetationshojde.15
                  + Kroneaabning.20,
                  data = envi.no2014,
                  method = "class")

brt.2014.all.predict <- predict(brt.2014.all, newdata = envi.2014, type = "class")
#stats::predict. Tester modellen der er trænet på data UDEN 2014, på data KUN fra 2014.

cf.2014 <- confusionMatrix(brt.2014.all.predict, envi.2014$Tilstandsklasse)
cf.2014 # Confusionmatrix for modellen trænet på data UDEN 2014-zonen

# ----------- 2017 ----------- #

brt.2017.all <- rpart(Tilstandsklasse ~   # function for boosted regression tree, testing "type" against all these other factors
                    EVI
                  + Aspekt.klasse
                  + Haeldning
                  + Elevation
                  + Organisk.lag
                  + Vegetationshojde.20
                  + Dodt.ved
                  + Successionstid
                  + Afstand.til.kyst
                  + TWI
                  + Kroneaabning.15
                  + Vegetationshojde.15
                  + Kroneaabning.20,
                  data = envi.no2017,
                  method = "class")

brt.2017.all.predict <- predict(brt.2017.all, newdata = envi.2017, type = "class")
cf.2017 <- confusionMatrix(brt.2017.all.predict, envi.2017$Tilstandsklasse)
cf.2017

rpart.plot(brt.2017.all)

# ----------- 2018 ----------- #

brt.2018.all <- rpart(Tilstandsklasse ~   # function for boosted regression tree, testing "type" against all these other factors
                    EVI
                  + Aspekt.klasse
                  + Haeldning
                  + Elevation
                  + Organisk.lag
                  + Vegetationshojde.20
                  + Dodt.ved
                  + Successionstid
                  + Afstand.til.kyst
                  + TWI
                  + Kroneaabning.15
                  + Vegetationshojde.15
                  + Kroneaabning.20,
                  data = envi.no2018,
                  method = "class")

brt.2018.all.predict <- predict(brt.2018.all, newdata = envi.2018, type = "class")
cf.2018 <- confusionMatrix(brt.2018.all.predict, envi.2018$Tilstandsklasse)
cf.2018


# ----------- 2019 ----------- #

brt.2019.all <- rpart(Tilstandsklasse ~   # function for boosted regression tree, testing "type" against all these other factors
                    EVI
                  + Aspekt.klasse
                  + Haeldning
                  + Elevation
                  + Organisk.lag
                  + Vegetationshojde.20
                  + Dodt.ved
                  + Successionstid
                  + Afstand.til.kyst
                  + TWI
                  + Kroneaabning.15
                  + Vegetationshojde.15
                  + Kroneaabning.20,
                  data = envi.no2019,
                  method = "class")

brt.2019.all.predict <- predict(brt.2019.all, newdata = envi.2019, type = "class")
cf.2019 <- confusionMatrix(brt.2019.all.predict, envi.2019$Tilstandsklasse)
cf.2019

# ------------ NatVeg ------------ #

brt.natveg.all <- rpart(Tilstandsklasse ~   # function for boosted regression tree, testing "type" against all these other factors
                      EVI
                    + Aspekt.klasse
                    + Haeldning
                    + Elevation
                    + Organisk.lag
                    + Vegetationshojde.20
                    + Dodt.ved
                    + Successionstid
                    + Afstand.til.kyst
                    + TWI
                    + Kroneaabning.15
                    + Vegetationshojde.15
                    + Kroneaabning.20,
                    data = envi.no.natveg,
                    method = "class")

brt.natveg.all.predict <- predict(brt.natveg.all, newdata = envi.natveg, type = "class")
cf.natveg <- confusionMatrix(brt.natveg.all.predict, envi.natveg$Tilstandsklasse)
cf.natveg


# ------------ Plantation ------------ #

brt.plant.all <- rpart(Tilstandsklasse ~   # function for boosted regression tree, testing "type" against all these other factors
                     EVI
                   + Aspekt.klasse
                   + Haeldning
                   + Elevation
                   + Organisk.lag
                   + Vegetationshojde.20
                   + Dodt.ved
                   + Successionstid
                   + Afstand.til.kyst
                   + TWI
                   + Kroneaabning.15
                   + Vegetationshojde.15
                   + Kroneaabning.20,
                   data = envi.no.plantation,
                   method = "class")

brt.plant.all.predict <- predict(brt.plant.all, newdata = envi.plantation, type = "class")
cf.plant <- confusionMatrix(brt.plant.all.predict, envi.plantation$Tilstandsklasse)
cf.plant

#------- Model based on full dataset -------#

# hvad er forskellen på CV ved "fitcontrol" og de ovenstående CV?

set.seed(50)

fitcontrol <- trainControl(method = "repeatedcv", number = 6, repeats = 3)
# "repeatedcv" defines that cross validation is used

gbm.fit.all <- train(Tilstandsklasse ~ 
                    EVI
                  + Aspekt
                  + Haeldning
                  + Elevation
                  + Organisk.lag
                  + Vegetationshojde.20
                  + Dodt.ved
                  + Successionstid
                  + Afstand.til.kyst
                  + TWI
                  + Kroneaabning.15
                  + Vegetationshojde.15
                  + Kroneaabning.20,
                  data = envi,
                  method = "gbm",
                  trControl = fitcontrol,
                  verbose = FALSE)

gbm.fit.all  # Explanation of result - https://topepo.github.io/caret/model-training-and-tuning.html

summary(gbm.fit.all,
        main = "Variables relative betydning for modelpræcision \nAlle variable") # hvilke variable har størst betydning


plot(gbm.fit.all)

# Model lavet ved gbm

tree.all.gbm <- gbm((unclass(Tilstandsklasse)-1) ~ 
      EVI
    + Aspekt
    + Haeldning
    + Elevation
    + Organisk.lag
    + Vegetationshojde.20
    + Dodt.ved
    + Successionstid
    + Afstand.til.kyst
    + TWI
    + Kroneaabning.15
    + Vegetationshojde.15
    + Kroneaabning.20,
    distribution = "bernoulli",
    data = envi,
    n.trees = 1000,
    cv.folds = 6)

summary.gbm(tree.all.gbm)
plot(tree.all.gbm, i.var = 8)


####------- Model with only field variables  ------ ####


# ----------- 2014 ----------- #

brt.2014.field <- rpart(Tilstandsklasse ~   # function for boosted regression tree, testing "type" against all these other factors
                    Organisk.lag
                  + Vegetationshojde.20
                  + Dodt.ved
                  + Kroneaabning.20,
                  data = envi.no2014,
                  method = "class")

rpart.plot(brt.2014.all, main = "Feltvariable, Uden 2014")

brt.2014.field.predict <- predict(brt.2014.field, newdata = envi.2014, type = "class")
cf.2014 <- confusionMatrix(brt.2014.field.predict, envi.2014$Tilstandsklasse)
cf.2014

# ----------- 2017 ----------- #

brt.2017.field <- rpart(Tilstandsklasse ~   # function for boosted regression tree, testing "type" against all these other factors
                    Organisk.lag
                  + Vegetationshojde.20
                  + Dodt.ved
                  + Kroneaabning.20,
                  data = envi.no2017,
                  method = "class")

brt.2017.field.predict <- predict(brt.2017.field, newdata = envi.2017, type = "class")
cf.2017 <- confusionMatrix(brt.2017.field.predict, envi.2017$Tilstandsklasse)
cf.2017


# ----------- 2018 ----------- #

brt.2018.field <- rpart(Tilstandsklasse ~   # function for boosted regression tree, testing "type" against all these other factors
                    Organisk.lag
                  + Vegetationshojde.20
                  + Dodt.ved
                  + Kroneaabning.20,
                  data = envi.no2018,
                  method = "class")

brt.2018.field.predict <- predict(brt.2018.field, newdata = envi.2018, type = "class")
cf.2018 <- confusionMatrix(brt.2018.field.predict, envi.2018$Tilstandsklasse)
cf.2018

# ----------- 2019 ----------- #

brt.2019.field <- rpart(Tilstandsklasse ~   # function for boosted regression tree, testing "type" against all these other factors
                    Organisk.lag
                  + Vegetationshojde.20
                  + Dodt.ved
                  + Kroneaabning.20,
                  data = envi.no2019,
                  method = "class")

brt.2019.field.predict <- predict(brt.2019.field, newdata = envi.2019, type = "class")
cf.2019 <- confusionMatrix(brt.2019.field.predict, envi.2019$Tilstandsklasse)
cf.2019

# ------------ NatVeg ------------ #

brt.natveg.field <- rpart(Tilstandsklasse ~   # function for boosted regression tree, testing "type" against all these other factors
                      Organisk.lag
                    + Vegetationshojde.20
                    + Dodt.ved
                    + Kroneaabning.20,
                    data = envi.no.natveg,
                    method = "class")

brt.natveg.field.predict <- predict(brt.natveg.field, newdata = envi.natveg, type = "class")
cf.natveg <- confusionMatrix(brt.natveg.field.predict, envi.natveg$Tilstandsklasse)
cf.natveg

# ------------ Plantation ------------ #

brt.plant.field <- rpart(Tilstandsklasse ~   # function for boosted regression tree, testing "type" against all these other factors
                     Organisk.lag
                   + Vegetationshojde.20
                   + Dodt.ved
                   + Kroneaabning.20,
                   data = envi.no.plantation,
                   method = "class")

brt.plant.field.predict <- predict(brt.plant.field, newdata = envi.plantation, type = "class")
cf.plant <- confusionMatrix(brt.plant.field.predict, envi.plantation$Tilstandsklasse)
cf.plant

#------- Model based on full dataset -------#

set.seed(50)

fitcontrol <- trainControl(method = "repeatedcv", number = 6, repeats = 3)


gbm.fit.field <- train(Tilstandsklasse ~   # function for boosted regression tree, testing "type" against all these other factors
                    Organisk.lag
                  + Vegetationshojde.20
                  + Dodt.ved
                  + Kroneaabning.20,
                  data = envi,
                  method = "gbm",
                  trControl = fitcontrol,
                  verbose = FALSE)

gbm.fit.field  # Explanation of result - https://topepo.github.io/caret/model-training-and-tuning.html

summary(gbm.fit.field, main = "Variables relative betydning for modelpræcision \nVariable fra feltarbejde")

plot(gbm.fit.field)

####------- Model with only remote sensing variables  ------ ####

# ----------- 2014 ----------- #

brt.2014.remote <- rpart(Tilstandsklasse ~   # function for boosted regression tree, testing "type" against all these other factors
                    EVI
                  + Aspekt
                  + Haeldning
                  + Elevation
                  + Successionstid
                  + Afstand.til.kyst
                  + TWI
                  + Kroneaabning.15
                  + Vegetationshojde.15,
                  data = envi.no2014,
                  method = "class")

brt.2014.remote.predict <- predict(brt.2014.remote, newdata = envi.2014, type = "class")
cf.2014 <- confusionMatrix(brt.2014.remote.predict, envi.2014$Tilstandsklasse)
cf.2014

# ----------- 2017 ----------- #

brt.2017.remote <- rpart(Tilstandsklasse ~   # function for boosted regression tree, testing "type" against all these other factors
                    EVI
                  + Aspekt
                  + Haeldning
                  + Elevation
                  + Successionstid
                  + Afstand.til.kyst
                  + TWI
                  + Kroneaabning.15
                  + Vegetationshojde.15,
                  data = envi.no2017,
                  method = "class")

brt.2017.remote.predict <- predict(brt.2017.remote, newdata = envi.2017, type = "class")
cf.2017 <- confusionMatrix(brt.2017.remote.predict, envi.2017$Tilstandsklasse)
cf.2017

# ----------- 2018 ----------- #

brt.2018.remote <- rpart(Tilstandsklasse ~   # function for boosted regression tree, testing "type" against all these other factors
                    EVI
                  + Aspekt
                  + Haeldning
                  + Elevation
                  + Successionstid
                  + Afstand.til.kyst
                  + TWI
                  + Kroneaabning.15
                  + Vegetationshojde.15,
                  data = envi.no2018,
                  method = "class")

brt.2018.remote.predict <- predict(brt.2018.remote, newdata = envi.2018, type = "class")
cf.2018 <- confusionMatrix(brt.2018.remote.predict, envi.2018$Tilstandsklasse)
cf.2018

# ----------- 2019 ----------- #

brt.2019.remote <- rpart(Tilstandsklasse ~   # function for boosted regression tree, testing "type" against all these other factors
                    EVI
                  + Aspekt
                  + Haeldning
                  + Elevation
                  + Successionstid
                  + Afstand.til.kyst
                  + TWI
                  + Kroneaabning.15
                  + Vegetationshojde.15,
                  data = envi.no2019,
                  method = "class")

brt.2019.remote.predict <- predict(brt.2019.remote, newdata = envi.2019, type = "class")
cf.2019 <- confusionMatrix(brt.2019.remote.predict, envi.2019$Tilstandsklasse)
cf.2019

# ------------ NatVeg ------------ #

brt.natveg.remote <- rpart(Tilstandsklasse ~   # function for boosted regression tree, testing "type" against all these other factors
                      EVI
                    + Aspekt
                    + Haeldning
                    + Elevation
                    + Successionstid
                    + Afstand.til.kyst
                    + TWI
                    + Kroneaabning.15
                    + Vegetationshojde.15,
                    data = envi.no.natveg,
                    method = "class")

brt.natveg.remote.predict <- predict(brt.natveg.remote, newdata = envi.natveg, type = "class")
cf.natveg <- confusionMatrix(brt.natveg.remote.predict, envi.natveg$Tilstandsklasse)
cf.natveg

# ------------ Plantation ------------ #

brt.plant.remote <- rpart(Tilstandsklasse ~   # function for boosted regression tree, testing "type" against all these other factors
                     EVI
                   + Aspekt
                   + Haeldning
                   + Elevation
                   + Successionstid
                   + Afstand.til.kyst
                   + TWI
                   + Kroneaabning.15
                   + Vegetationshojde.15,
                   data = envi.no.plantation,
                   method = "class")

brt.plant.remote.predict <- predict(brt.plant.remote, newdata = envi.plantation, type = "class")
cf.plant <- confusionMatrix(brt.plant.remote.predict, envi.plantation$Tilstandsklasse)
cf.plant

#------- Model based on full dataset -------#

set.seed(50)

fitcontrol <- trainControl(method = "repeatedcv", number = 6, repeats = 3)


gbm.fit.remote <- train(Tilstandsklasse ~   # function for boosted regression tree, testing "type" against all these other factors
                    EVI
                  + Aspekt
                  + Haeldning
                  + Elevation
                  + Successionstid
                  + Afstand.til.kyst
                  + TWI
                  + Kroneaabning.15
                  + Vegetationshojde.15,
                  data = envi,
                  method = "gbm",
                  trControl = fitcontrol,
                  verbose = FALSE)

gbm.fit.remote  # Explanation of result - https://topepo.github.io/caret/model-training-and-tuning.html

summary(gbm.fit.remote, main = "Variables relative betydning for modelpræcision \nRemote Sensing variables")

plot(gbm.fit.remote)

####--------- Plot Regression trees -------- ####
#-------- Alle variable --------#

rpart.plot(brt.2014.all, main = "Alle, Uden 2014")
rpart.plot(brt.2017.all, main = "Alle, Uden 2017")
rpart.plot(brt.2018.all, main = "Alle, Uden 2018")
rpart.plot(brt.2019.all, main = "Alle, Uden 2019")
rpart.plot(brt.natveg.all, main = "Alle, Uden Naturlig vegetation")
rpart.plot(brt.plant.all, main = "Alle, Uden Plantage")

#------- Feltvariable -------#
rpart.plot(brt.2014.field, main = "Feltvariable, Uden 2014")
rpart.plot(brt.2017.field, main = "Feltvariable, Uden 2017")
rpart.plot(brt.2018.field, main = "Feltvariable, Uden 2018")
rpart.plot(brt.2019.field, main = "Feltvariable, Uden 2019")
rpart.plot(brt.natveg.field, main = "Feltvariable, Uden Naturlig vegetation")
rpart.plot(brt.plant.field, main = "Feltvariable, Uden Plantage")

#-------- Remote sensing --------#
rpart.plot(brt.2014.remote, main = "Remote Sensing, Uden 2014")
rpart.plot(brt.2017.remote, main = "Remote Sensing, Uden 2017")
rpart.plot(brt.2018.remote, main = "Remote Sensing, Uden 2018")
rpart.plot(brt.2019.remote, main = "Remote Sensing, Uden 2019")
rpart.plot(brt.natveg.remote, main = "Remote Sensing, Uden Naturlig vegetation")
rpart.plot(brt.plant.remote, main = "Remote Sensing, Uden Plantage")