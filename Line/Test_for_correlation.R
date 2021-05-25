# Testing correlations between variables

# Line Larsdatter
# April 2021
# line@larsdatter.dk

#---------------------------------------#
#### COLIEARITY ####
#---------------------------------------#

#### Load data - både ark med og uden kategoriske variable ####

envi <- read.csv("C:/Users/Line/Desktop/DataR/Environment_samlet_model_ny.csv", header = TRUE, sep = ";", dec = ",")

envi$Tilstandsklasse <- as.factor(envi$Tilstandsklasse)
envi$Type <- as.factor(envi$Type)
envi$Aspekt.klasse <- as.factor(envi$Aspekt.klasse)
envi$Sediment <- as.factor(envi$Sediment)

names(envi)[1] <- "id" # rename first column

envi.num <- read.csv("C:/Users/Line/Desktop/DataR/Environment_samlet_model_num.csv", header = TRUE, sep = ";", dec = ",")

names(envi.num)[1] <- "id"


#### ----------- CORRELATIONS MATRIX -------------- ####

library(psych)

pairs.panels(envi[,5:19], #http://www.sthda.com/english/wiki/scatter-plot-matrices-r-base-graphs
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE, # show correlation ellipses
             main = "Korrelationsmatrix for uafhængige variable") 


# Numeric matrix, only numeric variables                                               # Pearsons correlation assumes normality
                                                                                       # Spearman correlation does not assume normality
cor.env <- cor(envi.num[,2:14], method = "spearman")                                   # Correlation values > 0.9 
cor.env                                                                                # should be considered alarming

write.csv(cor.env, "C:/Users/Line/Desktop/DataR/spearman_matrix.csv")
