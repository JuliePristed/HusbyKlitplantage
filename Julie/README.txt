This directory contains the scripts for the analysis of "Working Title" by Julie Pristed.

The directory contains five scripts:

"NMDS Ordinations"

"Regression models"

"Species Indicator Analysis"

"Species Richness" and

"SRI_calculations" by Jonathan von Oppen, modified by Julie Pristed to accomodate the study site in Husby.


The script "NMDS ordination" sorts the floristic records into diversity matrixes and runs NMDS ordinations on it. Additionaly it projects environmental gradients into the ordination space. Then it extracts the values from the ordinations and use those to create visualisation plots using ggplot2.  

The script "Regression models" tests for statistical assumptions as normality and coliniarity and correlations between important variables. It analyses variance and draws boxplots. It fits regression models using mixed effect models and selects the best models using Akaikes Information Criterion.

The script "Species Indicator Analysis runs a species indicator analysis on the zones and the habitattypes found in Husby Klitplantage. The analysis is based on species abundance.

The script "Species Richness" counts the number of unique species in found in Husby Klitplantage as well as star-species - highly sensitive species indicating high nature quality. It also counts unique species in each zone and habitat type. Last it evaluates the statistical difference in number of species and star-species among the zones and habitats.

The scipt "SRI-calculations" calculates solar radiation index (SRI) from the time of the fieldwork, solar zenith angle from location latitude and longitude, declination angle and radial angle of earth to sun. It writes the results into an excel file for further work.
