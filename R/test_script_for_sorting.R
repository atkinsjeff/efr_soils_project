#Ingestion script for Carrie

#library(aqp)       I don't know this one???
library(soilDB)
library(soiltexture)
library(scales)

# add this
library(tidyverse)



#dataEF <- (data_frame(read.csv("C:\\Users\\Carrie\\Desktop\\Soils Research\\southeast_soils_data_compiled_20220415.csv", header = TRUE, stringsAsFactors = FALSE, sep = ',')))

dataEF <- read.csv("./data/southeast_soils_data_compiled_20220415.csv")


# i think this is the easiest way to do this
AlfisolUdalfs <- dataEF[dataEF$soil_name %in% tolower(c('Bude', 'Providence', 'Besner', 'Arkana', 'Rexor', 'Estate', 'Wilkes', 'Enon',
                                                
                                                'Lexington', 'Maben', 'Mecklenburg', 'Acadia', 'Tippah', 'Glenmora', 'Kolin',
                                                
                                                'Bienville', 'Attoyac', 'Bernaldo', 'Portia', 'Woden', 'Gore', 'Susquehanna')), ] 
# soils of interest

head(AlfisolUdalfs)