#--------------------------------------------------------------------------------------------------------------------------------------------#
#                       USSR - Mise en forme table SAS                                                                                       #
#--------------------------------------------------------------------------------------------------------------------------------------------#

# Packages necessaires
#---------------------
library(haven) # Lecture des tables SAS
library(plyr) # Transformation des donnees

# Fonctions nécessaires
#----------------------
source("Mise en forme table SAS/Emissions GES - Mise en forme_R_fct.R",encoding = "UTF-8")

#--------------------------------#
#   0. chargement des bases      #
#--------------------------------#
corresp_cpa_nace_rev2 <- haven::read_sas(data_file = "Mise en forme table SAS/Fichdep/corresp_cpa_nace_rev2.sas7bdat")
nomenclature_cpa_rev2 <- haven::read_sas(data_file = "Mise en forme table SAS/Fichdep/nomenclature_cpa_rev2.sas7bdat")
nomenclature_nace_rev2 <- haven::read_sas(data_file = "Mise en forme table SAS/Fichdep/nomenclature_nace_rev2.sas7bdat")

# env_ac_ainah_r2
ges_r2 <- read.table(file = "Mise en forme table SAS/Fichdep/env_ac_ainah_r2.tsv", sep = "\t", header = TRUE,stringsAsFactors = FALSE)

#----------------------------#
#   I. Import des bases      #
#----------------------------#
ges_r2 <- Import_Eurostat(schema = ges_r2)

#---------------------------------#
# II. Mise en forme des données   #
#---------------------------------#
list_an <- c("A2014","A2013","A2012","A2011","A2010")
ges_r2_modif <- modif_table(df = ges_r2,list_an = list_an)

#---------------------------------------------------#
# III. Décompositon et enresgistrement              #
#---------------------------------------------------#
dossierExport <-  "Mise en forme table SAS/Fichfin/"
decomposition(pays = c("FR","EU28"),ges = c("CO2","CH4","N2O"),prg = c(1,25,298),an = list_an)
