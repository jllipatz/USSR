#--------------------------------------------------------------------------------------------------------------------------------------------------------------#
#                                                           USSR - Rrégionalisation main                                                                       #
#--------------------------------------------------------------------------------------------------------------------------------------------------------------#

# Nicolas Kempf

# MAJ : 15.05.2018

# Packages nécessaires
#---------------------
library(haven) # Lecture des tables SAS
library(plyr) # transformation et calculs statistiques
library(xlsx) # Lecture/ecriture de fichier excel

# Ajout de fonctions complémentaires
source("Rregionalisation_fct.R",encoding = "UTF-8")

#-------------------------------#
#    I. Chargement des bases    #
#-------------------------------#
clap_ent <- haven::read_sas(data_file = "donnees/clap/entrep.sas7bdat")
clap_etab <- haven::read_sas(data_file = "donnees/clap/etab.sas7bdat")

#----------------------------------#
#    II. Redressement et calage    #
#----------------------------------#
regionalisation <- Rregionalisation(clap_ent = clap_ent,clap_etab = clap_etab,fichierxls = "donnees/clap/clap.xls")

#----------------------------------#
#    III. Vérification             #
#----------------------------------#
summary(regionalisation)

#----------------------------------#
#    IV. Enregistrement            #
#----------------------------------#
save(regionalisation,file = "donnees/regionalisation.RData")
write.csv2(x = regionalisation,file = "donnees/regionalisation.csv")
