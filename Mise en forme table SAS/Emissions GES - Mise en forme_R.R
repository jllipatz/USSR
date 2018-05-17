#--------------------------------------------------------------------------------------------------------------------------------------------#
#                       USSR - Mise en forme table SAS                                                                                       #
#--------------------------------------------------------------------------------------------------------------------------------------------#

# Packages necessaires
#---------------------
library(haven) # Lecture des tables SAS
library(sqldf) # Utilisation du langage Sql

#----------------------------#
#   I. Import des bases      #
#----------------------------#

# corresp_cpa_nace_rev2
corresp_cpa_nace_rev2 <- read_sas(data_file = "Mise en forme table SAS/Fichdep/corresp_cpa_nace_rev2.sas7bdat")

# nomenclature_cpa_rev2
nomenclature_cpa_rev2 <- read_sas(data_file = "Mise en forme table SAS/Fichdep/nomenclature_cpa_rev2.sas7bdat")

# nomenclature_nace_rev2.sas7bdat
nomenclature_nace_rev2 <- read_sas(data_file = "Mise en forme table SAS/Fichdep/nomenclature_nace_rev2.sas7bdat")

# env_ac_ainah_r2
schema <- read.table(file = 'Mise en forme table SAS/Fichdep/env_ac_ainah_r2.tsv', sep = '\t', header = TRUE,stringsAsFactors = FALSE)
str(schema)

Import_Eurostat <- function(schema){
# Mise à NA des ": "
schema[schema == ": "] <- NA

# Changement des noms de colonnes
test <- 2:length(colnames(schema))
colnames(schema) <- c("champ",paste("A",substr(colnames(schema)[test],2,length(colnames(schema)[test])),sep=""))

# Les caractères spéciaux et lettres présents au milieu des données numériques sont supprimés
# (ex : b=rupture de série, c=confidentiel, d=définition différente, voir métadonnées, e=estimé, f=prévision, i=voir métadonnées (bientôt supprimé),
# n=non significatif, p=provisoire, r=révisé, s=estimation Eurostat (bientôt supprimé), u =peu fiable, z=non applicable, :=valeur manquante)
# "benscfpudirz:"
# En plus, convertion en numeric
df <- as.data.frame(sapply(schema[,2:ncol(schema)],function(x){ return(as.numeric(gsub("b|e|n|s|c|f|p|u|d|i|r|z", "", x)))}))
df$champ <- schema$champ

return(tmp)
}

#---------------------------------#
# II. Mise en forme des données   #
#---------------------------------#



