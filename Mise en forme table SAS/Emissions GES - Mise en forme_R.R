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
  
  return(df)
}

ges_r2 <- Import_Eurostat(schema = schema)
str(ges_r2)

#---------------------------------#
# II. Mise en forme des données   #
#---------------------------------#



# Selection des annees
nbAn <- 5

# Selection des 5 dernières années
df <- ges_r2[1:1000,c("champ","A2014","A2013","A2012","A2011","A2010")]

# Creation des variables
tmp <- strsplit( df$champ, "," )
tmp <- as.data.frame(do.call(rbind,tmp))
colnames(tmp) <- c("ges","code_tmp","unite","pays")
df <- cbind(df,tmp)

# Fusion des nomenclature
nomenclature <- merge(nomenclature_nace_rev2,corresp_cpa_nace_rev2,by="code_nace",all.x=TRUE)
nomenclature <- merge(nomenclature,nomenclature_cpa_rev2,by="code_cpa",all.x=TRUE)


colnames(corresp_cpa_nace_rev2)
colnames(nomenclature_cpa_rev2)
colnames(nomenclature_nace_rev2)

# test <- sqldf("	CREATE TABLE ges_r2_modif AS
# 	SELECT T1.*,
#               CASE WHEN substr(T2.code_nace,1,3)='HH_' THEN T2.num_nace ELSE \"NA\" END AS num_nace,
#               CASE WHEN substr(T2.code_nace,1,3)='HH_' THEN T2.code_nace ELSE '' END AS code_nace,
#               CASE WHEN substr(T2.code_nace,1,3)='HH_' THEN T2.libelle_nace ELSE '' END AS libelle_nace,
#               T4.num_cpa, T4.code_cpa, T4.libelle_cpa
#               FROM df AS T1
#               LEFT JOIN nomenclature_nace_rev2 AS T2 ON T1.code_tmp=T2.code_nace
#               LEFT JOIN corresp_cpa_nace_rev2 AS T3 ON T1.code_tmp=T3.code_nace
#               LEFT JOIN nomenclature_cpa_rev2 AS T4 ON T3.code_cpa=T4.code_cpa
#               WHERE T1.unite='THS_T' AND substr(T1.pays,1,2) IN ('FR','EU') and (T4.num_cpa IS NOT \"NA\" or T2.num_nace IS NOT \"NA\")
#               ORDER BY T1.pays, T1.ges, T4.num_cpa, T2.num_nace;")




