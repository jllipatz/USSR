#--------------------------------------------------------------------------------------------------------------------------------------------#
#                       USSR - Mise en forme table SAS                                                                                       #
#--------------------------------------------------------------------------------------------------------------------------------------------#

#----------------------------#
#   I. Import des bases      #
#----------------------------#

# Chargement de la base 
schema <- read.table(file = 'Mise en forme table SAS/Fichdep/env_ac_ainah_r2.tsv', sep = '\t', header = TRUE,stringsAsFactors = FALSE)
str(schema)

# Mise à NA des ": "
schema[schema == ": "] <- NA

# Changement des noms de colonnes
test <- 2:length(colnames(schema))
colnames(schema) <- c("champ",paste("A",substr(colnames(schema)[test],2,length(colnames(schema)[test])),sep=""))

# Les caractères spéciaux et lettres présents au milieu des données numériques sont supprimés 
# (ex : b=rupture de série, c=confidentiel, d=définition différente, voir métadonnées, e=estimé, f=prévision, i=voir métadonnées (bientôt supprimé), 
# n=non significatif, p=provisoire, r=révisé, s=estimation Eurostat (bientôt supprimé), u =peu fiable, z=non applicable, :=valeur manquante)
# "benscfpudirz:"
schema[,2:ncol(schema)] <- gsub("b|e|n|s|c|f|p|u|d|i|r|z", "", schema[,2:ncol(schema)])
