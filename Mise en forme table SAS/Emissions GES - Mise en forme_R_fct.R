#--------------------------------------------------------------------------------------------------------------------------------------------#
#                       USSR - Mise en forme table SAS - fonctions                                                                           #
#--------------------------------------------------------------------------------------------------------------------------------------------#


# I. Import des bases 
#-------------------------------------------------------------------------------------------------------------------------------------------------
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


# II. Mise en forme des données
#-------------------------------------------------------------------------------------------------------------------------------------------------
modif_table <- function(df,list_an=c("A2014","A2013","A2012","A2011","A2010")){
  
  # Creation des variables "ges","code_nace","unite","pays"
  tmp <- strsplit(df$champ, "," )
  tmp <- as.data.frame(do.call(rbind,tmp))
  colnames(tmp) <- c("ges","code_nace","unite","pays")
  df <- cbind(df,tmp)
  
  # Fusion des nomenclatures
  nomenclature <- merge(nomenclature_nace_rev2,corresp_cpa_nace_rev2,by="code_nace",all.x=TRUE)
  nomenclature <- merge(nomenclature,nomenclature_cpa_rev2,by="code_cpa",all.x=TRUE)
  df <- merge(df,nomenclature,by="code_nace",all.x=TRUE)
  
  # Selection des variables et selection des modalités
  lst_var <- c("champ",list_an,"pays","unite","ges","num_nace","code_nace","libelle_nace","num_cpa","code_cpa","libelle_cpa")
  df <- df[df$unite=="THS_T" & substr(df$pays,1,2) %in% c("FR","EU") & (!is.na(df$num_nace) | !is.na(df$num_cpa)),lst_var]
  
  # Suppression des code, num et libelle nace
  test <- !(substr(df$code_nace,1,3)=="HH_")
  df$num_nace[test] <- NA
  df$libelle_nace[test] <- NA
  df$code_nace[test] <- NA
  
  return(df[order(df$pays,df$ges,df$num_cpa,df$num_nace),]) 
}


# III. Décompositon
#-------------------------------------------------------------------------------------------------------------------------------------------------
decomposition <- function(pays,ges,prg,an) {
  # Toutes les combinaisons de paramètres
  combinaison <- expand.grid(pays,ges,prg,an,stringsAsFactors = F)
  colnames(combinaison) <- c("pays","ges","prg","an")
  
  # Decomposition et enregistrement
  apply(combinaison,1,FUN = groupAndSave)
  
}

groupAndSave <- function(combinaison){
  # Code pays
  codpays <- substr(combinaison[1],1,2)
  
  # Preparation base
  df.tmp <- ges_r2_modif[as.character(ges_r2_modif$pays) == combinaison[1] & ges_r2_modif$ges == combinaison[2],]
  df.tmp$bilan <- df.tmp[,combinaison[4]] * as.numeric(combinaison[3])
  
  # Production 
  production <- merge(plyr::ddply(.data = df.tmp,.variables = c("pays","ges","num_cpa"),.fun = summarise,bilan = sum(bilan)),
                      nomenclature_cpa_rev2,
                      by="num_cpa",all.x=TRUE
  )
  
  # Menage
  menage <- merge(plyr::ddply(.data = df.tmp,.variables = c("pays","ges","num_nace"),.fun = summarise,bilan = sum(bilan)),
                  nomenclature_nace_rev2,
                  by="num_nace",
                  all.x=TRUE
  )
  # Changement de nom
  nomBilan <- paste(combinaison[2],"_",codpays,"_",substr(combinaison[4],2,nchar(combinaison[4])),"_prg_",combinaison[3],sep="")
  colnames(menage)[colnames(menage) == "bilan"] <- nomBilan
  colnames(production)[colnames(production) == "bilan"] <- nomBilan
  
  # Enregistrement en csv
  write.csv2(x = menage,
             file=paste(dossierExport,nomBilan,"_menage.csv",sep=""))
  write.csv2(x = production,
             file=paste(dossierExport,nomBilan,"_production.csv",sep=""))
}

