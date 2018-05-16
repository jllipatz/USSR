#--------------------------------------------------------------------------------------------------------------------------------------------------------------#
#                                                           USSR - Rrégionalisation  - fonctions complementaires                                               #
#--------------------------------------------------------------------------------------------------------------------------------------------------------------#


Rregionalisation <- function(clap_ent,clap_etab,fichierxls=NULL){
  
  # Bases CLAP
  clap_ent <- haven::read_sas(data_file = "donnees/clap/entrep.sas7bdat")
  clap_ent <- clap_ent[,c("siren","nomen","DCSIEGE","APEN","REMBRUTE","CJ")]
  
  clap_etab <- haven::read_sas(data_file = "donnees/clap/etab.sas7bdat")
  clap_etab <- clap_etab[substr(clap_etab$DC,1,3) != "974",]
  clap_etab$codep <- substr(clap_etab$DC,1,3)
  clap_etab$pop <- 1
  
  #----------------------------------#
  #    II. Calcul de statistiques    #
  #----------------------------------#
  
  # agreger les donnees etab par siren et deapartement
  ent_dep <- ddply(.data = clap_etab,.variables = c("siren","codep"),.fun = summarise,
                   remb_ent_dep = sum(REMBRUTE),
                   nb_etab=sum(pop)) 
  
  ent_dep_tot <- ddply(.data = ent_dep,.variables = c("siren"),.fun = summarise,
                       nb_tot = sum(nb_etab),
                       nb_max = max(nb_etab),
                       rem_max = max(remb_ent_dep))
  
  # pour chaque croisement sirenXdepartement, calcul du taux rembrute departement / rembrute totale 
  clap_taux <- merge(ent_dep,ent_dep_tot,by="siren",all.x=TRUE)
  clap_regio <- merge(clap_ent,clap_taux,by="siren",all=FALSE)
  clap_regio$Part_Masse_Salariale <-  clap_regio$remb_ent_dep / clap_regio$REMBRUTE
  
  #----------------------------------#
  #    III. Redressement             #
  #----------------------------------#
  
  # redressement de ce taux en fonction des differents cas 
  clap_regio <- redressementSG(df = clap_regio)
  # table(clap_regio$redressement)
  
  #----------------------------------#
  #    IV. Calage                    #
  #----------------------------------#
  # calage des redressements sur la somme des part_masse_salariales non redressees, pour ne pas sur estimer les variables d'interets
  clap_regio_redresse <- calageSG(df=clap_regio)
  
  # integration des forcages manuels de regionalisation 
  if(!is.null(fichierxls)){
    regio_manuel <- xlsx::read.xlsx(fichierxls, sheetName = "regio")
    regio_manuel <- regio_manuel[regio_manuel$siren != "012345678",]
    colnames(regio_manuel)[colnames(regio_manuel) %in% c("siren","codep","dcsiege")] <- paste("new_",c("siren","codep","dcsiege"),sep="")
    clap_regio_force <- merge(clap_regio_redresse,regio_manuel,by.x=c("siren","codep"),by.y=c("new_siren","new_codep"),all=TRUE)
    
    test <- !is.na(clap_regio_force$part)
    clap_regio_force$clap[test] <- clap_regio_force$part[test]
    clap_regio_force$redressement[test] <- 40
    clap_regio_force$DCSIEGE[test] <- as.character(clap_regio_force$new_dcsiege[test])
    clap_regio_force <- clap_regio_force[,!colnames(clap_regio_force) %in% c("part","new_siren","new_codep","new_dcsiege","commentaires")]
  }else{
    clap_regio_force <- clap_regio_redresse
  }
  
  
  # ultime redressement : si le redressement sur un autre département a fait modifié le taux initial non redresse de plus de 1e-5 
  # => il a ete reventillé 
  # => redressement = 31
  # ajout de la variable permettant de distinguer les entreprises locales (S111) des entreprises non régionales (S112)
  # => entrep_loc = 1 si codep=dcsiege, 0 sinon
  
  clap_regio_force$entrep_loc <- 0
  clap_regio_force$entrep_loc[clap_regio_force$codep == substr(clap_regio_force$DCSIEGE,1,3)] <- 1
  test <- clap_regio_force$redressement == 0 & abs(clap_regio_force$Part_Masse_Salariale - clap_regio_force$part_masse_salariale_redressee) > 0.00001 
  clap_regio_force$redressement[test] <- 31
  
  table(clap_regio_force$redressement)
  
  # Factorisation de la variable redressement
  redressement.levels <- c("0","10","11","12","20","30","31","40")
  redressement.label <- c("Pas de redressement","1 car entreprise individuelle, proportionnellement au nombre etab/entrep",
                          "1 car siege en DFA, proportionnellement au nombre etab/entrep","1 car absent clap","0 car rem(entrep) = 0",
                          "max, proportionnellement au nombre etab/entrep","reventillé sur les dfa presents","forcé manuellement")
  
  # Ajout des libelles
  clap_regio_force$redressement.factor <- factor(x = as.character(clap_regio_force$redressement),levels = redressement.levels,labels = redressement.label)
  
  # table(clap_regio_force$redressement.factor)
  # summary(clap_regio_force)
  
  return(clap_regio_force)
}

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
redressementSG <- function(df){
  
  df$siegeDfa <- substr(df$DCSIEGE,1,3) %in% c('971','972','973')
  df$redressement <- 0
  
  # Cas : Masse salariale manquante
  test <- is.na(df$Part_Masse_Salariale)
  df$Part_Masse_Salariale[test] <- with(df[test,],
                                        ifelse(substr(CJ,1,1) %in% c("1","2") | siegeDfa,nb_etab/nb_tot,0))
  df$redressement[test] <- with(df[test,],
                                ifelse(substr(CJ,1,1) %in% c("1","2") ,10,
                                       ifelse(siegeDfa, 11, 20)))
  # Cas : Masse salariale nulle  
  test <- df$Part_Masse_Salariale == 0 & df$rem_max != 0
  df$Part_Masse_Salariale[test] <- with(df[test,],rem_max/REMBRUTE*nb_etab/nb_max)
  df$redressement[test] <- 30 
  
  return(df)
}

#----------------------------------------------------------------------------------------------------------------------------------------------------------------
calageSG <- function(df){
  
  df <- merge(df,
              ddply(.data = df[df$redressement<20,],.variables = "siren",.fun = summarise,
                    somme_part_nored = sum(Part_Masse_Salariale)),
              by="siren",all.x=TRUE)
  
  df <- merge(df,
              ddply(.data = df,.variables = "siren",.fun = summarise,
                    somme_part_tot = sum(Part_Masse_Salariale)),
              by="siren",all.x=TRUE)
  
  df$part_nored <- ifelse(is.na(df$somme_part_nored),0,df$somme_part_nored)
  df$part_tot <- ifelse(df$somme_part_tot == 0,1,df$somme_part_tot) 
  df$part_masse_salariale_redressee <- df$Part_Masse_Salariale * ifelse(is.na(df$somme_part_nored),0,df$somme_part_nored) / ifelse(df$somme_part_tot == 0 ,1, df$somme_part_tot)
  
  return(df)
}

