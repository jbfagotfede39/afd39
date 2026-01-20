#' Ré-encodage des SAPL
#'
#' Cette fonction permet de ré-encoder les noms des SAPL
#' @name formatage.sapl
#' @keywords formatage
#' @param data dataframe contenant au moins une colonne dont l'intitulé contient sapl
#' @import aquatools
#' @import DBI
#' @import glue
#' @import tidyverse
#' @export
#' @examples
#' formatage.sapl(data)

formatage.sapl <- function(
    data = data
){
  
  #### Évaluation des choix ####
  
  if(any(grepl("sapl",names(data))) == FALSE){
    stop("Pas de colonne contenant sapl")
  }
  
  #### Importation ####
  dbD <- BDD.ouverture("Data")
  sapl <- sf::st_read(dbD, query = "SELECT * FROM fd_production.pecheloisir_sapl;")
  DBI::dbDisconnect(dbD)
  
  #### Fusion des trois morceaux ####
  datapropre <-
    data %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("(?i)RAHON|Rahon|Orain", .), "L'Amicale de l'Orain", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("(?i)ARBOIS|Arbois|Cuisance", .), "La Cuisance", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("(?i)ARINTHOD|La Truite de la Valouse", .), "Pêche en Petite Montagne", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("(?i)AUMONT|Grozonne", .), "Les Amis de la Grozonne", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("(?i)BLETTERANS|Bletterans|La Seille Jurassienne", .), "La Seille Jurassienne", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("(?i)BOIS-D'AMONT|Orbe|BOIS", .), "La Truite de l'Orbe", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("(?i)CHAMPAGNOLE|Champagnole|Champagnolaise|champagnolaise", .), "La Gaule Régionale Champagnolaise", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("(?i)CHAUMERGY|Chaumergy|La Gaule du Val de Brenne", .), "La Gaule du Val de Brenne", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("(?i)CLAIRVAUX|Clairvaliens|clairvaliens|Pays des Lacs", .), "Ain - Pays des Lacs", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("(?i)CORNOD|La Truite Valousienne", .), "Pêche en Petite Montagne", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("(?i)CROTENAY|Crotenay", .), "Société de pêche de Crotenay", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("(?i)DOLE|Dole|Bas Jura|Bas-Jura", .), "La Gaule du Bas Jura", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("(?i)Fédération|FJPPMA|FEDE JURA PECHE PROTECT MILIEU AQUAT|FEDERATION DU JURA POUR LA PECHE ET LA PROTECTION DU MILIEU", .), "Fédération du Jura pour la Pêche et la Protection du Milieu Aquatique", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("(?i)FONCINE-LE-BAS|FONCINE LE BAS|La Sainette", .), "La Sainette", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("(?i)FONCINE-LE-HAUT|FONCINE LE HAUT|La Truite de la Baume", .), "La Truite de la Baume", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("(?i)F.R.D|FRD|Fraisans", .), "Fraisans-Ranchot-Dampierre", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("(?i)LA CHAUMUSSE|Lemme", .), "Les Pêcheurs de la Lemme", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("(?i)LES PLANCHES|La Langouette", .), "La Langouette", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("(?i)LONS-LE-SAUNIER|Lons le Saunier|Lédonienne", .), "La Gaule Lédonienne", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("(?i)l'Ain|LONS La Truite", .), "Ain - Pays des Lacs", .))) %>% # Pas Ain tout seul car il y a également APABR
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("(?i)MOIRANS|Moirantine|moirantine", .), "Vouglans Pêche", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("(?i)MASSELETTE", .), "La Masselette", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("(?i)MONTBARREY|La Gaule du Val d'Amour", .), "La Gaule du Val d'Amour", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("(?i)MOREZ|Haut Jura|Haut-jura|Morez|HT JURA", .), "Société de pêche du Haut-Jura", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("(?i)NOZEROY|Mièges", .), "La Gaule du Val de Mièges", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("(?i)PAGNEY|Pagney|Brème", .), "La Brème de l'Ognon", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("(?i)PORT-LESNEY|LESNEY|La Truite du Val d'Amour", .), "La Gaule Régionale Salinoise", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("(?i)SAINT-CLAUDE|Biennoise|Saint Claude", .), "La Biennoise", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("(?i)SAINT-JULIEN|Suranaise", .), "Pêche en Petite Montagne", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("(?i)SALINS-LES-BAINS|Salins-les-bains|Salinoise|salinoise", .), "La Gaule Régionale Salinoise", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("(?i)SELLIERES|Sellières|Brennoise", .), "La Gaule Brennoise", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("(?i)SIROD|Sirod", .), "La Truite du Val de Sirod", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("(?i)THERVAY|Thervay|Brochet", .), "Le Brochet de l'Ognon", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("(?i)THOIRETTE|Thoirette|Valouzienne|Pêche en Petite Montagne", .), "Pêche en Petite Montagne", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("(?i)VOITEUR|Voiteur|Haute Seille", .), "La Truite de la Haute Seille", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("(?i)Bisontine", .), "Pêche Sportive Bisontine", .))) %>% # LA PECHE SPORTIVE BISONTINE
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("(?i)Ain Bresse|Revermont|Bourg-en-Bresse|BOURG-EN-BRESSE", .), "Bourg-en-Bresse", .))) %>% # APABR - Doit forcément être placé après La Truite de l'Ain, détectée avec Ain
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("(?i)Oyonnax", .), "Amicale des Pêcheurs Ain Bresse Revermont", .))) # RLHB - Doit forcément être placé après La Truite de l'Ain, détectée avec Ain
  
  #### Test de complétude ####
  listesapl <- sapl$plosapl_sapl
  Erreurs <-
    datapropre %>% 
    filter_at(vars(contains("sapl")), all_vars(!(. %in% listesapl))) %>% 
    select(contains("sapl")) 
  
  if(nrow(Erreurs) > 0){warning(glue("Il y a des cas non reconnus : {distinct(Erreurs)}"))}
  
  #### Retour du tableau complet ####
  return(datapropre)
  
} # Fin de la fonction
