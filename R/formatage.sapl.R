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
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("RAHON|Rahon|Orain", .), "L'Amicale de l'Orain", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("ARBOIS|Arbois|Cuisance", .), "La Cuisance", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("ARINTHOD|La Truite de la Valouse", .), "Pêche en Petite Montagne", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("AUMONT|Grozonne", .), "Les Amis de la Grozonne", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("BLETTERANS|Bletterans|La Seille Jurassienne", .), "La Seille Jurassienne", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("BOIS-D'AMONT|Orbe|BOIS", .), "La Truite de l'Orbe", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("CHAMPAGNOLE|Champagnole|Champagnolaise|champagnolaise", .), "La Gaule Régionale Champagnolaise", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("CHAUMERGY|Chaumergy|La Gaule du Val de Brenne", .), "La Gaule du Val de Brenne", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("CLAIRVAUX|Clairvaliens|clairvaliens|Pays des Lacs", .), "Ain - Pays des Lacs", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("CORNOD|La Truite Valousienne", .), "Pêche en Petite Montagne", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("CROTENAY|Crotenay", .), "Société de pêche de Crotenay", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("DOLE|Dole|Bas Jura|Bas-Jura", .), "La Gaule du Bas Jura", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("Fédération|FJPPMA", .), "Fédération du Jura pour la Pêche et la Protection du Milieu Aquatique", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("FONCINE-LE-BAS|FONCINE LE BAS|La Sainette", .), "La Sainette", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("FONCINE-LE-HAUT|FONCINE LE HAUT|La Truite de la Baume", .), "La Truite de la Baume", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("F.R.D|FRD|Fraisans", .), "Fraisans-Ranchot-Dampierre", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("LA CHAUMUSSE|Lemme", .), "Les Pêcheurs de la Lemme", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("LES PLANCHES|La Langouette", .), "La Langouette", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("LONS-LE-SAUNIER|Lons le Saunier|Lédonienne", .), "La Gaule Lédonienne", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("l'Ain|LONS La Truite", .), "Ain - Pays des Lacs", .))) %>% # Pas Ain tout seul car il y a également APABR
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("MOIRANS|Moirantine|moirantine", .), "Vouglans Pêche", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("MONTBARREY|La Gaule du Val d'Amour", .), "La Gaule du Val d'Amour", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("MOREZ|Haut Jura|Haut-jura|Morez", .), "Société de pêche du Haut-Jura", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("NOZEROY|Mièges", .), "La Gaule du Val de Mièges", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("PAGNEY|Pagney|Brème", .), "La Brème de l'Ognon", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("PORT-LESNEY|LESNEY|La Truite du Val d'Amour", .), "La Gaule Régionale Salinoise", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("SAINT-CLAUDE|Biennoise|Saint Claude", .), "La Biennoise", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("SAINT-JULIEN|Suranaise", .), "Pêche en Petite Montagne", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("SALINS-LES-BAINS|Salins-les-bains|Salinoise|salinoise", .), "La Gaule Régionale Salinoise", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("SELLIERES|Sellières|Brennoise", .), "La Gaule Brennoise", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("SIROD|Sirod", .), "La Truite du Val de Sirod", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("THERVAY|Thervay|Brochet", .), "Le Brochet de l'Ognon", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("THOIRETTE|Thoirette|Valouzienne|Pêche en Petite Montagne", .), "Pêche en Petite Montagne", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("VOITEUR|Voiteur|Haute Seille", .), "La Truite de la Haute Seille", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("Ain Bresse|Revermont|Bourg-en-Bresse|BOURG-EN-BRESSE", .), "Amicale des Pêcheurs Ain Bresse Revermont", .))) # Doit forcément être placé après La Truite de l'Ain, détectée avec Ain
    
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
