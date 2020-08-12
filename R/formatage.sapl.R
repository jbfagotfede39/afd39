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


##### TODO LIST #####
# 
#####################

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
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("RAHON|Orain", .), "L'Amicale de l'Orain", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("ARBOIS|Cuisance", .), "La Cuisance", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("ARINTHOD|La Truite de la Valouse", .), "La Truite de la Valouse", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("AUMONT|Grozonne", .), "Les Amis de la Grozonne", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("BLETTERANS|La Seille Jurassienne", .), "La Seille Jurassienne", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("BOIS-D'AMONT|Orbe|BOIS", .), "La Truite de l'Orbe", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("CHAMPAGNOLE|Champagnolaise|champagnolaise", .), "La Gaule Régionale Champagnolaise", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("CHAUMERGY|La Gaule du Val de Brenne", .), "La Gaule du Val de Brenne", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("CLAIRVAUX|Clairvaliens|clairvaliens", .), "Les Pêcheurs Clairvaliens", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("CORNOD|La Truite Valousienne", .), "La Truite Valousienne", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("CROTENAY|Crotenay", .), "Société de pêche de Crotenay", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("DOLE|Bas Jura", .), "La Gaule du Bas Jura", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("Fédération|FJPPMA", .), "FJPPMA", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("FONCINE-LE-BAS|FONCINE LE BAS|La Sainette", .), "La Sainette", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("FONCINE-LE-HAUT|FONCINE LE HAUT|La Truite de la Baume", .), "La Truite de la Baume", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("F.R.D|FRD|Fraisans", .), "Fraisans-Ranchot-Dampierre", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("LA CHAUMUSSE|Lemme", .), "Les Pêcheurs de la Lemme", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("LES PLANCHES|La Langouette", .), "La Langouette", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("LONS-LE-SAUNIER|Lédonienne", .), "La Gaule Lédonienne", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("LONS-LE-SAUNIER|l'Ain|LONS La Truite", .), "La Truite de l'Ain", .))) %>% # Pas Ain tout seul car il y a également APABR
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("MOIRANS|Moirantine|moirantine", .), "La Gaule Moirantine", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("MONTBARREY|La Gaule du Val d'Amour", .), "La Gaule du Val d'Amour", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("MOREZ|Haut Jura|Haut-jura|Morez", .), "Société de pêche du Haut-Jura", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("NOZEROY|Mièges", .), "La Gaule du Val de Mièges", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("PAGNEY|Brème", .), "La Brème de l'Ognon", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("PORT-LESNEY|LESNEY|La Truite du Val d'Amour", .), "La Truite du Val d'Amour", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("SAINT-CLAUDE|Biennoise", .), "La Biennoise", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("SAINT-JULIEN|Suranaise", .), "La Gaule Suranaise", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("SALINS-LES-BAINS|Salinoise|salinoise", .), "La Gaule Régionale Salinoise", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("SELLIERES|Brennoise", .), "La Gaule Brennoise", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("SIROD|Sirod", .), "La Truite du Val de Sirod", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("THERVAY|Brochet", .), "Le Brochet de l'Ognon", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("THOIRETTE|Valouzienne", .), "La Valouzienne", .))) %>% 
    mutate_at(vars(contains("sapl")), ~(ifelse(grepl("VOITEUR|Haute Seille", .), "La Truite de la Haute Seille", .))) %>% 
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
