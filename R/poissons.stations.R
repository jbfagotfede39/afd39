#' Extraction de données de stations
#'
#' Cette fonction permet d'extraire les données complètes de l'ensemble des stations piscicoles (par défaut) ou d'une seule
#' @name poissons.stations
#' @param poi_coderhj Nom de la station
#' @keywords stations
#' @import DBI
#' @import glue
#' @import tidyverse
#' @export
#' @examples
#' stations <- poissons.stations()
#' station <- poissons.stations("SOR10-2")

poissons.stations <- function(
    poi_coderhj = NA_character_)
{
  
  #### Nettoyage & reformatage ####
  if(poi_coderhj == "") poi_coderhj <- NA_character_
  
  #### Collecte des données ####
  ## Ouverture de la BDD ##
  dbP <- BDD.ouverture(Type = "Poissons")
  
  ## Récupération des données ##
  if(is.na(poi_coderhj)) stations_v1 <- tbl(dbP,"stations") %>% collect(n = Inf)
  if(!is.na(poi_coderhj)) stations_v1 <- tbl(dbP,"stations") %>% filter(nom == poi_coderhj) %>% collect(n = Inf)
  
  ## Fermeture de la BDD ##
  DBI::dbDisconnect(dbP)
  
  #### Test ####
  # Test si le nom existe bien, sinon message d'avertissement #
  if(!is.na(poi_coderhj)) {
    if(nrow(stations_v1) == 0) warning(glue("Attention : nom de station ('{poi_coderhj}') absent de la base de données"))
  }

  return(stations_v1)
  
} # Fin de la fonction