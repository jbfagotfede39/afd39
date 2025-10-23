#' Extraction de données de topographie
#'
#' Cette fonction permet d'extraire les données de mesures topographiques
#' @name topographie.mesures
#' @param tplvop_id Identifiant du projet
#' @keywords topographie
#' @import DBI
#' @import glue
#' @import sf
#' @import tidyverse
#' @export
#' @examples
#' mesures <- topographie.mesures()
#' mesures <- topographie.mesures("1")
#' mesures <- c(17, 23) %>% tibble(id = .) %>% group_split(id) %>% map(~ topographie.mesures(.)) %>% reduce(rbind)

topographie.mesures <- function(
    tplvop_id = NA_character_)
{
  
  #### Nettoyage & reformatage ####
  if(tplvop_id == "") tplvop_id <- NA_character_
  
  #### Collecte des données ####
  ## Ouverture de la BDD ##
  dbD <- BDD.ouverture(Type = "Data")
  
  ## Récupération des données ##
  if(is.na(tplvop_id)) leves_v1 <- sf::st_read(dbD, query = "SELECT * FROM fd_production.topographie_leves;")
  if(!is.na(tplvop_id)) leves_v1 <- sf::st_read(dbD, query = glue("SELECT * FROM fd_production.topographie_leves WHERE tplv_tplvop_id = {tplvop_id};"))
  
  ## Fermeture de la BDD ##
  DBI::dbDisconnect(dbD)
  
  #### Test ####
  # Test si le nom existe bien, sinon message d'avertissement #
  if(!is.na(tplvop_id)) {
    if(nrow(leves_v1) == 0) warning(glue("Attention : nom de projet ('{tplvop_id}') absent de la base de données"))
  }

  return(leves_v1)
  
} # Fin de la fonction