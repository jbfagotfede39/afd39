#' Extraction de données d'habitats
#'
#' Cette fonction permet d'extraire les données de suivis d'habitats
#' @name topographie.habitats
#' @param tphabop_id Identifiant de l'opération
#' @keywords topographie
#' @import DBI
#' @import glue
#' @import sf
#' @import tidyverse
#' @export
#' @examples
#' mesures <- topographie.habitats()
#' mesures <- topographie.habitats("12")
#' mesures <- c(17, 23) %>% tibble(id = .) %>% group_split(id) %>% map(~ topographie.habitats(.)) %>% reduce(rbind)

topographie.habitats <- function(
    tphabop_id = NA_character_)
{
  
  #### Nettoyage & reformatage ####
  test_id <- tphabop_id %>% nchar()
  if(!(test_id %>% is.na())) {
    if(test_id == 0) tphabop_id <- NA_character_}
  
  #### Collecte des données ####
  ## Ouverture de la BDD ##
  dbD <- BDD.ouverture(Type = "Data")
  
  ## Récupération des données ##
  if(is.na(tphabop_id)) habitats_v1 <- sf::st_read(dbD, query = "SELECT * FROM fd_production.topographie_habitats;")
  if(!is.na(tphabop_id)) habitats_v1 <- sf::st_read(dbD, query = glue("SELECT * FROM fd_production.topographie_habitats WHERE tphab_tphabop_id = {tphabop_id};"))
  
  ## Fermeture de la BDD ##
  DBI::dbDisconnect(dbD)
  
  #### Test ####
  # Test si le nom existe bien, sinon message d'avertissement #
  if(!is.na(tphabop_id)) {
    if(nrow(habitats_v1) == 0) stop(glue("Attention : id de projet ('{tphabop_id}') absent de la base de données"))
  }

  #### Nettoyage & reformatage ####
  habitats_v2 <-
    habitats_v1 %>% 
    select(-contains("_modif"))
  
  #### Sortie ####
  return(habitats_v2)
  
} # Fin de la fonction