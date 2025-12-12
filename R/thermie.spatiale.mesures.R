#' Extraction de données de thermie spatialisées
#'
#' Cette fonction permet d'extraire les données de thermies spatiales (IRTa par exemple) sous forme de ponctuels
#' @name thermie.spatiale.mesures
#' @param thspaop_id Identifiant du projet
#' @keywords thermie
#' @import DBI
#' @import glue
#' @import sf
#' @import tidyverse
#' @export
#' @examples
#' mesures <- thermie.spatiale.mesures()
#' mesures <- thermie.spatiale.mesures("1")
#' mesures <- c(1, 2) %>% tibble(id = .) %>% group_split(id) %>% map(~ thermie.spatiale.mesures(.)) %>% reduce(rbind)
#' mesures <- thermie.spatiale.mesures("1") %>% filter(thspapo_type_valeur == "Lissage par ondelette sur le linéaire") %>% mapview::mapview(zcol = "thspapo_t_med")
#' mesures <- thermie.spatiale.mesures("2") %>% filter(thspapo_type_valeur == "Centroïde de patch froid") %>% mapview::mapview(zcol = "thspapo_a_med", cex = "thspapo_superficie")

thermie.spatiale.mesures <- function(
    thspaop_id = NA_character_)
{
  
  #### Nettoyage & reformatage ####
  test_id <- thspaop_id %>% nchar()
  if(!(test_id %>% is.na())) {
    if(test_id == 0) thspaop_id <- NA_character_}
  
  #### Collecte des données ####
  ## Ouverture de la BDD ##
  dbD <- BDD.ouverture(Type = "Data")
  
  ## Récupération des données ##
  if(is.na(thspaop_id)) mesures_v1 <- table.recuperation("thermie_spatiale_points")
  if(!is.na(thspaop_id)) mesures_v1 <- table.recuperation("thermie_spatiale_points", glue("thspapo_thspaop_id = {thspaop_id}"))

  ## Fermeture de la BDD ##
  DBI::dbDisconnect(dbD)
  
  #### Test ####
  # Test si le nom existe bien, sinon message d'avertissement #
  if(!is.na(thspaop_id)) {
    if(nrow(mesures_v1) == 0) stop(glue("Attention : nom de projet ('{thspaop_id}') absent de la base de données"))
  }

  return(mesures_v1)
  
} # Fin de la fonction