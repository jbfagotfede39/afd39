#' Extraction de données d'opérations de suivi des habitats
#'
#' Cette fonction permet d'extraire les données des opérations de suivis d'habitats
#' @name topographie.operations.habitats
#' @param id Éventuel identifiant de l'opération
#' @param tphabop_projet_id Éventuel identifiant du projet
#' @keywords topographie
#' @import DBI
#' @import glue
#' @import sf
#' @import tidyverse
#' @export
#' @examples
#' mesures <- topographie.operations.habitats()
#' mesures <- topographie.operations.habitats("12")
#' mesures <- c(17, 23) %>% tibble(id = .) %>% group_split(id) %>% map(~ topographie.operations.habitats(.)) %>% reduce(rbind)

topographie.operations.habitats <- function(
    id = NA_character_,
    tphabop_id = NA_character_
    )
{
  
  #### Nettoyage & reformatage ####
  test_id <- id %>% nchar()
  if(!(test_id %>% is.na())) {
    if(test_id == 0) id <- NA_character_}

  test_tphabop_id <- tphabop_id %>% nchar()
  if(!(test_tphabop_id %>% is.na())) {
    if(test_tphabop_id == 0) tphabop_id <- NA_character_}
  
  #### Test de cohérence ####
  # if(is.na(id) & is.na(tphabop_id)) stop("Il faut fournir au moins un id ou un tphabop_id")
  if(!is.na(id) & !is.na(tphabop_id)) stop("Il ne faut fournir qu'un id ou un tphabop_id")
  
  #### Collecte des données ####
  if(is.na(id) & is.na(tphabop_id)) operations <- table.recuperation("topographie_operations_habitats")
  if(!is.na(id)) operations <- table.recuperation("topographie_operations_habitats", glue("id = {id}"))
  if(!is.na(tphabop_id)) operations <- table.recuperation("topographie_operations_habitats", glue("tphabop_projet_id = {tphabop_projet_id}"))
  
  #### Sortie ####
  return(operations)
  
} # Fin de la fonction