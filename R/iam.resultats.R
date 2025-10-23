#' Extraction des résultats d'IAM
#'
#' Cette fonction permet de collecter les résultats des IAM
#' @name iam.resultats
#' @param tpiam_op_id Identifiant de l'opération
#' @keywords topographie
#' @import DBI
#' @import glue
#' @import sf
#' @import tidyverse
#' @export
#' @examples
#' mesures <- iam.resultats()
#' mesures <- iam.resultats("12")
#' mesures <- c(17, 23) %>% tibble(id = .) %>% group_split(id) %>% map(~ iam.resultats(.)) %>% reduce(rbind)

iam.resultats <- function(
    tpiam_op_id = NA_character_
)
{
  
  #### Nettoyage & reformatage ####
  test_tpiam_op_id <- tpiam_op_id %>% nchar()
  if(!(test_tpiam_op_id %>% is.na())) {
    if(test_tpiam_op_id == 0) tpiam_op_id <- NA_character_}
  
  #### Test de cohérence ####
  # if(is.na(tpiam_op_id)) stop("Il faut fournir un tpiam_op_id")
  
  #### Collecte des données ####
  if(is.na(tpiam_op_id)) iam <- table.recuperation("topographie_iam")
  if(!is.na(tpiam_op_id)) iam <- table.recuperation("topographie_iam", glue("tpiam_op_id = {tpiam_op_id}"))
  
  #### Sortie ####
  return(iam)
  
} # Fin de la fonction
