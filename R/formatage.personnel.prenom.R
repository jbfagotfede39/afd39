#' Reformatage des id en prénom
#'
#' Cette fonction permet de reformater les id du personnel en prénom
#' @name formatage.personnel.prenom
#' @param data Dataframe contenant les données à transformer
#' @param colonne_entree Champ contenant la donnée à ré-écrire
#' @import glue
#' @import stringr
#' @import tidyverse
#' @importFrom dplyr select
#' @export
#' @examples
#' formatage.personnel.prenom(data, colonne_entree = "tpswot_personnel")
#' formatage.personnel.prenom(data, colonne_entree = "tpswot_personnel", conservation = T)

formatage.personnel.prenom <- function(
  data,
  colonne_entree = NA_character_
  )
  {

  #### Test de cohérence ####
  if(nchar(colonne_entree) == 0) stop("Pas de champs en entrée")
  if(colonne_entree %in% names(data) == FALSE) stop(glue("Le champs {colonne_entree} est absent du jeu de données d'entrée"))

  #### Collecte des données ####
  operateurs <- table.recuperation("gestion_operateurs")
  
  #### Nettoyage & reformatage ####
  data_v2 <-
    data %>% 
    rename(colonne_a_joindre := !!colonne_entree) %>% 
    left_join(operateurs %>% select(id, gestop_prenom) %>% rename(personnel = gestop_prenom), join_by(colonne_a_joindre == id)) %>%
    select(-colonne_a_joindre)
  
  #### Sortie ####
  return(data_v2)
  
} # Fin de la fonction