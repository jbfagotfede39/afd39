#' Reformatage des prénoms/noms en id
#'
#' Cette fonction permet de reformater les prénoms/noms du personnel en id
#' @name formatage.personnel.id
#' @param data Dataframe contenant les données à transformer
#' @param colonne_entree Champ contenant la donnée à ré-écrire
#' @param conservation Conservation du champs colonne_entree dans la sortie (avec suffixe \code{_libelle}) : \code{F} par défaut
#' @import glue
#' @import stringr
#' @import tidyverse
#' @importFrom dplyr select
#' @export
#' @examples
#' formatage.personnel.id(data, colonne_entree = "tpswot_personnel")
#' formatage.personnel.id(data, colonne_entree = "tpswot_personnel", conservation = T)

formatage.personnel.id <- function(
  data,
  colonne_entree = NA_character_,
  conservation = F
  )
  {

  #### Test de cohérence ####
  if(nchar(colonne_entree) == 0) stop("Pas de champs en entrée")
  if(colonne_entree %in% names(data) == FALSE) stop(glue("Le champs {colonne_entree} est absent du jeu de données d'entrée"))

  #### Collecte des données ####
  dbD <- BDD.ouverture("Data")
  operateurs <- tbl(dbD, in_schema("fd_referentiels", "gestion_operateurs")) %>% collect()
  DBI::dbDisconnect(dbD)
  
  #### Nettoyage & reformatage ####
  if(!str_detect(colonne_entree, "_libelle")) colonne_entree_libelle <- glue("{colonne_entree}_libelle")
  
  data_v2 <-
    data %>% 
    rename(colonne_a_joindre := !!colonne_entree) %>% 
    {if(conservation == T) mutate(., !!colonne_entree_libelle := colonne_a_joindre, .after = "colonne_a_joindre") else .} %>% 
    left_join(operateurs %>% select(id, gestop_prenom, gestop_nom) %>% mutate(personnel = glue("{gestop_prenom} {gestop_nom}"), .keep = "unused"), join_by(colonne_a_joindre == personnel)) %>%
    mutate(colonne_a_joindre := id, .keep = "unused") %>%
    # mutate(colonne_entree := personnel, .keep = "unused") %>%
    rename(!!colonne_entree := colonne_a_joindre)
  
  #### Sortie ####
  return(data_v2)
  
} # Fin de la fonction
