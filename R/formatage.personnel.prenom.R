#' Reformatage des id en prénom
#'
#' Cette fonction permet de reformater les id du personnel en prénom
#' @name formatage.personnel.prenom
#' @param data Dataframe contenant les données à transformer
#' @param colonne_entree Champ contenant la donnée à ré-écrire
#' @param conservation Conservation du champs colonne_entree dans la sortie (avec suffixe \code{_id}) : \code{F} par défaut
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
  if(!str_detect(colonne_entree, "_id")) colonne_entree_id <- glue("{colonne_entree}_id")
  
  data_v2 <-
    data %>% 
    rename(colonne_a_joindre := !!colonne_entree) %>% 
    {if(conservation == T) mutate(., !!colonne_entree_id := colonne_a_joindre, .after = "colonne_a_joindre") else .} %>% 
    left_join(operateurs %>% select(id, gestop_prenom) %>% rename(personnel = gestop_prenom), join_by(colonne_a_joindre == id)) %>%
    mutate(colonne_a_joindre := personnel, .keep = "unused") %>%
    # mutate(colonne_entree := personnel, .keep = "unused") %>%
    rename(!!colonne_entree := colonne_a_joindre)
  
  #### Sortie ####
  return(data_v2)
  
} # Fin de la fonction