#' Extraction des données des captures de MI
#'
#' Récupère les données de captures d'une opération de suivi MI
#' @name MI.captures
#' @param id_operation id de l'opération recherchée
#' @param sortie Forme du dataframe de sortie - \code{Brute} (par défault) ou \code{Complète} (avec données des prélèvements et de station)
#' @keywords donnees
#' @import DBI
#' @import tidyverse
#' @export
#' @examples
#' MI.captures()
#' MI.captures(12)
#' c(12, 14) %>% purrr::map_dfr(~ MI.captures(.))
#' captures <- operations %>% group_split(id) %>% purrr::map_dfr(~ MI.captures(.$id))
#' c("GLA5-5", "GLA6-2") %>% purrr::map_dfr(~ MI.operations(., id_operation = T)) %>% group_split(id) %>% purrr::map_dfr(~ MI.captures(.$id))

MI.captures <- function(
  id_operation = NA_integer_,
  sortie = c("Brute","Complète")
)
{
  #### Vérification ####
  if(is.na(id_operation)) stop("Aucun identifiant d'opération fourni")
  ## Évaluation des choix
  sortie <- match.arg(sortie)
  
  ##### Récupération des données #####
  ## Connexion à la BDD ##
  dbD <- BDD.ouverture("Data")
  
  ## Promesse de données ##
  suppressWarnings(operations <- tbl(dbD, dbplyr::in_schema("fd_production", "macroinvertebres_operations")))
  suppressWarnings(prelevements <- tbl(dbD, dbplyr::in_schema("fd_production", "macroinvertebres_prelevements")))
  captures <- tbl(dbD, dbplyr::in_schema("fd_production", "macroinvertebres_captures"))
  
  ## Collecte ##
  suppressWarnings(
    resultat <- 
      captures %>% 
      left_join(prelevements %>% select(-contains("_modif"), -geom), by = c("micapt_miprlvt_id" = "id")) %>% 
      left_join(operations %>% select(-contains("_modif")), by = c("miprlvt_miop_id" = "id")) %>% 
      filter(miprlvt_miop_id == id_operation) %>% 
      {if(sortie == "Brute") select(., id, micapt_miprlvt_id, micapt_taxon, micapt_abondance, micapt_typeabondance, micapt_volumeabondance, micapt_stade, micapt_sexe, micapt_remarques) else .} %>%
      {if(sortie == "Complète") select(., everything(), -contains("modif_")) else .} %>%
      collect(n = Inf)
  )

  #### Sortie ####
  return(resultat)
  
} # Fin de la fonction