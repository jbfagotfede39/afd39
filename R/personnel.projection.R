#' Adjonction des projections de réalisation annuelles
#'
#' Cette fonction permet d'adjoindre au données théoriques de réalisation une projection temporelle annuelle
#' @name personnel.projection
#' @keywords personnel
#' @param data Jeu de données en entrée, issu du temps de travail théorique de \code{personnel.projet}
#' @param type Type de projection à conserver \code{vide} par défaut <-> pas de filtrage
#' @import glue
#' @import openxlsx
#' @import RPostgreSQL
#' @import stringr
#' @import tidyverse
#' @export
#' @examples
#' personnel.projection(tps_travail_theorique)
#' personnel.projection(tps_travail_theorique, type = "Brouillon")
#' personnel.projection(tps_travail_theorique, type = c("Brouillon", "Brouillon 2"))

personnel.projection <- function(
  data = NA_character_,
  type = NA_character_
)
{
  
  #### Contexte ####
  type_n <- length(type)
  if(type_n == 1){
    if(is.na(type))
      type_n <- 0}
  
  #### Collecte des données ####
  dbD <- BDD.ouverture("Data")
  tpstravail_projection <- tbl(dbD, dbplyr::in_schema("fd_production", "tpstravail_projection")) %>% select(-id, -contains("_modif")) %>% collect(n = Inf)
  DBI::dbDisconnect(dbD)
  tpstravail_projection <- 
    tpstravail_projection %>% 
    # {if(!is.na(type)) filter(., grepl(type, tpswprj_projection_type)) else .}
    {if(type_n > 0) filter(., grepl(paste(str_to_lower(type), collapse='|'), str_to_lower(tpswprj_projection_type))) else .}

  #### Nettoyage & reformatage ####
  data_v2 <-
    data %>% 
    select(-contains("_modif")) %>% 
    left_join(tpstravail_projection, join_by(tpswrecap_projet == tpswprj_projet_id, tpswrecap_detail == tpswprj_detail, tpswrecap_poste == tpswprj_poste, tpswrecap_personnel_id == tpswprj_personnel)) %>% 
    mutate(tpswprj_annee = ifelse(is.na(tpswprj_annee), year(today()), tpswprj_annee)) %>% # Complément des données lacunaires
    mutate(tpswprj_proportion = ifelse(is.na(tpswprj_proportion), 1, tpswprj_proportion)) # Complément des données lacunaires
  
  #### Calcul ####
  data_v3 <-
    data_v2 %>% 
    mutate(tpswrecap_heures_ponderees = round(tpswrecap_heures * tpswprj_proportion, 2), .after = "tpswprj_proportion") %>% 
    mutate(tpswrecap_argent_pondere = round(tpswrecap_argent * tpswprj_proportion, 2), .after = "tpswrecap_heures_ponderees") # %>% 
    # view()
    # pivot_wider(,
    #             names_from = c(estimate, moe),
    #             values_from = tpswrecap_heures)
    # view()
  
  #### Sortie ####
  return(data_v3)
  
} # Fin de la fonction
