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

  #### Test de cohérence ####
  tpstravail_projection_verif <-
    tpstravail_projection %>% 
    group_by(tpswprj_projet_id, tpswprj_projection_type, tpswprj_detail, tpswprj_poste, tpswprj_personnel) %>% 
    summarise(total = sum(tpswprj_proportion)) %>% 
    filter(!(total == 0)) %>%
    filter(!(total >= 0.97 & total <= 1.03)) # à cause des arrondis
  if(tpstravail_projection_verif %>% nrow() != 0) stop(glue("Attention : il faut vérifier les projets {glue_collapse(tpstravail_projection_verif$tpswprj_projet_id, sep = ',', last = ' et ')} car les totaux ne sont pas égaux à 1"))

  #### Nettoyage & reformatage ####
  tpstravail_projection_fin <- tpstravail_projection %>% filter(!is.na(tpswprj_personnel))
  tpstravail_projection_semi_fin <- 
    tpstravail_projection %>% 
    filter(is.na(tpswprj_personnel) & !is.na(tpswprj_detail)) %>% 
    # group_by(tpswprj_projet_id, tpswprj_projection_type, tpswprj_annee, tpswprj_detail, tpswprj_proportion)
    select(-contains("poste"), -contains("personnel")) %>% 
    rename_all(~ (paste0(., "_sf")))
  tpstravail_projection_grossier <- 
    tpstravail_projection %>% 
    filter(is.na(tpswprj_personnel) & is.na(tpswprj_detail)) %>% 
    # group_by(tpswprj_projet_id, tpswprj_projection_type, tpswprj_annee, tpswprj_detail, tpswprj_proportion)
    select(-contains("poste"), -contains("personnel")) %>% 
    rename_all(~ (paste0(., "_g")))

  data_v2 <-
    data %>% 
    select(-contains("_modif")) %>% 
    left_join(tpstravail_projection_fin, join_by(tpswrecap_projet == tpswprj_projet_id, tpswrecap_detail == tpswprj_detail, tpswrecap_poste == tpswprj_poste, tpswrecap_personnel_id == tpswprj_personnel)) %>% # pour jointure fine, par projet par action par poste par personnel 
    left_join(tpstravail_projection_semi_fin, join_by(tpswrecap_projet == tpswprj_projet_id_sf, tpswrecap_detail == tpswprj_detail_sf)) %>% # pour jointure semi fine, par projet par action
    left_join(tpstravail_projection_grossier, join_by(tpswrecap_projet == tpswprj_projet_id_g)) %>% # pour jointure grossière, par projet
    mutate(tpswprj_projection_type = ifelse(is.na(tpswprj_projection_type), tpswprj_projection_type_sf, tpswprj_projection_type)) %>% # Complément des données lacunaires avec les données semi-fines de la jointure précédente
    mutate(tpswprj_projection_type = ifelse(is.na(tpswprj_projection_type), tpswprj_projection_type_g, tpswprj_projection_type)) %>% # Complément des données lacunaires avec les données semi-fines de la jointure précédente
    mutate(tpswprj_annee = ifelse(is.na(tpswprj_annee), tpswprj_annee_sf, tpswprj_annee)) %>% # Complément des données lacunaires avec les données semi-fines de la jointure précédente
    mutate(tpswprj_annee = ifelse(is.na(tpswprj_annee), tpswprj_annee_g, tpswprj_annee)) %>% # Complément des données lacunaires avec les données semi-fines de la jointure précédente
    mutate(tpswprj_proportion = ifelse(is.na(tpswprj_proportion), tpswprj_proportion_sf, tpswprj_proportion)) %>% # Complément des données lacunaires avec les données semi-fines de la jointure précédente
    mutate(tpswprj_proportion = ifelse(is.na(tpswprj_proportion), tpswprj_proportion_g, tpswprj_proportion)) %>% # Complément des données lacunaires avec les données grossières de la jointure précédente
    mutate(tpswprj_annee = ifelse(is.na(tpswprj_annee), year(today()), tpswprj_annee)) %>% # Complément des données lacunaires
    mutate(tpswprj_proportion = ifelse(is.na(tpswprj_proportion), 1, tpswprj_proportion)) #%>% # Complément des données lacunaires
    # select(-contains("_sf"), -contains("g"))
  
  #### Calcul ####
  data_v3 <-
    data_v2 %>% 
    mutate(tpswrecap_heures_ponderees = round(tpswrecap_heures * tpswprj_proportion, 2), .after = "tpswprj_proportion") %>% 
    mutate(tpswrecap_argent_pondere = round(tpswrecap_argent * tpswprj_proportion, 2), .after = "tpswrecap_heures_ponderees")
  
  #### Vérification ####
  if(nrow(tpstravail_projection) != (nrow(tpstravail_projection_fin) + nrow(tpstravail_projection_semi_fin) + nrow(tpstravail_projection_grossier))) stop("Il y a un écart dans les décomptes partiels de proportions projetées")
    
  #### Sortie ####
  return(data_v3)
  
} # Fin de la fonction
