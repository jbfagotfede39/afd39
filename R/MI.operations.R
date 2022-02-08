#' Listing des opérations (MI) pour une station
#'
#' Cette fonction permet de lister les différentes opérations pour une liste donnée de stations
#' @name MI.operations
#' @param code_station Code RHJ de la station
#' @param id_operation Affichage de l'id des opérations - \code{FALSE} (par défault)
#' @param sortie Forme du dataframe de sortie - \code{Simple} (par défault), \code{Propre} (format diffusable, avec stations) ou \code{Complet} (tous les champs)
#' @keywords macro-invertébrés
#' @import DBI
#' @import sf
#' @import tidyverse
#' @export
#' @examples
#' MI.operations()
#' MI.operations(id_operation = TRUE)
#' MI.operations("BCH5-7")
#' MI.operations("BCH5-7", id_operation = TRUE)
#' MI.operations("BCH5-7", sortie = "Complet")
#' c("GLA5-5", "GLA6-2") %>% purrr::map_dfr(~ MI.operations(., sortie = "Propre", id_operation = T))
#' captures <- Stations %>% group_split(chsta_coderhj) %>% purrr::map_dfr(~ MI.operations(.$chsta_coderhj, sortie = "Propre"))

MI.operations <- function(
  code_station = NA_character_,
  id_operation = F,
  sortie = c("Simple","Propre","Complet")
)
{

  ## Évaluation des choix
  sortie <- match.arg(sortie)
  
  #### Ouverture de la BDD ####
  ## Connexion à la BDD
  dbD <- BDD.ouverture("Data")

  #### Récupération des données ####
  operations <- sf::st_read(dbD, query = "select * from fd_production.macroinvertebres_operations") %>% collect(n = Inf)
  captures <- tbl(dbD, dbplyr::in_schema("fd_production", "macroinvertebres_captures")) %>% collect(n = Inf)
  prelevements <- tbl(dbD, dbplyr::in_schema("fd_production", "macroinvertebres_prelevements")) %>% collect(n = Inf)
  habitats <- tbl(dbD, dbplyr::in_schema("fd_production", "macroinvertebres_habitats")) %>% collect(n = Inf)
  habitats_reference <- tbl(dbD, dbplyr::in_schema("fd_referentiels", "macroinvertebres_habitats_reference")) %>% collect(n = Inf)
  especes_reference <- tbl(dbD, dbplyr::in_schema("fd_referentiels", "systematique_especes")) %>% collect(n = Inf)
  genres_reference <- tbl(dbD, dbplyr::in_schema("fd_referentiels", "systematique_genres")) %>% collect(n = Inf)
  sous_familles_reference <- tbl(dbD, dbplyr::in_schema("fd_referentiels", "systematique_sousfamilles")) %>% collect(n = Inf)
  familles_reference <- tbl(dbD, dbplyr::in_schema("fd_referentiels", "systematique_familles")) %>% collect(n = Inf)
  ordres_reference <- tbl(dbD, dbplyr::in_schema("fd_referentiels", "systematique_ordres")) %>% collect(n = Inf)
  
  # ## Fermeture de la BDD ##
  # DBI::dbDisconnect(dbD)
  
  ## Simplification ##
  
  #### Travail sur l'ensemble des stations ####
  if(is.na(code_station) & id_operation == F & sortie == "Simple"){
    operations <- 
      operations %>%
      select(miop_coderhj, miop_date) %>% 
      # # dplyr::rename(Station = miop_coderhj, Date = miop_date) %>% 
      arrange(miop_coderhj, miop_date)}
  
  if(is.na(code_station) & id_operation == F & sortie == "Propre"){
    operations <- 
      operations %>%
      select(miop_coderhj, miop_date, miop_coord_x, miop_coord_y) %>% 
      # # dplyr::rename(Station = miop_coderhj, Date = miop_date) %>% 
      arrange(miop_coderhj, miop_date)}
  
  if(is.na(code_station) & id_operation == F & sortie == "Complet"){
    operations <- 
      operations %>%
      # # dplyr::rename(Station = miop_coderhj, Date = miop_date) %>% 
      arrange(miop_coderhj, miop_date)}
  
  if(is.na(code_station) & id_operation == T & sortie == "Simple"){
    operations <- 
      operations %>%
      select(id, miop_coderhj, miop_date) %>% 
      # dplyr::rename(Station = miop_coderhj, Date = miop_date) %>% 
      arrange(miop_coderhj, miop_date)}
  
  if(is.na(code_station) & id_operation == T & sortie == "Propre"){
    operations <- 
      operations %>%
      select(id, miop_coderhj, miop_date, miop_coord_x, miop_coord_y) %>% 
      # dplyr::rename(Station = miop_coderhj, Date = miop_date) %>% 
      arrange(miop_coderhj, miop_date)}
  
  if(is.na(code_station) & id_operation == T & sortie == "Complet"){
    operations <- 
      operations %>%
      # dplyr::rename(Station = miop_coderhj, Date = miop_date) %>% 
      arrange(miop_coderhj, miop_date)}
  
  #### Travail sur une seule station ####
  # Test si le nom existe bien, sinon message d'erreur et arrêt de la fonction #
  if(!is.na(code_station)) {
  if(dim(operations %>% filter(operations$miop_coderhj %in% code_station))[1] == 0)
    warning("Attention : station(s) absente(s) des opérations de la base de données")}
  
  if(!is.na(code_station) & id_operation == F & sortie == "Simple"){
    operations <- 
      operations %>%
      filter(miop_coderhj %in% code_station) %>% 
      select(miop_coderhj, miop_date) %>% 
      # dplyr::rename(Station = miop_coderhj, Date = miop_date) %>% 
      arrange(miop_coderhj, miop_date)}
  
  if(!is.na(code_station) & id_operation == F & sortie == "Propre"){
    operations <- 
      operations %>%
      filter(miop_coderhj %in% code_station) %>% 
      select(miop_coderhj, miop_date, miop_coord_x, miop_coord_y) %>% 
      # dplyr::rename(Station = miop_coderhj, Date = miop_date) %>% 
      arrange(miop_coderhj, miop_date)}
  
  if(!is.na(code_station) & id_operation == F & sortie == "Complet"){
    operations <- 
      operations %>%
      filter(miop_coderhj %in% code_station) %>% 
      # # dplyr::rename(Station = miop_coderhj, Date = miop_date) %>% 
      arrange(miop_coderhj, miop_date)}  
  
  if(!is.na(code_station) & id_operation == T & sortie == "Simple"){
    operations <- 
      operations %>%
      select(id, miop_coderhj, miop_date) %>% 
      filter(miop_coderhj %in% code_station) %>% 
      # # dplyr::rename(Station = miop_coderhj, Date = miop_date) %>% 
      arrange(miop_coderhj, miop_date)}
  
  if(!is.na(code_station) & id_operation == T & sortie == "Propre"){
    operations <- 
      operations %>%
      filter(miop_coderhj %in% code_station) %>% 
      select(id, miop_coderhj, miop_date, miop_coord_x, miop_coord_y) %>% 
      # # dplyr::rename(Station = miop_coderhj, Date = miop_date) %>% 
      arrange(miop_coderhj, miop_date)}
  
  if(!is.na(code_station) & id_operation == T & sortie == "Complet"){
    operations <- 
      operations %>%
      filter(miop_coderhj %in% code_station) %>% 
      # # dplyr::rename(Station = miop_coderhj, Date = miop_date) %>% 
      arrange(miop_coderhj, miop_date)}

  return(operations)
  
} # Fin de la fonction
