#' Collecte/export d'une table en base de données
#'
#' Cette fonction permet de collecter/exporter une table en base de données
#' @name table.recuperation
#' @param table Nom de la table à exporter, sans le schéma
#' @param condition Éventuelle condition de filtrage de la table à collecter/exporter
#' @param export Exportation de la table (\code{FALSE} par défaut)
#' @keywords cartographie
#' @import DBI
#' @import dbplyr
#' @import glue
#' @import openxlsx2
#' @import rlang
#' @import sf
#' @import tidyverse
#' @export
#' @examples
#' table.recuperation("gis_emprise", export = T)
#' table.recuperation("chroniques_resultats")
#' table.recuperation("chroniques_resultats", export = T)
#' table.recuperation("topographie_operations_habitats", "id = 12")
#' table.recuperation("physicochimie_mesures", "pcmes_coderhj = 'ROU'")
#' c("gis_emprise", "pecheloisir_categories_polygones", "hydrographie_contextespdpg") %>% map(~ table.recuperation(., export = T))

table.recuperation <- function(
    table = NA_character_,
    condition = NA_character_,
    export = FALSE
)
{
  
  #### Test de cohérence ####
  if(is.na(table) == T) stop("Un nom de table doit être fourni")
  
  #### Collecte des données ####
  ##### Ouverture de la BDD #####
  dbD <- BDD.ouverture("Data")
  
  ##### Liste des tables disponibles #####
  tables <-
    tbl(dbD, in_schema("information_schema", "tables")) %>%
    filter(grepl("production|referentiels", table_schema)) %>%
    select(table_schema, table_name, table_type) %>%
    collect()
  
  tables_sf <-
    tbl(dbD, in_schema("public", "geometry_columns")) %>%
    filter(grepl("production|referentiels", f_table_schema)) %>%
    select(-f_table_catalog) %>%
    collect()
  
  ##### Recherche de la table #####
  tables_sf_filtrees <-
    tables_sf %>% 
    filter(grepl(table, f_table_name))
  if(tables_sf_filtrees %>% nrow() == 0) table_sf <- FALSE
  if(tables_sf_filtrees %>% nrow() >= 1) table_sf <- TRUE
  if(tables_sf_filtrees %>% nrow() > 1) warning(glue("Attention : plusieurs tables {table} comportent une géométrie"))
  
  tables_filtrees <-
    tables %>% 
    filter(grepl(table, table_name))
  if(tables %>% nrow() == 0 & table_sf == FALSE) stop(glue("Attention : la table {table} n'existe pas dans les schémas fd_production et fd_referentiels"))
  
  if(table_sf == TRUE) schema <- tables_sf_filtrees %>% pull(f_table_schema)
  if(table_sf == FALSE) schema <- tables_filtrees %>% pull(table_schema)
  
  #### Filtrage ####
  if(table_sf == TRUE) {
    if(is.na(condition)) donnees <- sf::st_read(dbD, query = glue("SELECT * FROM {schema}.{table};"))
    if(!is.na(condition)) donnees <- sf::st_read(dbD, query = glue("SELECT * FROM {schema}.{table} WHERE {condition};"))
  }
  if(table_sf == FALSE) {
    if(is.na(condition)) donnees <- tbl(dbD, in_schema(schema, table)) %>% collect()
    if(!is.na(condition)) donnees <- tbl(dbD, sql(glue("SELECT * FROM {schema}.{table} WHERE {condition}"))) %>% collect()
  }
  DBI::dbDisconnect(dbD)
  
  #### Nettoyage & reformatage ####
  data_sortie <- 
    donnees %>% 
    select(
      # -contains("id"), 
      -contains("_modif")
      )
  
  #### Export ####
  if(export == TRUE){
    name <- table %>% rlang::get_expr() # Récupération du nom de la variable d'entrée en rlang
    name_sans_schema <- name %>% str_replace("fd_referentiels.", "") %>% str_replace("fd_production.", "")

    data_sortie %>% 
      {if(table_sf == TRUE) SIG.export(., glue("{today()}_{name_sans_schema}"), shp = F, kml = F, excel = T) else .} %>% 
      {if(table_sf == FALSE) write_xlsx(., glue("{today()}_{name_sans_schema}.xlsx"))}
  }

  #### Sortie ####
  if(export == FALSE){
    return(data_sortie)
  }
  
} # Fin de la fonction
