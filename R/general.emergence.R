#' Extraction observations émergence de projets
#'
#' Cette fonction permet d'extraire les données des observations réalisées en vue de l'émergence de projets
#' 
#' @name general.emergence
#' @param export Un export sous forme de table SIG est réalisé si \code{TRUE}. \code{FALSE} par défault.
#' @param piecejointes Extraction des pièces jointes si \code{TRUE}. \code{FALSE} par défault.
#' @param exportventile Export complémentaire sous forme de ventilation thématique si \code{TRUE}. \code{FALSE} par défault.
#' @import aquatools
#' @import fs
#' @import purrr
#' @import sf
#' @import tidyverse
#' @import wkb
#' @export
#' @examples
#' general.emergence(export = TRUE)
#' general.emergence(piecejointes = TRUE)
#' general.emergence(export = TRUE, piecejointes = TRUE)
#' general.emergence(export = TRUE, exportventile = TRUE, piecejointes = TRUE)

##### TODO LIST #####
# 
#####################

general.emergence <- function(
  export = FALSE,
  exportventile = FALSE,
  piecejointes = FALSE
                          )
{

  #### Évaluation des choix ####
  # Recherche <- match.arg(Recherche)
  
  #### Importation ####
  dbD <- BDD.ouverture("Data")
  
  glossaire <-
    tbl(dbD, in_schema("fd_referentiels", "general_observations_parametres")) %>% 
    select(id:galobsparam_parametre_intitule) %>% 
    collect() %>% 
    mutate(galobsparam_parametre_intitulelong = glue("{galobsparam_theme_intitule} - {galobsparam_parametre_intitule}")) %>% 
    select(id, galobsparam_parametre_intitulelong)
  
  operateurs <-
    tbl(dbD, in_schema("fd_referentiels", "vue_gestion_operateurs")) %>% 
    select(id, gestop_prenom, gestop_nom) %>% 
    collect() %>% 
    mutate(operateur_identite = glue("{gestop_prenom} {gestop_nom}")) %>% 
    select(-starts_with("gestop"))
  
  qualification <-
    tbl(dbD, in_schema("fd_production", "general_observations_qualifiees")) %>% 
    select(-id, -contains("_modif")) %>% 
    collect()
  
  #### Transformation ####
  if(piecejointes == T){
  valeurs <-
    tbl(dbD, in_schema("fd_production", "general_observations")) %>% 
    filter(galobs_application_id == 2) %>% 
    select(-id, -galobs_application_id, -(galobs_piecejointe2:`_modif_date`)) %>% 
    collect()
  }
  if(piecejointes == F){
    valeurs <-
      tbl(dbD, in_schema("fd_production", "general_observations")) %>% 
      filter(galobs_application_id == 2) %>% 
      select(-id, -galobs_application_id, -(galobs_piecejointe1:`_modif_date`)) %>% 
      collect()
  }
  valeurs <- 
    valeurs %>% 
    left_join(glossaire, by = c('galobs_parametre_id' = 'id')) %>% 
    select(galobs_operateur_id:galobs_observation_id, galobsparam_parametre_intitulelong, everything()) %>%
    select(-galobs_parametre_id) %>% 
    left_join(qualification, by = c('galobs_observation_id' = 'galobsqu_observation_id')) %>% 
    mutate(valeur = ifelse(is.na(galobs_valeur_texte), galobs_valeur_numerique, galobs_valeur_texte)) %>%
    {if(!("galobs_piecejointe1nom" %in% names(.))) mutate(., galobs_piecejointe1nom = NA_character_) else .} %>% 
    {if(!("galobs_piecejointe1" %in% names(.))) mutate(., galobs_piecejointe1 = NA_character_) else .} %>% 
    mutate(valeur = ifelse(is.na(valeur), galobs_piecejointe1nom, valeur)) %>%
    select(galobs_observation_id, galobsqu_validite, galobsqu_interet, galobsqu_remarques, galobs_operateur_id, galobs_dateajout, galobs_observation_id, galobsparam_parametre_intitulelong, valeur, galobs_piecejointe1) 
  
  #### Mise en forme du tableau de synthèse ####
  synthese <- 
    valeurs %>% 
    select(-galobs_piecejointe1) %>% 
    pivot_wider(names_from = galobsparam_parametre_intitulelong, values_from = valeur) %>%
    mutate(`Général - Opérateur` = as.character(`Général - Opérateur`)) %>% 
    left_join(operateurs %>% mutate(id = as.character(id)), by = c("Général - Opérateur" = "id")) %>%
    mutate(`Général - Opérateur` = operateur_identite) %>%
    select(-operateur_identite) %>%
    mutate(type_observation = case_when(!is.na(`Continuité - Type`) ~ "Continuité",
                                        !is.na(`Morphologie - Type`) ~ "Morphologie",
                                        !is.na(`Physico-chimie - Type de pression`) ~ "Physico-chimie",
                                        !is.na(`Hydrologie - Type`) ~ "Hydrologie",
                                        !is.na(`Autre - Remarques`) ~ "Autre"
    )
    ) %>% 
    select(type_observation, everything()) %>% 
    mutate(X = `Général - Coordonnée X`) %>% 
    mutate(Y = `Général - Coordonnée Y`) %>% 
    sf::st_as_sf(coords = c("X","Y"), remove = TRUE) %>% 
    st_set_crs(2154)
  
  #### Exportation ####
  ### Tableau de synthèse ###
  if(export == T){
  synthese %>% 
    SIG.export(glue("{today()}_Table_synthèse_observations_émergence_projets"), shp = F, kml = F)
  }

  if(exportventile == T){
    synthese %>%
      group_split(type_observation) %>%
      # purrr::pluck(2)
      # purrr::map_dfr(~ SIG.export(glue("{today()}_Table_synthèse_observations_émergence_projets_{unique(.$type_observation)}"), shp = F, kml = F))
      purrr::map_dfr(~ st_write(data, dsn = glue("{today()}_Table_synthèse_observations_émergence_projets_{unique(.$type_observation)}.geojson"), driver = "GeoJSON", overwrite = TRUE))
  }
  
  ### Pièces jointes ###
  if(piecejointes == T){
    sortiefichiers <-
      valeurs %>% 
      filter(!is.na(galobs_piecejointe1)) %>% 
      select(galobs_observation_id, valeur, galobs_piecejointe1) %>% 
      mutate(nomfichier = glue("{galobs_observation_id}_{valeur}"))
    
    dir_create(glue("{today()}_Pièces_jointes_observations_émergence_projets"))
    setwd(glue("./{today()}_Pièces_jointes_observations_émergence_projets"))
    
    sortiefichiers %>% 
      group_split(nomfichier) %>% 
      purrr::map_dfr(~ writeBin(wkb::hex2raw(.$galobs_piecejointe1), .$nomfichier))
    
    setwd("..")
  }
  
  #### Affichage des résultats ####
  return(synthese)
  }
