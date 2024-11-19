#' Génération SIG pour fiches atlas poissons
#'
#' Cette fonction permet de générer le fichier geojson nécessaire à la création des fiches de l'atlas 2021-02-01_Modèle_atlas_poissons_V1
#' @name poissons.atlas.fiches
#' @param data Jeu de données en entrée (opérations issues de poissons.operations)
#' @param projet Nom du projet
#' @param export \code{FALSE} par défaut. Permet d'exporter les données
#' @param commentaires \code{FALSE} par défaut. Permet d'extraire le commentaire associé aux opérations
#' @keywords poissons
#' @import sf
#' @import stringr
#' @import tidyverse
#' @export
#' @examples
#' poissons.atlas.fiches(data)
#' poissons.atlas.fiches(data, export=T)
#' poissons.captures("AIN18-4") %>% poissons.atlas.fiches(export=T)

poissons.atlas.fiches <- function(
  data = data,
  projet = NA_character_,
  export = F,
  commentaires = F
)
{

  #### Évaluation des choix ####
  
  #### Collecte des données ####
  IPR <- poissons.IPR(data)
  
  ## Données de référence ##
  ecosystemes <- poissons.ecosystemes() %>% select(codeecosysteme, nomecosysteme)
  dbD <- BDD.ouverture("Data")
  communes <- sf::st_read(dbD, query = "SELECT * FROM fd_referentiels.topographie_communes WHERE tpcomm_departement_insee = '39';")
  contextesPDPG <- sf::st_read(dbD, query = "SELECT * FROM fd_referentiels.hydrographie_contextespdpg;")
  DBI::dbDisconnect(dbD)
  
  #### Regroupement des données ####
  synthese_atlas_poissons <-
    data %>% 
    dplyr::select(codeecosysteme, X, Y, TypeCoord, Station, repereamont, codesiermc, categoriepiscicole, reservoirbiologique, reservedepeche, gestionnaire, typetheorique, especesreperes, Date, longueurprospectee, largeurlameeau, profondeur, surface) %>% 
    left_join(IPR %>% select(-CodeSIERMC), by = c("Station", "Date")) %>% 
    left_join(ecosystemes, by = "codeecosysteme") %>% 
    sf::st_as_sf(coords = c("X","Y")) %>% 
    st_set_crs(2154) %>% 
    st_join(communes %>% dplyr::select(tpcomm_commune_libelle)) %>% 
    st_join(contextesPDPG %>% dplyr::select(hycont_contexte_code, hycont_contexte_libelle, hycont_domaine_piscicole, hycont_espece_repere, hycont_contexte_etat))%>% 
    mutate(especesreperes = str_trim(word(especesreperes, 1, sep = fixed(",")))) %>% # Formule nécessaire dans le cas où il y a plusieurs espèces repère, permet de ne conserver que la première
    mutate(reservoirbiologique = ifelse(reservoirbiologique == "true", "Oui", "Non")) %>% 
    mutate(reservedepeche = ifelse(reservedepeche == "true", "Oui", "Non")) %>% 
    mutate(localisation = projet)
  
  #### Vérifications ####
  ### Pas d'espèce repère ###
  especes_reperes_vides <- synthese_atlas_poissons %>% filter(is.na(especesreperes)) %>% st_drop_geometry() %>% select(Station) %>% arrange(Station)
  if(nrow(especes_reperes_vides) == 1) stop(glue("Pas d'espèce repère dans la station {pull(especes_reperes_vides)}"))
  if(nrow(especes_reperes_vides) == 2) stop(glue("Pas d'espèce repère dans les stations {glue_collapse(pull(especes_reperes_vides), sep = ',', last = ' et ')}"))
  if(nrow(especes_reperes_vides) == 2) stop(glue_collapse("Pas d'espèce repère dans les stations ", ", ", last = " et "))
  
  #### Exportation des données ####
  if(export == T) synthese_atlas_poissons %>% SIG.export("Atlas_Resultats_poissons", shp = F, kml = F, excel = F)
  if(export == F) return(synthese_atlas_poissons)

} # Fin de la fonction

