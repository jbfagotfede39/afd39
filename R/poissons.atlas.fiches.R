#' Génération SIG pour fiches atlas poissons
#'
#' Cette fonction permet de générer le fichier geojson nécessaire à la création des fiches de l'atlas 2021-02-01_Modèle_atlas_poissons_V1
#' @name poissons.atlas.fiches
#' @param data Jeu de données en entrée (opérations issues de poissons.operations)
#' @param projet Nom du projet
#' @param export \code{FALSE} par défault. Permet d'exporter les données
#' @param commentaires \code{FALSE} par défault. Permet d'extraire le commentaire associé aux opérations
#' @keywords poissons
#' @export
#' @import sf
#' @import stringr
#' @import tidyverse
#' @examples
#' poissons.atlas.fiches(data)
#' poissons.atlas.fiches(data, export=T)
#' poissons.captures("AIN18-4") %>% poissons.atlas.fiches(export=T)

##### -------------- A FAIRE -------------- #####
# 
# -------------- A FAIRE -------------- #

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
  SyntheseAtlasPoissons <-
    data %>% 
    dplyr::select(codeecosysteme, X, Y, TypeCoord, Station, repereamont, codesiermc, categoriepiscicole, reservoirbiologique, reservedepeche, typetheorique, especesreperes, Date, longueurprospectee, largeurlameeau, profondeur, surface) %>% 
    left_join(IPR, by = c("Station", "Date")) %>% 
    left_join(ecosystemes, by = "codeecosysteme") %>% 
    sf::st_as_sf(coords = c("X","Y")) %>% 
    st_set_crs(2154) %>% 
    st_join(communes %>% dplyr::select(tpcomm_commune_libelle)) %>% 
    st_join(contextesPDPG %>% dplyr::select(hycont_contexte_code)) %>% 
    mutate(especesreperes = str_trim(word(especesreperes, 1, sep = fixed(",")))) %>% # Formule nécessaire dans le cas où il y a plusieurs espèces repère, permet de ne conserver que la première
    mutate(reservoirbiologique = ifelse(reservoirbiologique == "true", "Oui", "Non")) %>% 
    mutate(reservedepeche = ifelse(reservedepeche == "true", "Oui", "Non")) %>% 
    mutate(localisation = projet)
  
  
  #### Vérifications ####
  ### Pas d'espèce repère ###
  especesreperesVides <- SyntheseAtlasPoissons %>% filter(is.na(especesreperes)) %>% st_drop_geometry() %>% select(Station) %>% arrange(Station)
  if(nrow(especesreperesVides) == 1) stop(glue("Pas d'espèce repère dans la station {pull(especesreperesVides)}"))
  if(nrow(especesreperesVides) == 2) stop(glue("Pas d'espèce repère dans les stations {glue_collapse(pull(especesreperesVides), sep = ',', last = ' et ')}"))
  if(nrow(especesreperesVides) == 2) stop(glue_collapse("Pas d'espèce repère dans les stations ", ", ", last = " et "))
  glue_collapse(1:4, ", ", last = " and ")
  
  
  #### Exportation des données ####
  if(export == T) SyntheseAtlasPoissons %>% SIG.export("Atlas_Resultats_poissons", shp = F, kml = F, excel = F)
  if(export == F) return(SyntheseAtlasPoissons)

} # Fin de la fonction
