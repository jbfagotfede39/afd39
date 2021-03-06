#' Listage des stations de chroniques
#'
#' Cette fonction permet de lister les stations de la BDD Chroniques
#' @name chronique.stations
#' @param Territoire Territoire concerné. Unique ou sous forme de vecteur (c("Villerserine", "Villers-Robert") par exemple)
#' @param Echelle Échelle spatiale de la synthèse (commune, canton, communauté de communes, département, région, contexte de PDPG, Hydro-écorégion, entité GEMAPI, Maître d'ouvrage, Milieu, Bassin versant, Sous-bassin versant, Polygone autre)
#' @param Sortie Forme du dataframe de sortie - \code{Simple}, \code{Propre} (format diffusable, avec stations) ou \code{Complet} (par défault - tous les champs)
#' @keywords stations
#' @import sf
#' @import tidyverse
#' @export
#' @examples
#' chronique.stations("CD39","MO")
#' chronique.stations("Suran","Milieu")
#' chronique.stations("Ain","Bassin")
#' chronique.stations("Serpentine","Sous-bassin")
#' chronique.stations("Lons-le-Saunier","Commune")
#' chronique.stations("39","Département")

##### TODO LIST #####
# La commande chronique.stations("39", "Département") génère des doublons de ligne avec la jointure avec les communes : à reprendre
# Permettre le fonctionnement lorsqu'on envoie un polygone issu de stations.territoire("ContextePDPG", c("39.19", "39.33", "39.62", "39.16", "39.17", "39.21", "39.24", "39.18", "39.22", "39.23", "39.18", "39.18", "39.25"))
# Ajouter un paramètre dplyr::filter(chsta_fonctionnement == "En cours")
# Il y a un ou deux champs qui ne veulent pas être sortis en shp : à supprimer dans le format complet
#####################

chronique.stations <- function(Territoire = NA_character_, 
                               Echelle = c("Commune", "Canton", "ComCom", "Département", "Région", "ContextePDPG", "HER", "GEMAPI", "MO", "Milieu", "Bassin", "Sous-bassin", "Polygone"),
                               Sortie = c("Complet","Propre","Simple")
)
{
  
  ## Évaluation des choix
  Echelle <- match.arg(Echelle)
  Sortie <- match.arg(Sortie)
  
  #### Chargement des données ####
  dbD <- BDD.ouverture("Data")
  Stations <- 
    sf::st_read(dbD, query = "SELECT * FROM fd_production.chroniques_stations;") %>% 
    arrange(chsta_coderhj)
  DBI::dbDisconnect(dbD)
  
  #### Filtrage ####
  if(Echelle == "MO"){
    Vue <-
    Stations %>% 
    filter(grepl(Territoire, chsta_mo)) %>% 
    arrange(chsta_coderhj)
  }
  
  if(Echelle == "Milieu"){
    Vue <-
    Stations %>% 
    filter(chsta_milieu == Territoire) %>% 
    arrange(chsta_coderhj)
  }
  
  if(Echelle == "Bassin" | Echelle == "Sous-bassin" | Echelle == "Commune" | Echelle == "Canton" | Echelle == "ComCom" | Echelle == "Département" | Echelle == "Région" | Echelle == "ContextePDPG" | Echelle == "HER" | Echelle == "GEMAPI" | Echelle == "Polygone"){ 
    if(Echelle != "Polygone"){TerritoireRecherche <- stations.territoire(Echelle = Echelle, Territoire = Territoire)}
    if(Echelle == "Polygone"){TerritoireRecherche <- Territoire}
    Vue <-
    Stations %>% 
    st_join(TerritoireRecherche, left = FALSE) %>% 
    arrange(chsta_coderhj)
  }
    
  # if(Echelle == "Bassin" | Echelle == "Sous-bassin" | Echelle == "Commune" | Echelle == "Canton" | Echelle == "ComCom" | Echelle == "Département" | Echelle == "Région" | Echelle == "ContextePDPG" | Echelle == "HER" | Echelle == "GEMAPI") 
  #   TerritoireRecherche <- stations.territoire(Echelle = Echelle, Territoire = Territoire)
  #   Vue <-
  #   Stations %>% 
  #   st_join(TerritoireRecherche, left = FALSE) %>% 
  #   arrange(chsta_coderhj)
  
  ## Simplification ##
  if(Sortie == "Propre"){
    Vue <- 
      Vue %>% 
      rename_at(vars(matches("id.x")), list(~str_replace(., "id.x", "id"))) %>%
      select(id:chsta_milieu, chsta_commune, chsta_departement, chsta_coord_x:chsta_reseauthermietype)
    }
  
  ## Affichage des résultats ##
  return(Vue)
}