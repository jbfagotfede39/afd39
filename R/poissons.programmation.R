#' Extraction des suivis piscicoles programmés
#'
#' Cette fonction permet de réaliser une extraction des suivis piscicoles programmés, à partir de la table fd_production.poissons_programmation
#' @name poisson.programmation
#' @param annee Année recherchée
#' @param projet_id Projet recherché (facultatif)
#' @param geojson Export au format geojson (\code{true} par défaut)
#' @param excel Export au format excel (\code{true} par défaut)
#' @param shp Export au format shapefile (\code{false} par défaut)
#' @param kml Export au format kml (\code{false} par défaut)
#' @import glue
#' @import openxlsx
#' @import RPostgreSQL
#' @import tidyverse
#' @export
#' @examples
#' poisson.programmation(2022)
#' poisson.programmation(2022, 224)
#' poisson.programmation(2022, 224, geojson = F, excel = F)

##### TODO LIST #####
# 
#####################

poisson.programmation <- function(
  annee = NA_integer_,
  projet_id = NA_integer_,
  geojson = T,
  excel = T,
  shp = F,
  kml = F
  )
{
  
  #### Évaluation des choix ####
  
  #### Vérification ####
  if(is.na(annee) == TRUE & is.na(projet_id) == TRUE) stop("Année ou projet non défini : l'un ou l'autre est au moins nécessaire")
  
  #### Connexion à la BDD et chargement des données ####
  dbD <- BDD.ouverture("Data")
  poissons_programmation <- tbl(dbD, dbplyr::in_schema("fd_production", "poissons_programmation")) %>% collect()
  poissons_moyens <- tbl(dbD, dbplyr::in_schema("fd_referentiels", "poissons_moyens")) %>% collect(n = Inf)
  DBI::dbDisconnect(dbD)

  #### Filtrage des résultats ####
  poissons_programmation_filtre <- 
    poissons_programmation %>% 
    {if(!is.na(annee)) filter(., year(poiprg_date) == annee) else .} %>%
    {if(!is.na(projet_id)) filter(., poiprg_projet_id == projet_id) else .}
      
  #### Regroupement/transformation des données ####
  poissons_programmation_synthese_1 <- 
    poissons_programmation_filtre %>% 
    left_join(poissons.stations(), by = c("poiprg_codestation_id" = "codestation")) %>% 
    select(nom, id:poiprg_remarques, xlambert, ylambert, codesiermc,)
  
  operations_recentes <-
    poissons.resultats(poissons_programmation_synthese_1 %>% rename(Nom = nom)) %>% 
    group_by(nom) %>% 
    arrange(desc(datedebut.x)) %>% 
    slice(1)
  
  poissons_programmation_synthese_2 <-
    poissons_programmation_synthese_1 %>% 
    left_join(operations_recentes, by = c("nom", "xlambert", "ylambert", "codesiermc"))
  
  #### Collecte et jointure des données de moyens nécessaires ####
  poissons_programmation_synthese_3 <-
    poissons_programmation_synthese_2 %>% 
    left_join(poissons_moyens %>% select(-id,-contains("_modif")), by = c("nom" = "poissmoyens_nom")) %>% 
    select(nomecosysteme, poiprg_projet_id, poiprg_objectif, nom, codesiermc, datedebut.x, poiprg_date, largeurlameeau, largeurlitmineur, nombreanodes, profondeur, gestionnaire, typelambert, xlambert, ylambert, poissmoyens_duree, poissmoyens_nb_professionnels, poissmoyens_nb_benevoles, poissmoyens_remarques, poiprg_pdm_pression, poiprg_commentaires, poiprg_remarques) %>% 
    arrange(poiprg_date, nom)

  #### Nettoyage et mise en forme ####
  poissons_programmation_synthese_4 <-
    poissons_programmation_synthese_3 %>% 
    filter(typelambert == "L93") %>% 
    select(-typelambert) %>% 
    st_as_sf(coords = c("xlambert","ylambert")) %>% 
    st_set_crs(2154)
  
  #### Contrôle ####
  if(nrow(poissons_programmation_filtre) != nrow(poissons_programmation_synthese_4)) warning("Attention : certaines stations ont disparues du fait de coordonnées non L93")

  #### Sortie des résultats ####
  ### Nom de fichier ###
  nom_fichier <- glue("{today()}_Programmation_suivis_piscicoles")
  if(!is.na(annee) & is.na(projet_id)) nom_fichier <- glue("{nom_fichier}_{annee}")
  if(is.na(annee) & !is.na(projet_id)) nom_fichier <- glue("{nom_fichier}_projet_{projet_id}")
  if(!is.na(annee) & !is.na(projet_id)) nom_fichier <- glue("{nom_fichier}_{annee}_projet_{projet_id}")
  
  ### Sortie ###
  if(geojson == T) poissons_programmation_synthese_4 %>% SIG.export(nom_fichier, shp = F, kml = F, geojson = T, excel = F)
  if(excel == T) poissons_programmation_synthese_3 %>% write.xlsx(glue("{nom_fichier}.xlsx"), sheetName = "Feuille1", row.names = F, showNA = F, overwrite = T)
  if(shp == T) poissons_programmation_synthese_4 %>% SIG.export(nom_fichier, shp = T, kml = F, geojson = F, excel = F)
  if(kml == T) poissons_programmation_synthese_4 %>% SIG.export(nom_fichier, shp = F, kml = T, geojson = F, excel = F)
  
  return(poissons_programmation_synthese_4)
  
} # Fin de la fonction
