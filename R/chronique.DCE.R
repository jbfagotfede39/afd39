#' Réaliser un export au format DCE AE-OFB de données thermiques
#'
#' Permet de réaliser un export de données thermiques au format DCE AE-OFB
#' @name chronique.DCE
#' @keywords chronique
#' @param data Data.frame issu de chronique.mesures, ne pouvant pas contenir différentes stations
#' @param typemesure Défini le type de données et modifie le traitement en fonction
#' @param projet Nom du projet
#' @param export Si \code{TRUE}, exporte les résultats (\code{FALSE} par défaut)
#' @param dep39 Si \code{FALSE} (par défault), ne va pas rechercher les données de stations dans la base locale et donc export simplifié. Si \code{TRUE}, fait la jointure SIG. Possibilité d'utiliser \code{autre} afin de sélectionner un fichier source de stations
#' @param fichierStations Permet de passer automatiquement la localisation du fichier de stations si on le souhaite, sinon le demandera
#' @param fichierSuivis Permet de passer automatiquement la localisation du fichier de suivis si on le souhaite, sinon le demandera
#' @param fichierCapteurs Permet de passer automatiquement la localisation du fichier des capteurs si on le souhaite, sinon le demandera
#' @import glue
#' @import openxlsx
#' @import sf
#' @import tidyverse
#' @export
#' @examples
#' chronique.DCE(data)
#' chronique.mesures("SUR0-9", "Thermie") %>% chronique.DCE()

chronique.DCE <- function(  
  data = data,
  typemesure = c("Thermie", "Thermie barométrique", "Thermie piézométrique", "Barométrie", "Piézométrie", "Piézométrie brute", "Piézométrie compensée", "Piézométrie calée", "Piézométrie NGF", "Oxygénation", "Hydrologie", "Pluviométrie"),
  projet = NA_character_,
  export = T,
  dep39 = F,
  fichierStations = NA_character_,
  fichierSuivis = NA_character_,
  fichierCapteurs = NA_character_
)
{
  
  #### Évaluation des choix ####
  typemesure <- match.arg(typemesure)
  
  #### Module d'analyse des données ####
  
  #### Collecte des données ####
  if(dep39 == TRUE){
  dbD <- BDD.ouverture("Data")
  listeStations <- 
    sf::st_read(dbD, query = "SELECT * FROM fd_production.chroniques_stations;") %>% 
    filter(chsta_coderhj %in% data$chmes_coderhj) %>% 
    collect() %>% 
    mutate(chsta_codepointprlvmt = NA_character_) # manquant
  SuiviTerrain <- chronique.suivi(unique(data$chmes_coderhj), Recherche = "Station")
  Capteurs <- unique(data$chmes_capteur) %>% purrr::map_dfr(~ chronique.capteurs(., Recherche = "Numéro"))
  DBI::dbDisconnect(dbD)
  }
  
  if(dep39 == "autre"){
    ### Stations ###
    listeStations <- data %>% distinct(chmes_coderhj)
    if(!is.na(fichierStations)) fnameStations <- fichierStations
    # if(is.na(fichierStations)) fnameStations <- tk_choose.files(caption = "Fichier de stations")
    if(is.na(fichierStations)) stop("Localisation du fichier des stations à saisir manuellement")
    Stations <- chronique.ouverture("Stations", "Thermie", fnameStations)
    
    if(!is.na(fichierSuivis)) fnameSuivi <- fichierSuivis
    # if(is.na(fichierSuivis)) fnameSuivi <- tk_choose.files(caption = "Fichier de suivi de terrain")
    if(is.na(fichierSuivis))  stop("Localisation du fichier du suivi de terrain à saisir manuellement")
    SuiviTerrain <- chronique.ouverture("Suivis", "Thermie", fnameSuivi)
    
    listeStations <- 
      Stations %>% 
      filter(chsta_coderhj %in% listeStations$chmes_coderhj) %>% 
      mutate(chsta_codepointprlvmt = NA_character_) %>% # manquant
      sf::st_as_sf(coords = c("chsta_coord_x","chsta_coord_y")) %>% 
      st_set_crs(2154) %>% 
      mutate(chsta_coord_x = st_coordinates(.)[,1]) %>% 
      mutate(chsta_coord_y = st_coordinates(.)[,2])
    
    ### Capteurs ###
    listeCapteurs <- data %>% distinct(chmes_capteur)
    if(!is.na(fichierCapteurs)) fnameCapteurs <- fichierCapteurs
    # if(is.na(fichierCapteurs)) fnameCapteurs <- tk_choose.files(caption = "Fichier des capteurs")
    if(is.na(fichierCapteurs))  stop("Localisation du fichier des capteurs à saisir manuellement")
    Capteurs <- chronique.ouverture("Capteurs", "Thermie", fnameCapteurs)
  }

  ##### Mise au format DCE #####
  ### Pose-relève ###
  OngletPoseReleve <-
    listeStations %>% 
    rowwise() %>% 
    mutate(tpcomm_commune_libelle = aquatools::BV.ComByCoordL93(chsta_coord_x,chsta_coord_y) %>% dplyr::select(name) %>% as.character()) %>% 
    ungroup() %>% 
    mutate(chsta_detailsloc = glue('{chsta_rive} - {chsta_detailsloc}')) %>% 
    dplyr::select(chsta_codesie, chsta_codepointprlvmt, chsta_codemo, chsta_mo, chsta_milieu, tpcomm_commune_libelle, chsta_coderhj, chsta_coord_x, chsta_coord_y, chsta_detailsloc, chsta_ombrage, chsta_facies) %>% 
    {if(nrow(SuiviTerrain) != 0) right_join(., SuiviTerrain %>% 
                 mutate(chsvi_date = format(chsvi_date, format="%d/%m/%Y")) %>% 
                 mutate(chsvi_profondeur = ifelse(chsvi_profondeur > 10, chsvi_profondeur / 10, chsvi_profondeur)) %>%
                 mutate(chsvi_remarques = paste0(chsvi_action, " - ", chsvi_remarques)) %>% 
                 dplyr::select(chsvi_coderhj, chsvi_capteur, chsvi_date, chsvi_profondeur, chsvi_valeur, chsvi_operateurs, chsvi_remarques),
               by = c('chsta_coderhj' = 'chsvi_coderhj')
    ) else .} %>% 
    {if(nrow(SuiviTerrain) == 0) mutate(., chsvi_capteur = NA_character_) else .} %>% 
    {if(nrow(SuiviTerrain) == 0) mutate(., chsvi_date = NA_character_) else .} %>% 
    {if(nrow(SuiviTerrain) == 0) mutate(., chsvi_profondeur = NA_character_) else .} %>% 
    {if(nrow(SuiviTerrain) == 0) mutate(., chsvi_valeur = NA_character_) else .} %>% 
    {if(nrow(SuiviTerrain) == 0) mutate(., chsvi_operateurs = NA_character_) else .} %>% 
    {if(nrow(SuiviTerrain) == 0) mutate(., chsvi_remarques = NA_character_) else .} %>% 
    {if(nrow(Capteurs) != 0 & nrow(SuiviTerrain) != 0) right_join(., Capteurs %>% 
                                              dplyr::select(chcap_numerocapteur, chcap_modelecapteur),
                                            by = c('chsvi_capteur' = 'chcap_numerocapteur')
    ) else .} %>% 
    {if(nrow(Capteurs) == 0 | nrow(SuiviTerrain) == 0) mutate(., chcap_modelecapteur = NA_character_) else .} %>% 
    dplyr::select(chsta_codesie, chsta_codepointprlvmt, chsta_codemo, chsta_mo, chsta_milieu, tpcomm_commune_libelle, chsta_coderhj, chsta_coord_x, chsta_coord_y, chsvi_capteur, chcap_modelecapteur, chsvi_date, chsta_detailsloc, chsta_ombrage, chsta_facies, chsvi_profondeur, chsvi_valeur, chsvi_operateurs, chsvi_remarques) %>% 
    rename(CODE_STA_SANDRE = chsta_codesie, 
           CODE_PT_PRELEVT_SANDRE = chsta_codepointprlvmt,
           COD_STA_METIER = chsta_codemo, 
           OPERATEUR = chsta_mo, 
           RIV_NOM = chsta_milieu, 
           COM_NOM = tpcomm_commune_libelle, 
           NOM_STA_METIER = chsta_coderhj,
           X_93 = chsta_coord_x, 
           Y_93 = chsta_coord_y, 
           COD_SONDE = chsvi_capteur,
           MODEL_SONDE = chcap_modelecapteur,
           DATE_POSE = chsvi_date,
           EMPLACEMENT_POSE = chsta_detailsloc,
           OMBRAGE = chsta_ombrage,
           FACIES_POSE = chsta_facies,
           PROF_POSE = chsvi_profondeur,
           T_POSE = chsvi_valeur,
           AGENT_POSE = chsvi_operateurs,
           RQ_POSE = chsvi_remarques) %>%
    filter(!(is.na(COD_STA_METIER) & is.na(RIV_NOM) & is.na(X_93))) %>% # Rustine temporaire pour effacer le suivi qui ne correspond pas à la station : code à reprendre en profondeur
    st_drop_geometry()

  ### Chronique ###
  OngletChronique <-
    data %>% 
    mutate(chmes_date = format(chmes_date, format="%d/%m/%Y")) %>% 
    right_join(listeStations,
               by = c('chmes_coderhj' = 'chsta_coderhj')
    ) %>% 
    mutate(chmes_capteur = ifelse("chmes_capteur" %in% names(.), chmes_capteur, NA_character_)) %>% 
    mutate(chmes_validation = ifelse("chmes_validation" %in% names(.), chmes_validation, NA_character_)) %>% 
    mutate(chmes_mode_acquisition = ifelse("chmes_mode_acquisition" %in% names(.), chmes_mode_acquisition, "Mesuré")) %>% 
    mutate(chmes_referentiel_temporel = ifelse("chmes_referentiel_temporel" %in% names(.), chmes_referentiel_temporel, NA_character_)) %>% 
    dplyr::select(chsta_codesie, chsta_codepointprlvmt, chsta_codemo, chmes_coderhj, chmes_capteur, chmes_date, chmes_heure, chmes_referentiel_temporel, chmes_valeur, chmes_validation, chmes_mode_acquisition) %>% 
    rename(CODE_STA_SANDRE = chsta_codesie, 
           CODE_PT_PRELEVT_SANDRE = chsta_codepointprlvmt,
           COD_STA_METIER = chsta_codemo, 
           NOM_STA_METIER = chmes_coderhj,
           COD_SONDE = chmes_capteur, 
           DATE = chmes_date, 
           HEURE = chmes_heure, 
           REF_HORAIRE = chmes_referentiel_temporel, 
           T = chmes_valeur)
  
  ##### Export à proprement parler #####
  if(export == TRUE){
  list_of_datasets <- list("Pose_relève" = OngletPoseReleve, "chronique de température" = OngletChronique)
  openxlsx::write.xlsx(list_of_datasets, file = glue("./{projet}/Sorties/Données/DCE/{unique(data$chmes_coderhj)}_données_format_DCE.xlsx"))
  }
  if(export == FALSE){
    warning("La fonction export = F est à développer")
  }
  
} # Fin de la fonction
