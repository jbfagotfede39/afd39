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
#' @import lubridate 
#' @import openxlsx
#' @import sf
#' @import tcltk
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
  fichierSuivis = NA_character_
)
{
  
  ##### -------------- A FAIRE -------------- #####
  #
  # 
  # -------------- A FAIRE -------------- #  
  
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
  DBI::dbDisconnect(dbD)
  }
  
  if(dep39 == "autre"){
    listeStations <- data %>% distinct(chmes_coderhj)
    if(!is.na(fichierStations)) fnameStations <- fichierStations
    if(is.na(fichierStations)) fnameStations <- tk_choose.files(caption = "Fichier de stations")
    Stations <- chronique.ouverture("Stations", "Thermie", fnameStations)
    
    if(!is.na(fichierSuivis)) fnameSuivi <- fichierSuivis
    if(is.na(fichierSuivis)) fnameSuivi <- tk_choose.files(caption = "Fichier de suivi de terrain")
    SuiviTerrain <- chronique.ouverture("Suivis", "Thermie", fnameSuivi)
    
    listeStations <- 
      Stations %>% 
      filter(chsta_coderhj %in% listeStations$chmes_coderhj) %>% 
      mutate(chsta_codepointprlvmt = NA_character_) # manquant
  }

  ##### Mise au format DCE #####
  ### Pose-relève ###
  OngletPoseReleve <-
    listeStations %>% 
    rowwise() %>% 
    mutate(tpcomm_commune_libelle = aquatools::BV.ComByCoordL93(chsta_coord_x,chsta_coord_y) %>% select(name) %>% as.character()) %>% 
    ungroup() %>% 
    select(chsta_codesie, chsta_codepointprlvmt, chsta_codemo, chsta_milieu, tpcomm_commune_libelle, chsta_coderhj, chsta_coord_x, chsta_coord_y, chsta_detailsloc) %>% 
    {if(nrow(SuiviTerrain) != 0) right_join(., SuiviTerrain %>% 
                 mutate(chsvi_date = format(chsvi_date, format="%d/%m/%Y")) %>% 
                 mutate(chsvi_profondeur = NA_character_) %>% # manquant
                 mutate(chsvi_remarques = paste0(chsvi_action, " - ", chsvi_remarques)) %>% 
                 select(chsvi_coderhj, chsvi_capteur, chsvi_date, chsvi_profondeur, chsvi_valeur, chsvi_operateurs, chsvi_remarques),
               by = c('chsta_coderhj' = 'chsvi_coderhj')
    ) else .} %>% 
    {if(nrow(SuiviTerrain) == 0) mutate(., chsvi_capteur = NA_character_) else .} %>% 
    {if(nrow(SuiviTerrain) == 0) mutate(., chsvi_date = NA_character_) else .} %>% 
    {if(nrow(SuiviTerrain) == 0) mutate(., chsvi_profondeur = NA_character_) else .} %>% 
    {if(nrow(SuiviTerrain) == 0) mutate(., chsvi_valeur = NA_character_) else .} %>% 
    {if(nrow(SuiviTerrain) == 0) mutate(., chsvi_operateurs = NA_character_) else .} %>% 
    {if(nrow(SuiviTerrain) == 0) mutate(., chsvi_remarques = NA_character_) else .} %>% 
    select(chsta_codesie, chsta_codepointprlvmt, chsta_codemo, chsta_milieu, tpcomm_commune_libelle, chsta_coderhj, chsta_coord_x, chsta_coord_y, chsvi_capteur, chsvi_date, chsvi_profondeur, chsvi_valeur, chsvi_operateurs, chsta_detailsloc, chsvi_remarques) %>% 
    rename(CODE_STA_SANDRE = chsta_codesie, 
           CODE_PT_PRELEVT_SANDRE = chsta_codepointprlvmt,
           COD_STA_METIER = chsta_codemo, 
           RIV_NOM = chsta_milieu, 
           COM_NOM = tpcomm_commune_libelle, 
           NOM_STA_METIER = chsta_coderhj,
           X_93 = chsta_coord_x, 
           Y_93 = chsta_coord_y, 
           COD_SONDE = chsvi_capteur,
           DATE_POSE = chsvi_date,
           PROF_POSE = chsvi_profondeur,
           T_POSE = chsvi_valeur,
           AGENT_POSE = chsvi_operateurs,
           EMPLACEMENT_POSE = chsta_detailsloc,
           RQ_POSE = chsvi_remarques)
  
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
    select(chsta_codesie, chsta_codepointprlvmt, chsta_codemo, chmes_coderhj, chmes_capteur, chmes_date, chmes_heure, chmes_valeur, chmes_validation, chmes_mode_acquisition) %>% 
    rename(CODE_STA_SANDRE = chsta_codesie, 
           CODE_PT_PRELEVT_SANDRE = chsta_codepointprlvmt,
           COD_STA_METIER = chsta_codemo, 
           NOM_STA_METIER = chmes_coderhj,
           COD_SONDE = chmes_capteur, 
           DATE = chmes_date, 
           HEURE = chmes_heure, 
           T = chmes_valeur)
  
  ##### Export à proprement parler #####
  if(export == TRUE){
  list_of_datasets <- list("Pose_relève" = OngletPoseReleve, "chronique de température" = OngletChronique)
  openxlsx::write.xlsx(list_of_datasets, file = paste0("./",projet, "/Sorties/Données/DCE/", unique(data$chmes_coderhj), "_données_format_DCE.xlsx"))
  }
  if(export == FALSE){
    warning("La fonction export = F est à développer")
  }
  
} # Fin de la fonction
