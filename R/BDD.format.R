#' Mise au format des données pour ajout BDD
#'
#' Cette fonction permet de formater les données pour les ajouter aux bases de données respectives
#' @name BDD.format
#' @param data Jeu de données à mettre au format de la base de données
#' @param traitementforce `FALSE` (par défaut) ou `TRUE`
#' @param Type Thématique de travail : `MI`, `Chroniques`, `PC`, `Temps de travail`, `Topographie`, `AEP`
#' @keywords data
#' @import DBI
#' @import glue
#' @import lubridate
#' @import sf
#' @import testit
#' @import tidyverse
#' @export
#' @examples
#' BDD.format(data)

###### À faire #####
# Ajout d'un test pour les suivis de chronique : si le champ fonctionnement contient perdue, alors le champ action ne peut être que disparue, sinon stop et signalement
# 
####################

BDD.format <- function(
  data = data,
  traitementforce = FALSE,
  Type = c("MI", "Chroniques", "PC", "Temps de travail", "Topographie", "AEP")
  )
{
  
  ##### Évaluation des choix #####
  Type <- match.arg(Type)

  ###### Contexte ######
  if(traitementforce == "TRUE"){Type <- match.arg(Type)} # Évaluation des choix
  if(Type == "MI") warning("Attention le type par défaut est MI")
  
  ###### MI ######
  Testtraitementforce <- 0
  if(traitementforce == TRUE & Type == "MI") Testtraitementforce <- 1
  if(traitementforce == FALSE & Type == "MI") Testtraitementforce <- 1
  if(Testtraitementforce == 1){
    
  ## Connexion à la BDD ##
  dbD <- BDD.ouverture("Data")
  
  ## Récupération des données ##
  Habitats <- structure(list(id = integer(0), mihab_miop_id = integer(0), mihab_mihabref_id = integer(0), 
                             mihab_recouvrement = numeric(0), mihab_margdom = character(0), 
                             mihab_remarques = character(0), `_modif_utilisateur` = character(0), 
                             `_modif_type` = character(0), `_modif_date` = structure(numeric(0), tzone = "", class = c("POSIXct", 
                                                                                                                       "POSIXt"))), row.names = integer(0), class = c("tbl_df", 
                                                                                                                                                                      "tbl", "data.frame"))
  Prelevements <- structure(list(id = integer(0), miprlvt_miop_id = integer(0), 
                                 miprlvt_numech_dce = integer(0), miprlvt_numech_mag20 = integer(0), 
                                 miprlvt_numech_commun = integer(0), miprlvt_mihabref_id_substrat = integer(0), 
                                 miprlvt_mihabref_id_vitesse = integer(0), miprlvt_mihabref_id_hauteur = integer(0), 
                                 miprlvt_phasedce = character(0), miprlvt_ibgn = logical(0), 
                                 miprlvt_intensitecolmatage = numeric(0), miprlvt_stabilite = character(0), 
                                 miprlvt_naturevegetation = character(0), miprlvt_micapt_abondancevegetation = numeric(0), 
                                 miprlvt_remarques = character(0), `_modif_utilisateur` = character(0), 
                                 `_modif_type` = character(0), `_modif_date` = structure(numeric(0), tzone = "", class = c("POSIXct", 
                                                                                                                           "POSIXt"))), row.names = integer(0), class = c("tbl_df", 
                                                                                                                                                                          "tbl", "data.frame"))
  Captures <- structure(list(id = integer(0), micapt_miprlvt_id = integer(0), 
                             micapt_taxon = character(0), micapt_abondance = numeric(0), 
                             micapt_typeabondance = character(0), micapt_volumeabondance = character(0), 
                             micapt_stade = character(0), micapt_sexe = character(0), 
                             micapt_remarques = character(0), `_modif_utilisateur` = character(0), 
                             `_modif_type` = character(0), `_modif_date` = structure(numeric(0), tzone = "", class = c("POSIXct", 
                                                                                                                       "POSIXt"))), row.names = integer(0), class = c("tbl_df", 
                                                                                                                                                                      "tbl", "data.frame"))
  
  # Travail sur les habitats #
  if(all(colnames(data) %in% colnames(Habitats))) {
    
    # Ajout des ID
    data <-
      data %>% 
      mutate(id = row_number() + as.numeric(dbGetQuery(dbD, "SELECT MAX(id) FROM fd_production.macroinvertebres_habitats;")))
  }
  
  # Travail sur les prélèvements #
  if(all(colnames(data) %in% colnames(Prelevements))) {

    # Transformation des formats
    data <- 
      data %>% 
      mutate(id = as.integer(id)) %>% 
      mutate(miprlvt_miop_id = as.integer(miprlvt_miop_id)) %>% 
      mutate(miprlvt_numech_dce = as.integer(miprlvt_numech_dce)) %>% 
      mutate(miprlvt_numech_mag20 = as.integer(miprlvt_numech_mag20)) %>% 
      mutate(miprlvt_numech_commun = as.integer(miprlvt_numech_commun))
    
    # Ajout des ID
    data <-
      data %>% 
      mutate(id = row_number() + as.numeric(dbGetQuery(dbD, "SELECT MAX(id) FROM fd_production.macroinvertebres_prelevements;")))
  }
  
  # Travail sur les captures #
  if(all(colnames(data) %in% colnames(Captures))) {

    # Transformation des formats
    data <- 
      data %>% 
      mutate(id = as.integer(id)) %>% 
      mutate(micapt_miprlvt_id = as.integer(micapt_miprlvt_id)) %>% 
      mutate(micapt_abondance = as.integer(micapt_abondance))
    
    # Ajout des ID
    data <-
      data %>% 
      mutate(id = row_number() + as.numeric(dbGetQuery(dbD, "SELECT MAX(id) FROM fd_production.macroinvertebres_captures;")))
  } # Fin de travail sur les captures
  DBI::dbDisconnect(dbD)
  } # Fin de travail sur les MI
  
  ###### Chroniques ######
  Testtraitementforce <- 0
  if(traitementforce == TRUE & Type == "Chroniques") Testtraitementforce <- 1
  if(traitementforce == FALSE & Type == "Chroniques") Testtraitementforce <- 1
  if(Testtraitementforce == 1){
  
  ## Création des données type ##
    data("chronique_structure")
    Stations <- stations_structure
    Capteurs <- capteurs_structure
    Mesures <- mesures_structure
    mesures_sans_chmes_referentiel_temporel <- mesures_structure %>% select(-chmes_referentiel_temporel) # rustine temporaire le temps de modifier les scripts d'importation automatique
    SuiviTerrain <- suivis_structure
    commentaires <- commentaires_structure
    
    if(all(colnames(data) %in% colnames(mesures_sans_chmes_referentiel_temporel))) data <- data %>% mutate(chmes_referentiel_temporel = NA_character_) # rustine temporaire le temps de modifier les scripts d'importation automatique
  
  # MesuresGroupees
  MesuresGroupees <- structure(list(id = integer(0), chmesgr_coderhj_id = integer(0), 
                                    chmesgr_capteur_id = integer(0), chmesgr_date = structure(numeric(0), class = "Date"), 
                                    chmesgr_periodicite = character(0), chmesgr_typeagregation = character(0), 
                                    chmesgr_valeur = numeric(0), chmesgr_unite = character(0), 
                                    chmesgr_typemesure = character(0), chmesgr_validation = character(0), 
                                    chmesgr_mode_acquisition = character(0), chmesgr_mode_integration = character(0), 
                                    chmesgr_source = character(0), chmesgr_remarques = character(0), 
                                    `_modif_utilisateur` = character(0), `_modif_type` = character(0), 
                                    `_modif_date` = structure(numeric(0), tzone = "", class = c("POSIXct", 
                                                                                                "POSIXt"))), row.names = integer(0), class = c("tbl_df", 
                                                                                                                                               "tbl", "data.frame"))
  
  # Mesures #
  if(all(colnames(data) %in% colnames(Mesures))) {
    
    # Arrondi des valeurs
    data$chmes_valeur <- round(as.numeric(data$chmes_valeur),3) # On arrondi à 3 chiffres après la virgule
    
    # Complément des heures #
    data$chmes_heure <- ifelse(nchar(data$chmes_heure) == 5, paste0(data$chmes_heure, ":00"), data$chmes_heure) 
    
    # Correction des unités #
    data$chmes_unite <- ifelse(data$chmes_typemesure == "Piézométrie NGF", "NGF", data$chmes_unite)
    
    # Transformation des formats
    data$id <- as.integer(data$id)
    data$chmes_date <- as.character(data$chmes_date)
    
    # Ajout des ID
    data$id <- row_number(data$chmes_valeur) + as.numeric(tbl(dbD,in_schema("fd_production", "chroniques_mesures")) %>% summarise(max = max(id, na.rm = TRUE)) %>% collect()) # Pour incrémenter les id à partir du dernier
    if(dim(filter(data, is.na(id)))[1] > 0 & dim(filter(data, is.na(chmes_validation)))[1] == 0) data$id <- row_number(data$chmes_validation) + as.numeric(tbl(dbD,in_schema("fd_production", "chroniques_mesures")) %>% summarise(max = max(id, na.rm = TRUE)) %>% collect())
    if(dim(filter(data, is.na(id)))[1] > 0) stop("Tous les id ne sont pas complétés")
  }
  
  # Mesures groupées #
  if(all(colnames(data) %in% colnames(MesuresGroupees))) {
    
    # Ajout des ID
    data$id <- row_number(data$chmesgr_coderhj_id) + as.numeric(dbGetQuery(dbD, "SELECT MAX(id) FROM fd_production.chroniques_mesuresgroupees;")) # Pour incrémenter les id à partir du dernier
    data <- data %>% arrange(id)
  }
  
  # SuiviTerrain #
  if(all(colnames(data) %in% colnames(SuiviTerrain))) {
    
    # Travail sur les MO #
    data <-
      data %>% 
      mutate(chsvi_mo = ifelse(chsvi_mo == "Fédé 39", "FJPPMA", chsvi_mo)) %>% 
      mutate(chsvi_mo = ifelse(chsvi_mo == "FD39", "FJPPMA", chsvi_mo)) %>% 
      mutate(chsvi_mo = ifelse(chsvi_mo == "Fédé39", "FJPPMA", chsvi_mo)) %>% 
      mutate(chsvi_mo = ifelse(chsvi_mo == "PNR HJ", "PNRHJ", chsvi_mo)) %>% 
      mutate(chsvi_mo = ifelse(chsvi_mo == "PNR", "PNRHJ", chsvi_mo))
    
    # Travail sur les stations #
    data$chsvi_coderhj <- str_replace(data$chsvi_coderhj, " ", "") # On efface les espaces en trop dans les noms de station
    data$chsvi_coderhj <- str_to_upper(data$chsvi_coderhj, locale = "fr") # On met les noms de station en majuscules
    data <- 
      data %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "VOUCHARTREUSE", "VOUchartreuse", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "VOUGRINGALETPORT", "VOUgringaletport", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "VOUMERCANTINE", "VOUmercantine", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "VOUPATORNAY", "VOUpatornay", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "VOUSURCHAUFFANT", "VOUsurchauffant", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "VOUBELLECIN", "VOUbellecin", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "BONSEUIL", "BONseuil", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "BONZH", "BONzh", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "GCLVANNE", "GCLvanne", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "GCLPLONGEOIR", "GCLplongeoir", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "GCLZHAVAL", "GCLzhaval", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "GCLZHENTRE2", "GCLzhentre2", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "ILABLOC", "ILAbloc", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "LEM-2-2", "LEM2-2", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "NCZ6-2TRÉMONTAGNE", "NCZ6-2", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = stringr::str_replace(chsvi_coderhj, "_", "-")) %>% # Remplacement du caractère _ par -
      mutate(chsvi_coderhj = stringr::str_replace(chsvi_coderhj, "BIS$", "bis")) %>% # Remplacement en fin de station de BIS par bis
      mutate(chsvi_coderhj = stringr::str_replace(chsvi_coderhj, "LAC$", "lac")) %>% # Remplacement en fin de station de LAC par lac
      mutate(chsvi_coderhj = stringr::str_replace(chsvi_coderhj, "BARO$", "baro")) %>% # Remplacement en fin de station de BARO par baro
      mutate(chsvi_coderhj = stringr::str_replace(chsvi_coderhj, "AMONT$", "amont")) %>% # Remplacement en fin de station de AMONT par amont
      mutate(chsvi_coderhj = stringr::str_replace(chsvi_coderhj, "AVAL$", "aval")) %>% # Remplacement en fin de station de AVAL par aval
      mutate(chsvi_coderhj = stringr::str_replace(chsvi_coderhj, "ATMO$", "atmo")) # Remplacement en fin de station de ATMO par atmo
      #mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "", "", chsvi_coderhj)) %>% 
    
    # Travail sur les dates #
    if(mean(nchar(data$chsvi_date), na.rm = T) >= 6.5){ # Cas où toutes les dates ressemblent vraiment à des dates, pas 43020 (issu du format de champ Date de excel)
      if(testit::has_warning(ymd(data$chsvi_date)) == TRUE & testit::has_warning(dmy(data$chsvi_date)) == FALSE) data$chsvi_date <- as.character(format(dmy(data$chsvi_date), format="%Y-%m-%d"))
    }
    
    if(mean(nchar(data$chsvi_date), na.rm = T) == 5){ # Cas où aucune date ne ressemble pas à des dates, comme 43020 (issu du format de champ Date de excel)
      data <-
        data %>% 
        mutate(chsvi_date = ymd("1899-12-30") + as.numeric(chsvi_date))
    }
    
    if(mean(nchar(data$chsvi_date), na.rm = T) >= 4.5 & mean(nchar(data$chsvi_date), na.rm = T) < 6.5 & mean(nchar(data$chsvi_date), na.rm = T) != 5){ # Cas où certaines dates ressemblent vraiment à des dates, mais pas toutes : certaines sont de la forme 43020 (issu du format de champ Date de excel)
      if(testit::has_warning(ymd(data$chsvi_date)) == TRUE & testit::has_warning(dmy(data$chsvi_date)) == TRUE){
        data <-
          data %>% 
          mutate(chsvi_datebis = chsvi_date) %T>% 
          
          {options(warn=-1)} %>% 
          mutate(chsvi_date = format(ymd(.$chsvi_date), format="%Y-%m-%d")) %>% 
          mutate(chsvi_date = ifelse(is.na(chsvi_date), format(dmy(.$chsvi_datebis), format="%Y-%m-%d"), chsvi_date)) %>% 
          mutate(chsvi_date = ifelse(is.na(chsvi_date), format(ymd("1899-12-30") + as.numeric(chsvi_datebis), format="%Y-%m-%d"), chsvi_date)) %T>% 
          {options(warn=0)} %>% 
          
          select(-chsvi_datebis)
      }
    }
    if(data %>% filter(is.na(chsvi_date)) %>% nrow() > 0) stop("Présence de chsvi_date à la valeur NA")
    
    # Travail sur les heures #
    if(any(!is.na(data$chsvi_heure))){
    data <- 
      data %>% 
      mutate(count = nchar(chsvi_heure)) %>%
      mutate(chsvi_heure = ifelse(count == 2, glue("{chsvi_heure}00"), chsvi_heure)) %>%  # Cas d'une saisie avec 9h par exemple
      mutate(chsvi_heure = ifelse(count == 3, glue("{chsvi_heure}00"), chsvi_heure)) %>%  # Cas d'une saisie avec 10h par exemple
      select(-count) %>%
      mutate(chsvi_heure = str_replace(chsvi_heure, "h", ":")) %>% # On remplace le h par :
      mutate(chsvi_heure = str_replace(chsvi_heure, "H", ":")) %>% # On remplace le H par :
      mutate(chsvi_heure = ifelse(grepl("Oubli", chsvi_heure), NA_character_, chsvi_heure)) %>% 
      mutate(chsvi_heure = ifelse(grepl("oubli", chsvi_heure), NA_character_, chsvi_heure)) %>% 
      mutate(count = str_count(.$chsvi_heure, ":")) %>%
      mutate(chsvi_heure = ifelse(!is.na(chsvi_heure) & count == 1, glue("{chsvi_heure}:00"), chsvi_heure)) %>%
      select(-count) %>%
      mutate(count = nchar(.$chsvi_heure)) %>%
      mutate(chsvi_heure = ifelse(!is.na(chsvi_heure) & count == 7, glue("0{chsvi_heure}"), chsvi_heure)) %>%
      select(-count) %>%
      rowwise() %>% 
      mutate(chsvi_heure = ifelse(any(class(.$chsvi_heure) == "POSIXct"), format(chsvi_heure, format="%H:%M:%S"), chsvi_heure)) %>% 
      ungroup()
    if(testit::has_warning(format(ymd_hms(paste(data$chsvi_date,"-",data$chsvi_heure)), format="%H:%M:%S")) == FALSE) data$chsvi_heure <- format(ymd_hms(paste(data$chsvi_date,"-",data$chsvi_heure)), format="%H:%M:%S") # Afin de ré-écrire les heures proprement
    if(testit::has_warning(format(ymd_hms(data$chsvi_heure), format="%H:%M:%S")) == FALSE) data$chsvi_heure <- format(ymd_hms(data$chsvi_heure), format="%H:%M:%S") # Afin de ré-écrire les heures proprement
    }
    
    # Transformation des numéros de capteurs
    data <- 
      data %>% 
      mutate(chsvi_capteur = str_replace(chsvi_capteur, "\\..*", "")) %>% # On supprime d'éventuels .0 à la fin
      mutate(chsvi_capteur = str_replace(chsvi_capteur, "O", "0")) %>%  # On supprime d'éventuels O par des 0
      mutate(chsvi_capteur = str_replace(chsvi_capteur, "\\?", NA_character_)) # On supprime d'éventuels ?
    
    # Travail sur les valeurs manuelles #
    data <-
      data %>% 
      mutate(chsvi_valeur = str_replace(chsvi_valeur, "°C", "")) %>% # On efface le °C
      mutate(chsvi_valeur = str_replace(chsvi_valeur, "°c", "")) %>% # On efface le °c
      mutate(chsvi_valeur = str_replace(chsvi_valeur, "°", "")) %>% # On efface le °
      mutate(chsvi_valeur = ifelse(chsvi_valeur == "-", NA, chsvi_valeur)) %>% # On met des NA pour les valeurs absentes
      mutate(chsvi_valeur = ifelse(chsvi_valeur == "Impossible", NA, chsvi_valeur)) %>% # On met des NA pour les valeurs absentes
      mutate(chsvi_valeur = as.numeric(sub(",", ".", .$chsvi_valeur))) %>%
      mutate(chsvi_valeur = round(as.numeric(chsvi_valeur), 2)) # On arrondi à 1 chiffre après la virgule

    # Transformation des unités
    data <-
      data %>%
      mutate(chsvi_unite = str_replace(chsvi_unite, "degré Celsius", "°C"))
    
    # Travail sur les profondeurs
    data <-
      data %>%
      mutate(chsvi_profondeur = str_replace(chsvi_profondeur, "m", "")) %>% 
      mutate(chsvi_profondeur = str_replace(chsvi_profondeur, " m", "")) %>% 
      mutate(chsvi_profondeur = as.numeric(sub(",", ".", chsvi_profondeur)))

    # Transformation des actions
    data <-
      data %>% 
      mutate(chsvi_action = str_to_sentence(chsvi_action)) %>% # Pour n'avoir une majuscule qu'au début
      mutate(chsvi_action = dplyr::recode(chsvi_action,
                                       # "disparue" = "Disparue",
                                       "Sonde disparue" = "Disparue",
                                       "Perdue" = "Disparue",
                                       "releve" = "Relève",
                                       # "relève" = "Relève",
                                       "Relève et repose" = "Relève",
                                       "Relevé" = "Relève",
                                       # "pose" = "Pose",
                                       "Repose" = "Pose",
                                       "Relève, dépose" = "Dépose"
                                       )
             )

    # Vérification des types d'action
    if(dim(filter(data, chsvi_action == "changement de pile"))[1] > 0){
      data <- 
        data %>% 
        mutate(chsvi_remarques = ifelse(chsvi_action == "changement de pile", paste0(chsvi_remarques, " - Changement de pile"), chsvi_remarques)) %>% 
        mutate(chsvi_action = ifelse(chsvi_action == "changement de pile", "Relève", chsvi_action)) %>% 
        mutate(chsvi_remarques = ifelse(chsvi_remarques == "NA - Changement de pile", "Changement de pile", chsvi_remarques))
    }
    if(dim(filter(data, grepl("Dépose", chsvi_remarques)))[1] > 0){
    data <- 
      data %>% 
      mutate(chsvi_action = ifelse(grepl("Dépose", chsvi_remarques), "Dépose", chsvi_action))
    }
    bug_type_action <- data %>% filter(!(chsvi_action == "Disparue"|chsvi_action == "Pose"|chsvi_action == "Dépose"|chsvi_action == "Relève"|chsvi_action == "Mesure manuelle"|chsvi_action == "Entretien"))
    n_bug_type_action <- bug_type_action %>% distinct(chsvi_action) %>% nrow()
    if(n_bug_type_action == 1) stop(glue("Action(s) saisie(s) de type inconnu : {bug_type_action %>% distinct(chsvi_action) %>% pull()}"))
    if(n_bug_type_action > 1) stop(glue("Action(s) saisie(s) de types inconnus : {glue_collapse(bug_type_action %>% distinct(chsvi_action) %>% pull(), ', ', last = ' et ')}"))
    
    # Transformation des formats
    data$id <- as.integer(data$id)
    data$chsvi_date <- as.character(data$chsvi_date)
    
    # Ajout des ID
    data$id <- row_number(data$chsvi_coderhj) + as.numeric(dbGetQuery(dbD, "SELECT MAX(id) FROM fd_production.chroniques_suiviterrain;")) # Pour incrémenter les id à partir du dernier
    data <- data %>% arrange(id)
    
    # Données générales et ordre des colonnes
    data <- 
      data %>% 
      mutate(`_modif_type` = "I") %>% 
      mutate(`_modif_date` = now()) %>% 
      select(match(colnames(SuiviTerrain),names(.)))

  }
  
  # Capteurs #
  if(all(colnames(data) %in% colnames(Capteurs))) {
    data$id <- row_number(data$chcap_numerocapteur) + as.numeric(dbGetQuery(dbD, "SELECT MAX(id) FROM fd_production.chroniques_capteurs;")) # Pour incrémenter les id à partir du dernier
  }
  
  # Stations #
  if(all(colnames(data) %in% colnames(Stations))) {
    data <-
      data %>% 
      mutate(id = row_number(data$chsta_coderhj) + as.numeric(dbGetQuery(dbD, "SELECT MAX(id) FROM fd_production.chroniques_stations;"))) %>% # Pour incrémenter les id à partir du dernier
      #arrange(id) %>% # Pour conserver le même ordre que celui dans le fichier de saisie
      mutate(chsta_mo = ifelse(chsta_mo == "FD39", "FJPPMA", chsta_mo)) %>% 
      mutate(chsta_mo = ifelse(chsta_mo == "CD39", "CD39_CR_Ain", chsta_mo)) %>% 
      mutate(chsta_mo = ifelse(chsta_mo == "CD39-FJPPMA", "CD39_CR_Ain - FJPPMA", chsta_mo)) %>% 
      mutate(chsta_mo = ifelse(chsta_mo == "ONEMA", "OFB", chsta_mo)) %>% 
      mutate(chsta_mo = ifelse(chsta_mo == "AFB", "OFB", chsta_mo)) %>% 
      mutate(chsta_mo = ifelse(chsta_mo == "CD39_CR_Ain - ONEMA", "CD39_CR_Ain - OFB", chsta_mo)) %>% 
      mutate(chsta_mo = ifelse(chsta_mo == "CD39_CR_Ain - AFB", "CD39_CR_Ain - OFB", chsta_mo)) %>% 
      mutate(chsta_profsonde = str_replace(chsta_profsonde, "m", "")) %>% # On efface le m
      mutate(chsta_profsonde = as.numeric(sub(",", ".", .$chsta_profsonde))) %>% # On efface le m
      mutate(chsta_distberge = str_replace(chsta_distberge, "m", "")) %>% # On efface le m
      mutate(chsta_distberge = as.numeric(sub(",", ".", .$chsta_distberge))) %>% # On efface le m
      mutate(chsta_numphoto = ifelse(is.na(chsta_numphoto), "acompleter.png", chsta_numphoto)) %>% 
      mutate(`_modif_type` = "I") %>% 
      mutate(`_modif_date` = now()) %>% 
      select(match(colnames(Stations),names(.)))
  }
  
  # Résultats #
  if(length(colnames(data)) > 22) {
    if(colnames(data)[46] == "Percentile90diurneAB"){
      data <- 
        data %>% 
        rename_all(list(~ stringi::stri_trans_general(., "latin-ascii"))) %>% # Pour remplacer les caractères accentués par les mêmes sans accents
        rename_all(list(~ paste0("chres_", .))) %>%
        rename_all(list(~ gsub("[[:punct:]]", "_", .))) %>%
        rename_all(list(~ tolower(.))) %>% 
        mutate(chres_aquatoolsversion = packageVersion("aquatools") %>% as.character()) %>% 
        mutate(id = row_number() + as.numeric(dbGetQuery(dbD, "SELECT MAX(id) FROM fd_production.chroniques_resultats"))) %>% # Pour incrémenter les id à partir du dernier
        mutate(`_modif_utilisateur` = NA_character_) %>% 
        mutate(`_modif_type` = "I") %>% 
        mutate(`_modif_date` = now()) %>% 
        select(id, everything(), `_modif_utilisateur`, `_modif_type`,`_modif_date`)
    }
  } # fin de travail sur les résultats
  } # Fin de travail sur les chroniques
  
  ##### PC #####
  Testtraitementforce <- 0
  if(traitementforce == TRUE & Type == "PC") Testtraitementforce <- 1
  if(traitementforce == FALSE & Type == "PC") Testtraitementforce <- 1
  if(Testtraitementforce == 1){
    
  ## Connexion à la BDD ##
  dbD <- BDD.ouverture("Data")
    
  ## Récupération des données ##
  # pc <- tbl(dbD, in_schema("fd_production", "physicochimie_mesures")) %>% collect(n = 10)
  # operations <- tbl(dbD, in_schema("fd_production", "physicochimie_suiviterrain")) %>% collect(n = 10)
  
  pc <- 
    structure(list(id = integer(0), pcmes_coderhj = character(0), 
                   pcmes_codesie = character(0), pcmes_milieu = character(0), 
                   pcmes_date = character(0), pcmes_heure = character(0), pcmes_parametresandre = character(0), 
                   pcmes_parametrenom = character(0), pcmes_valeur = character(0), 
                   pcmes_unitenom = character(0), pcmes_unitesandre = character(0), 
                   pcmes_supportnom = character(0), pcmes_supportsandre = integer(0), 
                   pcmes_qualificationnom = character(0), pcmes_qualificationsandre = character(0), 
                   pcmes_validationnom = character(0), pcmes_validationsandre = character(0), 
                   pcmes_vraisemblancesandre = character(0), pcmes_profondeurlacustre = numeric(0), 
                   pcmes_zonelacustre = character(0), pcmes_laboratoirenom = character(0), 
                   pcmes_laboratoiresiret = character(0), pcmes_fractionnom = character(0), 
                   pcmes_fractionsandre = character(0), pcmes_coderemarque = character(0), 
                   pcmes_codeinsitu = character(0), pcmes_limitequantification = character(0), 
                   pcmes_limitedetection = character(0), pcmes_methodenom = character(0), 
                   pcmes_methodesandre = character(0), pcmes_accreditation = character(0), 
                   pcmes_producteurnom = character(0), pcmes_producteursandre = character(0), 
                   pcmes_origineproducteursandre = character(0), pcmes_remarques = character(0), 
                   `_modif_utilisateur` = character(0), `_modif_type` = character(0), 
                   `_modif_date` = structure(numeric(0), tzone = "", class = c("POSIXct", 
                                                                               "POSIXt"))), class = c("tbl_df", "tbl", "data.frame"), row.names = integer(0))
  
  operations <-
    structure(list(id = integer(0), pcsvi_mo = character(0), pcsvi_coderhj = character(0), 
                   pcsvi_codesie = character(0), pcsvi_date = character(0), 
                   pcsvi_heure = character(0), pcsvi_coord_x = numeric(0), pcsvi_coord_y = numeric(0), 
                   pcsvi_coord_type = character(0), pcsvi_coord_precision = numeric(0), 
                   pcsvi_operateurs = character(0), pcsvi_materiel = character(0), 
                   pcsvi_meteo = character(0), pcsvi_hydrologie = character(0), 
                   pcsvi_remarques = character(0), `_modif_utilisateur` = character(0), 
                   `_modif_type` = character(0), `_modif_date` = structure(numeric(0), tzone = "", class = c("POSIXct", 
                                                                                                             "POSIXt")), geom = character(0)), class = c("tbl_df", "tbl", 
                                                                                                                                                         "data.frame"), row.names = integer(0))

  ## Travail sur les mesures de PC ##
  if(all(colnames(data) %in% colnames(pc))) {
    
    # Transformation des formats
    data <-
      data %>% 
      mutate(pcmes_date = format(ymd(data$pcmes_date), format="%Y-%m-%d")) %>% # Car sinon transformation automatique des formats de date
    # Ajout des ID
      mutate(id = row_number() + as.numeric(dbGetQuery(dbD, "SELECT MAX(id) FROM fd_production.physicochimie_mesures;")))
  } # Fin de travail sur les mesures de PC
  
  if(all(colnames(data) %in% colnames(operations))) {
    
    # Transformation des formats
    data <-
      data %>% 
      mutate(pcsvi_date = format(ymd(data$pcsvi_date), format="%Y-%m-%d")) %>% # Car sinon transformation automatique des formats de date
      # Ajout des ID
      mutate(id = row_number() + as.numeric(dbGetQuery(dbD, "SELECT MAX(id) FROM fd_production.physicochimie_suiviterrain;")))
  } # Fin de travail sur les opérations de PC
  
  DBI::dbDisconnect(dbD)
  
  } # Fin de travail sur PC
  
  ##### Temps de travail ####
  Testtraitementforce <- 0
  if(traitementforce == TRUE & Type == "Temps de travail") Testtraitementforce <- 1
  if(traitementforce == FALSE & Type == "Temps de travail") Testtraitementforce <- 1
  if(Testtraitementforce == 1){
    ## Connexion à la BDD ##
    dbD <- BDD.ouverture("Data")
    
    ## Récupération des données ##
    tps_w_opentime <- structure(list(jour = structure(numeric(0), tzone = "UTC", class = c("POSIXct", 
                                                                                 "POSIXt")), domaine = character(0), activité = character(0), 
                           `sous-projet` = character(0), statut = character(0), `date de lancement` = character(0), 
                           `date de clôture` = character(0), métier = character(0), 
                           Utilisateur = character(0), `numéro de matricule` = logical(0), 
                           `temps (h)` = numeric(0), commentaire = character(0), Validé = character(0), 
                           `code analytique (domaine)` = logical(0), `code analytique` = logical(0), 
                           # `financeur(s)...16` = logical(0), `financeur(s)...17` = logical(0), 
                           `coût horaire (direct)` = numeric(0), `coût horaire (production)` = numeric(0), 
                           description = logical(0), de = character(0), à = character(0)), row.names = integer(0), class = c("tbl_df", 
                                                                                                                             "tbl", "data.frame"))
    projets_comptabilite <- structure(list(projet_id = character(0), compte_id = character(0), 
                                           num_mvt = character(0), journal = character(0), date = structure(numeric(0), class = "Date"), 
                                           num_piece = character(0), libelle_ecriture = character(0), 
                                           S = character(0), lett = character(0), ecriture_sens = character(0), 
                                           valeur = numeric(0)), row.names = integer(0), class = c("tbl_df", 
                                                                                                   "tbl", "data.frame"))
    TpsW <- tbl(dbD, dbplyr::in_schema("fd_production", "tpstravail_detail")) %>% collect(n = 1)
    RecapTpsW <- tbl(dbD, dbplyr::in_schema("fd_production", "tpstravail_recapitulatif")) %>% collect(n = 1)
    Projets <- tbl(dbD, dbplyr::in_schema("fd_production", "projets_liste")) %>% collect(n = Inf)
    Personnels <- tbl(dbD, dbplyr::in_schema("fd_referentiels", "gestion_operateurs")) %>% filter(gestop_mo == 3 & gestop_type == "Salarié") %>% select(id:gestop_qualite) %>% collect(n = Inf)

    ## Travail sur les données OpenTime ##
    if(all(colnames(data) %in% colnames(tps_w_opentime))) {
      
      # Transformation des formats
      data <-
        data %>% 
          # nettoyage
          rename(date = jour, poste = `métier`, personnel = Utilisateur, duree = `temps (h)`, remarques = commentaire, validation = `Validé`, heuredebut = de, heurefin = `à`) %>% 
          select(-statut, -`date de lancement`, -`date de clôture`, -`numéro de matricule`, -poste) %>% 
          select(date:`sous-projet`, personnel, heuredebut, heurefin, duree, validation, remarques) %>%
          mutate(date = as_date(date)) %>% 
          mutate(duree = round(duree, 2)) %>% 
          mutate(personnel = ifelse(personnel == "Fagot Jean Baptiste", "Fagot Jean-Baptiste", personnel)) %>% # Ajout du tiret manquant
          mutate(validation = str_to_title(validation)) %>% # Ajout d'une majuscule à oui/non
          # Complément/modification
          left_join(Personnels %>% mutate(personnel = glue("{gestop_nom} {gestop_prenom}")), by = c("personnel")) %>% 
          mutate(personnel = id) %>% 
          select(-contains("gestop"), -id) %>% 
          mutate(heuredebut = ifelse(grepl("Absence", domaine), "00:00:00", heuredebut)) %>% 
          mutate(heurefin = ifelse(grepl("Absence", domaine), "23:59:59", heurefin)) %>% 
          # Mise en forme 
          rename_all(list(~ stringi::stri_trans_general(., "latin-ascii"))) %>% # Pour remplacer les caractères accentués par les mêmes sans accents
          rename_all(list(~ paste0("tpswot_", .))) %>%
          rename_all(list(~ gsub("[[:punct:]]", "_", .))) %>%
          rename_all(list(~ tolower(.))) %>% 
          mutate(id = row_number() + as.numeric(dbGetQuery(dbD, "SELECT nextval('fd_production.tpstravail_opentime_id_seq');"))) %>% # Pour incrémenter les id à partir du dernier
          mutate(`_modif_utilisateur` = NA_character_) %>% 
          mutate(`_modif_type` = NA_character_) %>% 
          mutate(`_modif_date` = NA) %>% 
          select(id, everything(), `_modif_utilisateur`, `_modif_type`,`_modif_date`)
    } # Fin de travail sur les données OpenTime
    
    ## Travail sur les données de comptabilité ##
    if(all(colnames(data) %in% colnames(projets_comptabilite))) {
      
      # Transformation des formats
      data <-
        data %>% 
        # Nettoyage
        rename(ecriture_id = num_mvt, piece = num_piece, ecriture_libelle = libelle_ecriture, ecriture_valeur = valeur, s_temporaire = S, lett_temporaire = lett) %>% 
        select(projet_id, date, compte_id, ecriture_id, ecriture_libelle, piece, journal, ecriture_sens, ecriture_valeur) %>%
        mutate(date = as_date(date)) %>% 
        # Complément/modification
        mutate(presencelogiciel = NA_character_, .after = ecriture_valeur) %>% 
        mutate(prjcompta_remarques = NA_character_, .after = presencelogiciel) %>% 
        # mise en forme 
        rename_all(list(~ stringi::stri_trans_general(., "latin-ascii"))) %>% # Pour remplacer les caractères accentués par les mêmes sans accents
        rename_all(list(~ paste0("prjcompta_", .))) %>%
        rename_all(list(~ gsub("[[:punct:]]", "_", .))) %>%
        rename_all(list(~ tolower(.))) %>% 
        mutate(id = row_number() + as.numeric(dbGetQuery(dbD, "SELECT nextval('fd_production.projets_comptabilite_id_seq');"))) %>% # Pour incrémenter les id à partir du dernier
        mutate(`_modif_utilisateur` = NA_character_) %>% 
        mutate(`_modif_type` = NA_character_) %>% 
        mutate(`_modif_date` = NA) %>% 
        select(id, everything(), `_modif_utilisateur`, `_modif_type`,`_modif_date`)
    } # Fin de travail sur les données de comptabilité
    
    DBI::dbDisconnect(dbD)
    
  } # Fin de travail sur Temps de travail
  
  
  ##### Topographie #####
  Testtraitementforce <- 0
  if(traitementforce == TRUE & Type == "Topographie") Testtraitementforce <- 1
  if(traitementforce == FALSE & Type == "Topographie") Testtraitementforce <- 1
  if(Testtraitementforce == 1){
    ## Connexion à la BDD ##
    dbD <- BDD.ouverture("Data")
    
    ## Récupération des données ##
    leves <- tbl(dbD, in_schema("fd_production", "topographie_leves")) %>% collect(n = 10)
    # Operations <- tbl(dbD, in_schema("fd_production", "physicochimie_suiviterrain")) %>% collect(n = 10)
    
    ## Travail sur les mesures de levés ##
    if(all(colnames(data) %in% colnames(leves))) {
      
      # Transformation des formats
      data <-
        data %>% 
        # mutate(pcmes_date = format(ymd(data$pcmes_date), format="%Y-%m-%d")) %>% # Car sinon transformation automatique des formats de date
        # Ajout des ID
        mutate(id = row_number() + as.numeric(dbGetQuery(dbD, "SELECT MAX(id) FROM fd_production.topographie_leves;")))
    } # Fin de travail sur les mesures de levés topographiques
    
    DBI::dbDisconnect(dbD)
    
  } # Fin de travail sur topographie
  
  
  ##### AEP #####
  Testtraitementforce <- 0
  if(traitementforce == TRUE & Type == "AEP") Testtraitementforce <- 1
  if(traitementforce == FALSE & Type == "AEP") Testtraitementforce <- 1
  if(Testtraitementforce == 1){
    ## Connexion à la BDD ##
    dbD <- BDD.ouverture("Data")
    
    ## Récupération des données ##
    prelevements <- tbl(dbD, in_schema("fd_referentiels", "hydrologie_prelevements")) %>% arrange(desc(id)) %>% collect(n = 10)
    
    ## Travail sur les prélèvements ##
    if(all(colnames(data) %in% colnames(prelevements))) {
      
      # Transformation des formats
      data <-
        data %>% 
        # mutate(pcmes_date = format(ymd(data$pcmes_date), format="%Y-%m-%d")) %>% # Car sinon transformation automatique des formats de date
        # Ajout des ID
        mutate(id = row_number() + as.numeric(dbGetQuery(dbD, "SELECT MAX(id) FROM fd_referentiels.hydrologie_prelevements;")))
    } # Fin de travail sur les prélèvements
    
    DBI::dbDisconnect(dbD)
    
  } # Fin de travail sur AEP
  
  ##### Commun #####
data <- as_tibble(data)

  return(data)
  
} # Fin de la fonction
