#' Exécuter le traitement de données de chroniques
#'
#' Permet d'exécuter plusieurs opérations de traitement des données de chronique d'une station
#' @name chronique.traitement
#' @keywords chronique
#' @param data Data.frame issu de chronique.mesures, pouvant contenir différentes stations
#' @param projet Nom du projet
#' @param export Si \code{TRUE} (par défaut), exporte les résultats/figures. Si \code{FALSE}, ne les exporte pas.
#' @param exportfigures Si \code{TRUE} (par défaut), exporte les figures. Si \code{FALSE}, ne les exporte pas.
#' @param style \code{boxplot} ou \code{violon}
#' @param exportfigures_chronique_classique Si \code{TRUE} (par défaut), exporte les vues annuelles des chroniques. Si \code{FALSE}, ne les exporte pas.
#' @param exportfigures_cumul_degresjours Si \code{TRUE} (par défaut), exporte les cumuls de degrés-jours. Si \code{FALSE}, ne les exporte pas.
#' @param exportfigures_boxplot_interannuel Si \code{TRUE} (par défaut), exporte les boxplots interannuels. Si \code{FALSE}, ne les exporte pas.
#' @param exportfigures_classes_calendaires Si \code{TRUE} (par défaut), exporte les vues sous forme de classes calendaires. Si \code{FALSE}, ne les exporte pas.
#' @param exportfigures_profil_longitudinal Si \code{TRUE} (par défaut), exporte les figures de profils longitudinaux. Si \code{FALSE}, ne les exporte pas.
#' @param exportfigures_preferendums_especes Si \code{TRUE} (par défaut), exporte les figures de préférendums des espèces avec les Vmm30j. Si \code{FALSE}, ne les exporte pas.
#' @param exportDCE Si \code{TRUE}, exporte les mesures au format DCE. Si \code{FALSE} (par défaut), ne les exporte pas.
#' @param filtrage Si \code{TRUE}, filtre les résultats avec chronique.resultats.filtrage pour ne conserver que les pertinents pour les représentations graphiques inter-annuelles/inter-sites. \code{FALSE} (par défaut)
#' @param filtrageanneevmm Si \code{TRUE} (par défaut), ne conserve que les résultats dont l'année de fin de vmm30j est égale à l'année biologique du résultat
#' @param filtragedatefperiode Ne conserver que les résultats dont la date de fin de période est postérieure à la date choisie ("07-15", soit le 15 juillet par défaut)
#' @param filtragenbj Nombre de journées minimales pour chaque résultat à conserver (75 par défaut).
#' @param seuils Seuils de valeurs (25,22,19,15,4 par défaut) dont il faut tester les dépassements. L'ordre de sortie correspond à l'ordre de cette liste.
#' @param seuilexcesdefaut Seuil en-deça duquel les dépassements sont testés par défaut, sinon ils sont testés par excès (valeur de 12 par défaut).
#' @param dep39 Si \code{FALSE} (par défaut), ne va pas rechercher les données de stations dans la base locale et donc export simplifié. Si \code{TRUE}, fait la jointure SIG. Possibilité d'utiliser \code{autre} afin de sélectionner un fichier source de stations
#' @param localisation_stations Si \code{NA} (par défaut), ouvre un applet pour localiser le fichier des stations
#' @param localisation_commentaires Si \code{NA} (par défaut), ouvre un applet pour localiser le fichier des commentaires
#' @param localisation_suiviterrain Si \code{NA} (par défaut), ouvre un applet pour localiser le fichier de suivi de terrain
#' @param localisation_capteurs Si \code{NA} (par défaut), ouvre un applet pour localiser le fichier des capteurs
#' @param localisation_donnees Si \code{NA} (par défaut), ouvre un applet pour localiser le fichier des données brutes
#' @param archivage Si \code{Aucun} (par défaut), ne va pas créer une archives .zip du répertoire de sortie. Si \code{Partiel}, créé une archive et conserve le répertoire. Si \code{Complet}, créé une archive et supprimer le répertoire.
#' @param log Si \code{Simple} (par défaut), le log ne sera pas très bavard. Si \code{Verbeux}, le log incluera tous les retours du tidyverse. Si \code{Aucun}, absence de log
#' @import fs
#' @import glue
#' @import logr
#' @import lubridate
#' @import openxlsx
#' @import sf
#' @import tidyverse
#' @import zip
#' @export
#' @examples
#' chronique.traitement(data)
#' chronique.traitement(data, export = F)
#' DataTravail <- chronique.mesures("SUR0-9", "Thermie") %>% chronique.traitement()
#' DataTravail <- chronique.mesures("SUR0-9", "Thermie") %>% chronique.traitement(., export = F, filtrage = T)
#' DataTravail <- tbl(db,"Mesures") %>% filter(CodeRDT == "DOU393-2" | CodeRDT == "DOU394-5") %>% collect() %>% chronique.traitement()

chronique.traitement <- function(  
  data = data,
  projet = NA_character_,
  export = T,
  exportfigures = T,
  style = c("boxplot","violon"),
  exportfigures_chronique_classique = T,
  exportfigures_cumul_degresjours = T,
  exportfigures_boxplot_interannuel = T,
  exportfigures_classes_calendaires = T,
  exportfigures_profil_longitudinal = T,
  exportfigures_preferendums_especes = T,
  exportDCE = F,
  filtrage = F,
  filtrageanneevmm = TRUE,
  filtragedatefperiode = "07-15",
  filtragenbj = 75,
  seuils = c(25,22,19,15,4),
  seuilexcesdefaut = 12,
  dep39 = c(FALSE, TRUE, "autre"),
  localisation_stations = NA,
  localisation_commentaires = NA,
  localisation_suiviterrain = NA,
  localisation_capteurs = NA,
  localisation_donnees = NA,
  archivage = c("Aucun","Partiel","Complet"),
  log = c("Simple", "Verbeux", "Aucun")
  )
{

##### -------------- A FAIRE -------------- #####
# Ajouter un test qui vérifie, avec de sortir les figures de préférendums thermiques, si on travaille bien sur des données thermiques
# Implanter la fonction chronique.resultats.periode() dans chronique.analyse() ou chronique.traitement() plutôt, afin que le calcul de l'enveloppe soit directement juste, notamment pour les autres FD
# Il faudrait ajouter une suppression automatique du répertoire de sortie s'il existe déjà
# Export fichier readme qui explique le fonctionnement + auteur + date + noms/versions (session_info) des packages 
# Il serait intéressant de créer un test avant chaque calcul de figure, qui vérifie si le fichier de sortie existe déjà dans le répertoire de sortie, auquel cas la figure n'est pas de nouveau générée. Intérêt dans le cas de gros traitement qui plantent en cours de route : pas tout à refaire pour les figures lorsqu'on a résolu le pb
# Ajouter une vérification qu'il n'y a pas un double commentaire sur une clé, ce qui aurait pour effet de produire un double ligne de résultats et donc risque de confusion + bug dans la génération de figures qui s'appuient dessus (preferendums thermiques par ex)
# Essayer de supprimer le warning qui apparaît suite à # Analyse des données # (In bind_rows_(x, .id) : Unequal factor levels: coercing to character)
# Faire un outil de regroupement des données brutes au format large à partir de la fin du code de 2020-02-13_Export_suivi_FUR_format_DCE.R  
# -------------- A FAIRE -------------- #  

#### Évaluation des choix ####
  archivage <- match.arg(archivage)
  style <- match.arg(style)
  log <- match.arg(log)
  
  if(exportDCE == TRUE) export <- T
  if(export == FALSE) exportDCE <- F
  if(export == FALSE) exportfigures <- F # À laisser dans cet ordre
  if(exportfigures == TRUE) export <- T # À laisser dans cet ordre
  if(exportfigures == FALSE) exportfigures_chronique_classique <- F
  if(exportfigures == FALSE) exportfigures_cumul_degresjours <- F
  if(exportfigures == FALSE) exportfigures_boxplot_interannuel <- F
  if(exportfigures == FALSE) exportfigures_classes_calendaires <- F
  if(exportfigures == FALSE) exportfigures_profil_longitudinal <- F
  if(exportfigures == FALSE) exportfigures_preferendums_especes <- F
  if(export == FALSE) archivage <- "Aucun"

#### Mise en route du log ####
  if(log != "Aucun"){
    options("logr.on" = TRUE) # Turn logger on
    
    if(log == "Simple"){options("logr.autolog" = FALSE)} # Turn autolog off
    if(log == "Verbeux"){options("logr.autolog" = TRUE)} # Turn autolog on
    # Autolog will automatically print logging entries for many dplyr and tidyr functions
  }

#### Modification du projet avec la date #####
projet <- glue('{today()}_{projet}')

#### Vérification des répertoires ####
if(export == TRUE){
  # Répertoire principal
  if(file.exists(paste0("./",projet)) == FALSE){
    dir.create(paste0("./",projet), showWarnings = FALSE, recursive = FALSE)
  }
  
#### Création et ouverture du fichier de log ####
  if(log != "Aucun"){
    log <- file.path(glue("{projet}/{projet}.log")) # Create log file location
    logfile <- log_open(log) # Open log
    put("Fin de l'ouverture du log") # Log
  }
  
  # Répertoires secondaires
  if(file.exists(paste0("./",projet, "/log/")) == FALSE){dir.create(paste0("./",projet, "/log/"), showWarnings = FALSE, recursive = FALSE)}
  if(file.exists(paste0("./",projet, "/Sorties/")) == FALSE){
  dir.create(paste0("./",projet, "/Sorties/"), showWarnings = FALSE, recursive = FALSE)
  dir.create(paste0("./",projet, "/Sorties/Données/"), showWarnings = FALSE, recursive = FALSE)
  dir.create(paste0("./",projet, "/Sorties/Données/Agrégations_diverses/"), showWarnings = FALSE, recursive = FALSE)
  if(exportDCE == TRUE){dir.create(paste0("./",projet, "/Sorties/Données/DCE/"), showWarnings = FALSE, recursive = FALSE)}
  dir.create(paste0("./",projet, "/Sorties/Résultats/"), showWarnings = FALSE, recursive = FALSE)
  dir.create(paste0("./",projet, "/Sorties/Stations/"), showWarnings = FALSE, recursive = FALSE)
  dir.create(paste0("./",projet, "/Sorties/Vues/"), showWarnings = FALSE, recursive = FALSE)
  dir.create(paste0("./",projet, "/Sorties/Vues/Interannuelles"), showWarnings = FALSE, recursive = FALSE)
  dir.create(paste0("./",projet, "/Sorties/Vues/Intersites"), showWarnings = FALSE, recursive = FALSE)
  if(exportfigures_classes_calendaires == TRUE){dir.create(paste0("./",projet, "/Sorties/Vues/Calendaires"), showWarnings = FALSE, recursive = FALSE)}
  if(exportfigures_preferendums_especes == TRUE){dir.create(paste0("./",projet, "/Sorties/Vues/Preferendums_biologiques"), showWarnings = FALSE, recursive = FALSE)}
  dir.create(paste0("./",projet, "/Sorties/Vues/Annuelles_absolu-fixe/"), showWarnings = FALSE, recursive = FALSE)
  dir.create(paste0("./",projet, "/Sorties/Vues/Annuelles_absolu-libre/"), showWarnings = FALSE, recursive = FALSE)
  dir.create(paste0("./",projet, "/Sorties/Vues/Annuelles_relatif-fixe/"), showWarnings = FALSE, recursive = FALSE)
  dir.create(paste0("./",projet, "/Sorties/Vues/Annuelles_relatif-libre/"), showWarnings = FALSE, recursive = FALSE)
}

if(file.exists(paste0("./",projet, "/Sorties/")) == TRUE & file.exists(paste0("./",projet, "/Sorties/Données/")) == FALSE){
    dir.create(paste0("./",projet, "/Sorties/Données/"), showWarnings = FALSE, recursive = FALSE)
  if(exportDCE == TRUE){dir.create(paste0("./",projet, "/Sorties/Données/DCE/"), showWarnings = FALSE, recursive = FALSE)}
}

if(file.exists(paste0("./",projet, "/Sorties/")) == TRUE & file.exists(paste0("./",projet, "/Sorties/Résultats")) == FALSE){
  dir.create(paste0("./",projet, "/Sorties/Résultats/"), showWarnings = FALSE, recursive = FALSE)
}
  
  if(file.exists(paste0("./",projet, "/Sorties/")) == TRUE & file.exists(paste0("./",projet, "/Sorties/Stations")) == FALSE){
    dir.create(paste0("./",projet, "/Sorties/Stations/"), showWarnings = FALSE, recursive = FALSE)
  }
  
if(file.exists(paste0("./",projet, "/Sorties/")) == TRUE & file.exists(paste0("./",projet, "/Sorties/Vues/")) == FALSE){
  dir.create(paste0("./",projet, "/Sorties/Vues/"), showWarnings = FALSE, recursive = FALSE)
  dir.create(paste0("./",projet, "/Sorties/Vues/Interannuelles"), showWarnings = FALSE, recursive = FALSE)
  dir.create(paste0("./",projet, "/Sorties/Vues/Intersites"), showWarnings = FALSE, recursive = FALSE)
  if(exportfigures_classes_calendaires == TRUE){dir.create(paste0("./",projet, "/Sorties/Vues/Calendaires"), showWarnings = FALSE, recursive = FALSE)}
  if(exportfigures_preferendums_especes == TRUE){dir.create(paste0("./",projet, "/Sorties/Vues/Preferendums_biologiques"), showWarnings = FALSE, recursive = FALSE)}
  dir.create(paste0("./",projet, "/Sorties/Vues/Annuelles_absolu-fixe/"), showWarnings = FALSE, recursive = FALSE)
  dir.create(paste0("./",projet, "/Sorties/Vues/Annuelles_absolu-libre/"), showWarnings = FALSE, recursive = FALSE)
  dir.create(paste0("./",projet, "/Sorties/Vues/Annuelles_relatif-fixe/"), showWarnings = FALSE, recursive = FALSE)
  dir.create(paste0("./",projet, "/Sorties/Vues/Annuelles_relatif-libre/"), showWarnings = FALSE, recursive = FALSE)
}

if(export == TRUE & dep39 != TRUE & file.exists(paste0("./",projet, "/Entrées/")) == FALSE){
  dir.create(paste0("./",projet, "/Entrées/"), showWarnings = FALSE, recursive = FALSE)
  dir.create(paste0("./",projet, "/Entrées/Données/"), showWarnings = FALSE, recursive = FALSE)
  dir.create(paste0("./",projet, "/Entrées/Stations/"), showWarnings = FALSE, recursive = FALSE)
  dir.create(paste0("./",projet, "/Entrées/Capteurs/"), showWarnings = FALSE, recursive = FALSE)
  dir.create(paste0("./",projet, "/Entrées/Commentaires/"), showWarnings = FALSE, recursive = FALSE)
  dir.create(paste0("./",projet, "/Entrées/Suivi/"), showWarnings = FALSE, recursive = FALSE)
}

if(log != "Aucun") put("Fin de la création des répertoires") # Log
} # Fin de if(export == TRUE){}
  
#### Notices ####
if(export == TRUE){
  formatage.abreviation(thematique = "Chronique", formatage = "Propre", export = T) # Export dans le répertoire courant
  file_move("./Glossaire.xlsx", glue("./{projet}/")) # Déplacement à la racine 
}
if(log != "Aucun") put("Fin de l'exportation du glossaire") # Log

#### Préparation des données ####
data <-
  data %>% 
  formatage.annee.biologique() # Calcul de l'année biologique
if(log != "Aucun") put("Fin de la préparation des données") # Log

#### Nettoyage des données ####
if("chmes_typemesure" %in% colnames(data) == FALSE){
  data <- 
    data %>% 
    group_by(chmes_coderhj, chmes_anneebiol) %>% 
    filter(n_distinct(chmes_date) > 30) %>% # Pour supprimer les années biologiques avec moins de 30 dates différentes
    ungroup()
}

if("chmes_typemesure" %in% colnames(data) == TRUE){
data <- 
  data %>% 
  group_by(chmes_coderhj, chmes_anneebiol,chmes_typemesure) %>% 
  filter(n_distinct(chmes_date) > 30) %>% # Pour supprimer les années biologiques avec moins de 30 dates différentes
  ungroup()
}
if(log != "Aucun") put("Fin du nettoyage des données") # Log

#### Importation des données nécessaires aux exportations ####
listeStations <- data %>% distinct(chmes_coderhj)
listeCapteurs <- data %>% distinct(chmes_capteur)

if(export == TRUE){
  dbD <- BDD.ouverture("Data")
  HER <- sf::st_read(dbD, query = "SELECT * FROM fd_referentiels.hydrographie_hydroecoregions;")
  DBI::dbDisconnect(dbD)
}

if(export == TRUE & dep39 == TRUE){
  dbD <- BDD.ouverture("Data")
  listeStations <- sf::st_read(dbD, query = "SELECT * FROM fd_production.chroniques_stations;") %>% filter(chsta_coderhj %in% listeStations$chmes_coderhj) %>% collect() %>% dplyr::select(chsta_coderhj:chsta_departement, chsta_coord_x:chsta_coord_type, chsta_fonctionnement:chsta_reseauthermietype, chsta_altitude, chsta_distancesource, chsta_typetheorique, chsta_sprep)
  communes <- sf::st_read(dbD, query = "SELECT * FROM fd_referentiels.topographie_communes WHERE (tpcomm_departement_insee = '39' OR tpcomm_departement_insee = '25' OR tpcomm_departement_insee = '01');")
  contextesPDPG <- sf::st_read(dbD, query = "SELECT * FROM fd_referentiels.hydrographie_contextespdpg;")
  Commentaires <- tbl(dbD, in_schema("fd_production", "chroniques_commentaires")) %>% collect(n = Inf)
  SuiviTerrain <- listeStations$chsta_coderhj %>% map_dfr(~ chronique.suivi(., Recherche = "Station", Sortie = "Propre"))
  SuiviCapteurs <- listeCapteurs$chmes_capteur %>% map_dfr(~ chronique.capteurs(., Recherche = "Numéro", Sortie = "Propre"))
  DBI::dbDisconnect(dbD)
}

if(export == TRUE & dep39 == "autre"){

  # if(is.na(localisation_stations)) fnameStations <- tk_choose.files(caption = "Fichier de stations")
  if(is.na(localisation_stations)) stop("Localisation du fichier des stations à saisir manuellement")
  if(!is.na(localisation_stations)) fnameStations <- adresse.switch(localisation_stations)
  Stations <- chronique.ouverture("Stations", "Thermie", fnameStations)
  
  # if(is.na(localisation_commentaires)) fnameCommentaires <- tk_choose.files(caption = "Fichier de commentaires")
  if(is.na(localisation_commentaires))  stop("Localisation du fichier des commentaires à saisir manuellement")
  if(!is.na(localisation_commentaires)) fnameCommentaires <- adresse.switch(localisation_commentaires)
  Commentaires <- chronique.ouverture("Commentaires", "Thermie", fnameCommentaires)
  
  if(exportDCE == TRUE){
    # if(is.na(localisation_suiviterrain)) fnameSuivi <- tk_choose.files(caption = "Fichier de suivi de terrain")
    if(is.na(localisation_suiviterrain))  stop("Localisation du fichier du suivi de terrain à saisir manuellement")
    if(!is.na(localisation_suiviterrain)) fnameSuivi <- adresse.switch(localisation_suiviterrain)
    SuiviTerrain <- chronique.ouverture("Suivis", "Thermie", fnameSuivi)
    
    # if(is.na(localisation_capteurs)) fnameCapteurs <- tk_choose.files(caption = "Fichier des capteurs")
    if(is.na(localisation_capteurs))  stop("Localisation du fichier des capteurs à saisir manuellement")
    if(!is.na(localisation_capteurs)) fnameCapteurs <- adresse.switch(localisation_capteurs)
    Capteurs <- chronique.ouverture("Capteurs", "Thermie", fnameCapteurs)
  }
  
  # if(is.na(localisation_donnees)) fnameDonnesbrutes <- tk_choose.dir(caption = "Répertoire des chroniques") # On interroge tout de suite l'opérateur pour ne pas le déranger ensuite seulement pour le répertoire des données brutes
  if(is.na(localisation_donnees))  stop("Localisation du répertoire des données à saisir manuellement")
  if(!is.na(localisation_donnees)) fnameDonnesbrutes <- adresse.switch(localisation_donnees)
  
}
if(log != "Aucun") put("Fin de l'importation des données nécessaires aux exportations") # Log

#### Analyse des données ####
DataTravail <- 
  data %>%
  group_split(chmes_coderhj, chmes_anneebiol, chmes_typemesure) %>% 
    purrr::map_dfr(~ chronique.analyse(., seuils = seuils, seuilexcesdefaut = seuilexcesdefaut)) %>% 
    ungroup()
# ça pourra crasher par ici lorsqu'on fera un essai mixant chmes_typemesure == "Thermie" avec un autre chmes_typemesure à cause de la jointure à réaliser et un nb de champ différent (absence de TRF$DateDebutDegresJours et la suite)

DataTravail <- 
  DataTravail %>% 
    group_by(Coderhj, Typemesure) %>% 
    summarise(min = min(AnneeVMM),
              max = max(AnneeVMM)) %>% 
    mutate(PeriodeTotale = paste0(min, " - ", max)) %>% 
    dplyr::select(-min,-max) %>% 
    left_join(DataTravail, by = c("Coderhj", "Typemesure")) %>% 
  ungroup()
if(log != "Aucun") put("Fin de l'analyse des données") # Log

#### Sortie stations ####
if(export == TRUE & dep39 == "autre"){
  ## Préparation format SIG ##
  listeStations <- Stations %>% filter(chsta_coderhj %in% listeStations$chmes_coderhj) %>% dplyr::select(chsta_coderhj:chsta_codecontextepdpg, chsta_coord_x:chsta_coord_type, chsta_fonctionnement:chsta_reseauthermietype, chsta_altitude, chsta_distancesource, chsta_typetheorique, chsta_sprep)
  listeStations <-
    listeStations %>% 
    rowwise() %>% 
    mutate(tpcomm_commune_libelle = aquatools::BV.ComByCoordL93(chsta_coord_x,chsta_coord_y) %>% dplyr::select(name) %>% as.character()) %>% 
    mutate(chsta_departement = aquatools::BV.ComByCoordL93(chsta_coord_x,chsta_coord_y) %>% dplyr::select(codeDepartement) %>% as.character()) %>% 
    ungroup() %>% 
    sf::st_as_sf(coords = c("chsta_coord_x","chsta_coord_y")) %>% 
    st_set_crs(2154)
}

## Export en SIG ##
if(export == TRUE & dep39 != FALSE){
  SIG.export(listeStations, paste0("./",projet, "/Sorties/Stations/", format(now(), format="%Y-%m-%d"), "_Stations"))
}
if(log != "Aucun") put("Fin de la sortie des stations") # Log

#### Sortie suivi de terrain #####
if(export == TRUE & dep39 == TRUE){
  SuiviTerrain %>% 
    openxlsx::write.xlsx(paste0("./",projet, "/Sorties/", format(now(), format="%Y-%m-%d"), "_suivi_terrain.xlsx"), sheetName = "SuiviTerrain", row.names = F, showNA = F, colWidths="auto")
}
if(log != "Aucun") put("Fin de la sortie du suivi de terrain") # Log

#### Sortie données capteurs #####
if(export == TRUE & dep39 == TRUE){
  SuiviCapteurs %>% 
    openxlsx::write.xlsx(paste0("./",projet, "/Sorties/", format(now(), format="%Y-%m-%d"), "_historique_capteurs.xlsx"), sheetName = "Capteurs", row.names = F, showNA = F, colWidths="auto")
  }
if(log != "Aucun") put("Fin de la sortie des données de capteurs") # Log

#### Sortie résultats élaborés ####
if(export == TRUE & dep39 == TRUE){
DataTravailSIG <- listeStations %>% left_join(DataTravail %>% mutate(intervalMax = as.numeric(sub(",", ".", IntervalleMax))), by = c("chsta_coderhj" = "Coderhj"))
DataTravailSIG <- DataTravailSIG %>% left_join(communes %>% st_drop_geometry() %>% dplyr::select(tpcomm_commune_insee, tpcomm_commune_libelle), by = c('chsta_commune' = "tpcomm_commune_insee"))
DataTravailSIG <- DataTravailSIG %>% st_join(HER) %>% dplyr::select(-id)
DataTravailSIG <- DataTravailSIG %>% st_join(contextesPDPG %>% dplyr::select(hycont_contexte_code))
DataTravailSIG <- DataTravailSIG %>% left_join(Commentaires %>% dplyr::select(chres_coderhj, chres_typemesure, chres_anneebiol, chres_commentaire), by = c('chsta_coderhj' = "chres_coderhj", "AnneeVMM" = "chres_anneebiol", "Typemesure" = "chres_typemesure"))
SIG.export(DataTravailSIG, paste0("./",projet, "/Sorties/Résultats/", format(now(), format="%Y-%m-%d"), "_Resultats"), shp = F)
SIG.export(DataTravailSIG, paste0("./",projet, "/Sorties/Résultats/", "Atlas_Resultats"), shp = F, excel = F, kml = F)
}

if(export == TRUE & dep39 == "autre"){
  DataTravailSIG <- listeStations %>% left_join(DataTravail %>% mutate(intervalMax = as.numeric(sub(",", ".", IntervalleMax))), by = c("chsta_coderhj" = "Coderhj"))
  DataTravailSIG <- DataTravailSIG %>% st_join(HER) %>% dplyr::select(-id)
  DataTravailSIG <- DataTravailSIG %>% 
  {if("chsta_codecontextepdpg" %in% names(DataTravailSIG)) mutate(., hycont_contexte_code = chsta_codecontextepdpg) %>% dplyr::select(., -chsta_codecontextepdpg) else .} %>%
  {if(!("chsta_codecontextepdpg" %in% names(DataTravailSIG))) mutate(., hycont_contexte_code = NA_character_) else .} %>% 
  {if(!("chsta_milieu" %in% names(DataTravailSIG))) mutate(., chsta_milieu = NA_character_) else .}
  DataTravailSIG <- DataTravailSIG %>% left_join(Commentaires %>% dplyr::select(chres_coderhj, chres_typemesure, chres_anneebiol, chres_commentaire), by = c('chsta_coderhj' = "chres_coderhj", "AnneeVMM" = "chres_anneebiol", "Typemesure" = "chres_typemesure"))
  SIG.export(DataTravailSIG, paste0("./",projet, "/Sorties/Résultats/", format(now(), format="%Y-%m-%d"), "_Resultats"), shp = F)
  SIG.export(DataTravailSIG, paste0("./",projet, "/Sorties/Résultats/", "Atlas_Resultats"), shp = F, excel = F, kml = F)
}

if(export == TRUE & dep39 == FALSE){
  DataTravail %>% 
    openxlsx::write.xlsx(paste0("./",projet, "/Sorties/Résultats/", format(now(), format="%Y-%m-%d"), "_Resultats.xlsx"), sheetName = "Feuille1", row.names = F, showNA = F, colWidths="auto")
}
if(log != "Aucun") put("Fin de la sortie des résultats élaborés") # Log

#### Sorties format DCE ####
if(exportDCE == TRUE){
if(export == TRUE & dep39 == TRUE){
  data %>%
    group_split(chmes_coderhj) %>% # Permet d'éclater le dataframe en x dataframe, x étant le nb de modalités de chmes_coderhj
    purrr::map(~ chronique.DCE(data = ., projet = projet, export = T, dep39 = T))
}

if(export == TRUE & dep39 == "autre"){
  data %>%
    group_split(chmes_coderhj) %>% # Permet d'éclater le dataframe en x dataframe, x étant le nb de modalités de chmes_coderhj
    purrr::map(~ chronique.DCE(data = ., projet = projet, export = T, dep39 = "autre", fichierStations = fnameStations, fichierSuivis = fnameSuivi, fichierCapteurs = fnameCapteurs))
}

if(export == TRUE & dep39 == FALSE){
  data %>%
    group_split(chmes_coderhj) %>% # Permet d'éclater le dataframe en x dataframe, x étant le nb de modalités de chmes_coderhj
    purrr::map(~ chronique.DCE(data = ., projet = projet, export = T, dep39 = F, fichierStations = fnameStations, fichierSuivis = fnameSuivi, fichierCapteurs = fnameCapteurs))
}
}
if(log != "Aucun") put("Fin de la sortie des données au format DCE") # Log

#### Sorties agrégées #### 
if(export == TRUE){
  ## Calcul et exportation des données agrégées ##
  mesures_agregees_completes <- 
    data %>%
    group_split(chmes_coderhj) %>% # Permet d'éclater le dataframe en x dataframe, x étant le nb de modalités de chmes_coderhj
    #purrr::map_dfr(~ chronique.agregation(data = ., projet = projet, export = T)) # Fonctionne mais génère un message d'erreur : qqsoit le nb de chmes_coderhj : l'erreur intervient toujours après que le dernier fichier ait été généré (qqsoit le fichier) et arrête donc la fonction chronique.traitement
    purrr::map(~ chronique.agregation(data = ., projet = projet, export = T))
  
  ## Extraction des données agrégées journalières ##
  for(i in 1:length(mesures_agregees_completes)) { # Afin de regrouper les données journalières après un traitement intégral exporté et stocké
    if(i == 1) mesures_agregees_journalieres <- mesures_agregees_completes[[c(1, 2)]]
    if(i != 1){
      mesures_agregees_journalieres <-
        mesures_agregees_journalieres %>%
        union(mesures_agregees_completes[[c(i, 2)]])
    } # Fin de condition de i
  } # Fin de boucle
  mesures_agregees_journalieres <-
    mesures_agregees_journalieres %>% 
    ungroup() %>% 
    formatage.annee.biologique()
} # Fin de sorties agrégées

if(export == FALSE){
  # Soit adapter le code du export = TRUE pour que les sorties directes fonctionnent, 
  # # Exemple d'essai 
  # data %>% 
  #   group_by(chmes_coderhj) %>% 
  #   nest() %>% 
  #   purrr::map(chronique.agregation(data = ., export = F))
  # soit faire une agrégation batarde comme celle dans le rapport de Vogna 2018 avec des union, mais moins propre
}
if(log != "Aucun") put("Fin de la sortie des données agrégées") # Log

#### Sorties graphiques ####
### Contexte ###
contexte_stations <- listeStations %>% chronique.contexte()

if (exportfigures == TRUE) {
### Sortie graphique chronique complète ###
## Type de mesures spécifié ##
  if(exportfigures_chronique_classique == T){ # Debut de l'interrupteur volontaire d'export des vues de chroniques classiques
    if (all(export & "chmes_typemesure" %in% colnames(data)) == TRUE) {
      # Y libre sans vmm30j
      data %>%
        group_split(chmes_coderhj, chmes_anneebiol, chmes_typemesure) %>%
        purrr::map_dfr(~ chronique.figure(data = ., Titre = as.character(paste0(unique(.$chmes_coderhj), " - ", unique(.$chmes_anneebiol))), duree = "Complet", typemesure = unique(.$chmes_typemesure), complement = TRUE, Ymin = NA, Ymax = NA, save = T, projet = projet, format = ".png"))
      # Y libre avec vmm30j
      data %>%
        group_split(chmes_coderhj, chmes_anneebiol, chmes_typemesure) %>%
        purrr::map_dfr(~ chronique.figure(data = ., Titre = as.character(paste0(unique(.$chmes_coderhj), " - ", unique(.$chmes_anneebiol))), duree = "Complet", typemesure = unique(.$chmes_typemesure), complement = TRUE, Ymin = NA, Ymax = NA, Vmm30j = T, save = T, projet = projet, format = ".png"))
      # Y fixé sans vmm30j
      data %>%
        group_split(chmes_coderhj, chmes_anneebiol, chmes_typemesure) %>%
        purrr::map_dfr(~ chronique.figure(data = ., Titre = as.character(paste0(unique(.$chmes_coderhj), " - ", unique(.$chmes_anneebiol))), duree = "Complet", typemesure = unique(.$chmes_typemesure), complement = TRUE, Ymin = -1, Ymax = 30, save = T, projet = projet, format = ".png"))
      # Y fixé avec vmm30j
      data %>%
        group_split(chmes_coderhj, chmes_anneebiol, chmes_typemesure) %>%
        purrr::map_dfr(~ chronique.figure(data = ., Titre = as.character(paste0(unique(.$chmes_coderhj), " - ", unique(.$chmes_anneebiol))), duree = "Complet", typemesure = unique(.$chmes_typemesure), complement = TRUE, Ymin = -1, Ymax = 30, Vmm30j = T, save = T, projet = projet, format = ".png"))
    }
    
    ## Type de mesures non spécifié ##
    if (export == T) {
      if (all("chmes_typemesure" %in% colnames(data)) != TRUE) {
        warning("Vérification nécessaire car plusieurs typemesure donc ce paramètre n'est pas pris en compte dans les sorties graphiques")
        # Y libre sans vmm30j
        data %>%
          group_split(chmes_coderhj, chmes_anneebiol) %>%
          purrr::map_dfr(~ chronique.figure(data = ., Titre = as.character(paste0(unique(unlist(.$chmes_coderhj)), " - ", unique(unlist(.$chmes_anneebiol)))), duree = "Complet", typemesure = "Thermie", Ymin = NA, Ymax = NA, save = T, projet = projet, format = ".png")) # Fonctionne si plusieurs années
        # Y libre avec vmm30j
        data %>%
          group_split(chmes_coderhj, chmes_anneebiol) %>%
          purrr::map_dfr(~ chronique.figure(data = ., Titre = as.character(paste0(unique(unlist(.$chmes_coderhj)), " - ", unique(unlist(.$chmes_anneebiol)))), duree = "Complet", typemesure = "Thermie", Ymin = NA, Ymax = NA, Vmm30j = T, save = T, projet = projet, format = ".png")) # Fonctionne si plusieurs années
        # Y fixé sans vmm30j
        data %>%
          group_split(chmes_coderhj, chmes_anneebiol) %>%
          purrr::map_dfr(~ chronique.figure(data = ., Titre = as.character(paste0(unique(unlist(.$chmes_coderhj)), " - ", unique(unlist(.$chmes_anneebiol)))), duree = "Complet", typemesure = "Thermie", Ymin = -1, Ymax = 30, save = T, projet = projet, format = ".png")) # Fonctionne si plusieurs années
        # Y fixé avec vmm30j
        data %>%
          group_split(chmes_coderhj, chmes_anneebiol) %>%
          purrr::map_dfr(~ chronique.figure(data = ., Titre = as.character(paste0(unique(unlist(.$chmes_coderhj)), " - ", unique(unlist(.$chmes_anneebiol)))), duree = "Complet", typemesure = "Thermie", Ymin = -1, Ymax = 30, Vmm30j = T, save = T, projet = projet, format = ".png")) # Fonctionne si plusieurs années
      }
    }
    if(log != "Aucun") put("Fin de la sortie graphique des chroniques complètes") # Log
    
    ### Chronique incomplète ###
    ## Type de mesures spécifié ##
    if (all(export & "chmes_typemesure" %in% colnames(data)) == TRUE) {
      # Y libre sans vmm30j
      data %>%
        group_split(chmes_coderhj, chmes_anneebiol, chmes_typemesure) %>%
        purrr::map_dfr(~ chronique.figure(data = ., Titre = as.character(paste0(unique(unlist(.$chmes_coderhj)), " - ", unique(unlist(.$chmes_anneebiol)))), duree = "Relatif", typemesure = unique(.$chmes_typemesure), Ymin = NA, Ymax = NA, save = T, projet = projet, format = ".png")) # Fonctionne si plusieurs années
      # Y libre avec vmm30j
      data %>%
        group_split(chmes_coderhj, chmes_anneebiol, chmes_typemesure) %>%
        purrr::map_dfr(~ chronique.figure(data = ., Titre = as.character(paste0(unique(unlist(.$chmes_coderhj)), " - ", unique(unlist(.$chmes_anneebiol)))), duree = "Relatif", typemesure = unique(.$chmes_typemesure), Ymin = NA, Ymax = NA, Vmm30j = T, save = T, projet = projet, format = ".png")) # Fonctionne si plusieurs années
      # Y fixé sans vmm30j
      data %>%
        group_split(chmes_coderhj, chmes_anneebiol, chmes_typemesure) %>%
        purrr::map_dfr(~ chronique.figure(data = ., Titre = as.character(paste0(unique(unlist(.$chmes_coderhj)), " - ", unique(unlist(.$chmes_anneebiol)))), duree = "Relatif", typemesure = unique(.$chmes_typemesure), Ymin = -1, Ymax = 30, save = T, projet = projet, format = ".png")) # Fonctionne si plusieurs années
      # Y fixé avec vmm30j
      data %>%
        group_split(chmes_coderhj, chmes_anneebiol, chmes_typemesure) %>%
        purrr::map_dfr(~ chronique.figure(data = ., Titre = as.character(paste0(unique(unlist(.$chmes_coderhj)), " - ", unique(unlist(.$chmes_anneebiol)))), duree = "Relatif", typemesure = unique(.$chmes_typemesure), Ymin = -1, Ymax = 30, Vmm30j = T, save = T, projet = projet, format = ".png")) # Fonctionne si plusieurs années
    }
    
    ## Type de mesures non spécifié ##
    if (all("chmes_typemesure" %in% colnames(data)) != TRUE) {
      warning("Vérification nécessaire car plusieurs typemesure donc ce paramètre n'est pas pris en compte dans les sorties graphiques")
      # Y libre sans vmm30j
      data %>%
        group_split(chmes_coderhj, chmes_anneebiol) %>%
        purrr::map_dfr(~ chronique.figure(data = ., Titre = as.character(paste0(unique(unlist(.$chmes_coderhj)), " - ", unique(unlist(.$chmes_anneebiol)))), duree = "Relatif", typemesure = "Thermie", Ymin = NA, Ymax = NA, save = T, projet = projet, format = ".png")) # Fonctionne si plusieurs années
      # Y libre avec vmm30j
      data %>%
        group_split(chmes_coderhj, chmes_anneebiol) %>%
        purrr::map_dfr(~ chronique.figure(data = ., Titre = as.character(paste0(unique(unlist(.$chmes_coderhj)), " - ", unique(unlist(.$chmes_anneebiol)))), duree = "Relatif", typemesure = "Thermie", Ymin = NA, Ymax = NA, Vmm30j = T, save = T, projet = projet, format = ".png")) # Fonctionne si plusieurs années
      # Y fixé sans vmm30j
      data %>%
        group_split(chmes_coderhj, chmes_anneebiol) %>%
        purrr::map_dfr(~ chronique.figure(data = ., Titre = as.character(paste0(unique(unlist(.$chmes_coderhj)), " - ", unique(unlist(.$chmes_anneebiol)))), duree = "Relatif", typemesure = "Thermie", Ymin = -1, Ymax = 30, save = T, projet = projet, format = ".png")) # Fonctionne si plusieurs années
      # Y fixé avec vmm30j
      data %>%
        group_split(chmes_coderhj, chmes_anneebiol) %>%
        purrr::map_dfr(~ chronique.figure(data = ., Titre = as.character(paste0(unique(unlist(.$chmes_coderhj)), " - ", unique(unlist(.$chmes_anneebiol)))), duree = "Relatif", typemesure = "Thermie", Ymin = -1, Ymax = 30, Vmm30j = T, save = T, projet = projet, format = ".png")) # Fonctionne si plusieurs années
    }
    if(log != "Aucun") put("Fin de la sortie graphique des données incomplètes") # Log
  } # Fin de l'interrupteur volontaire d'export des vues de chroniques classiques
  
  ### Sortie graphique cumul degrés-jours ###
  if(exportfigures_cumul_degresjours == T){
    if (all(export & "chmes_typemesure" %in% colnames(data) & n_distinct(data$chmes_typemesure) == 1) == TRUE) {
      data %>%
        group_split(chmes_coderhj, chmes_typemesure) %>%
        purrr::map_dfr(~ chronique.figure.cumul(data = ., Titre = as.character(paste0(unique(.$chmes_coderhj))), typemesure = unique(.$chmes_typemesure), save = T, projet = projet, format = ".png"))
    } # Fin de Sortie graphique cumul degrés-jours
    if(log != "Aucun") put("Fin de la sortie graphique des cumul degrés-jours") # Log
  } # Fin de l'interrupteur volontaire d'export des cumuls de degrés-jours

  ### Sorties graphiques vue classes calendaires ###
  if(exportfigures_classes_calendaires == T){
    if (all(export & "chmes_typemesure" %in% colnames(data) & n_distinct(data$chmes_typemesure) == 1) == TRUE) {
      ## Sortie par station ##
      mesures_agregees_journalieres %>% 
        group_split(chmes_coderhj) %>% 
        purrr::map(~ chronique.figure.classescalendaires(., stations = listeStations, origine_donnees = contexte_stations$mo, projet = projet, save = T))
      ## Sortie par année ##
      mesures_agregees_journalieres %>% 
        group_split(chmes_anneebiol) %>% 
        purrr::map(~ chronique.figure.classescalendaires(., stations = listeStations, origine_donnees = contexte_stations$mo, projet = projet, save = T))
      ## Sortie par station par année ##
      mesures_agregees_journalieres %>% 
        group_split(chmes_coderhj, chmes_anneebiol) %>% 
        purrr::map(~ chronique.figure.classescalendaires(., stations = listeStations, origine_donnees = contexte_stations$mo, projet = projet, save = T))
    } # Fin de la condition de résultats suffisants
    if(log != "Aucun") put("Fin de la sortie graphique des vues de classes calendaires") # Log
  } # Fin de l'interrupteur volontaire d'export des vues de classes calendaires
  
  ### Sorties graphiques basées sur les résultats (filtrés ou non) ###
  if(filtrage == F){dataaconserver <- DataTravailSIG}
  if(filtrage == T){
    # Filtrage des données brutes à partir des résultats à conserver
    dataaconserver <- 
      DataTravailSIG %>% 
      chronique.resultats.filtrage(anneevmm = filtrageanneevmm, datefperiode = filtragedatefperiode, nbj = filtragenbj) %>% 
      chronique.cle(formatcle = "SAT")
    
    datafiltrees <-
      data %>% 
      chronique.cle(formatcle = "SAT") %>% 
      filter(Cle %in% dataaconserver$Cle) %>% 
      dplyr::select(-Cle)
    
    # Vérification de la quantité
    if(nrow(datafiltrees) != 0) data <- datafiltrees # Cas général
    if(nrow(datafiltrees) == 0) data <- data # Dans le cas où il ne reste aucune année suite au filtrage, du coup on n'en fait pas
    if(nrow(datafiltrees) == 0) warning("Filtrage pour les vues non réalisé car il n'y aurait plus aucune donnée")
  }
    
  ## Sortie graphique boxplot interannuel ##
  if(exportfigures_boxplot_interannuel == T){
    if (all(export & "chmes_typemesure" %in% colnames(data) & n_distinct(data$chmes_typemesure) == 1) == TRUE) {
      # Boxplot
      data %>%
        group_split(chmes_coderhj, chmes_typemesure) %>%
        purrr::map_dfr(~ chronique.figure.interannuelle(data = ., Titre = as.character(paste0(unique(.$chmes_coderhj))), typemesure = unique(.$chmes_typemesure), style = "boxplot", save = T, projet = projet, format = ".png"))
      # Violon
      data %>%
        group_split(chmes_coderhj, chmes_typemesure) %>%
        purrr::map_dfr(~ chronique.figure.interannuelle(data = ., Titre = as.character(paste0(unique(.$chmes_coderhj))), typemesure = unique(.$chmes_typemesure), style = "violon", save = T, projet = projet, format = ".png"))
    } # Fin de la condition de résultats suffisants
    if(log != "Aucun") put("Fin de la sortie graphique des boxplots interannuels") # Log
  } # Fin de l'interrupteur volontaire d'export des boxplots interannuels
  
  ## Sortie graphique profil longitudinal ##
  if(exportfigures_profil_longitudinal == T){
    if (all(export & "chmes_typemesure" %in% colnames(data) & n_distinct(data$chmes_typemesure) == 1) == TRUE) {
      if(DataTravailSIG %>% group_by(chsta_milieu) %>% st_drop_geometry() %>% distinct(chsta_coderhj) %>% group_by(chsta_milieu) %>% summarise(N = n()) %>% filter(N > 2) %>% nrow() != 0){ # Début de test s'il y a bien au moins un milieu à tester
        dataaconserver %>% # Le filtrage général est réalisé plus en amont avec ce qui remplit dataaconserver
          st_drop_geometry() %>% 
          filter(chsta_milieu %in% (DataTravailSIG %>% 
                                      group_by(chsta_milieu) %>% 
                                      st_drop_geometry() %>% 
                                      distinct(chsta_coderhj) %>% 
                                      group_by(chsta_milieu) %>% 
                                      summarise(N = n()) %>% 
                                      filter(N > 2) %>% 
                                      pull(chsta_milieu)
          )
          ) %>% # Ici on filtre pour ne conserver que les milieux pour lesquels il y a au moins trois stations
          group_split(chsta_milieu) %>%
          purrr::map_dfr(~ chronique.figure.longitudinale(data = ., save = T, projet = projet, format = ".png"))
      } # Fin de test s'il y a bien au moins un milieu à tester
    } # Fin de Sortie graphique profil longitudinal
    if(log != "Aucun") put("Fin de la sortie graphique des profils longitudinaux") # Log
  } # Fin de l'interrupteur volontaire d'export des profils longitudinaux
  
  ## Sortie graphique preferendums des espèces ##
  # if (all(export) == TRUE) {
  if(exportfigures_preferendums_especes == T){
    # Toutes espèces ou liste détaillé d'espèces #
    dataaconserver %>% # Le filtrage général est réalisé plus en amont avec ce qui remplit dataaconserver
      chronique.cle(formatcle = "SAT") %>% 
      filter(chsta_transmission == "Non") %>% # Temporaire = pour éliminer HER14-8 hydrologie suite à la jointure des résultats avec les stations, le temps qu'on travaille directement par ID
      mutate(chsta_sprep = ifelse(is.na(chsta_sprep), "Toutes espèces", chsta_sprep)) %>% 
      group_split(Cle) %>%
      purrr::map_dfr(~ chronique.figure.preferendums(staderecherche = "Adulte", tmm30j = .$VMaxMoy30J, liste_especes = .$chsta_sprep, titre = as.character(glue('{unique(.$chsta_coderhj)} - {unique(.$Annee)}')), save = T, projet = projet, format = ".png"))
    # } # Fin de Sortie preferendums thermiques des espèces
    if(log != "Aucun") put("Fin de la sortie graphique des preferendums des espèces") # Log
  } # Fin de l'interrupteur volontaire d'export des preferendums des espèces
    
} # Fin de exportfigures == T

#### Informations de session ####
  if(export == TRUE){
session <- devtools::session_info()

write(glue("Généré le {now()} avec le package Aquatools (https://github.com/jbfagotfede39/aquatools/). \n"), file = glue("./{projet}/log/session_info.txt"))
write(capture.output(session), file = glue("./{projet}/log/session_info.txt"), append = TRUE)
}
if(log != "Aucun") put("Fin de sortie des informations de session") # Log

#### Déplacement des données d'entrée ####
## Stations ##
if(export == TRUE & dep39 == "autre"){
file_move(fnameStations, glue('./{projet}/Entrées/Stations/'))
}

## Suivi de terrain ##
if(export == TRUE & dep39 == "autre" & exportDCE == T){
  file_move(fnameSuivi, glue('./{projet}/Entrées/Suivi/'))
}

## Commentaires #####
if(export == TRUE & dep39 == "autre"){
  file_move(fnameCommentaires, glue('./{projet}/Entrées/Commentaires/'))
}

## Capteurs #####
if(export == TRUE & dep39 == "autre" & exportDCE == T){
  file_move(fnameCapteurs, glue('./{projet}/Entrées/Capteurs/'))
}
  
## Données brutes ##
if(export == TRUE & dep39 == "autre"){
dir_ls(fnameDonnesbrutes) %>% 
  purrr::map_dfr(file_move(., glue('./{projet}/Entrées/Données/')))
}
if(log != "Aucun") put("Fin de déplacement des données d'entrée") # Log

#### Arrêt du log et fermeture du fichier ####
if(log != "Aucun"){
  log_close() # Close log file
  options("logr.on" = FALSE) # Turn logger off
  options("logr.autolog" = FALSE) # Turn autolog off
}

#### Zippage ####
if(export == TRUE & archivage != "Aucun"){
  zip::zipr(zipfile = glue("{projet}.zip"), files = glue("./{projet}"))
}
if(export == TRUE & archivage == "Complet"){
  fs::dir_delete(glue("./{projet}"))
}

#### Sortie ####

return(DataTravail)

} # Fin de la fonction
