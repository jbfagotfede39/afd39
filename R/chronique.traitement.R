#' Exécuter le traitement de données de chroniques
#'
#' Permet d'exécuter plusieurs opérations de traitement des données de chronique d'une station
#' @name chronique.traitement
#' @keywords chronique
#' @param data Data.frame issu de chronique.mesures, pouvant contenir différentes stations
#' @param export Si \code{TRUE} (par défault), exporte les résultats/figures. Si \code{FALSE}, ne les exporte pas.
#' @param exportfigures Si \code{TRUE} (par défaut), exporte les figures. Si \code{FALSE}, ne les exporte pas.
#' @param exportDCE Si \code{TRUE}, exporte les mesures au format DCE. Si \code{FALSE} (par défault), ne les exporte pas.
#' @param filtrage Si \code{TRUE}, filtre les résultats avec chronique.resultats.filtrage pour ne conserver que les pertinents pour les représentations graphiques inter-annuelles/inter-sites. \code{FALSE} (par défault)
#' @param filtrageanneevmm Si \code{TRUE} (par défault), ne conserve que les résultats dont l'année de fin de vmm30j est égale à l'année biologique du résultat
#' @param filtragedatefperiode Ne conserver que les résultats dont la date de fin de période est postérieure à la date choisie ("07-15", soit le 15 juillet par défault)
#' @param filtragenbj Nombre de journées minimales pour chaque résultat à conserver (75 par défault).
#' @param typemesure Défini le type de données et modifie le traitement en fonction
#' @param projet Nom du projet
#' @param dep39 Si \code{FALSE} (par défault), ne va pas rechercher les données de stations dans la base locale et donc export simplifié. Si \code{TRUE}, fait la jointure SIG. Possibilité d'utiliser \code{autre} afin de sélectionner un fichier source de stations
#' @param archivage Si \code{Aucun} (par défault), ne va pas créer une archives .zip du répertoire de sortie. Si \code{Partiel}, créé une archive et conserve le répertoire. Si \code{Complet}, créé une archive et supprimer le répertoire.
#' @import fs
#' @import glue
#' @import openxlsx
#' @import purrr
#' @import sf
#' @import stringr
#' @import tcltk
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
  projet = as.character(NA),
  export = T,
  exportfigures = T,
  exportDCE = F,
  filtrage = F,
  anneevmm = TRUE,
  datefperiode = "07-15",
  nbj = 75,
  typemesure = c("Thermie", "Thermie barométrique", "Thermie piézométrique", "Barométrie", "Piézométrie", "Piézométrie brute", "Piézométrie compensée", "Piézométrie calée", "Piézométrie NGF", "Oxygénation", "Hydrologie", "Pluviométrie"),
  dep39 = c(FALSE, TRUE, "autre"),
  archivage = c("Aucun","Partiel","Complet"),
  style = c("boxplot","violon") 
  )
{

##### -------------- A FAIRE -------------- #####
# Il serait intéressant d'ajouter dans le fichier session_info.txt les dates/heures de début et de fin ainsi que le temps de calcul (replacer l'export du fichier à la fin, en stockant l'heure de démarrage)
# Export fichier readme qui explique le fonctionnement + auteur + date + noms/versions (session_info) des packages 
# Essayer de supprimer le warning qui apparaît suite à # Analyse des données # (In bind_rows_(x, .id) : Unequal factor levels: coercing to character)
# Faire un outil de regroupement des données brutes au format large à partir de la fin du code de 2020-02-13_Export_suivi_FUR_format_DCE.R  
# -------------- A FAIRE -------------- #  

#### Évaluation des choix ####
  typemesure <- match.arg(typemesure)
  archivage <- match.arg(archivage)
  style <- match.arg(style)
  if(exportDCE == TRUE) export <- T
  if(export == FALSE) exportDCE <- F
  if(export == FALSE) exportfigures <- F
  if(export == FALSE) archivage <- "Aucun"
  
#### Modification du projet avec la date #####
projet <- glue('{format(now(), format="%Y-%m-%d")}_{projet}')
  
#### Vérification des répertoires ####
if(export == TRUE){
  if(file.exists(paste0("./",projet)) == FALSE){
    dir.create(paste0("./",projet), showWarnings = FALSE, recursive = FALSE)
  }
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
  dir.create(paste0("./",projet, "/Sorties/Vues/Annuelles_absolu-fixé/"), showWarnings = FALSE, recursive = FALSE)
  dir.create(paste0("./",projet, "/Sorties/Vues/Annuelles_absolu-libre/"), showWarnings = FALSE, recursive = FALSE)
  dir.create(paste0("./",projet, "/Sorties/Vues/Annuelles_relatif-fixé/"), showWarnings = FALSE, recursive = FALSE)
  dir.create(paste0("./",projet, "/Sorties/Vues/Annuelles_relatif-libre/"), showWarnings = FALSE, recursive = FALSE)
}

if(file.exists(paste0("./",projet, "/Sorties/")) == TRUE & file.exists(paste0("./",projet, "/Sorties/Données/")) == FALSE){
    dir.create(paste0("./",projet, "/Sorties/Données/"), showWarnings = FALSE, recursive = FALSE)
    dir.create(paste0("./",projet, "/Sorties/Données/DCE/"), showWarnings = FALSE, recursive = FALSE)
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
  dir.create(paste0("./",projet, "/Sorties/Vues/Annuelles_absolu-fixé/"), showWarnings = FALSE, recursive = FALSE)
  dir.create(paste0("./",projet, "/Sorties/Vues/Annuelles_absolu-libre/"), showWarnings = FALSE, recursive = FALSE)
  dir.create(paste0("./",projet, "/Sorties/Vues/Annuelles_relatif-fixé/"), showWarnings = FALSE, recursive = FALSE)
  dir.create(paste0("./",projet, "/Sorties/Vues/Annuelles_relatif-libre/"), showWarnings = FALSE, recursive = FALSE)
}
}
  
if(export == TRUE & dep39 != TRUE & file.exists(paste0("./",projet, "/Entrées/")) == FALSE){
  dir.create(paste0("./",projet, "/Entrées/"), showWarnings = FALSE, recursive = FALSE)
  dir.create(paste0("./",projet, "/Entrées/Données/"), showWarnings = FALSE, recursive = FALSE)
  dir.create(paste0("./",projet, "/Entrées/Stations/"), showWarnings = FALSE, recursive = FALSE)
  dir.create(paste0("./",projet, "/Entrées/Commentaires/"), showWarnings = FALSE, recursive = FALSE)
  dir.create(paste0("./",projet, "/Entrées/Suivi/"), showWarnings = FALSE, recursive = FALSE)
}

#### Préparation des données ####
data <-
  data %>% 
  formatage.annee.biologique() # Calcul de l'année biologique

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

#### Analyse des données ####
DataTravail <- 
  data %>%
  group_split(chmes_coderhj, chmes_anneebiol, chmes_typemesure) %>% 
    purrr::map_dfr(~ chronique.analyse(.)) %>% 
    ungroup()
# ça pourra crasher par ici lorsqu'on fera un essai mixant chmes_typemesure == "Thermie" avec un autre chmes_typemesure à cause de la jointure à réaliser et un nb de champ différent (absence de TRF$DateDebutDegresJours et la suite)

DataTravail <- 
  DataTravail %>% 
    group_by(Coderhj, Typemesure) %>% 
    summarise(min = min(AnneeVMM),
              max = max(AnneeVMM)) %>% 
    mutate(PeriodeTotale = paste0(min, " - ", max)) %>% 
    select(-min,-max) %>% 
    left_join(DataTravail, by = c("Coderhj", "Typemesure")) %>% 
  ungroup()

##### Importation des données nécessaires aux exportations #####
listeStations <- data %>% distinct(chmes_coderhj)
listeCapteurs <- data %>% distinct(chmes_capteur)

if(export == TRUE){
dbD <- BDD.ouverture("Data")
HER <- sf::st_read(dbD, query = "SELECT * FROM fd_referentiels.hydrographie_hydroecoregions;")
DBI::dbDisconnect(dbD)
}

if(export == TRUE & dep39 == TRUE){
  dbD <- BDD.ouverture("Data")
  listeStations <- sf::st_read(dbD, query = "SELECT * FROM fd_production.chroniques_stations;") %>% filter(chsta_coderhj %in% listeStations$chmes_coderhj) %>% collect() %>% select(chsta_coderhj:chsta_departement, chsta_coord_x:chsta_coord_type, chsta_fonctionnement:chsta_reseauthermietype, chsta_altitude, chsta_distancesource, chsta_typetheorique)
  communes <- sf::st_read(dbD, query = "SELECT * FROM fd_referentiels.topographie_communes WHERE (tpcomm_departement_insee = '39' OR tpcomm_departement_insee = '25' OR tpcomm_departement_insee = '01');")
  contextesPDPG <- sf::st_read(dbD, query = "SELECT * FROM fd_referentiels.hydrographie_contextespdpg;")
  Commentaires <- tbl(dbD, in_schema("fd_production", "chroniques_commentaires")) %>% collect(n = Inf)
  SuiviTerrain <- listeStations$chsta_coderhj %>% map_dfr(~ chronique.suivi(., Recherche = "Station", Sortie = "Propre"))
  SuiviCapteurs <- listeCapteurs$chmes_capteur %>% map_dfr(~ chronique.capteurs(., Recherche = "Numéro", Sortie = "Propre"))
  DBI::dbDisconnect(dbD)
}

if(export == TRUE & dep39 == "autre"){
  fnameStations <- tk_choose.files(caption = "Fichier de stations")
  Stations <- chronique.ouverture("Stations", "Thermie", fnameStations)
  
  fnameCommentaires <- tk_choose.files(caption = "Fichier de commentaires")
  Commentaires <- chronique.ouverture("Commentaires", "Thermie", fnameCommentaires)
  
  if(exportDCE == TRUE){
  fnameSuivi <- tk_choose.files(caption = "Fichier de suivi de terrain")
  SuiviTerrain <- chronique.ouverture("Suivis", "Thermie", fnameSuivi)
  }
  
  fnameDonnesbrutes <- tk_choose.dir(caption = "Répertoire des chroniques") # On interroge tout de suite l'opérateur pour ne pas le déranger ensuite seulement pour le répertoire des données brutes
  
}

##### Sortie stations #####
if(export == TRUE & dep39 == "autre"){
  ## Préparation format SIG ##
  listeStations <- Stations %>% filter(chsta_coderhj %in% listeStations$chmes_coderhj) %>% select(chsta_coderhj:chsta_codecontextepdpg, chsta_coord_x:chsta_coord_type, chsta_fonctionnement:chsta_reseauthermietype, chsta_altitude, chsta_distancesource, chsta_typetheorique)
  listeStations <-
    listeStations %>% 
    rowwise() %>% 
    mutate(tpcomm_commune_libelle = aquatools::BV.ComByCoordL93(chsta_coord_x,chsta_coord_y) %>% select(name) %>% as.character()) %>% 
    mutate(chsta_departement = aquatools::BV.ComByCoordL93(chsta_coord_x,chsta_coord_y) %>% select(codeDepartement) %>% as.character()) %>% 
    ungroup() %>% 
    sf::st_as_sf(coords = c("chsta_coord_x","chsta_coord_y")) %>% 
    st_set_crs(2154)
}

## Export en SIG ##
if(export == TRUE & dep39 != FALSE){
  SIG.export(listeStations, paste0("./",projet, "/Sorties/Stations/", format(now(), format="%Y-%m-%d"), "_Stations"))
}

#### Sortie suivi de terrain #####
if(export == TRUE & dep39 == TRUE){
  SuiviTerrain %>% 
    openxlsx::write.xlsx(paste0("./",projet, "/Sorties/", format(now(), format="%Y-%m-%d"), "_suivi_terrain.xlsx"), sheetName = "SuiviTerrain", row.names = F, showNA = F, colWidths="auto")
}

#### Sortie données capteurs #####
if(export == TRUE & dep39 == TRUE){
  SuiviCapteurs %>% 
    openxlsx::write.xlsx(paste0("./",projet, "/Sorties/", format(now(), format="%Y-%m-%d"), "_historique_capteurs.xlsx"), sheetName = "Capteurs", row.names = F, showNA = F, colWidths="auto")
  }

##### Sortie résultats élaborés #####
if(export == TRUE & dep39 == TRUE){
DataTravailSIG <- listeStations %>% left_join(DataTravail %>% mutate(intervalMax = as.numeric(sub(",", ".", IntervalleMax))), by = c("chsta_coderhj" = "Coderhj"))
DataTravailSIG <- DataTravailSIG %>% left_join(communes %>% st_drop_geometry() %>% select(tpcomm_commune_insee, tpcomm_commune_libelle), by = c('chsta_commune' = "tpcomm_commune_insee"))
DataTravailSIG <- DataTravailSIG %>% st_join(HER) %>% select(-id)
DataTravailSIG <- DataTravailSIG %>% st_join(contextesPDPG %>% select(hycont_contexte_code))
DataTravailSIG <- DataTravailSIG %>% left_join(Commentaires %>% select(chres_coderhj, chres_typemesure, chres_anneebiol, chres_commentaire), by = c('chsta_coderhj' = "chres_coderhj", "AnneeVMM" = "chres_anneebiol", "Typemesure" = "chres_typemesure"))
SIG.export(DataTravailSIG, paste0("./",projet, "/Sorties/Résultats/", format(now(), format="%Y-%m-%d"), "_Résultats"), shp = F)
SIG.export(DataTravailSIG, paste0("./",projet, "/Sorties/Résultats/", "Atlas_Résultats"), shp = F, excel = F, kml = F)
}

if(export == TRUE & dep39 == "autre"){
  DataTravailSIG <- listeStations %>% left_join(DataTravail %>% mutate(intervalMax = as.numeric(sub(",", ".", IntervalleMax))), by = c("chsta_coderhj" = "Coderhj"))
  DataTravailSIG <- DataTravailSIG %>% st_join(HER) %>% select(-id)
  DataTravailSIG <- DataTravailSIG %>% 
  {if("chsta_codecontextepdpg" %in% names(DataTravailSIG)) mutate(., hycont_contexte_code = chsta_codecontextepdpg) %>% select(., -chsta_codecontextepdpg) else .} %>%
  {if(!("chsta_codecontextepdpg" %in% names(DataTravailSIG))) mutate(., hycont_contexte_code = NA_character_) else .}
  DataTravailSIG <- DataTravailSIG %>% left_join(Commentaires %>% select(chres_coderhj, chres_typemesure, chres_anneebiol, chres_commentaire), by = c('chsta_coderhj' = "chres_coderhj", "AnneeVMM" = "chres_anneebiol", "Typemesure" = "chres_typemesure"))
  SIG.export(DataTravailSIG, paste0("./",projet, "/Sorties/Résultats/", format(now(), format="%Y-%m-%d"), "_Résultats"), shp = F)
  SIG.export(DataTravailSIG, paste0("./",projet, "/Sorties/Résultats/", "Atlas_Résultats"), shp = F, excel = F, kml = F)
}

if(export == TRUE & dep39 == FALSE){
  DataTravail %>% 
    openxlsx::write.xlsx(paste0("./",projet, "/Sorties/Résultats/", format(now(), format="%Y-%m-%d"), "_Résultats.xlsx"), sheetName = "Feuille1", row.names = F, showNA = F, colWidths="auto")
}

##### Sorties format DCE #####
if(exportDCE == TRUE){
if(export == TRUE & dep39 == TRUE){
  data %>%
    group_split(chmes_coderhj) %>% # Permet d'éclater le dataframe en x dataframe, x étant le nb de modalités de chmes_coderhj
    purrr::map(~ chronique.DCE(data = ., projet = projet, export = T, dep39 = T))
}

if(export == TRUE & dep39 == "autre"){
  data %>%
    group_split(chmes_coderhj) %>% # Permet d'éclater le dataframe en x dataframe, x étant le nb de modalités de chmes_coderhj
    purrr::map(~ chronique.DCE(data = ., projet = projet, export = T, dep39 = "autre", fichierStations = fnameStations, fichierSuivis = fnameSuivi))
}

if(export == TRUE & dep39 == FALSE){
  data %>%
    group_split(chmes_coderhj) %>% # Permet d'éclater le dataframe en x dataframe, x étant le nb de modalités de chmes_coderhj
    purrr::map(~ chronique.DCE(data = ., projet = projet, export = T, dep39 = F, fichierStations = fnameStations, fichierSuivis = fnameSuivi))
}
}

##### Sorties agrégées ##### 
if(export == TRUE){
  data %>%
    group_split(chmes_coderhj) %>% # Permet d'éclater le dataframe en x dataframe, x étant le nb de modalités de chmes_coderhj
    #purrr::map_dfr(~ chronique.agregation(data = ., projet = projet, export = T)) # Fonctionne mais génère un message d'erreur : qqsoit le nb de chmes_coderhj : l'erreur intervient toujours après que le dernier fichier ait été généré (qqsoit le fichier) et arrête donc la fonction chronique.traitement
    purrr::map(~ chronique.agregation(data = ., projet = projet, export = T))
}

if(export == FALSE){
  # Soit adapter le code du export = TRUE pour que les sorties directes fonctionnent, 
  # # Exemple d'essai 
  # data %>% 
  #   group_by(chmes_coderhj) %>% 
  #   nest() %>% 
  #   purrr::map(chronique.agregation(data = ., export = F))
  # soit faire une agrégation batarde comme celle dans le rapport de Vogna 2018 avec des union, mais moins propre
}


##### Sorties graphiques #####
### Sortie graphique chronique complète ###
## Type de mesures spécifié ##
if (exportfigures == TRUE) {
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
  if (export == T) {
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
  }

  ### Sortie graphique boxplot interannuel ###
  if(filtrage == T){
    # Filtrage des données brutes
    dataaconserver <- 
      DataTravail %>% 
      chronique.resultats.filtrage(anneevmm = filtrageanneevmm, datefperiode = filtragedatefperiode, nbj = filtragenbj) %>% 
      chronique.cle(formatcle = "SAT") 
    
    datafiltrees <-
      data %>% 
      chronique.cle(formatcle = "SAT") %>% 
      filter(Cle %in% dataaconserver$Cle) %>% 
      select(-Cle)
    
    # Vérification de la quantité
    if(nrow(datafiltrees) != 0) data <- datafiltrees # Cas général
    if(nrow(datafiltrees) == 0) data <- data # Dans le cas où il ne reste aucune année suite au filtrage, du coup on n'en fait pas
    if(nrow(datafiltrees) == 0) warning("Filtrage non réalisé car il n'y aurait plus aucune donnée")
  }
    
  if (all(export & "chmes_typemesure" %in% colnames(data) & n_distinct(data$chmes_typemesure) == 1) == TRUE) {
    # Boxplot
    data %>%
      group_split(chmes_coderhj, chmes_typemesure) %>%
      purrr::map_dfr(~ chronique.figure.interannuelle(data = ., Titre = as.character(paste0(unique(.$chmes_coderhj))), typemesure = unique(.$chmes_typemesure), style = "boxplot", save = T, projet = projet, format = ".png"))
    # Violon
    data %>%
      group_split(chmes_coderhj, chmes_typemesure) %>%
      purrr::map_dfr(~ chronique.figure.interannuelle(data = ., Titre = as.character(paste0(unique(.$chmes_coderhj))), typemesure = unique(.$chmes_typemesure), style = "violon", save = T, projet = projet, format = ".png"))
  }
}
  
#### Notices ####
if(export == TRUE){
  formatage.abreviation(thematique = "Chronique", formatage = "Propre", export = T) # Export dans le répertoire courant
  file_move("./Glossaire.xlsx", glue("./{projet}/")) # Déplacement à la racine 
}
  
#### Informations de session ####
  if(export == TRUE){
Session <- devtools::session_info()

write(paste0("Généré le ", now(), " avec le package Aquatools (https://github.com/jbfagotfede39/aquatools/). \n"),file=paste0("./",projet, "/session_info.txt"))
write(capture.output(Session),file=paste0("./",projet, "/session_info.txt"),append=TRUE)
}
  
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
  
## Données brutes ##
if(export == TRUE & dep39 == "autre"){
dir_ls(fnameDonnesbrutes) %>% 
  purrr::map_dfr(file_move(., glue('./{projet}/Entrées/Données/')))
}
  
##### Zippage #####
if(export == TRUE & archivage != "Aucun"){
  zip::zipr(zipfile = paste0(projet,".zip"), files = paste0("./",projet))
}
if(export == TRUE & archivage == "Complet"){
  fs::dir_delete(paste0("./",projet))
}

#### Sortie ####

return(DataTravail)

} # Fin de la fonction
