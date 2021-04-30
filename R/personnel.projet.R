#' Extraction des données de temps de travail par projet
#'
#' Extrait au format excel les données plus ou moins détaillées des coûts du personnel par projet
#' @name personnel.projet
#' @keywords personnel
#' @param projet Intitulé du projet que l'on souhaite calculer
#' @param projection Si \code{TRUE} (par défaut), va chercher les données projetées.
#' @param anciennebase Si \code{TRUE}, va chercher les données détaillées également dans l'ancienne base en plus d'Opentime
#' @param export Si \code{TRUE} (par défaut), exporte les résultats
#' @import glue
#' @import openxlsx
#' @import RPostgreSQL
#' @import stringr
#' @import tidyverse
#' @export
#' @examples
#' personnel.projet("Étude Valouse")

###### À faire #####
# Déplacer moe/client de tpswrecap vers tpswprj et ajouter moa (maître d'oeuvre vs maître d'ouvrage)
# 
####################

personnel.projet <- function(
  projet = NA_character_,
  projection = TRUE,
  anciennebase = FALSE,
  export = TRUE
)
{
  
  ##### Récupération des données #####
  ## Connexion à la BDD ##
  dbD <- BDD.ouverture("Data")
  
  ## Récupération des données ##
  if(anciennebase == T){TpsW <- tbl(dbD, dbplyr::in_schema("fd_production", "tpstravail_detail")) %>% filter(tpswdetail_projet == projet) %>% collect(n = Inf)}
  TpsWOT <- tbl(dbD, dbplyr::in_schema("fd_production", "tpstravail_opentime")) %>% collect(n = Inf)
  Projets <- tbl(dbD, dbplyr::in_schema("fd_production", "tpstravail_projets")) %>% filter(tpswprj_projet == projet) %>% collect(n = Inf)
  RecapTpsW <- tbl(dbD, dbplyr::in_schema("fd_production", "tpstravail_recapitulatif")) %>% filter(tpswrecap_projet %in% !! Projets$id) %>% collect(n = Inf)
  CoutsAnnuels <- tbl(dbD, dbplyr::in_schema("fd_referentiels", "gestion_coutsannuels")) %>% filter(gestctan_projet_id %in% !! Projets$id) %>% collect(n = Inf)
  Personnels <- tbl(dbD, dbplyr::in_schema("fd_referentiels", "gestion_operateurs")) %>% filter(gestop_mo == 3 & gestop_type == "Salarié") %>% select(id:gestop_qualite) %>% collect(n = Inf)
  
  id_max <- as.numeric(tbl(dbD,in_schema("fd_production", "tpstravail_recapitulatif")) %>% summarise(max = max(id, na.rm = TRUE)) %>% collect())
  
  ##### Vérification de l'existence de ce projet #####
  if(dim(Projets)[1] == 0) stop("Projet non répertorié")
  if(anciennebase == T & dim(TpsW)[1] == 0) stop("Pas de données détaillées ancienne base pour ce projet")
  if(projection == T & dim(RecapTpsW)[1] == 0) stop("Pas de données récapitulatives (au moins projetées) pour ce projet")
  
  #### Calcul des données élaborées si absentes ####
  nRecapTpsW <- 0
  if(nrow(RecapTpsW) != 0) nRecapTpsW <- nrow(RecapTpsW %>% filter(tpswrecap_programmation == "Réalisé"))
  if(nRecapTpsW <= 5){ # On en conserve 5 dans le cas de la présence de réalisé sur le matériel/prestation
    
    ### Extraction des coûts unitaires ###
    if(projection == T){
    CoutPersonnelAttendu <-
      RecapTpsW %>%
      filter(tpswrecap_programmation == "Attendu") %>%
      filter(!is.na(tpswrecap_poste)) %>%
      filter(tpswrecap_poste != "") %>%
      distinct(tpswrecap_poste, tpswrecap_personnel, tpswrecap_coutunitaire)
    
    if(nrow(CoutsAnnuels) != 0){
    CoutPersonnelReel <- 
      CoutsAnnuels %>% 
      filter(gestctan_type == "Réalisé N") %>% 
      filter(!is.na(gestctan_poste_id)) %>% 
      filter(gestctan_poste_id != "") %>% 
      left_join(Personnels %>% mutate(tpswrecap_personnel = glue("{gestop_prenom} {gestop_nom}")) %>% rename(tpswrecap_poste = gestop_qualite) %>% select(id, tpswrecap_personnel, tpswrecap_poste), by = c("gestctan_operateur_id" = "id")) %>% 
      rename(tpswrecap_coutunitaire = gestctan_coutjournaliermajore) %>% 
      distinct(tpswrecap_poste, tpswrecap_personnel, tpswrecap_coutunitaire) %>% 
      select(tpswrecap_poste, tpswrecap_personnel, tpswrecap_coutunitaire)
    }
    
    if((nrow(CoutsAnnuels) != 0) & (nrow(CoutPersonnelReel) != 0)) CoutPersonnel <- CoutPersonnelReel
    if(nrow(CoutPersonnel) == 0) CoutPersonnel <- CoutPersonnelAttendu
    
    if(nrow(CoutPersonnelReel) == 0) stop("Pas de coûts unitaires réalisés <-> N+1")
    } # Fin de if(projection = T)
    
    ### Recherche et éventuel regroupement de données détaillées
    if(!grepl("AERMC", projet)){ # Cas où ce n'est pas une convention avec l'AE
      TpsWOTfiltre <- 
        TpsWOT %>% 
        mutate(tpswot_activite = ifelse(grepl("APD Bonlieu", tpswot_activite), "APD Bonlieu 2018", tpswot_activite)) %>% 
        mutate(tpswot_activite = ifelse(grepl("Suivi qualité temps réel", tpswot_activite) & tpswot_date <= ymd("2021-04-15"), "Suivi thermique Vouglans - Volet 2018-2019", tpswot_activite)) %>%
        filter(tpswot_activite %in% projet)
      
      if(dim(TpsWOTfiltre)[1] == 0) stop(glue('Pas de données détaillées OpenTime au niveau tpswot_activite pour le projet {projet}')) # Sans doute nécessaire d'ajouter ultérieurement un sélecteur pour affiner la recherche dans ce scénario
      
      TpsW <-
        TpsWOTfiltre %>% 
        rename_at(vars(contains("tpswot_")), list( ~ str_replace(., "tpswot_", "tpswdetail_"))) %>%
        left_join(Personnels %>% mutate(tpswdetail_personnelcomplet = glue("{gestop_prenom} {gestop_nom}")), by = c("tpswdetail_personnel" = "id")) %>% 
        rename(tpswdetail_poste = gestop_qualite) %>% 
        select(-contains("gestop")) %>% 
        mutate(tpswdetail_personnel = tpswdetail_personnelcomplet) %>% 
        select(-tpswdetail_personnelcomplet) %>% 
        mutate(tpswdetail_poste = ifelse(tpswdetail_poste == "Responsable du Pôle Technique & Développement", "Ingénieur responsable du pôle technique", tpswdetail_poste)) %>% 
        rename(tpswdetail_projet = tpswdetail_activite) %>% 
        # rename(tpswdetail_detail = tpswdetail_remarques) %>% # Ancienne version avant 2021-04-14
        mutate(tpswdetail_detail = glue("{tpswdetail_sous_projet} - {tpswdetail_remarques}")) %>% # Nouvelle version avant 2021-04-14
        mutate(tpswdetail_detail = str_replace(tpswdetail_detail, " - NA", "")) %>% # Nouvelle version avant 2021-04-14
        mutate(tpswdetail_detail = str_replace(tpswdetail_detail, "NA - ", "")) %>% # Nouvelle version avant 2021-04-14
        mutate(tpswdetail_temps = tpswdetail_duree/7) %>% 
        rename(tpswdetail_id = id) %>% 
        mutate(tpswdetail_statut = NA_character_) %>% 
        mutate(tpswdetail_ecosysteme = NA_character_) %>% 
        mutate(tpswdetail_massedeau = NA_character_) %>% 
        mutate(tpswdetail_projet = projet) %>% 
        mutate(tpswdetail_actionaermc = NA_character_) %>% 
        mutate(tpswdetail_sousactionaermc = NA_character_) %>% 
        mutate(tpswdetail_priorite = NA_character_) %>% 
        mutate(tpswdetail_natureprojet = NA_character_) %>% 
        mutate(tpswdetail_duree = NA) %>% 
        mutate(tpswdetail_priorite = NA) %>% 
        mutate(tpswdetail_completude = NA_character_) %>% 
        mutate(tpswdetail_remarques = NA_character_) %>% 
        mutate(tpswdetail_modif_utilisateur = NA_character_) %>% 
        mutate(tpswdetail_modif_type = NA_character_) %>% 
        mutate(tpswdetail_modif_date = NA) %>% 
        select(tpswdetail_id, tpswdetail_date, tpswdetail_personnel, tpswdetail_poste, tpswdetail_statut, tpswdetail_ecosysteme, tpswdetail_massedeau, tpswdetail_projet, tpswdetail_detail, tpswdetail_actionaermc, tpswdetail_sousactionaermc, tpswdetail_natureprojet, tpswdetail_temps, tpswdetail_duree, tpswdetail_priorite, tpswdetail_completude, tpswdetail_remarques, contains("tpswdetail_modif_")) %>% 
        {if(anciennebase == T) union(., TpsW) else .}
      
    if(projection == T) warning("Attention aux éventuels doublons de CoutPersonnel pour Chargé de développement ")
      
      ### Calcul à proprement parler
  DataToAdd <- 
    TpsW %>% 
    filter(tpswdetail_projet == projet) %>% 
    filter(!is.na(tpswdetail_temps)) %>% 
    group_by(tpswdetail_projet, tpswdetail_poste, tpswdetail_personnel, tpswdetail_detail) %>%
    summarise(tpswrecap_jours = sum(tpswdetail_temps, na.rm = T)) %>% 
    mutate(tpswrecap_jours = round(tpswrecap_jours / 0.5) * 0.5) %>% # Arrondi strict à la demi-journée
    ungroup() %>% 
    {if(projection == T) left_join(., CoutPersonnel %>% select(-tpswrecap_poste), by = c("tpswdetail_personnel" = "tpswrecap_personnel")) else .} %>% 
    {if(projection == F) mutate(., tpswrecap_coutunitaire = NA_real_) else .} %>% 
    mutate(tpswrecap_programmation = "Réalisé") %>% 
    mutate(tpswrecap_natureprojet = as.character(NA)) %>% 
    mutate(tpswrecap_moe = as.character(NA)) %>% 
    mutate(tpswrecap_client = as.character(NA)) %>% 
    mutate(tpswrecap_actionaermc = as.character(NA)) %>%  
    mutate(tpswrecap_sousactionaermc = as.character(NA)) %>%  
    mutate(tpswrecap_argent = tpswrecap_coutunitaire * tpswrecap_jours) %>% 
    mutate(tpswrecap_quantite = as.numeric(NA)) %>%  
    mutate(tpswrecap_quantitepersonnel = as.numeric(NA))
  }
  
    ### Ancienne méthode jusqu'à 2019 sans OpenTime
#   if(grepl("AERMC",projet)){ # Cas où c'est une convention de l'AE
#     DataToAdd <- 
#       TpsW %>% 
#       filter(tpswdetail_projet == projet) %>% 
#       filter(!is.na(tpswdetail_temps)) %>% 
#       group_by(tpswdetail_projet, tpswdetail_actionaermc, tpswdetail_sousactionaermc, tpswdetail_poste, tpswdetail_personnel) %>%
#       summarise(tpswrecap_jours = sum(tpswdetail_temps, na.rm = T)) %>% 
#       ungroup() %>% 
#       # left_join(CoutPersonnel, by = c("tpswdetail_poste" = "tpswrecap_poste")) %>% # Version historique
#       left_join(CoutPersonnel %>% select(-tpswrecap_poste), by = c("tpswdetail_personnel" = "tpswrecap_personnel")) %>% # Version utilisée pour correction soldes AC AE 2017 et 2018
#       mutate(tpswrecap_programmation = "Réalisé") %>%
#       mutate(tpswrecap_natureprojet = as.character(NA)) %>% 
#       mutate(tpswrecap_moe = as.character(NA)) %>% 
#       mutate(tpswrecap_client = as.character(NA)) %>% 
#       mutate(tpswdetail_detail = as.character(NA)) %>% 
#       mutate(tpswrecap_argent = tpswrecap_coutunitaire * tpswrecap_jours) %>% 
#       mutate(tpswrecap_quantite = as.numeric(NA)) %>%  
#       mutate(tpswrecap_quantitepersonnel = as.numeric(NA))
#   }
  
  ### Nouvelle méthode à partir de 2020 avec OpenTime
  
  if(grepl("AERMC",projet)){ # Cas où c'est une convention de l'AE
    DataToAdd <- 
      TpsWOT %>% 
      personnel.formatAC(., projet) %>% 
      mutate(tpswdetail_ecosysteme = NA_character_) %>% 
      mutate(tpswdetail_massedeau = NA_character_) %>% 
      mutate(tpswdetail_projet = projet) %>% 
      mutate(tpswdetail_temps = NA_character_) %>% 
      select(tpswot_date, tpswdetail_personnel, tpswdetail_poste, tpswdetail_statut, tpswdetail_ecosysteme, tpswdetail_massedeau, tpswdetail_projet, tpswdetail_detail, tpswdetail_actionaermc, tpswdetail_sousactionaermc, tpswdetail_temps, tpswot_duree) %>% # Remise en ordre
      rename_all(~sub("tpswot", 'tpswdetail', .x)) %>%
      group_by(tpswdetail_projet, tpswdetail_actionaermc, tpswdetail_sousactionaermc, tpswdetail_poste, tpswdetail_personnel) %>%
      summarise(tpswrecap_jours = sum(tpswdetail_duree, na.rm = T)/7) %>% 
      mutate(tpswrecap_jours = round(tpswrecap_jours / 0.5) * 0.5) %>% # Arrondi strict à la demi-journée
      ungroup() %>% 
      left_join(CoutPersonnel %>% select(-tpswrecap_poste), by = c("tpswdetail_personnel" = "tpswrecap_personnel")) %>% 
      mutate(tpswrecap_programmation = "Réalisé") %>% 
      # mutate(tpswrecap_natureprojet = as.character(NA)) %>% 
      mutate(tpswrecap_natureprojet = "Convention") %>% 
      mutate(tpswrecap_moe = as.character(NA)) %>% 
      mutate(tpswrecap_client = as.character(NA)) %>% 
      mutate(tpswdetail_detail = as.character(NA)) %>% 
      mutate(tpswrecap_argent = tpswrecap_coutunitaire * tpswrecap_jours) %>% 
      mutate(tpswrecap_quantite = as.numeric(NA)) %>%  
      mutate(tpswrecap_quantitepersonnel = as.numeric(NA))
      
      bug <- DataToAdd %>% filter(is.na(tpswdetail_sousactionaermc))
      if(nrow(bug) != 0) stop(glue('Il y a des actions/sous-actions AERMC non reconnues : {bug %>% distinct(tpswot_activite)}'))
  }
  
  colnames(DataToAdd) <- 
    DataToAdd %>% 
    colnames() %>% 
    str_replace("tpswdetail", "tpswrecap")

  if(nrow(RecapTpsW) == 0){
    RecapTpsW <- structure(list(id = integer(0), tpswrecap_programmation = character(0), 
                   tpswrecap_natureprojet = character(0), tpswrecap_moe = character(0), 
                   tpswrecap_client = character(0), tpswrecap_actionaermc = character(0), 
                   tpswrecap_sousactionaermc = character(0), tpswrecap_projet = integer(0), 
                   tpswrecap_detail = character(0), tpswrecap_poste = character(0), 
                   tpswrecap_personnel = character(0), tpswrecap_argent = numeric(0), 
                   tpswrecap_jours = numeric(0), tpswrecap_coutunitaire = numeric(0), 
                   tpswrecap_quantite = integer(0), tpswrecap_quantitepersonnel = numeric(0), 
                   `_modif_utilisateur` = character(0), `_modif_type` = character(0), 
                   `_modif_date` = structure(numeric(0), tzone = "", class = c("POSIXct", 
                                                                               "POSIXt"))), row.names = integer(0), class = c("tbl_df", 
                                                                                                                              "tbl", "data.frame"))
  }
  
DataToAdd <-
  DataToAdd %>% 
  left_join(Projets %>% ungroup() %>% select(id, tpswprj_projet, tpswprj_natureprojet), by = c("tpswrecap_projet" = "tpswprj_projet")) %>% # 
  mutate(tpswrecap_natureprojet = tpswprj_natureprojet) %>% 
  mutate(tpswrecap_projet = id) %>% 
  mutate(id = row_number() + id_max) %>%  # Pour incrémenter les id à partir du dernier
  mutate('_modif_utilisateur' = NA) %>% 
  mutate('_modif_date' = NA) %>% 
  mutate('_modif_type' = NA) %>% 
  select(match(names(RecapTpsW),colnames(.)))

# Écriture des données #
RPostgreSQL::dbWriteTable(conn = dbD,
                          name = c("fd_production","tpstravail_recapitulatif"),
                          value = DataToAdd,
                          #overwrite=F,
                          append=T,
                          row.names=FALSE)

# Mise à jour de la séquence des id #
dbGetQuery(dbD, "
SELECT setval('fd_production.tpstravail_recapitulatif_id_seq', COALESCE((SELECT MAX(id)+1 FROM fd_production.tpstravail_recapitulatif), 1), false);
           ")
}

#### Extraction des données élaborées ####
RecapTpsW <- 
    tbl(dbD, dbplyr::in_schema("fd_production", "tpstravail_recapitulatif")) %>% 
    filter(tpswrecap_projet %in% !! Projets$id) %>% 
    {if(!grepl("AERMC", Projets$tpswprj_projet)) filter(., tpswrecap_programmation == "Réalisé") else .} %>%
    collect(n = Inf)

#### Extraction accord-cadre AERMC ####
if(grepl("AERMC",projet)){
  Recapitulatif <- 
    RecapTpsW %>% 
    filter(tpswrecap_projet %in% !! Projets$id) %>% 
    mutate(tpswrecap_argent = round(tpswrecap_argent, 2))
  
  Temps <-
    Recapitulatif %>% 
    filter(!is.na(tpswrecap_jours)) %>% # pour supprimer les valeurs d'achat sans journées associées
    group_by(tpswrecap_programmation, tpswrecap_sousactionaermc, tpswrecap_poste) %>%
    summarise(tpswrecap_jours = sum(tpswrecap_jours)) %>% 
    pivot_wider(
      names_from = c(tpswrecap_programmation, tpswrecap_poste), 
      values_from = tpswrecap_jours
    ) %>% 
    select(tpswrecap_sousactionaermc, sort(tidyselect::peek_vars())) %>% # Pour trier les colonnes par ordre alphabétique, ce qui était fait par reshape2 mais pas par pivot_wider
    rename_all(~ paste0(., "_temps")) %>%
    rename(tpswrecap_sousactionaermc = tpswrecap_sousactionaermc_temps)

  Argent <-
    Recapitulatif %>% 
    mutate(tpswrecap_poste = ifelse(is.na(tpswrecap_poste), "Matériel", tpswrecap_poste)) %>% 
    group_by(tpswrecap_programmation, tpswrecap_sousactionaermc, tpswrecap_poste) %>%
    summarise(tpswrecap_argent = sum(tpswrecap_argent)) %>% 
    pivot_wider(
      names_from = c(tpswrecap_programmation, tpswrecap_poste), 
      values_from = tpswrecap_argent
    ) %>% 
    select(tpswrecap_sousactionaermc, sort(tidyselect::peek_vars())) %>% # Pour trier les colonnes par ordre alphabétique, ce qui était fait par reshape2 mais pas par pivot_wider
    rename_all(~ paste0(., "_argent")) %>%
    rename(tpswrecap_sousactionaermc = tpswrecap_sousactionaermc_argent)
  
  Recapitulatif <- 
    Temps %>% 
    left_join(Argent, by="tpswrecap_sousactionaermc") %>% 
    select(tpswrecap_sousactionaermc, 
           contains("Attendu_Responsable adminis"), contains("Attendu_Ingénieur resp"), contains("Attendu_Ingénieur hy"), contains("Attendu_Charg"), contains("Attendu_Technicien quali"), contains("Attendu_Mat"),
           contains("Réalisé_Responsable adminis"), contains("Réalisé_Ingénieur resp"), contains("Réalisé_Ingénieur hy"), contains("Réalisé_Charg"), contains("Réalisé_Technicien quali"), contains("Réalisé_Mat")
    )

  colnames(Recapitulatif) <- c("Thème", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Dépenses", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Dépenses")
  
  if(export == T){
  ## Création d'un classeur
  tempsprojet <- createWorkbook()
  ## Ajout d'une feuille
  addWorksheet(tempsprojet, sheetName = glue('Récapitulatif_AC_{str_extract(Projets$tpswprj_projet, "[[:digit:]]+$")}'))
  ## Ajout des données
  writeData(tempsprojet, 1, Recapitulatif, startCol = 1, startRow = 3, colNames = T) # writing content on the left-most column to be merged
  ## Ajout de cellules fusionnées
  mergeCells(tempsprojet, 1, cols = 2:12, rows = 1)
  mergeCells(tempsprojet, 1, cols = 13:23, rows = 1)
  writeData(tempsprojet, 1, "Attendu", startCol = 2, startRow = 1)
  writeData(tempsprojet, 1, "Réalisé", startCol = 13, startRow = 1)
  mergeCells(tempsprojet, 1, cols = 2:3, rows = 2)
  mergeCells(tempsprojet, 1, cols = 4:5, rows = 2)
  mergeCells(tempsprojet, 1, cols = 6:7, rows = 2)
  mergeCells(tempsprojet, 1, cols = 8:9, rows = 2)
  mergeCells(tempsprojet, 1, cols = 10:11, rows = 2)
  mergeCells(tempsprojet, 1, cols = 13:14, rows = 2)
  mergeCells(tempsprojet, 1, cols = 15:16, rows = 2)
  mergeCells(tempsprojet, 1, cols = 17:18, rows = 2)
  mergeCells(tempsprojet, 1, cols = 19:20, rows = 2)
  mergeCells(tempsprojet, 1, cols = 21:22, rows = 2)
  writeData(tempsprojet, 1, "Resp. admin. et fin.", startCol = 2, startRow = 2)
  writeData(tempsprojet, 1, "Mat.", startCol = 12, startRow = 2)
  writeData(tempsprojet, 1, "Resp. admin. et fin.", startCol = 13, startRow = 2)
  writeData(tempsprojet, 1, "Resp. techn.	", startCol = 4, startRow = 2)
  writeData(tempsprojet, 1, "Resp. techn.	", startCol = 15, startRow = 2)
  writeData(tempsprojet, 1, "Ing. hydr.", startCol = 6, startRow = 2)
  writeData(tempsprojet, 1, "Ing. hydr.", startCol = 17, startRow = 2)
  writeData(tempsprojet, 1, "Chargés de dével.", startCol = 8, startRow = 2)
  writeData(tempsprojet, 1, "Chargés de dével.", startCol = 19, startRow = 2)
  writeData(tempsprojet, 1, "Tech. PDPG", startCol = 10, startRow = 2)
  writeData(tempsprojet, 1, "Tech. PDPG", startCol = 21, startRow = 2)
  writeData(tempsprojet, 1, "Mat.", startCol = 23, startRow = 2)
  ## Centrage des cellules fusionnées
  centerStyle <- createStyle(halign = "center")
  addStyle(tempsprojet, 1, centerStyle, rows = 1:2, cols = 1:23, gridExpand = TRUE)
  ## Enregistrement du classeur
  saveWorkbook(tempsprojet, glue('{today()}_{projet}_récapitulatif_coût_personnel.xlsx'), overwrite = T) 
  } # fin de export = T
  if(export == F) return(Recapitulatif)
}

##### Extraction autre dossier que accord-cadre AERMC ####
if(!grepl("AERMC",projet)){
### Extraction des données de synthèse par poste ###
SynthesePoste <- 
  RecapTpsW %>% 
  filter(tpswrecap_projet %in% !! Projets$id) %>% 
  filter(!is.na(tpswrecap_jours)) %>% 
  group_by(tpswrecap_projet, tpswrecap_detail,tpswrecap_personnel) %>% 
  summarise(Jours = sum(tpswrecap_jours)
  ) %>% 
  arrange(tpswrecap_detail,tpswrecap_personnel) %>% 
  rename(Projet = tpswrecap_projet) %>% 
  rename(Detail = tpswrecap_detail) %>% 
  rename(Personnel = tpswrecap_personnel)
  
SynthesePoste <- as.data.frame(SynthesePoste)
  
### Extraction des données de synthèse par personnel ###
SynthesePersonnel <- 
  RecapTpsW %>% 
  filter(tpswrecap_projet %in% !! Projets$id) %>% 
  filter(!is.na(tpswrecap_jours)) %>% 
  group_by(tpswrecap_projet, tpswrecap_personnel) %>%
  summarise(Journées = sum(tpswrecap_jours)) %>% 
  rename(Projet = tpswrecap_projet) %>% 
  rename(Personnel = tpswrecap_personnel)
  
SynthesePersonnel <- as.data.frame(SynthesePersonnel)
  
### Extraction des données détaillées par personnel ###
  Detail <- 
    TpsW %>% 
    filter(tpswdetail_projet == projet) %>% 
    select(tpswdetail_personnel, tpswdetail_date, tpswdetail_poste, tpswdetail_statut, tpswdetail_projet, tpswdetail_detail, tpswdetail_ecosysteme, tpswdetail_temps) %>% 
    arrange(tpswdetail_personnel, tpswdetail_date) %>% 
    filter(!is.na(tpswdetail_temps)) %>% 
    select(-tpswdetail_detail) %>% 
    rename(Personnel = tpswdetail_personnel) %>% 
    rename(Date = tpswdetail_date) %>% 
    rename(Poste = tpswdetail_poste) %>% 
    rename(Statut = tpswdetail_statut) %>% 
    rename(Temps = tpswdetail_temps) %>% 
    rename(Projet = tpswdetail_projet) %>% 
    select(-tpswdetail_ecosysteme)
  
  Detail$Date <- as.character(Detail$Date)
  Detail <- as.data.frame(Detail)
  
  ### Extraction des données théoriques/réalisées ###
  Comparaison <- 
    tbl(dbD, dbplyr::in_schema("fd_production", "tpstravail_recapitulatif")) %>% 
    filter(tpswrecap_projet %in% !! Projets$id) %>% 
    collect() %>% 
    group_by(tpswrecap_programmation, tpswrecap_poste, tpswrecap_detail) %>% 
    summarise(tpswrecap_jours = sum(tpswrecap_jours)) %>% 
    pivot_wider(
      names_from = c(tpswrecap_programmation), 
      values_from = tpswrecap_jours,
      values_fn = mean
    )

  if(export == T){
  ### Écriture du fichier excel ###
  tempsprojet <- createWorkbook() # Création d'un classeur
  ## Ajout des feuilles
  addWorksheet(tempsprojet, sheetName = "SynthèsePersonnel")
  addWorksheet(tempsprojet, sheetName = "SynthèsePoste")
  addWorksheet(tempsprojet, sheetName = "Détail")
  addWorksheet(tempsprojet, sheetName = "Comparaison")
  ## Ajout des données
  writeData(tempsprojet, 1, SynthesePersonnel, startCol = 1, startRow = 1, colNames = T) # writing content on the left-most column to be merged
  writeData(tempsprojet, 2, SynthesePoste, startCol = 1, startRow = 1, colNames = T) # writing content on the left-most column to be merged
  writeData(tempsprojet, 3, Detail, startCol = 1, startRow = 1, colNames = T) # writing content on the left-most column to be merged
  writeData(tempsprojet, 4, Comparaison, startCol = 1, startRow = 1, colNames = T) # writing content on the left-most column to be merged
  ## Écriture du classeur
  saveWorkbook(tempsprojet, glue('{today()}_{projet}_récapitulatif_coût_personnel.xlsx'), overwrite = T)
  } # fin de export = T
  
  if(export == F){return(SynthesePersonnel)}
  
} # Fin de if(!grepl("AERMC",projet))
  
} # Fin de la fonction
