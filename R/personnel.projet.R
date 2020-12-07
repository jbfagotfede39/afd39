#' Extraction des données de temps de travail par projet
#'
#' Extrait au format excel les données plus ou moins détaillées des coûts du personnel par projet
#' @name personnel.projet
#' @keywords personnel
#' @import dbplyr
#' @import dplyr
#' @import glue
#' @import lubridate
#' @import openxlsx
#' @import tidyverse
#' @export
#' @examples
#' personnel.projet("Étude Valouse")

###### À faire #####
# Déplacer moe/client de tpswrecap vers tpswprj et ajouter moa (maître d'oeuvre vs maître d'ouvrage)
# 
####################

personnel.projet <- function(
  projet = NA_character_
  )
{
  
  ##### Récupération des données #####
  ## Connexion à la BDD ##
  dbD <- BDD.ouverture("Data")
  
  ## Récupération des données ##
  TpsW <- tbl(dbD, dbplyr::in_schema("fd_production", "tpstravail_detail")) %>% filter(tpswdetail_projet == projet) %>% collect(n = Inf)
  Projets <- tbl(dbD, dbplyr::in_schema("fd_production", "tpstravail_projets")) %>% filter(tpswprj_projet == projet) %>% collect(n = Inf)
  RecapTpsW <- tbl(dbD, dbplyr::in_schema("fd_production", "tpstravail_recapitulatif")) %>% filter(tpswrecap_projet %in% !! Projets$id) %>% collect(n = Inf)
  
  id_max <- as.numeric(tbl(dbD,in_schema("fd_production", "tpstravail_recapitulatif")) %>% summarise(max = max(id, na.rm = TRUE)) %>% collect())

  ##### Vérification de l'existence de ce projet #####
  if(dim(Projets)[1] == 0) stop("Projet non répertorié")
  if(dim(TpsW)[1] == 0) stop("Pas de données détaillées pour ce projet")
  if(dim(RecapTpsW)[1] == 0) stop("Pas de données récapitulatives (au moins projetées) pour ce projet")
  
  ##### Extration des coûts unitaires #####
  CoutPersonnel <- 
  RecapTpsW %>% 
    filter(tpswrecap_programmation == "Attendu") %>% 
    filter(!is.na(tpswrecap_poste)) %>% 
    filter(tpswrecap_poste != "") %>% 
    distinct(tpswrecap_poste, tpswrecap_coutunitaire)

  #### Calcul des données élaborées si absentes ####
if(dim(RecapTpsW %>% filter(tpswrecap_programmation == "Réalisé"))[1] == 0){
  if(!grepl("AERMC", projet)){ # Cas où ce n'est pas une convention avec l'AE
  DataToAdd <- 
    TpsW %>% 
    filter(tpswdetail_projet == projet) %>% 
    filter(!is.na(tpswdetail_temps)) %>% 
    group_by(tpswdetail_projet, tpswdetail_poste, tpswdetail_personnel, tpswdetail_detail) %>%
    summarise(tpswrecap_jours = sum(tpswdetail_temps)) %>% 
    ungroup() %>% 
    left_join(CoutPersonnel, by = c("tpswdetail_poste" = "tpswrecap_poste")) %>% 
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
  
  if(grepl("AERMC",projet)){ # Cas où c'est une convention de l'AE
    DataToAdd <- 
      TpsW %>% 
      filter(tpswdetail_projet == projet) %>% 
      filter(!is.na(tpswdetail_temps)) %>% 
      group_by(tpswdetail_projet, tpswdetail_actionaermc, tpswdetail_sousactionaermc, tpswdetail_poste, tpswdetail_personnel) %>%
      summarise(tpswrecap_jours = sum(tpswdetail_temps)) %>% 
      ungroup() %>% 
      left_join(CoutPersonnel, by = c("tpswdetail_poste" = "tpswrecap_poste")) %>% 
      mutate(tpswrecap_programmation = "Réalisé") %>% 
      mutate(tpswrecap_natureprojet = as.character(NA)) %>% 
      mutate(tpswrecap_moe = as.character(NA)) %>% 
      mutate(tpswrecap_client = as.character(NA)) %>% 
      mutate(tpswdetail_detail = as.character(NA)) %>% 
      mutate(tpswrecap_argent = tpswrecap_coutunitaire * tpswrecap_jours) %>% 
      mutate(tpswrecap_quantite = as.numeric(NA)) %>%  
      mutate(tpswrecap_quantitepersonnel = as.numeric(NA))
  }
  
colnames(DataToAdd) <- 
  DataToAdd %>% 
  colnames() %>% 
  str_replace("tpswdetail", "tpswrecap")

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
           contains("Attendu_Responsable adminis"), contains("Attendu_Responsable te"), contains("Attendu_Ingénieur hy"), contains("Attendu_Charg"), contains("Attendu_Technicien quali"),
           contains("Réalisé_Responsable adminis"), contains("Réalisé_Responsable te"), contains("Réalisé_Ingénieur hy"), contains("Réalisé_Charg"), contains("Réalisé_Technicien quali")
           )

  colnames(Recapitulatif) <- c("Thème", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses")

  ## Création d'un classeur
  tempsprojet <- createWorkbook()
  ## Ajout d'une feuille
  addWorksheet(tempsprojet, sheetName = glue('Récapitulatif_AC_{str_extract(Projets$tpswprj_projet, "[[:digit:]]+$")}'))
  ## Ajout des données
  writeData(tempsprojet, 1, Recapitulatif, startCol = 1, startRow = 3, colNames = T) # writing content on the left-most column to be merged
  ## Ajout de cellules fusionnées
  mergeCells(tempsprojet, 1, cols = 2:11, rows = 1)
  mergeCells(tempsprojet, 1, cols = 12:21, rows = 1)
  writeData(tempsprojet, 1, "Attendu", startCol = 2, startRow = 1)
  writeData(tempsprojet, 1, "Réalisé", startCol = 12, startRow = 1)
  mergeCells(tempsprojet, 1, cols = 2:3, rows = 2)
  mergeCells(tempsprojet, 1, cols = 4:5, rows = 2)
  mergeCells(tempsprojet, 1, cols = 6:7, rows = 2)
  mergeCells(tempsprojet, 1, cols = 8:9, rows = 2)
  mergeCells(tempsprojet, 1, cols = 10:11, rows = 2)
  mergeCells(tempsprojet, 1, cols = 12:13, rows = 2)
  mergeCells(tempsprojet, 1, cols = 14:15, rows = 2)
  mergeCells(tempsprojet, 1, cols = 16:17, rows = 2)
  mergeCells(tempsprojet, 1, cols = 18:19, rows = 2)
  mergeCells(tempsprojet, 1, cols = 20:21, rows = 2)
  writeData(tempsprojet, 1, "Resp. admin. et fin.", startCol = 2, startRow = 2)
  writeData(tempsprojet, 1, "Resp. admin. et fin.", startCol = 12, startRow = 2)
  writeData(tempsprojet, 1, "Resp. techn.	", startCol = 4, startRow = 2)
  writeData(tempsprojet, 1, "Resp. techn.	", startCol = 14, startRow = 2)
  writeData(tempsprojet, 1, "Ing. hydr.", startCol = 6, startRow = 2)
  writeData(tempsprojet, 1, "Ing. hydr.", startCol = 16, startRow = 2)
  writeData(tempsprojet, 1, "Chargés de dével.", startCol = 8, startRow = 2)
  writeData(tempsprojet, 1, "Chargés de dével.", startCol = 18, startRow = 2)
  writeData(tempsprojet, 1, "Tech. PDPG", startCol = 10, startRow = 2)
  writeData(tempsprojet, 1, "Tech. PDPG", startCol = 20, startRow = 2)
  ## Centrage des cellules fusionnées
  centerStyle <- createStyle(halign = "center")
  addStyle(tempsprojet, 1, centerStyle, rows = 1:2, cols = 1:22, gridExpand = TRUE)
  ## Enregistrement du classeur
  saveWorkbook(tempsprojet, glue('{today()}_{projet}_récapitulatif_coût_personnel.xlsx'), overwrite = T) # save workbook
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
}
  
} # Fin de la fonction
