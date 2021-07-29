#' Extraction des coûts de projet
#'
#' Cette fonction permet d'extraire les coûts d'un projet sous forme excel et png au moment de son montage. Elle s'appuie sur le format saisi dans la table tpstravail_recapitulatif et réalise les conversions à partir des volumes unitaires, et ordonne les sujets
#' @name projet.extraction
#' @param projet_id Id du projet concerné, à partir de la table fd_production.tpstravail_projets
#' @importFrom dplyr select
#' @import tidyverse
#' @export
#' @examples
#' projet.extraction(96)
#' Projets %>% group_split(id) %>% purrr::map(~ projet.extraction(.$id))

projet.extraction <- function(
  projet_id
)
{
  
  ## Connexion ##
  dbD <- BDD.ouverture("Data")
  
  ## Récupération des données ##
  Projets <- tbl(dbD, dbplyr::in_schema("fd_production", "tpstravail_projets")) %>% filter(id == projet_id) %>% collect()
  Actions <- tbl(dbD, dbplyr::in_schema("fd_production", "tpstravail_recapitulatif")) %>% filter(tpswrecap_projet == projet_id) %>% filter(tpswrecap_programmation == "Attendu") %>% collect()
  tab_projet <- tbl(dbD, dbplyr::in_schema("fd_production", "general_observations_operations")) %>% select(-id, -contains('_modif')) %>% collect()
  DBI::dbDisconnect(dbD)
  
  ### Nettoyage des données utiles
  NomProjet <- Projets %>% filter(id == projet_id) %>% select(tpswprj_projet) %>% pull()
  
  ProjetFormate <-
    Actions %>%
    projet.calculInitial()
  
  ## Coût global
  temporaire <-
    ProjetFormate %>%
    summarise(tpswrecap_natureprojet = "Total", tpswrecap_argent = sum(tpswrecap_argent))
  RecapBudget <- ProjetFormate %>% bind_rows(temporaire)
  
  ## Mise en forme du tableau
  RecapBudget <- 
    RecapBudget %>%
    select(tpswrecap_natureprojet, tpswrecap_detail, tpswrecap_argent) %>%
    mutate(Champbis = ifelse(lag(tpswrecap_natureprojet) == tpswrecap_natureprojet, NA, tpswrecap_natureprojet)) %>%
    mutate(Champbis = ifelse(row_number() == 1, tpswrecap_natureprojet, Champbis)) %>%
    mutate(tpswrecap_natureprojet = Champbis) %>%
    select(-Champbis) %>%
    group_by(tpswrecap_natureprojet) %>%
    arrange(tpswrecap_detail) %>%
    mutate(tpswrecap_argent = glue(tpswrecap_argent, " €")) %>%
    rename("Détail" = "tpswrecap_detail") %>%
    rename("Total (TTC)" = "tpswrecap_argent") %>%
    rename("Phase du projet" = "tpswrecap_natureprojet") %>%
    replace(is.na(.), "") # Pour remplacer les 0 par des NA
  
  ### Création du classeur excel ###
  recapitulatifexcel <- createWorkbook()
  
  addWorksheet(recapitulatifexcel, sheetName = glue('Chiffrage approximatif'))
  writeData(recapitulatifexcel, "Chiffrage approximatif", RecapBudget, startCol = 1, startRow = 1, colNames = T) # writing content on the left-most column to be merged
  
  ### Mise en forme ###
  ## Largeurs de cellules ##
  setColWidths(recapitulatifexcel, "Chiffrage approximatif", cols = 1:ncol(RecapBudget), widths = c(20, 35, 10))
  
  EurosStyle <- createStyle(numFmt="0,00 €")
  addStyle(recapitulatifexcel, 1, EurosStyle, rows = (nrow(RecapBudget)-1):(nrow(RecapBudget)+1), cols = 1:ncol(RecapBudget), gridExpand = TRUE)
  
  openxlsx::saveWorkbook(recapitulatifexcel, file = glue('{today()}_Chiffrage approximatif_{NomProjet}_VF.xlsx'), overwrite = T)
  
  ### Création du tableau png ###
  matriceGrasItalique <- matrix(c("plain", "plain", "plain"), ncol = ncol(RecapBudget), nrow = nrow(RecapBudget), byrow = TRUE)
  matriceGrasItalique[nrow(RecapBudget),] <- "bold.italic"
  
  tt1 <- 
    ttheme_minimal(
      core = list(
        bg_params = list(fill = blues9[1:2], col=NA),
        fg_params = list(fontface = matriceGrasItalique)
      ),
      colhead = list(fg_params=list(col="navyblue", fontface=4L)),
      rowhead = list(fg_params=list(col="white", fontface=3L))
    )
  png(glue("{NomProjet}_Chiffrage.png"), height = 50*nrow(RecapBudget), width = 150*ncol(RecapBudget))
  grid.table(RecapBudget, theme = tt1, rows = NULL)
  dev.off()

} # Fin de la fonction
