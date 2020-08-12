#' Export des mandats
#'
#' Cette fonction permet d'exporter les mandats des SAPL
#' @name associatif.mandats
#' @keywords formatage
#' @param mandat Mandat recherché : Tous (par défault), Président, Trésorier, Garde particulier, Délégué, Secrétaire, Vice-président, Administrateur
#' @param sapl SAPL recherchée : Toutes (par défault), ou nom officiel de la SAPL
#' @param formatage Type de formatage de la sortie : \code{Complet} (par défault), \code{Propre}
#' @param export Si \code{TRUE} (par défault), exporte les mandats au format excel. Si \code{FALSE}, ne l'exporte pas.
#' @import DBI
#' @import glue
#' @import tidyverse
#' @export
#' @examples
#' associatif.mandats()
#' associatif.mandats("Président", "Toutes")
#' associatif.mandats("Président", "La Truite de l'Orbe")


##### TODO LIST #####
# Essayer de gommer le warning à cause du fct_relevel pour les cas où un mandat n'existe pas dans le jeu de données concerné
# 
#####################

associatif.mandats <- function(
  mandat = c("Tous", "Président", "Trésorier", "Garde particulier", "Secrétaire", "Vice-président", "Administrateur", "Délégué"),
  sapl = "Toutes",
  formatage = c("Complet", "Propre"),
  export = T
){

  #### Évaluation des choix ####
  mandat <- match.arg(mandat)
  formatage <- match.arg(formatage)
  
  #### Importation préalable ####
  dbD <- BDD.ouverture("Data")
  listesapl <- dbGetQuery(dbD, "SELECT DISTINCT plosapl_sapl FROM fd_production.pecheloisir_sapl WHERE plosapl_departement_insee = 39 ;") %>% as_tibble()

  #### Test de complétude ####
  if(length(sapl) == 0){stop("Pas de sapl en entrée")}
  if(sapl != "Toutes" & !(sapl %in% listesapl$plosapl_sapl)){stop("Sapl non reconnue")}
  
  #### Importation des données nécessaires ####
datapropre <-
  tbl(dbD, in_schema("fd_referentiels", "vue_mandatsbenevsapl")) %>%
  {if (sapl != "Toutes") filter(., plosapl_sapl == sapl) else .} %>%
  {if (sapl == "Fédération du Jura pour la Pêche et la Protection du Milieu Aquatique") filter(., plosapl_sapl == sapl & plomandat_mandat != "Adhérent") else .} %>% # pour supprimer les délégués élus de la liste FD et n'avoir que les administrateurs, on pourra retrouver l'ensemble des délégués dans le fichier délégués
  {if (mandat != "Tous" & mandat != "Délégué") filter(., plomandat_mandat == mandat) else .} %>%
  {if (mandat != "Tous" & mandat == "Délégué") filter(., !is.na(plomandat_delegue)) else .} %>%
  collect() %>% 
    mutate(plomandat_mandat = as_factor(plomandat_mandat))

  DBI::dbDisconnect(dbD)
  
  #### Formatage ####
datapropre$plomandat_mandat <- fct_relevel(datapropre$plomandat_mandat, "Président", "Trésorier", "Vice-président", "Secrétaire", "Administrateur", "Garde particulier", "Adhérent")

datapropre <-
  datapropre %>% # 
  #{if (mandat == "Délégué") arrange(., plosapl_sapl, plobenev_nom, plobenev_prenom) else .} %>%
  arrange(plosapl_nom_usage, plomandat_mandat, plobenev_nom, plobenev_prenom)

if (formatage == "Propre") {
  warning("Fonctionnalité de sortie propre non développée")
}

  #### Retour du tableau complet ####
  if(export == F){return(datapropre)}
  if(export == T){
    if(file.exists("./Mandats_SAPL/") == FALSE){
      dir.create("./Mandats_SAPL/", showWarnings = FALSE, recursive = FALSE)
    }
    if(file.exists("./Mandats_SAPL/Tous_mandats/") == FALSE & mandat == "Tous" & sapl != "Toutes"){
      dir.create("./Mandats_SAPL/Tous_mandats/", showWarnings = FALSE, recursive = FALSE)
    }
    if(file.exists("./Mandats_SAPL/Toutes_SAPL/") == FALSE & mandat != "Tous" & sapl == "Toutes"){
      dir.create("./Mandats_SAPL/Toutes_SAPL/", showWarnings = FALSE, recursive = FALSE)
    }
    if(file.exists("./Mandats_SAPL/Uniques/") == FALSE & mandat != "Tous" & sapl != "Toutes"){
      dir.create("./Mandats_SAPL/Uniques/", showWarnings = FALSE, recursive = FALSE)
    }
    datapropre %>% 
      # {if (mandat == "Tous" & sapl == "Toutes") openxlsx::write.xlsx(., "Tous_mandats_toutes_SAPL.xlsx", sheetName = "Mandats", row.names = F, showNA = F, colWidths="auto") else .} %>% 
      # {if (mandat == "Tous" & sapl != "Toutes") openxlsx::write.xlsx(., glue("Tous_mandats_{sapl}.xlsx"), sheetName = "Mandats", row.names = F, showNA = F, colWidths="auto") else .} %>% 
      # {if (mandat != "Tous" & sapl == "Toutes") openxlsx::write.xlsx(., glue("{mandat}_toutes_SAPL.xlsx"), sheetName = "Mandats", row.names = F, showNA = F, colWidths="auto") else .} %>% 
      # {if (mandat != "Tous" & sapl != "Toutes") openxlsx::write.xlsx(., glue("{mandat}_{sapl}.xlsx"), sheetName = "Mandats", row.names = F, showNA = F, colWidths="auto") else .}
      # {if (mandat == "Tous" & sapl == "Toutes") openxlsx::write.xlsx(., glue("./Mandats_SAPL/{mandat}/{sapl}/Tous_mandats_toutes_SAPL.xlsx"), sheetName = "Mandats", row.names = F, showNA = F, colWidths="auto") else .} %>% 
      # {if (mandat == "Tous" & sapl != "Toutes") openxlsx::write.xlsx(., glue("./Mandats_SAPL/{mandat}/{sapl}/Tous_mandats_{sapl}.xlsx"), sheetName = "Mandats", row.names = F, showNA = F, colWidths="auto") else .} %>% 
      # {if (mandat != "Tous" & sapl == "Toutes") openxlsx::write.xlsx(., glue("./Mandats_SAPL/{mandat}/{sapl}/{mandat}_toutes_SAPL.xlsx"), sheetName = "Mandats", row.names = F, showNA = F, colWidths="auto") else .} %>% 
      # {if (mandat != "Tous" & sapl != "Toutes") openxlsx::write.xlsx(., glue("./Mandats_SAPL/{mandat}/{sapl}/{mandat}_{sapl}.xlsx"), sheetName = "Mandats", row.names = F, showNA = F, colWidths="auto") else .}
      {if (mandat == "Tous" & sapl == "Toutes") openxlsx::write.xlsx(., glue("./Mandats_SAPL/Tous_mandats_toutes_SAPL.xlsx"), sheetName = "Mandats", row.names = F, showNA = F, colWidths="auto") else .} %>% 
      {if (mandat == "Tous" & sapl != "Toutes") openxlsx::write.xlsx(., glue("./Mandats_SAPL/Tous_mandats/Tous_mandats_{sapl}.xlsx"), sheetName = "Mandats", row.names = F, showNA = F, colWidths="auto") else .} %>% 
      {if (mandat != "Tous" & sapl == "Toutes") openxlsx::write.xlsx(., glue("./Mandats_SAPL/Toutes_SAPL/{mandat}_toutes_SAPL.xlsx"), sheetName = "Mandats", row.names = F, showNA = F, colWidths="auto") else .} %>% 
      {if (mandat != "Tous" & sapl != "Toutes") openxlsx::write.xlsx(., glue("./Mandats_SAPL/Uniques/{mandat}_{sapl}.xlsx"), sheetName = "Mandats", row.names = F, showNA = F, colWidths="auto") else .}
  }
  
} # Fin de la fonction
