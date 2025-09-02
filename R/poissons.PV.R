#' Exportation des PV
#'
#' Cette fonction permet d'exporter les données de PV de gestion piscicole
#' @name poissons.PV
#' @param ecosysteme Code de l'écosystème. Si vide (par défaut), exporte l'intégralité du jeu de données
#' @keywords poissons
#' @import DBI
#' @import tidyverse 
#' @export
#' @examples
#' poissons.PV()
#' poissons.PV("ROU")

##### TODO LIST #####

#####################

#library(aquatools);library(dplyr);library(lubridate);ecosysteme="ROU"

poissons.PV <- function(
  ecosysteme = "")
{
  
  #### Importation des données ####
  ## Ouverture de la BDD ##
  dbD <- BDD.ouverture(Type = "Data")
  dbP <- BDD.ouverture(Type = "Poissons")
  
  ## Récupération des données ##
  Ecosystemes <- tbl(dbP,"ecosystemes") %>% collect(n = Inf)
  pv_lots <- tbl(dbP,"pv_lots") %>% collect(n = Inf)
  pv_pvs <- tbl(dbP,"pv_pvs") %>% collect(n = Inf)
  pv_promoteurs <- tbl(dbP,"pv_promoteurs") %>% collect(n = Inf)

  pecheloisir_sapl <- tbl(dbD, dbplyr::in_schema("fd_production", "pecheloisir_sapl")) %>% filter(plosapl_departement_insee == 39) %>% select(id, plosapl_sapl) %>% collect(n = Inf)

  ## Fermeture de la BDD ##
  DBI::dbDisconnect(dbD)
  DBI::dbDisconnect(dbP)
  
  #### Jointures ####
  ## Synthèse des données ##
  pv_lots <- left_join(pv_lots, Ecosystemes, by = c("codeecosysteme")) # Avec l'écosystème
  pv_lots <- left_join(pv_lots, pv_pvs, by = "numero_pv") # Pour rattacher le code du promoteur

  pv_lots <- pv_lots %>% left_join(pv_promoteurs %>% select(code_pv_promoteur, nom_pv_promoteur), by = c("code_pv_promoteur" = "code_pv_promoteur")) # Pour rattacher le nom du promoteur
  pv_lots <- pv_lots %>% left_join(pecheloisir_sapl, by = c("nom_pv_promoteur" = "plosapl_sapl"), keep = T) # Pour rattacher le nom et l'identifiant de référence de la SAPL de la table pecheloisir_sapl
  
  #### Cosmétique ####
  ## Format de dates ##
  pv_lots$date_pv <- ymd(pv_lots$date_pv)
  
  #### Filtrage ####
  ## Simplification ##
  # Travail sur un seul écosystème
  if(nchar(ecosysteme) != 0){
    pv_lots <- 
      pv_lots %>%
      filter(coderdt == ecosysteme) %>% 
      rename(Date = date_pv) %>% 
      arrange(desc(Date))}
  
  # Travail sur l'ensemble des écosystèmes
  if(nchar(ecosysteme) == 0){
    pv_lots <- 
      pv_lots %>%
      #filter(coderdt == ecosysteme) %>% 
      rename(Date = date_pv) %>% 
      arrange(desc(Date))}

  #### Sortie des résultats ####
  return(pv_lots)
  
} # Fin de la fonction