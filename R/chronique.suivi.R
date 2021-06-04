#' Extraction suivi terrain chroniques
#'
#' Cette fonction permet d'extraire des données de suivi de terrain
#' 
#' @name chronique.suivi
#' @param x Variable dont on cherche le suivi (MO, opérateur, Station, Date, Capteur)
#' @param Recherche Type de donnée de suivi
#' @param Sortie Forme du dataframe de sortie - \code{Complet} (par défault - tous les champs), \code{Propre} ou \code{Simple}
#' @import lubridate
#' @import tidyverse
#' @export
#' @examples
#' chronique.suivi("CG39", Recherche = "MO")
#' chronique.suivi("JB-Stéphane", Recherche = "Opérateur")
#' chronique.suivi("DRO14-2", Recherche = "Station")
#' chronique.suivi("2015-10-23", Recherche = "Date")
#' chronique.suivi("9759803", Recherche = "Capteur", Sortie = "Propre")
#' SuiviTerrain <- Stations$chsta_coderhj %>% map_dfr(~ chronique.suivi(., Recherche = "Station"))

##### TODO LIST #####
# 
#####################

chronique.suivi <- function(
  x = NA_character_, 
  Recherche = c("MO", "Opérateur", "Station", "Date", "Capteur"),
  Sortie = c("Complet","Propre","Simple")
                          )
{

  #### Évaluation des choix ####
  Recherche <- match.arg(Recherche)
  Sortie <- match.arg(Sortie)
  
  #### Test ####
  if(is.na(x)) stop("Il faut compléter la variable à rechercher")
  
  #### Connexion à la BDD ####
  dbD <- BDD.ouverture("Data")

  #### Recherche sur x en tant que telle ####
  Vue <-
    tbl(dbD, in_schema("fd_production", "chroniques_suiviterrain")) %>% 
    {if(Recherche == "MO") filter(., chsvi_mo == x) else .} %>%  # Traitement du MO
    {if(Recherche == "Opérateur") filter(., chsvi_operateurs == x) else .} %>%  # Traitement d'un opérateur
    {if(Recherche == "Station") filter(., chsvi_coderhj == x) else .} %>%  # Traitement d'une station
    {if(Recherche == "Date") filter(., chsvi_date == ymd(x)) else .} %>%  # Traitement d'une date
    {if(Recherche == "Capteur") filter(., chsvi_capteur == x) else .} %>% # Traitement d'un capteur
    collect(n = Inf)
  
  #### Fermeture de la connexion ####
  dbDisconnect(dbD)
  
  #### Formatage ####
  Vue <-
    Vue %>% 
    {if(nrow(Vue) != 0) mutate(., chsvi_date = ymd(chsvi_date)) else .} %>% # Dans le cas où il n'y a pas de suivi dans la base, car sinon ça plante : Error in lapply(list(...), .num_to_date) : objet 'chsvi_date' introuvable
    {if(nrow(Vue) != 0) arrange(., desc(chsvi_date), desc(chsvi_heure)) else .} %>% 
    {if(Sortie == "Propre") select(., -contains("modif")) else .}
  
  if(nrow(Vue) == 0) warning("Attention il n'y a aucune ligne correspondante")
  
  #### Affichage des résultats ####
  return(Vue)
}