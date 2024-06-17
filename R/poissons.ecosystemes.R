#' Extraction de données des écosystèmes
#'
#' Cette fonction permet d'extraire les données complètes de l'ensemble des écosystèmes (par défaut) ou d'un seul
#' @name poissons.ecosystemes
#' @param poi_ecosysteme Nom de l'écosystème
#' @keywords poissons
#' @import DBI
#' @import tidyverse
#' @export
#' @examples
#' listeCE <- poissons.ecosystemes()
#' listeCE <- poissons.ecosystemes("Valouse")

poissons.ecosystemes <- function(
  poi_ecosysteme = NA_character_)
{
  
  #### Nettoyage & reformatage ####
  if(is.na(nchar(poi_ecosysteme))) poi_ecosysteme <- NA_character_
  
  #### Collecte des données ####
  ## Ouverture de la BDD ##
  dbP <- BDD.ouverture(Type = "Poissons")

  ## Récupération des données ##
  if(is.na(poi_ecosysteme)) ecosysteme <- tbl(dbP,"ecosystemes") %>% collect(n = Inf)
  if(!is.na(poi_ecosysteme)) ecosysteme <- tbl(dbP,"ecosystemes") %>% filter(nomecosysteme == poi_ecosysteme) %>% collect(n = Inf)
  
  ## Fermeture de la BDD ##
  DBI::dbDisconnect(dbP)
  
  #### Test ####
  # Test si le nom existe bien, sinon message d'avertissement #
  if(!is.na(poi_ecosysteme)) {
    if(nrow(ecosysteme) == 0) warning(glue("Attention : écosystème ('{poi_ecosysteme}') absent de la base de données"))
  }
  
  ## Sortie des résultats ##
  return(ecosysteme)
  
} # Fin de la fonction