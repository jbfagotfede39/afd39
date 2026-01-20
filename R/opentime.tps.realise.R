#' Ouverture données OpenTime "détails temps passé"
#' 
#' Ouvre les données "détails temps passé" OpenTime et les reformate
#' @name opentime.tps.realise
#' @keywords personnel
#' @param fichier Fichiers \code{.xlsx} issu de OpenTime à traiter (\code{détails temps passé})
#' @import readxl
#' @import tidyverse
#' @export
#' @examples
#' "monfichier.xlsx" %>% opentime.tps.realise() %>% BDD.format(Type = "Temps de travail") %>% opentime.projet()

opentime.tps.realise <- function(
    fichier = NA_character_
)
{
  #### Nettoyage & reformatage ####
  data_v2 <- 
    fichier %>% 
    read_excel() %>% 
    select(-`statut domaine`, -`financeur(s)...16`) %>% 
    select(-`jour de la semaine`) %>% 
    select(-`financeur(s)...17`) %>% 
    rename(tpswot_domaine = domaine) %>% 
    rename(tpswot_activite = activité) %>% 
    rename(metier = métier) %>% 
    rename(duree_h = `temps (h)`) %>% 
    rename(montant = `coût horaire (production)`)
  
  #### Sortie ####
  return(data_v2)
  
} # Fin de la fonction
