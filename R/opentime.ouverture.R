#' Ouverture des fichiers excels de données détaillées
#'
#' Ouvre les données excel à partir des données fines téléchargées depuis OpenTime et les reformate
#' @name opentime.ouverture
#' @keywords personnel
#' @param fichier
#' @import readxl
#' @import tidyverse
#' @export
#' @examples
#' "monfichier.xlsx" %>% opentime.ouverture %>% BDD.format(Type = "Temps de travail") %>% opentime.projet()

opentime.ouverture <- function(
    fichier = NA_character_
)
{
  #### Nettoyage & reformatage ####
  data_v2 <- 
    fichier %>% 
    read_excel() %>% 
    select(-`statut domaine`, -`financeur(s)...16`) %>% 
    select(-`jour de la semaine`) %>% 
    select(-`financeur(s)...17`)
  
  #### Sortie ####
  return(data_v2)
  
} # Fin de la fonction
