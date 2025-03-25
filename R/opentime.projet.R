#' Extraction des identifiants de projet issus des données OpenTime téléchargées
#'
#' Ajoute une colonne `projet_id` à partir des données fines téléchargées depuis OpenTime
#' @name opentime.projet
#' @keywords personnel
#' @import glue
#' @import openxlsx
#' @import stringr
#' @import tidyverse
#' @export
#' @examples
#' donnees_telechargees %>% BDD.format(Type = "Temps de travail") %>% opentime.projet()

opentime.projet <- function(
    data = NA_character_
)
{
  #### Nettoyage & reformatage ####
  data_v2 <-
    data %>% 
    filter(tpswot_domaine == "Projets") %>% 
    mutate(projet_id = ifelse(tpswot_domaine == "Projets", str_sub(tpswot_activite, 1, 5), NA_character_), .after = "tpswot_domaine") %>% # On conserve les 5 premiers caractères
    mutate(projet_id = ifelse(tpswot_domaine == "Projets", str_replace_all(projet_id, "[^0-9]", ""), NA_character_), .after = "tpswot_domaine") %>% 
    mutate(projet_id = ifelse(tpswot_domaine == "Projets", str_replace(projet_id, " ", NA_character_), NA_character_), .after = "tpswot_domaine") %>% 
    mutate(projet_id = ifelse(tpswot_domaine == "Projets" & projet_id == "", NA_character_, projet_id), .after = "tpswot_domaine") %>% 
    mutate(projet_id = as.numeric(projet_id))
  
  #### Sortie ####
  return(data_v2)
} # Fin de la fonction
