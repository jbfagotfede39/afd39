#' Ouverture données OpenTime "détails prévisionnel"
#' 
#' Ouvre les données "détails temps passé" OpenTime et les reformate
#' @name opentime.tps.attendu
#' @keywords personnel
#' @param fichier Fichiers \code{.xlsx} issu de OpenTime à traiter (\code{détails prévisonnels})
#' @import janitor
#' @import readxl
#' @import tidyverse
#' @export
#' @examples
#' "monfichier.xlsx" %>% opentime.tps.attendu() %>% opentime.association.personnel() %>% BDD.format(Type = "Temps de travail") %>% opentime.projet()

opentime.tps.attendu <- function(
    fichier = NA_character_
)
{
  #### Nettoyage & reformatage ####
  data_v2 <- 
    fichier %>% 
    read_excel() %>% 
    clean_names() %>% 
    select(-date_de_lancement) %>% 
    select(-date_de_cloture) %>% 
    mutate(montant = ifelse(montant < 0, montant * -1, montant)) %>% 
    rename(duree_h = duree) %>% 
    rename(tpswot_domaine = domaine) %>% 
    rename(tpswot_activite = activite) %>% 
    rename(depenses_externes = lignes) %>% 
    relocate(depenses_externes, .after = montant)
  
  #### Sortie ####
  return(data_v2)
  
} # Fin de la fonction
