#' Association du personnel aux données OpenTime
#' 
#' Associe proprement les personnels aux données issues de OpenTime
#' @name opentime.association.personnel
#' @keywords personnel
#' @param data Jeu de données à traiter, issu de \code{opentime.tps.attendu()} par exemple
#' @param format Format de la sortie : \code{libelle_separe} (par défaut), \code{libelle_complet} ou \code{id}
#' @import DBI
#' @import tidyverse
#' @export
#' @examples
#' "monfichier.xlsx" %>% opentime.tps.attendu() %>% opentime.association.personnel() %>% opentime.projet()

opentime.association.personnel <- function(
    data = NA_character_,
    format = c("libelle_separe", "libelle_complet", "id")
)
{
  #### Évaluation des choix ####
  format <- match.arg(format)

  #### Nettoyage & reformatage ####
  data_v2 <- 
    data %>% 
    mutate(id = ifelse(metier == "Ingénieur JBF", 1, NA), .after = metier) %>% 
    mutate(id = ifelse(metier == "Responsable du pôle technique", 1, id)) %>% 
    mutate(id = ifelse(metier == "Chargé de développement VR", 6, id)) %>% 
    mutate(id = ifelse(metier == "Chargé de développement PM", 7, id)) %>% 
    mutate(id = ifelse(metier == "Chargé de développement SP", 8, id)) %>% 
    # mutate(id = ifelse(metier == "Responsable du pôle technique", 9, id)) %>% 
    mutate(id = ifelse(metier == "Directeur", 9, id)) %>% 
    mutate(id = ifelse(metier == "Technicien AL", 10, id)) %>% 
    mutate(id = ifelse(metier == "Ingénieur AL", 10, id)) %>% 
    mutate(id = ifelse(metier == "Chargé de développement FM", 11, id)) %>% 
    mutate(id = ifelse(metier == "Responsable du pôle développement", 11, id)) %>% 
    mutate(id = ifelse(metier == "Responsable du pôle administratif", 36, id)) %>% 
    mutate(id = ifelse(metier == "Assistante administrative", 43, id)) %>%
    mutate(id = ifelse(metier == "Technicien MD", 72, id))

  data_v3 <-
    data_v2 %>% 
    # {if(format == "id") formatage.personnel.id(., "id") else .} %>% 
    {if(format == "libelle_separe") formatage.personnel.prenom(., "id") else .} %>% 
    {if(format == "libelle_complet") formatage.personnel.prenomnom(., "id") else .} %>% 
    {if(format == "libelle_complet") select(., -id) else .} %>% 
    {if(format == "libelle_complet") mutate(., gestop_libelle = glue("{gestop_prenom} {gestop_nom}")) else .} %>% 
    {if("duree_h" %in% names(.)) relocate(., duree_h, .after = last_col()) else .} %>% 
    {if("montant" %in% names(.)) relocate(., montant, .after = last_col()) else .} %>% 
    {if("depenses_externes" %in% names(.)) relocate(., depenses_externes, .after = last_col()) else .}
  
  #### Vérification ####
  if(data_v3 %>% filter(metier == "Responsable du pôle technique") %>% nrow() != 0) warning("Attention : présence de lignes avec la poste 'Responsable du pôle technique' qui présente une ambiguité entre Mehdi et JB")
  
  #### Sortie ####
  return(data_v3)
  
} # Fin de la fonction
