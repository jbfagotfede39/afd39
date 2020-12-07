#' Listage des capteurs de chroniques
#'
#' Cette fonction permet de lister les capteurs de la BDD Chroniques
#' @name chronique.capteurs
#' @param x Nom recherché
#' @param Recherche Type de données recherchées
#' @param Sortie Forme du dataframe de sortie - \code{Complet} (par défault - tous les champs), \code{Propre} ou \code{Simple}
#' @keywords capteurs
#' @keywords chronique
#' @import dplyr
#' @export
#' @examples
#' chronique.capteurs("CD39","Propriétaire")
#' chronique.capteurs("Thermie","Type")
#' chronique.capteurs("Hobo UA-001-64","Modèle")
#' chronique.capteurs("10165890","Numéro")
#' chronique.capteurs("PDPG","Projet")
#' chronique.capteurs("OK","État")
#' SuiviCapteurs <- listeCapteurs$chmes_capteur %>% map_dfr(~ chronique.capteurs(., Recherche = "Numéro"))

##### TODO LIST #####

#####################

chronique.capteurs <- function(x = "CD39", 
                               Recherche = c("Propriétaire", "Type", "Modèle", "Numéro", "État", "Projet"),
                               Sortie = c("Complet","Propre","Simple")
)
{
  
  #### Évaluation des choix ####
  Recherche <- match.arg(Recherche)
  Sortie <- match.arg(Sortie)
  
  #### Connexion à la BDD ####
  dbD <- BDD.ouverture("Data")
  
  #### Recherche sur x en tant que telle ####
  Vue <-
    tbl(dbD, in_schema("fd_production", "chroniques_capteurs")) %>% 
    filter(case_when(Recherche == "Propriétaire" ~ chcap_proprietaire == x, # Traitement d'un propriétaire
                     Recherche == "Type" ~ chcap_typecapteur == x, # Traitement d'un type
                     Recherche == "Modèle" ~ chcap_modelecapteur == x, # Traitement d'une modèle
                     Recherche == "Numéro" ~ chcap_numerocapteur == x, # Traitement d'une numéro de capteur
                     Recherche == "État" ~ chcap_etat == x, # Traitement d'un état
                     Recherche == "Projet" ~ chcap_projet == x # Traitement d'un projet
    )
    ) %>%
    collect(n = Inf)
  
  #### Fermeture de la connexion ####
  dbDisconnect(dbD)
  
  #### Formatage ####
  Vue <-
    Vue %>% 
    {if (nrow(.) != 0) arrange(., chcap_numerocapteur) else .} %>% 
    {if(Sortie == "Propre") select(., -contains("modif")) else .}
  
  #### Affichage des résultats ####
  return(Vue)
}