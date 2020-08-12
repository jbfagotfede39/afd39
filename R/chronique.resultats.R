#' Listage des résultats de chroniques
#'
#' Cette fonction permet de lister les résultats de la BDD Chroniques
#' @name chronique.resultats
#' @param Nom Nom recherché
#' @param Recherche Type de données recherchées
#' @keywords chronique
#' @import tidyverse
#' @export
#' @examples
#' chronique.resultats("HER0-6","Station")
#' chronique.resultats("2017","Année biologique")
#' Resultats <- Stations$chsta_coderhj %>% map_dfr(~ chronique.resultats(., Recherche = "Station"))

##### TODO LIST #####
# 
#####################

chronique.resultats <- function(x = NA_character_ ,
                               Recherche = c("Station", "Année biologique")
)
{
  
  #### Évaluation des choix ####
  Recherche <- match.arg(Recherche)
  
  #### Connexion à la BDD ####
  dbD <- BDD.ouverture("Data")
  
  #### Chargement des données ####
  # Je ne parviens pas à tout mettre dans une seule requête avec filtre conditionnel à cause du datatype différent entre chres_coderhj et chres_annee, car on a alors un cas potentiel avec chres_annee = 'BAE3-2amont' par exemple qui ne peut pas être interprété par postgres car datatype de chres_annee = double
if(Recherche == "Station"){
  Resultats <-
    tbl(dbD, in_schema("fd_production", "chroniques_resultats")) %>% 
    filter(chres_coderhj == x) %>% 
    collect()
}

if(Recherche == "Année biologique"){
  Resultats <-
    tbl(dbD, in_schema("fd_production", "chroniques_resultats")) %>% 
    filter(chres_annee == x) %>% 
    collect()
}

  #### Fermeture de la BDD ####
  DBI::dbDisconnect(dbD)
  
  #### Formatage ####
    Vue <-
    Resultats %>% 
    arrange(chres_coderhj, chres_annee)
  
  #### Affichage des résultats ####
  return(Vue)
}