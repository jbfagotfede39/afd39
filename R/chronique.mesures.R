#' Extraction de données brutes de chronique
#'
#' Permet d'extraire les données de chronique d'une station
#' 
#' @name chronique.mesures
#' @keywords chronique
#' @import aquatools
#' @import RPostgreSQL
#' @import tidyverse
#' @param CodeStation Code RHJ de la station
#' @param Type Type de chronique recherchée : Thermie (par défaut), piézométrie, Hydrologie, Oxygénation, pluviométrie ou toutes les données
#' @param annee Année biologique recherchée si spécifiée, sinon ensemble de la chronique (par défaut)
#' @param Valide Validité des données (\code{TRUE} par défaut)
#' @export
#' @examples
#' chronique.mesures("HER0-6", "Thermie") 
#' chronique.mesures("DRO6-8", "Thermie", "2013")
#' chronique.mesures("GCLzhaval", "Piézométrie", "2015", Valide = F)
#' chronique.mesures("GCLzhaval", "Tout", "2015", Valide = F)
#' Mesures <- Stations %>% group_split(chsta_coderhj) %>% purrr::map_dfr(~ chronique.mesures(.$chsta_coderhj, "Thermie"))
#' c("GLA4-4", "GLA6-9") %>% purrr::map_dfr(~ chronique.mesures(., "Tout", Valide = F))

chronique.mesures <- function(  
  CodeStation = character(0),
  Type = c("Thermie", "Piézométrie", "Tout", "Hydrologie", "Oxygénation", "Pluviométrie"),
  annee = numeric(0),
  Valide = T)
  
{
  
#### Évaluation des choix ####
Type <- match.arg(Type)
if(nchar(as.character(CodeStation)) == 0) stop("Attention : pas de station spécifiée")

##### Connexion à la BDD #####
dbD <- BDD.ouverture("Data")

##### Collecte des données #####
if(Valide == F & Type != "Tout"){
  Mesures <- 
    tbl(dbD, in_schema("fd_production", "chroniques_mesures")) %>% 
    filter(chmes_typemesure == Type) %>% 
    filter(chmes_coderhj == as.character(CodeStation)) %>% 
    #filter(chmes_validation == "Validé") %>% 
    collect()
}

if(Valide == T & Type != "Tout"){
  Mesures <- tbl(dbD, in_schema("fd_production", "chroniques_mesures")) %>% 
    filter(chmes_typemesure == Type) %>% 
    filter(chmes_coderhj == as.character(CodeStation)) %>% 
    filter(chmes_validation == "Validé") %>% 
    collect()
}

if(Valide == F & Type == "Tout"){
  Mesures <- 
    tbl(dbD, in_schema("fd_production", "chroniques_mesures")) %>% 
    #filter(chmes_typemesure == Type) %>% 
    filter(chmes_coderhj == as.character(CodeStation)) %>% 
    #filter(chmes_validation == "Validé") %>% 
    collect()
}

if(Valide == T & Type == "Tout"){
  Mesures <- tbl(dbD, in_schema("fd_production", "chroniques_mesures")) %>% 
    #filter(chmes_typemesure == Type) %>% 
    filter(chmes_coderhj == as.character(CodeStation)) %>% 
    filter(chmes_validation == "Validé") %>% 
    collect()
}

## Fermeture de la BDD ##
RPostgreSQL::dbDisconnect(dbD)

##### Filtrage en fonction de la période #####
if(length(as.character(annee)) != 0){
  Mesures <-
    Mesures %>%
    formatage.annee.biologique() %>%
    filter(chmes_anneebiol == as.character(annee))
}

##### Mise en forme #####
Mesures <- 
  Mesures %>% 
  {if (nrow(Mesures) != 0) arrange(., chmes_date, chmes_heure) else .}

### Format compatible avec les jointures en cas d'absence de données ###
if(nrow(Mesures) == 0) {
  data(chronique_structure)
  Mesures <- mesures_structure
}

##### Sortie #####
return(Mesures)

} # Fin de la fonction