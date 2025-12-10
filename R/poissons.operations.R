#' Listing des opérations pour une station
#'
#' Cette fonction permet de lister les différentes opérations pour une liste donnée de stations
#' @name poissons.operations
#' @param ListeStations Dataframe contenant un colonne "Nom" avec le code de la station (RHJ)
#' @param CodeOperation Affichage du CodeOperation - \code{FALSE} (par défault)
#' @param codeinventaire Affichage du codeinventaire - \code{FALSE} (par défault)
#' @param Sortie Forme du dataframe de sortie - \code{Simple} (par défault), \code{Propre} (format diffusable, avec stations) ou \code{Complet} (tous les champs)
#' @keywords poissons
#' @import DBI
#' @import tidyverse
#' @export
#' @examples
#' poissons.operations()
#' poissons.operations(CodeOperation = TRUE)
#' poissons.operations(data.frame(Nom = "SOR10-2"))
#' poissons.operations(data.frame(Nom = "SOR10-2"), CodeOperation = TRUE)
#' poissons.operations(listeStations, Sortie = "Propre")
#' poissons.operations(data.frame(Nom = "SOR10-2"), Sortie = "Complet")

poissons.operations <- function(
  ListeStations = data.frame(Nom = character(0)),
  CodeOperation = FALSE,
  codeinventaire = FALSE,
  Sortie = c("Simple", "Propre", "Complet")
)
{

  #### Évaluation des choix ####
  Sortie <- match.arg(Sortie)
  
  #### Collecte des données ####
  ## Ouverture de la BDD ##
  ## Connexion à la BDD
  dbP <- BDD.ouverture(Type = "Poissons")

  ## Récupération des données ##
  Operations <- tbl(dbP,"operations") %>% collect(n = Inf)
  Inventaires <- tbl(dbP,"inventaires") %>% collect(n = Inf)
  Stations <- tbl(dbP,"stations") %>% collect(n = Inf)
  
  ## Fermeture de la BDD ##
  DBI::dbDisconnect(dbP)
  
  ## Synthèse des données ##
  Inventaires <- merge(Inventaires, Stations, by = c("codestation"))
  Operations <- merge(Operations, Inventaires, by = c("codeinventaire"))
  
  ## Format de Date ##
  Operations$DateDebut.x <- ymd(Operations$datedebut.x)
  Operations$DateDebut.x <- format(Operations$datedebut.x, "%Y-%m-%d")
  
  #### Test de cohérence ####
  if(dim(Operations %>% filter(nom %in% ListeStations$Nom))[1] == 0 & dim(ListeStations)[1] != 0)
    warning("Attention : station(s) absente(s) des opérations de la base de données")
  
  #### Nettoyage & reformatage ####
  renommage <- c("CodeOperation" = "codeoperation", 
                 # "CodeInventaire" = "codeinventaire", 
                 "Station" = "nom", 
                 "Date" = "datedebut.x", 
                 "X" = "xlambert", 
                 "Y" = "ylambert", 
                 "TypeCoord" = "typelambert"
  )
  
  Operations_v2 <-
    Operations %>% 
    {if(ListeStations %>% nrow() != 0) filter(., nom %in% ListeStations$Nom) else .} %>%
    {if(Sortie == "Simple") select(., codeinventaire, codeoperation, nom, datedebut.x) else .} %>%
    {if(Sortie == "Propre") select(., codeinventaire, codeoperation, nom, limiteamont, datedebut.x, modeechantillonnage, xlambert, ylambert, typelambert) else .} %>%
    {if(CodeOperation == F & Sortie != "Complet") select(., -codeoperation) else .} %>%
    {if(codeinventaire == F & Sortie != "Complet") select(., -codeinventaire) else .} %>%
    rename(any_of(renommage)) %>%
    arrange(Station, Date)
  
  data_sortie <- Operations_v2 %>% as_tibble()
    
  #### Sortie ####
  return(data_sortie)
  
} # Fin de la fonction