#' Exportation des IPR
#'
#' Cette fonction permet d'exporter les résultats IPR de pêche
#' @name poissons.IPR
#' @param ListeOperations Dataframe contenant un colonne "Station" avec le code de la station (RHJ) et une colonne "Date"
#' @param sortie Forme du dataframe de sortie - \code{Simple} (par défaut), \code{Propre} (format diffusable, avec stations) ou \code{Complet} (tous les champs)
#' @keywords poissons
#' @import DBI
#' @import tidyverse
#' @export
#' @examples
#' poissons.IPR()
#' poissons.IPR(listeOperations)
#' poissons.IPR(data.frame(Station = "CUI3-8", Date = "2024-09-18"))
#' poissons.IPR(data.frame(Station = "CUI3-8", Date = "2024-09-18"), sortie = "Propre")
#' poissons.IPR(data.frame(Station = "CUI3-8", Date = "2024-09-18"), sortie = "Complet")

poissons.IPR <- function(
  ListeOperations = data.frame(Station = character(0), Date = character(0)),
  expertise = TRUE,
  sortie = c("Simple", "Propre", "Complet")
)
{
  ## Évaluation des choix ##
  sortie <- match.arg(sortie)
  
  ## Ouverture de la BDD ##
  dbP <- BDD.ouverture(Type = "Poissons")
  
  ## Récupération des données ##
  Operations <- tbl(dbP,"operations") %>% collect(n = Inf) %>% select(codeoperation, avisexpertcourt, avisexpert)
  Stations <- tbl(dbP,"stations") %>% collect(n = Inf)
  IPR <- tbl(dbP,"iprs") %>% collect(n = Inf)
  
  ## Fermeture de la BDD ##
  DBI::dbDisconnect(dbP)
  
  ## Synthèse des données ##
  IPR <- left_join(IPR, Operations, by = c("codeoperation" = "codeoperation"))
  IPR <- left_join(IPR, Stations, by = c("codestation"))

  ## Format de dates ##
  IPR$dateipr <- ymd(IPR$dateipr)
  
  ## Simplification ##
  IPR <- 
    IPR %>%
    rename(Station = nom, Date = dateipr) %>% 
    arrange(Station, Date) %>% 
    rename(CodeSIERMC = codesiermc, Altitude = altitude, Classe = classe, Score = score, Qualite = qualite, AvisExpertCourt = avisexpertcourt, AvisExpert = avisexpert, Especes = especes) %>% 
    mutate(N = str_count(.$Especes, ",") + 1)
  
  # Travail sur toutes les opérations #
  if(nrow(ListeOperations) == 0){
  IPR <- 
    IPR %>% 
    {if(sortie == "Simple") select(., Station, CodeSIERMC, Altitude, Date, Classe, Score, Qualite, Especes, N) else .} %>% 
    {if(sortie == "Propre") select(., Station, CodeSIERMC, Altitude, Date, Classe, Score, Qualite, contains("AvisExpert"), Especes, N) else .} %>% 
    {if(sortie == "Complet") select(., Station, CodeSIERMC, Altitude, Date, Classe, Score, Qualite, contains("AvisExpert"), Especes, N, everything()) else .}
  }

  # Travail sur quelques opérations #
  if(nrow(ListeOperations) != 0){
    ListeOperations <- ListeOperations %>% mutate(Cle = paste0(Station, " - ", Date))
    IPR <-
      IPR %>% 
      mutate(Cle = paste0(Station, " - ", Date)) %>% 
      filter(Cle %in% ListeOperations$Cle) %>% 
      {if(sortie == "Simple") select(., Station, CodeSIERMC, Altitude, Date, Classe, Score, Qualite, Especes, N) else .} %>% 
      {if(sortie == "Propre") select(., Station, CodeSIERMC, Altitude, Date, Classe, Score, Qualite, contains("AvisExpert"), Especes, N) else .} %>% 
      {if(sortie == "Complet") select(., Station, CodeSIERMC, Altitude, Date, Classe, Score, Qualite, contains("AvisExpert"), Especes, N, everything()) else .}
  }

  # Rendu du résultat #
  return(IPR)
  
} # Fin de la fonction