#' Exportation des IPR
#'
#' Cette fonction permet d'exporter les résultats IPR de pêche
#' @name poissons.IPR
#' @param ListeOperations Dataframe contenant un colonne "Station" avec le code de la station (RHJ) et une colonne "Date"
#' @param expertise \code{TRUE} par défault
#' @keywords poissons
#' @import DBI
#' @import tidyverse
#' @export
#' @examples
#' poissons.IPR()
#' poissons.IPR(listeOperations)
#' poissons.IPR(data.frame(Station = "SOR10-2", Date = "2012-11-03"))
#' poissons.IPR(data.frame(Station = "SOR10-2", Date = "2012-11-03"), expertise = FALSE)

##### TODO LIST #####
# Ré-écriture nécessaire afin de réduire au maximum le volume de données à transférer
#####################

#library(aquatools);library(dplyr);library(lubridate)

poissons.IPR <- function(
  ListeOperations = data.frame(Station = character(0), Date = character(0)),
  expertise=TRUE)
{
  
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
  if(dim(ListeOperations)[1] == 0 & expertise == TRUE){
  IPR <-
    IPR %>% 
    select(Station, CodeSIERMC, Altitude, Date, Classe, Score, Qualite, AvisExpertCourt, AvisExpert, Especes, N)
  }
  
  if(dim(ListeOperations)[1] == 0 & expertise == FALSE){
    IPR <-
      IPR %>% 
      select(Station, CodeSIERMC, Altitude, Date, Classe, Score, Qualite, Especes, N)
  }
  
  # Travail sur quelques opérations #
  if(dim(ListeOperations)[1] != 0 & expertise == TRUE){
    ListeOperations <- ListeOperations %>% mutate(Cle = paste0(Station, " - ", Date))
    IPR <-
      IPR %>% 
      mutate(Cle = paste0(Station, " - ", Date)) %>% 
      filter(Cle %in% ListeOperations$Cle) %>% 
      select(Station, CodeSIERMC, Altitude, Date, Classe, Score, Qualite, AvisExpertCourt, AvisExpert, Especes, N)
  }
  
  if(dim(ListeOperations)[1] != 0 & expertise == FALSE){
    ListeOperations <- ListeOperations %>% mutate(Cle = paste0(Station, " - ", Date))
    IPR <-
      IPR %>% 
      mutate(Cle = paste0(Station, " - ", Date)) %>% 
      filter(Cle %in% ListeOperations$Cle) %>% 
      select(Station, CodeSIERMC, Altitude, Date, Classe, Score, Qualite, Especes, N)
  }
  
  # Rendu du résultat #
  return(IPR)
  
} # Fin de la fonction