#' Extraction des résultats pour une station
#'
#' Cette fonction permet de collecter les différents résultats pour une liste donnée de stations
#' @name poissons.resultats
#' @param ListeStations Dataframe contenant un colonne "Nom" avec le code de la station (RHJ)
#' @param Sortie Forme du dataframe de sortie - \code{Simple} (par défault), \code{Propre} (format diffusable, avec stations) ou \code{Complet} (tous les champs)
#' @param periode Permet de limiter la durée des données traitées : 5ans, 10ans (par défaut), 20ans, Complet, 4campagnes
#' @keywords donnees
#' @import DBI
#' @import tidyverse
#' @export
#' @examples
#' poissons.resultats()
#' Resultats <- poissons.resultats()
#' poissons.resultats(data.frame(Nom = "SOR10-2"), Sortie = "Propre")

poissons.resultats <- function(
  ListeStations = data.frame(Nom = character(0)),
  Sortie = c("Simple","Propre","Complet"),
  periode = c("10ans","5ans", "20ans","Complet","4campagnes")
)
  {
  
  ## Évaluation des choix ##
  Sortie <- match.arg(Sortie)
  periode <- match.arg(periode)
  
  ## Ouverture de la BDD ##
  dbP <- BDD.ouverture(Type = "Poissons")
  
  ##### Récupération des données #####
  Resultats <- tbl(dbP,"resultats") %>% collect(n = Inf)
  Operations <- tbl(dbP,"operations") %>% collect(n = Inf)
  Inventaires <- tbl(dbP,"inventaires") %>% collect(n = Inf)
  Stations <- tbl(dbP,"stations") %>% collect(n = Inf)
  Ecosystemes <- tbl(dbP,"ecosystemes") %>% collect(n = Inf)
  Communes <- tbl(dbP,"communes") %>% collect(n = Inf)
  
  ## Fermeture de la BDD ##
  DBI::dbDisconnect(dbP)
  
  ## Renommage des colonnes Observations ##
  Resultats <- Resultats %>% rename(ObservationsResultats = observations)
  Inventaires <- Inventaires %>% rename(ObservationInventaires = observations)
  Stations <- Stations %>% rename(ObservationStations = observations)
  Ecosystemes <- Ecosystemes %>% rename(ObservationsEcosystemes = observations)
  Communes <- Communes %>% rename(ObservationsCommunes = observations)
  
  ##### Synthèse des données #####
  Inventaires <- left_join(Inventaires, Stations, by = c("codestation"))
  Operations <- left_join(Operations, Inventaires, by = c("codeinventaire"))
  Resultats <- left_join(Resultats, Operations, by = c("codeoperation"))
  
  ##### Remplacement des codes communes et écosystèmes #####
  Resultats <- left_join(Resultats, Ecosystemes, by = c("codeecosysteme.y" = "codeecosysteme"))
  Communes <- select(Communes, codecommune, commune)
  Resultats <- left_join(Resultats, Communes, by = c("codecommune"))
  
  ##### Transformation des formats ####
  ## Dates ##
  Resultats$datedebut.x <- ymd(Resultats$datedebut.x)
  
  ## Arrondis ##
  Resultats$densitenumeriquebrute <- round(Resultats$densitenumeriquebrute,1)
  Resultats$densiteponderalebrute <- round(Resultats$densiteponderalebrute,1)
  Resultats$densitenumeriqueestimee <- round(Resultats$densitenumeriqueestimee,1)
  Resultats$densiteponderaleestimee <- round(Resultats$densiteponderaleestimee,1)
  
  ##### Filtrage station #####
if(dim(ListeStations)[1] != 0){
  Resultats <-
    Resultats %>% 
    filter(nom %in% ListeStations$Nom) %>% 
    arrange(codeespece)
}
  
  ##### Limitation temporelle des résultats #####
  if(periode == "5ans") limitetemporelle <- now()-years(5)
  if(periode == "10ans") limitetemporelle <- now()-years(10)
  if(periode == "20ans") limitetemporelle <- now()-years(20)
  if(periode == "Complet") limitetemporelle <- now()-years(100)
  Resultats <- Resultats %>% filter(datedebut.x >= limitetemporelle)
  
  if(periode == "4campagnes") warning("Attention, pas de distinction par station dans le cas où il y en a plusieurs : à développer")
  if(periode == "4campagnes") limitetemporelle <- Resultats %>% distinct(datedebut.x) %>% arrange(desc(datedebut.x)) %>% pull() %>% nth(4)
  
  ##### Simplification #####
if(dim(ListeStations)[1] == 0 & Sortie == "Simple"){
 Resultats <-
   Resultats %>%
   dplyr::rename(Station = nom, Date = datedebut.x) %>% 
   select(Station, Date, codeespece)
}
  
  if(Sortie == "Propre"){
    Resultats <-
      Resultats %>%
      select(nomecosysteme, nom, codesiermc, xlambert, ylambert, datedebut.x, modeechantillonnage, codeespece, n_sommecapturepassage1, n_sommecapturepassage2, n_sommecapturepassage3, m_sommecapturepassage1, m_sommecapturepassage2, m_sommecapturepassage3, nombretotalcaptures, biomassetotalecapturee, estimationeffectifnumerique, estimationeffectifponderal, intervalleconfiancenumerique, intervalleconfianceponderal, densitenumeriqueestimee, densiteponderaleestimee, coteabondancenumerique, coteabondanceponderale)
  }

  return(Resultats)
} # Fin de la fonction