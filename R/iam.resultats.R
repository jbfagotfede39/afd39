#' Extraction des résultats d'IAM
#'
#' Cette fonction permet de collecter les résultats des IAM
#' @name iam.resultats
#' @param ListeStations Dataframe contenant un colonne "Nom" avec le code de la station (RHJ)
#' @param Sortie Forme du dataframe de sortie - \code{Simple} (par défault), \code{Propre} (format diffusable, avec stations) ou \code{Complet} (tous les champs)
#' @keywords donnees
#' @import DBI
#' @import dplyr
#' @import lubridate
#' @param periode Permet de limiter la durée des données traitées : 5ans, 10ans (par défaut), 20ans, Complet, 4campagnes
#' @export
#' @examples
#' iam.resultats()
#' Resultats <- iam.resultats()
#' Resultats <- iam.resultats(Sortie = "Complet")
#' iam.resultats(data.frame(Nom = "SOR10-2"), Sortie = "Propre", periode = "20ans")
#' iam.resultats(Sortie = "Complet") %>% st_join(stations.territoire("Commune", c("Montmorot"))) %>% filter(!is.na(tpcomm_commune_libellesansarticle))

iam.resultats <- function(
  ListeStations = data.frame(Nom = character(0)),
  Sortie = c("Simple","Propre","Complet"),
  periode = c("10ans","5ans","20ans","Complet","4campagnes")
)
  {
  
  ## Évaluation des choix ##
  Sortie <- match.arg(Sortie)
  periode <- match.arg(periode)
  
  ## Ouverture de la BDD ##
  dbD <- BDD.ouverture("Data")
  
  ##### Récupération des données #####
  Resultats <- 
    sf::st_read(dbD, query = "SELECT * FROM fd_production.topographie_iam;") %>% 
    arrange(tpiam_coderhj, tpiam_date) %>%
    collect(n = Inf)
  
  ## Fermeture de la BDD ##
  DBI::dbDisconnect(dbD)
  
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
  if(periode == "4campagnes") limitetemporelle <- Resultats %>% distinct(tpiam_date) %>% arrange(desc(tpiam_date)) %>% pull() %>% nth(4)
  Resultats <- Resultats %>% filter(tpiam_date >= limitetemporelle)
  
  ##### Simplification #####
if(dim(ListeStations)[1] == 0 & Sortie == "Simple"){
  stop("Paramétrage de la sortie à développer")
 # Resultats <-
 #   Resultats %>%
 #   dplyr::rename(Station = nom, Date = tpiam_date) %>% 
 #   select(Station, Date, codeespece)
}
  
  if(dim(ListeStations)[1] == 0 & Sortie == "Propre"){
    stop("Paramétrage de la sortie à développer")
    # Resultats <-
    #   Resultats %>%
    #   select(Nom, DateDebut, Codeespece, TailleMinimum, TailleMaximum, Nombre, Poids)
  }
  
  if(dim(ListeStations)[1] != 0 & Sortie == "Complet"){
    # Resultats <-
    #   Resultats %>%
    #   select(Nom, DateDebut, Codeespece, TailleMinimum, TailleMaximum, Nombre, Poids)
    # Rien à faire
  }

  return(Resultats)
} # Fin de la fonction