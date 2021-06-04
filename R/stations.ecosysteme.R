#' Listage des stations d'un cours d'eau
#'
#' Cette fonction permet de lister les stations d'un cours d'eau donné dans les BDD poisson et chronique
#' 
#' @name stations.ecosysteme
#' @param Nom du cours d'eau
#' @param shp Si \code{FALSE} (par défault), n'exporte pas de shp dans le répertoire courant.
#' @keywords stations
#' @import sf
#' @import tidyverse
#' @export
#' @examples
#' stations.ecosysteme()
#' stations.ecosysteme("Ain")
#' stations.ecosysteme("Ain", shp=T)

##### TODO LIST #####
# À revoir entièrement pour homogénéisation avec nvelles fonctions (stations.coderhj, chaînage d'écosystèmes avec ecosystemes.affluents et enfin stations.territoires)
# Rajouter le choix de l'écosystème avec les codes (avec recherche dans le champ observation) et de même avec le code de la masse d'eau, avec un interrupteur disant où la fonction doit chercher initialement. Le reste du code reste identique.
# Option pour chercher les stations des afférences
# Si aucune capture sur la station poisson, indiquer comme type de données "autre"
# Ajouter pour la PC une jointure avec les coordonnées géographiques des stations SANDRE
# Rajouter les sorties des stations IAM, MI et PC
#####################

stations.ecosysteme <- function(
  ecosysteme = "",
  shp = F){
  
  #### Base poissons ####
  ## Ouverture de la BDD ##
  ## Ouverture de la BDD ##
  dbP <- BDD.ouverture(Type = "Poissons")
  
  ## Récupération des données de l'écosystème ##
  if(nchar(ecosysteme) != 0){Ecosystemes <- tbl(dbP,"ecosystemes") %>% filter(nomecosysteme == ecosysteme) %>% collect()}
  if(nchar(ecosysteme) == 0){Ecosystemes <- tbl(dbP,"ecosystemes") %>% collect()}

  # Recherche des stations qui ont un Codeecosysteme = à ce Codeecosysteme et transformation
  Code <- as.character(Ecosystemes[1,1])
  
  if(nchar(ecosysteme) != 0){
  StationsPoissons <- 
    tbl(dbP,"stations") %>% 
    filter(codeecosysteme == Code) %>% 
    collect() %>% 
    rename(X = xlambert) %>% 
    rename(Y = ylambert) %>% 
    rename(TypeCoord = typelambert) %>% 
    mutate(Poisson = "Oui")}
  
  if(nchar(ecosysteme) == 0){
    StationsPoissons <- 
      tbl(dbP,"stations") %>% 
      #filter(codeecosysteme == Code) %>% 
      collect() %>% 
      rename(X = xlambert) %>% 
      rename(Y = ylambert) %>% 
      rename(TypeCoord = typelambert) %>% 
      mutate(Poisson = "Oui")}
  
  ## Fermeture de la BDD ##
  DBI::dbDisconnect(dbP)

  #### Base chronique ####
  ## Ouverture de la BDD ##
  dbD <- BDD.ouverture("Data")
  
  ## Récupération des données de l'écosystème ##
  if(nchar(ecosysteme) != 0){
  StationsChroniques <- 
    sf::st_read(dbD, query = "select * from fd_production.chroniques_stations;") %>% 
    filter(chsta_milieu == ecosysteme) %>% 
    rename(Nom = chsta_coderhj) %>% 
    mutate(Chronique = "Oui")}
  
  if(nchar(ecosysteme) == 0){
    StationsChroniques <- 
      sf::st_read(dbD, query = "select * from fd_production.chroniques_stations;") %>% 
      rename(Nom = chmes_coderhj) %>% 
      mutate(Chronique = "Oui")}
  
  StationsChroniques <-
    StationsChroniques %>% 
    st_drop_geometry() %>% 
    rename(X = chsta_coord_x) %>% 
    rename(Y = chsta_coord_y) %>% 
    rename(TypeCoord = chsta_coord_type)
  
  #### Base physico-chimie ####
  ## Collecte des stations pour les mesures
  StationsPC <-
    tbl(dbD, in_schema("fd_production", "physicochimie_mesures")) %>%
    distinct(pcmes_coderhj, pcmes_codesie) %>% 
    collect() %>% 
    left_join(sf::st_read(dbD, query = "select * from fd_production.physicochimie_suiviterrain;") %>% st_drop_geometry() %>% distinct(pcsvi_coderhj, pcsvi_codesie), by = c("pcmes_coderhj" = "pcsvi_coderhj")) %>% 
    filter(!(is.na(pcmes_codesie) & is.na(pcsvi_codesie))) %>% 
    mutate(StationSANDRE = ifelse(is.na(pcmes_codesie), pcsvi_codesie, pcmes_codesie)) %>% 
    mutate(StationSANDREVerif = ifelse(pcmes_codesie == pcsvi_codesie, "Ok", "Problème"))
  
  if(length(which(StationsPC == "Problème")) != 0) stop("Problème de doublon")
  
  ## Fermeture de la BDD ##
  DBI::dbDisconnect(dbD)
  
  # Ensemble des stations # 
  StationsPC <-
    StationsPC %>% 
    distinct(pcmes_coderhj, StationSANDRE) %>% 
    rename(Nom = pcmes_coderhj) %>% 
    mutate(PC = "Oui")
  
  ## Récupération des données de l'écosystème ##
  # if(nchar(ecosysteme) != 0){
  #   StationsPC <- 
  #     StationsPC %>% 
  #     filter(Milieu == ecosysteme)}
  
  #### Synthèse ####
  Synthese <- 
    StationsPoissons %>% 
    rename(Nom = nom) %>% 
    full_join(StationsChroniques, by = c("Nom", "X", "Y", "TypeCoord")) %>% 
    select(Nom, X, Y, TypeCoord, Poisson, Chronique) %>% 
    full_join(StationsPC %>% select(Nom, PC), by = c("Nom")) %>% 
    arrange(Nom)
  
  #### Amélioration de la sortie ####
  Synthese <- 
    Synthese %>% 
    rename(CodeRDT = Nom) %>% 
    tidyr::separate(CodeRDT, c("MilieuTemporaire", "fin"), sep = " A ", remove = F) # Séparation du nom en deux parties
  
  # Travail sur les stations sans CodeRDT
  Synthese2 <-
    Synthese %>% 
    filter(!is.na(fin)) %>% 
    bind_rows(Synthese %>% filter(is.na(fin)) %>% formatage.ecosysteme(Operation = "Simplification", ColonneEntree = "MilieuTemporaire", ColonneSortie = "codemilieu")
)
  
  # Travail sur les stations avec CodeRDT
  acronymes <- formatage.abreviation() %>% filter(Type == "Écosystème")
  
  Synthese <-
    Synthese2 %>% 
    filter(is.na(codemilieu)) %>% # On prend ceux qui n'ont pas de codeRDT
    full_join(Synthese2 %>% 
                filter(!is.na(codemilieu)) %>% 
                left_join(acronymes, by = c(codemilieu = "Acronyme")), by = c("CodeRDT", "MilieuTemporaire", "fin", "X", "Y", "TypeCoord", "Poisson", "Chronique", "PC", "codemilieu")) %>% # on fusionne avec ceux qui en ont un et avec la traduction
    mutate(Milieu = ifelse(is.na(Definition), MilieuTemporaire, Definition)) %>%
    mutate(CodeRDT = ifelse(is.na(CodeRDT), coderhj.y, CodeRDT)) %>% 
    select(CodeRDT, Milieu, X:PC) %>% 
    rename(Nom = CodeRDT)
  
  #### Filtrage ####
  if(nchar(ecosysteme) != 0){
    Synthese <-
      Synthese %>%
      filter(tolower(Milieu) == tolower(ecosysteme))}
  
  # Test si le nom existe bien, sinon message d'erreur et arrêt de la fonction #
  if(dim(Synthese)[1] == 0) 
    stop("Attention : nom d'écosystème absent des bases de données")

  # Sortie
  if(shp == F){
    return(Synthese)
  }

  # Export shp
  if(shp == T){
    SIG.export(Synthese %>% filter(TypeCoord == "L93"), paste0(format(now(), format="%Y-%m-%d"),"_",ecosysteme,"_Export_stations.shp"))
  }
  
} # Fin de la fonction