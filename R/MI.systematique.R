#' Complément des données de MI
#'
#' Cette fonction permet de compléter les données de systématique des MI
#' @name MI.systematique
#' @param data Jeu de données à compléter
#' @keywords MI
#' @import tidyverse
#' @export
#' @examples
#' MI.systematique(data)

###### À faire #####
# 
####################

MI.systematique <- function(data)
{
  
  ## Connexion à la BDD ##
  dbMI <- BDD.ouverture(Type = "Macroinvertébrés")
  
  ## Récupération des données ##
  HabitatsReference <- tbl(dbMI,"HabitatsReference") %>% collect(n = Inf)
  Prelevements <- tbl(dbMI,"Prelevements") %>% collect(n = Inf)
  Captures <- tbl(dbMI,"Captures") %>% collect(n = Inf)
  EspecesReference <- tbl(dbMI,"EspecesReference") %>% collect(n = Inf)
  GenresReference <- tbl(dbMI,"GenresReference") %>% collect(n = Inf)
  SousFamillesReference <- tbl(dbMI,"SousFamillesReference") %>% collect(n = Inf)
  FamillesReference <- tbl(dbMI,"FamillesReference") %>% collect(n = Inf)
  OrdresReference <- tbl(dbMI,"OrdresReference") %>% collect(n = Inf)
  
  ## Fermeture de la BDD ##
  DBI::dbDisconnect(dbMI)
  
  # Assemblage des morceaux de systématique
  Systematique <- full_join(EspecesReference, GenresReference, "GenreID")
  Systematique <- bind_rows(Systematique, Systematique %>% filter(!is.na(Espece)) %>% select(3:9))
  Systematique <- full_join(Systematique, SousFamillesReference, "SousFamilleID")
  Systematique$FamilleID <- ifelse(!is.na(Systematique$FamilleID.x), Systematique$FamilleID.x, Systematique$FamilleID.y) # Pour tout remettre les FamilleID dans la même colonne
  Systematique <- select(Systematique, -FamilleID.x, -FamilleID.y)
  #Systematique <- bind_rows(Systematique, Systematique %>% filter(!is.na(Genre)) %>% select(3:9))
  Systematique <- full_join(Systematique, FamillesReference, "FamilleID")
  Systematique <- bind_rows(Systematique, Systematique %>% filter(!is.na(Genre)) %>% select(10:17))
  Systematique <- full_join(Systematique, OrdresReference, "OrdreID")
  Systematique <- bind_rows(Systematique, Systematique %>% filter(!is.na(Ordre)) %>% select(18:23))

  Systematique <- distinct(Systematique)
  
  # Travail sur les captures #
  #if(all(colnames(data) %in% colnames(Captures))) {
    
    # Ajout de la systématique #
    SyntEsp <- merge(Systematique, data, by.x="Espece", by.y="Taxon")
    SyntGen <- merge(select(Systematique, 5:23), data, by.x="Genre", by.y="Taxon")
    SyntSSFam <- merge(select(Systematique, 9:23), data, by.x="SousFamille", by.y="Taxon")
    SyntFam <- merge(select(Systematique, 12:23), data, by.x="Famille", by.y="Taxon")
    SyntOrd <- merge(select(Systematique, 18:23), data, by.x="Ordre", by.y="Taxon")
    
    data <- full_join(SyntEsp, SyntGen)
    data <- full_join(data, SyntSSFam)
    data <- full_join(data, SyntFam)
    data <- full_join(data, SyntOrd)
    
    data <- distinct(data)
    
    data <-
      data %>% 
      mutate(FamilleSensIBGN = ifelse(!is.na(Famille), Famille, Ordre))
    
  #}
  
  return(data)
  
} # Fin de la fonction