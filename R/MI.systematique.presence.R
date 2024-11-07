#' Vérification existence MI
#'
#' Cette fonction permet de vérifier la présence des taxons de MI
#' @name MI.systematique.presence
#' @param data Jeu de données à vérifier, issu de MI.captures
#' @keywords MI
#' @import DBI
#' @import dplyr
#' @import tidyverse
#' @export
#' @examples
#' MI.systematique.presence(data)

###### À faire #####
# Revoir la très sale section # Assemblage des morceaux de systématique
####################

MI.systematique.presence <- function(data)
{
  
  ## Connexion à la BDD ##
  dbD <- BDD.ouverture("Data")
  
  ## Récupération des données ##
  Prelevements <- tbl(dbD, in_schema("fd_production", "macroinvertebres_prelevements")) %>% collect(n = Inf)
  HabitatsReference <- tbl(dbD, in_schema("fd_referentiels", "macroinvertebres_habitats_reference")) %>% collect(n = Inf)
  EspecesReference <- tbl(dbD, in_schema("fd_referentiels", "systematique_especes")) %>% collect(n = Inf)
  GenresReference <- tbl(dbD, in_schema("fd_referentiels", "systematique_genres")) %>% collect(n = Inf)
  SousFamillesReference <- tbl(dbD, in_schema("fd_referentiels", "systematique_sousfamilles")) %>% collect(n = Inf)
  FamillesReference <- tbl(dbD, in_schema("fd_referentiels", "systematique_familles")) %>% collect(n = Inf)
  OrdresReference <- tbl(dbD, in_schema("fd_referentiels", "systematique_ordres")) %>% collect(n = Inf)
  
  ## Fermeture de la BDD ##
  DBI::dbDisconnect(dbD)

  # Assemblage des morceaux de systématique
  Systematique <- full_join(EspecesReference, GenresReference, by = c("sysesp_genre_id" = "id"))
  Systematique <- bind_rows(Systematique, Systematique %>% filter(!is.na(sysesp_ranglibelle)) %>% select(-contains('_modif'), -contains('_remarques')))
  # Systematique <- bind_rows(Systematique, Systematique %>% filter(!is.na(sysesp_ranglibelle)) %>% select(3:9))
  Systematique <- full_join(Systematique, SousFamillesReference, by = c("sysgen_sousfamille_id" = "id"))
  Systematique$famille_id <- ifelse(!is.na(Systematique$sysgen_famille_id), Systematique$sysgen_famille_id, Systematique$sysssfam_famille_id) # Pour tout remettre les FamilleID dans la même colonne
  Systematique <- select(Systematique, -sysgen_famille_id, -sysssfam_famille_id)
  #Systematique <- bind_rows(Systematique, Systematique %>% filter(!is.na(Genre)) %>% select(3:9))
  Systematique <- full_join(Systematique, FamillesReference, by = c("famille_id" = "id"))
  Systematique <- bind_rows(Systematique, Systematique %>% filter(!is.na(sysgen_ranglibelle)) %>% select(-contains('_modif'), -contains('_remarques')))
  Systematique <- full_join(Systematique, OrdresReference, by = c("sysfam_ordre_id" = "id"))
  Systematique <- bind_rows(Systematique, Systematique %>% filter(!is.na(sysord_ranglibelle)) %>% select(18:23))
  Systematique <- Systematique %>% select(-contains('_modif'), -contains('_remarques'))
  
  Systematique <- distinct(Systematique) %>% filter(!is.na(sysord_ranglibelle))
  
  # Travail sur les captures #

    # Vérification de l'existence des taxons dans la BDD #
    Absents <- setdiff(unique(data$micapt_taxon), Systematique$sysesp_ranglibelle)
    Absents <- setdiff(Absents, Systematique$sysgen_ranglibelle)
    Absents <- setdiff(Absents, Systematique$sysssfam_ranglibelle) #
    Absents <- setdiff(Absents, Systematique$sysfam_ranglibelle)
    Absents <- setdiff(Absents, Systematique$sysord_ranglibelle)
    
    #### Sortie des résultats ####
    if(length(Absents) > 0){
      Absents <- sort(Absents)
      return(Absents)
    }
    if(length(Absents) == 0){
      print("Aucun taxon absent des données de référence")
    }

} # Fin de la fonction
