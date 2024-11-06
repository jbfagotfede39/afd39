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

MI.systematique <- function(data)
{
  
  #### Récupération des données ####
  ## Connexion à la BDD ##
  dbD <- BDD.ouverture("Data")
  
  especes_reference <- tbl(dbD, dbplyr::in_schema("fd_referentiels", "systematique_especes")) %>% select(-contains('modif')) %>% arrange(sysesp_ranglibelle) %>% collect(n = Inf)
  genres_reference <- tbl(dbD, dbplyr::in_schema("fd_referentiels", "systematique_genres")) %>% select(-contains('modif')) %>% arrange(sysgen_ranglibelle) %>% collect(n = Inf)
  sous_familles_reference <- tbl(dbD, dbplyr::in_schema("fd_referentiels", "systematique_sousfamilles")) %>% select(-contains('modif')) %>% arrange(sysssfam_ranglibelle) %>% collect(n = Inf)
  familles_reference <- tbl(dbD, dbplyr::in_schema("fd_referentiels", "systematique_familles")) %>% select(-contains('modif')) %>% arrange(sysfam_ranglibelle) %>% collect(n = Inf)
  ordres_reference <- tbl(dbD, dbplyr::in_schema("fd_referentiels", "systematique_ordres")) %>% select(-contains('modif')) %>% arrange(sysord_ranglibelle) %>% collect(n = Inf)
  
  ## Fermeture de la BDD ##
  DBI::dbDisconnect(dbD)
  
  #### Assemblage des morceaux de systématique ####
  systematique <- especes_reference %>% full_join(genres_reference, by = c("sysesp_genre_id" = "id"))
  systematique <- systematique %>% bind_rows(systematique %>% filter(!is.na(sysesp_rangsandre)))
  systematique <- systematique %>% full_join(sous_familles_reference, by = c("sysgen_sousfamille_id" = "id"))
  systematique$famille_id <- ifelse(!is.na(systematique$sysgen_famille_id), systematique$sysgen_famille_id, systematique$sysssfam_famille_id) # Pour tout remettre les FamilleID dans la même colonne
  systematique <- systematique %>% select(-sysgen_famille_id, -sysssfam_famille_id)
  systematique <- systematique %>% full_join(familles_reference, by = c("famille_id" = "id"))
  systematique <- systematique %>% bind_rows(systematique %>% filter(!is.na(sysesp_genre_id)) %>% select(10:17))
  systematique <- systematique %>% full_join(ordres_reference, by = c("sysfam_ordre_id" = "id"))
  systematique <- systematique %>% bind_rows(systematique %>% filter(!is.na(sysfam_ordre_id)) %>% select(18:23))
  
  systematique <-
    distinct(systematique) %>% # Dédoublonnage
    rename(sysesp_espece_id = id)
  
  #### Travail sur les captures ####
  ##### Ajout de la systématique #####
  synthese_especes <- systematique %>% left_join(data, by = c("sysesp_ranglibelle" = "micapt_taxon"))
  synthese_genres <- systematique %>% filter(!is.na(sysesp_ranglibelle) & !is.na(sysgen_ranglibelle)) %>% select(contains("sys")) %>% left_join(data, by = c("sysgen_ranglibelle" = "micapt_taxon"))
  synthese_sousfamilles <- systematique %>% filter(!is.na(sysesp_ranglibelle) & !is.na(sysgen_ranglibelle) & !is.na(sysssfam_ranglibelle)) %>% select(contains("sysssfam"), contains("sysfam"), contains("sysord")) %>% left_join(data, by = c("sysssfam_ranglibelle" = "micapt_taxon"))
  synthese_familles <- systematique %>% filter(!is.na(sysesp_ranglibelle) & !is.na(sysgen_ranglibelle) & !is.na(sysssfam_ranglibelle) & !is.na(sysfam_ranglibelle)) %>% select(contains("sysfam"), contains("sysord")) %>% left_join(data, by = c("sysfam_ranglibelle" = "micapt_taxon"))
  synthese_ordres <- systematique %>% filter(!is.na(sysesp_ranglibelle) & !is.na(sysgen_ranglibelle) & !is.na(sysssfam_ranglibelle) & !is.na(sysfam_ranglibelle) & !is.na(sysord_ranglibelle)) %>% select(contains("sysord")) %>% left_join(data, by = c("sysord_ranglibelle" = "micapt_taxon"))
  
  data <-
    synthese_especes %>% 
    full_join(synthese_genres) %>% 
    full_join(synthese_sousfamilles) %>% 
    full_join(synthese_familles) %>% 
    full_join(synthese_ordres) %>% 
    distinct()
    
  ##### Calcul de la famille au sens de l'IBGN #####
  data <-
    data %>%
    mutate(famille_sens_ibgn_libelle = ifelse(!is.na(sysfam_ranglibelle), sysfam_ranglibelle, sysord_ranglibelle)) %>% 
    mutate(gi_ibgn = ifelse(!is.na(sysfam_gi_ibgn), sysfam_gi_ibgn, sysord_gi_ibgn)) %>% 
    mutate(gi_cb2 = ifelse(!is.na(sysfam_gi_cb2), sysfam_gi_cb2, sysord_gi_cb2)) %>% 
    mutate(gi_ibl = ifelse(!is.na(sysgen_gi_ibl), sysgen_gi_ibl, sysfam_gi_ibl))

  return(data)
  
} # Fin de la fonction
