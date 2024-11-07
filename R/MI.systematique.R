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
  
  #### Contexte ####
  nrow_avant <- data %>% nrow()
  
  #### Collecte des données ####
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
  systematique_1 <- 
    ordres_reference %>% 
    left_join(familles_reference %>% rename(sysfam_id = id), by = c("id" = "sysfam_ordre_id"))
  
  systematique_2 <-
    systematique_1 %>% 
    left_join(sous_familles_reference %>% rename(sysssfam_id = id), by = c("sysfam_id" = "sysssfam_famille_id"))
  
  systematique_3 <-
    systematique_2 %>% 
    filter(!is.na(sysssfam_ranglibelle)) %>% 
    left_join(genres_reference %>% rename(sysgen_id = id), by = c("sysssfam_id" = "sysgen_sousfamille_id"))
  
  systematique_3_bis <-
    systematique_1 %>% 
    left_join(genres_reference %>% filter(is.na(sysgen_sousfamille_id)) %>% rename(sysgen_id = id), by = c("sysfam_id" = "sysgen_famille_id"))
  
  systematique_4 <-
    systematique_3 %>% 
    bind_rows(systematique_3_bis) %>% 
    left_join(especes_reference %>% rename(sysesp_id = id), by = c("sysgen_id" = "sysesp_genre_id"))
  
  systematique_complete <-
    ordres_reference %>% 
    bind_rows(systematique_1) %>% 
    bind_rows(systematique_2) %>% 
    bind_rows(systematique_3) %>% 
    bind_rows(systematique_3_bis) %>% 
    bind_rows(systematique_4) %>% 
    distinct() %>% 
    rename(sysord_id = id)
  
  #### Travail sur les captures ####
  ##### Jointure entre les taxons des captures et la systématique #####
  data_v2 <-
    data %>% 
    select(id, micapt_taxon) %>% 
    rename(id_capture = id)
  
  synthese_especes <- data_v2 %>% left_join(systematique_complete, by = c("micapt_taxon" = "sysesp_ranglibelle")) %>% filter(!is.na(sysesp_id))
  data_v2_sans_especes <- data_v2 %>% filter(!(id_capture %in% synthese_especes$id_capture))
  synthese_genres <- data_v2_sans_especes %>% left_join(systematique_complete %>% filter(is.na(sysesp_id)), by = c("micapt_taxon" = "sysgen_ranglibelle")) %>% filter(!is.na(sysgen_id))
  data_v2_sans_genres <- data_v2_sans_especes %>% filter(!(id_capture %in% synthese_genres$id_capture))
  synthese_ss_familles <- data_v2_sans_genres %>% left_join(systematique_complete %>% filter(is.na(sysgen_id)), by = c("micapt_taxon" = "sysssfam_ranglibelle")) %>% filter(!is.na(sysssfam_id))
  data_v2_sans_ss_familles <- data_v2_sans_genres %>% filter(!(id_capture %in% synthese_ss_familles$id_capture))
  synthese_familles <- data_v2_sans_ss_familles %>% left_join(systematique_complete %>% filter(is.na(sysssfam_id) & is.na(sysgen_id)), by = c("micapt_taxon" = "sysfam_ranglibelle")) %>% filter(!is.na(sysfam_id))
  data_v2_sans_familles <- data_v2_sans_ss_familles %>% filter(!(id_capture %in% synthese_familles$id_capture))
  synthese_ordres <- data_v2_sans_familles %>% left_join(systematique_complete %>% filter(is.na(sysfam_id)) %>% mutate(sysord_ranglibelle_bis = sysord_ranglibelle), by = c("micapt_taxon" = "sysord_ranglibelle_bis")) %>% filter(!is.na(sysord_id))
  data_v2_sans_ordres <- data_v2_sans_familles %>% filter(!(id_capture %in% synthese_ordres$id_capture))

  data_v3 <-
    synthese_especes %>% 
    bind_rows(synthese_genres) %>% 
    bind_rows(synthese_ss_familles) %>% 
    bind_rows(synthese_familles) %>% 
    bind_rows(synthese_ordres)
  
  ##### Jointure entre les captures systématisées et l'ensembles des informations relatives aux captures #####
  data_v4 <-
    data_v3 %>% 
    rename(id = id_capture) %>% 
    # left_join(data)
    left_join(data,by = join_by(micapt_taxon, id))

  ##### Calcul de la famille au sens de l'IBGN #####
  data_v5 <-
    data_v4 %>%
    mutate(famille_sens_ibgn_libelle = ifelse(!is.na(sysfam_ranglibelle), sysfam_ranglibelle, sysord_ranglibelle), .after = "micapt_taxon") %>% 
    mutate(gi_ibgn = ifelse(!is.na(sysfam_gi_ibgn), sysfam_gi_ibgn, sysord_gi_ibgn), .after = "famille_sens_ibgn_libelle") %>% 
    mutate(gi_cb2 = ifelse(!is.na(sysfam_gi_cb2), sysfam_gi_cb2, sysord_gi_cb2), .after = "gi_ibgn") %>% 
    mutate(gi_ibl = ifelse(!is.na(sysgen_gi_ibl), sysgen_gi_ibl, sysfam_gi_ibl), .after = "gi_cb2")

  #### Vérification ####
  if(data_v2_sans_ordres %>% nrow() != 0) stop("Présence de taxons sans jointure")
  nrow_apres <- data_v5 %>% nrow()
  if(nrow_apres != nrow_avant) stop("Incohérence dans le nombre de lignes avant et après traitement")
  
  #### Sortie ####
  
  return(data_v5)
  
} # Fin de la fonction
