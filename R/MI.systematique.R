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
  
  ## Connexion à la BDD ##
  dbMI <- BDD.ouverture(Type = "Macroinvertébrés")
  
  ## Récupération des données ##
  especes_reference <- tbl(dbD, dbplyr::in_schema("fd_referentiels", "systematique_especes")) %>% select(-contains('modif')) %>% arrange(sysesp_ranglibelle) %>% collect(n = Inf)
  genres_reference <- tbl(dbD, dbplyr::in_schema("fd_referentiels", "systematique_genres")) %>% select(-contains('modif')) %>% arrange(sysgen_ranglibelle) %>% collect(n = Inf)
  sous_familles_reference <- tbl(dbD, dbplyr::in_schema("fd_referentiels", "systematique_sousfamilles")) %>% select(-contains('modif')) %>% arrange(sysssfam_ranglibelle) %>% collect(n = Inf)
  familles_reference <- tbl(dbD, dbplyr::in_schema("fd_referentiels", "systematique_familles")) %>% select(-contains('modif')) %>% arrange(sysfam_ranglibelle) %>% collect(n = Inf)
  ordres_reference <- tbl(dbD, dbplyr::in_schema("fd_referentiels", "systematique_ordres")) %>% select(-contains('modif')) %>% arrange(sysord_ranglibelle) %>% collect(n = Inf)
  
  ## Fermeture de la BDD ##
  # DBI::dbDisconnect(dbMI)
  
  # Assemblage des morceaux de systématique
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
  
  # Travail sur les captures #
  #if(all(colnames(data) %in% colnames(Captures))) {
    
    # Ajout de la systématique #
  synthese_especes <- merge(systematique, data, by.x="sysesp_ranglibelle", by.y="micapt_taxon")
  synthese_genres <- merge(select(systematique, 6:28), data, by.x="sysgen_ranglibelle", by.y="micapt_taxon")
  synthese_sousfamilles <- merge(select(systematique, 11:28), data, by.x="sysssfam_ranglibelle", by.y="micapt_taxon")
  synthese_familles <- merge(select(systematique, 14:28), data, by.x="sysfam_ranglibelle", by.y="micapt_taxon")
  synthese_ordres <- merge(select(systematique, 22:28), data, by.x="sysord_ranglibelle", by.y="micapt_taxon")

  data <- 
    synthese_especes %>% 
    full_join(synthese_genres, by = c("sysgen_ranglibelle", "sysgen_rangsandre", "sysgen_sousfamille_id", "sysgen_remarques", "sysssfam_ranglibelle", "sysssfam_rangsandre", "sysssfam_remarques", "famille_id", "sysfam_ranglibelle", "sysfam_rangsandre", "sysfam_ordre_id", "sysfam_gi_cb2", "sysfam_gi_ibgn", "sysfam_remarques", "sysord_ranglibelle", "sysord_rangsandre", "sysord_rangsuperieur_id", "sysord_affichagesensibilite", "id", "micapt_miprlvt_id", "micapt_abondance", "micapt_typeabondance", "micapt_volumeabondance", "micapt_stade", "micapt_sexe", "micapt_remarques", "miprlvt_miop_id", "miprlvt_numech_dce", "miprlvt_numech_mag20", "miprlvt_numech_commun", "miprlvt_mihabref_id_substrat", "miprlvt_mihabref_id_vitesse", "miprlvt_mihabref_id_hauteur", "miprlvt_phasedce", "miprlvt_ibgn", "miprlvt_intensitecolmatage", "miprlvt_stabilite", "miprlvt_naturevegetation", "miprlvt_abondancevegetation", "miprlvt_remarques", "miop_coderhj", "miop_codemo", "miop_codesie", "miop_date", "miop_mo_id", "miop_moe_id", "miop_operateurs", "miop_bibliographie", "miop_coord_x", "miop_coord_y", "miop_amont_coord_x", "miop_amont_coord_y", "miop_aval_coord_x", "miop_aval_coord_y", "miop_longueur", "miop_largeur_mouillee", "miop_largeur_plein_bord", "miop_remarques", "geom")) %>% 
    full_join(synthese_sousfamilles, by = c("sysssfam_ranglibelle", "sysssfam_rangsandre", "sysssfam_remarques", "famille_id", "sysfam_ranglibelle", "sysfam_rangsandre", "sysfam_ordre_id", "sysfam_gi_cb2", "sysfam_gi_ibgn", "sysfam_remarques", "sysord_ranglibelle", "sysord_rangsandre", "sysord_rangsuperieur_id", "sysord_affichagesensibilite", "id", "micapt_miprlvt_id", "micapt_abondance", "micapt_typeabondance", "micapt_volumeabondance", "micapt_stade", "micapt_sexe", "micapt_remarques", "miprlvt_miop_id", "miprlvt_numech_dce", "miprlvt_numech_mag20", "miprlvt_numech_commun", "miprlvt_mihabref_id_substrat", "miprlvt_mihabref_id_vitesse", "miprlvt_mihabref_id_hauteur", "miprlvt_phasedce", "miprlvt_ibgn", "miprlvt_intensitecolmatage", "miprlvt_stabilite", "miprlvt_naturevegetation", "miprlvt_abondancevegetation", "miprlvt_remarques", "miop_coderhj", "miop_codemo", "miop_codesie", "miop_date", "miop_mo_id", "miop_moe_id", "miop_operateurs", "miop_bibliographie", "miop_coord_x", "miop_coord_y", "miop_amont_coord_x", "miop_amont_coord_y", "miop_aval_coord_x", "miop_aval_coord_y", "miop_longueur", "miop_largeur_mouillee", "miop_largeur_plein_bord", "miop_remarques", "geom")) %>% 
    full_join(synthese_familles, by = c("famille_id", "sysfam_ranglibelle", "sysfam_rangsandre", "sysfam_ordre_id", "sysfam_gi_cb2", "sysfam_gi_ibgn", "sysfam_remarques", "sysord_ranglibelle", "sysord_rangsandre", "sysord_rangsuperieur_id", "sysord_affichagesensibilite", "id", "micapt_miprlvt_id", "micapt_abondance", "micapt_typeabondance", "micapt_volumeabondance", "micapt_stade", "micapt_sexe", "micapt_remarques", "miprlvt_miop_id", "miprlvt_numech_dce", "miprlvt_numech_mag20", "miprlvt_numech_commun", "miprlvt_mihabref_id_substrat", "miprlvt_mihabref_id_vitesse", "miprlvt_mihabref_id_hauteur", "miprlvt_phasedce", "miprlvt_ibgn", "miprlvt_intensitecolmatage", "miprlvt_stabilite", "miprlvt_naturevegetation", "miprlvt_abondancevegetation", "miprlvt_remarques", "miop_coderhj", "miop_codemo", "miop_codesie", "miop_date", "miop_mo_id", "miop_moe_id", "miop_operateurs", "miop_bibliographie", "miop_coord_x", "miop_coord_y", "miop_amont_coord_x", "miop_amont_coord_y", "miop_aval_coord_x", "miop_aval_coord_y", "miop_longueur", "miop_largeur_mouillee", "miop_largeur_plein_bord", "miop_remarques", "geom")) %>% 
    full_join(synthese_ordres, by = c("sysord_ranglibelle", "sysord_rangsandre", "sysord_rangsuperieur_id", "sysord_affichagesensibilite", "id", "micapt_miprlvt_id", "micapt_abondance", "micapt_typeabondance", "micapt_volumeabondance", "micapt_stade", "micapt_sexe", "micapt_remarques", "miprlvt_miop_id", "miprlvt_numech_dce", "miprlvt_numech_mag20", "miprlvt_numech_commun", "miprlvt_mihabref_id_substrat", "miprlvt_mihabref_id_vitesse", "miprlvt_mihabref_id_hauteur", "miprlvt_phasedce", "miprlvt_ibgn", "miprlvt_intensitecolmatage", "miprlvt_stabilite", "miprlvt_naturevegetation", "miprlvt_abondancevegetation", "miprlvt_remarques", "miop_coderhj", "miop_codemo", "miop_codesie", "miop_date", "miop_mo_id", "miop_moe_id", "miop_operateurs", "miop_bibliographie", "miop_coord_x", "miop_coord_y", "miop_amont_coord_x", "miop_amont_coord_y", "miop_aval_coord_x", "miop_aval_coord_y", "miop_longueur", "miop_largeur_mouillee", "miop_largeur_plein_bord", "miop_remarques", "geom")) %>% 
    distinct()
    
  # Calcul de la famille au sens de l'IBGN
  data <-
    data %>%
    mutate(famille_sens_ibgn_libelle = ifelse(!is.na(sysfam_ranglibelle), sysfam_ranglibelle, sysord_ranglibelle)) %>% 
    mutate(gi_ibgn = ifelse(!is.na(sysfam_gi_ibgn), sysfam_gi_ibgn, sysord_gi_ibgn)) %>% 
    mutate(gi_cb2 = ifelse(!is.na(sysfam_gi_cb2), sysfam_gi_cb2, sysord_gi_cb2)) %>% 
    mutate(gi_ibl = ifelse(!is.na(sysgen_gi_ibl.y), sysgen_gi_ibl.y, sysfam_gi_ibl.y.y))
  
  #}
  
  return(data)
  
} # Fin de la fonction