#' Tableau de suivi des projets
#'
#' Cette fonction permet d'établir le tableau de suivi des projets
#' @name projet.suivi
#' @param opentime_attendu Fichier OpenTime des détails prévisionnel
#' @param opentime_realise Fichier OpenTime des détails temps passé
#' @param filtrage_periode_projets_clos Durée (en mois) de conservation des projets soldés
#' @param filtrage_discussion Si \code{FALSE} (par défaut), on conserve les projets "En développement|En discussion|En engagement"
#' @param save Si \code{TRUE} (\code{FALSE} par défaut), exportation et ouverture du suivi en fichier xlsx
#' @import cli
#' @import openxlsx2
#' @import tidyverse
#' @export
#' @examples
#' projet.suivi("/Users/jean-baptistefagot/Downloads/export_détails_prévisionnel_20260311.xlsx", "/Users/jean-baptistefagot/Downloads/export_détails_temps_passé_20260311.xlsx")
#' previsionnel <- "/Users/jean-baptistefagot/Nextcloud/NAS-FD/FD39/Activité/Suivi_activité/opentime_prévisionnels/" %>% dir_ls() %>% last() ; realise <- "/Users/jean-baptistefagot/Nextcloud/NAS-FD/FD39/Activité/Suivi_activité/opentime_temps_réels/" %>% dir_ls() %>% last(); projet.suivi(previsionnel, realise)

projet.suivi <- function(
    opentime_attendu = NA_character_,
    opentime_realise = NA_character_,
    filtrage_periode_projets_clos = 24,
    filtrage_discussion = FALSE,
    save = T
    
)
{
  #### Test de cohérence ####
  test_opentime_attendu <- opentime_attendu %>% nchar()
  if(!(test_opentime_attendu %>% is.na())) {
    if(test_opentime_attendu == 0) opentime_attendu <- NA_character_}
  
  test_opentime_realise <- opentime_realise %>% nchar()
  if(!(test_opentime_realise %>% is.na())) {
    if(test_opentime_realise == 0) opentime_realise <- NA_character_}
  
  ##### Test #####
  if(is.na(opentime_attendu)) stop(glue("Aucun fichier {col_red('opentime_attendu')} à traiter"))
  if(is.na(opentime_realise)) stop(glue("Aucun fichier {col_red('opentime_realise')} à traiter"))
  
  #### Collecte des données ####
  opentime_attendu <- opentime_attendu %>% opentime.tps.attendu() %>% opentime.association.personnel() %>% BDD.format(Type = "Temps de travail") %>% opentime.projet()
  opentime_realise <- opentime_realise %>% opentime.tps.realise() %>% opentime.association.personnel() %>% BDD.format(Type = "Temps de travail") %>% opentime.projet()
  projets <- table.recuperation("projets_liste")
  evenements <- table.recuperation("projets_evenements")
  recapitulatif <- table.recuperation("tpstravail_recapitulatif")
  opentime <- table.recuperation("tpstravail_opentime")
  comptabilite <- table.recuperation("projets_comptabilite") %>% filter(prjcompta_presencelogiciel == "Présent") %>% filter(prjcompta_journal != "ODINV") %>% filter(prjcompta_journal != "OD") # Car "OD" et "ODINV" ne contient que des écritures d'inventaire <-> provisions, dont on ne souhaite pas tenir compte dans le bilan par projet.
  typologie_prestation <- table.recuperation("gestion_typologieprestation")
  cout_type_prestation <- table.recuperation("gestion_coutsunitaires") %>% left_join(typologie_prestation %>% select(id, gesttyppresta_libelle), by = c("gestctunit_prestation_id" = "id")) %>% select(id, gestctunit_prestation_id, gesttyppresta_libelle, everything())
  mo <- table.recuperation("gestion_mo") %>% filter(gestmo_activite == TRUE) %>% arrange(gestmo_intitulecourt)
  
  #### Nettoyage & reformatage ####
  ##### Sous-tableau projet #####
  liste_projets <-
    projets %>% 
    {if(filtrage_discussion == TRUE) filter(., !grepl("En développement|En discussion|En engagement", prjlst_etat)) else .} %>% # On ne conserve pas les projets qui sont en développement ou bien en discussion ou bien abandonné
    filter(!grepl("Abandonné", prjlst_etat)) %>% # On ne conserve pas les projets qui sont abandonnés
    filter(!grepl("Soldé", prjlst_etat)) %>% # On ne conserve pas les projets qui sont soldés mais on rajoute les moins anciens ligne suivante
    union(projets %>% filter(prjlst_etat == "Soldé") %>% filter(prjlst_datecloture > (today() - months(filtrage_periode_projets_clos)))) %>% # On supprime également les dossiers soldés depuis plus de filtrage_periode_projets_clos mois
    mutate(prjlst_etat = factor(prjlst_etat, levels = c("En cours", "En développement", "En discussion", "En engagement", "À facturer", "Facturé", "Soldé"))) %>% 
    arrange(prjlst_etat, prjlst_datefinattendue, prjlst_datelancement, prjlst_natureprojet, prjlst_pilote_id) %>% 
    mutate(prjlst_etat = as.character(prjlst_etat)) %>% 
    # left_join(operateurs %>% select(id, gestop_prenom) %>% rename(prjlst_pilote = gestop_prenom), by = c("prjlst_pilote_id" = "id")) %>% 
    formatage.personnel.prenom(colonne_entree = "prjlst_pilote_id") %>% rename(prjlst_pilote = personnel) %>% 
    select(prjlst_structure, id, prjlst_projet, prjlst_natureprojet, prjlst_pilote, prjlst_etat, prjlst_priorite, prjlst_datelancement, prjlst_datedebutattendue, prjlst_datefinattendue, prjlst_datecloture)
  # arrange(prjlst_priorite)
  
  ##### Comptabilité #####
  depenses_realisees_hors_tps_travail <-
    comptabilite %>% 
    filter(prjcompta_projet_id %in% liste_projets$id) %>% 
    filter(prjcompta_ecriture_sens == "Débit") %>% 
    filter(!grepl("^\\[Z\\]", prjcompta_ecriture_libelle)) %>% # Pour négliger les écritures n'étant pas incluses dans les frais facturables de projets
    group_by(prjcompta_projet_id) %>% 
    summarise(depenses_hors_tps_travail_realise = sum(prjcompta_ecriture_valeur))
  
  depenses_realisees_tps_travail <-
    recapitulatif %>% 
    filter(tpswrecap_projet %in% liste_projets$id) %>% 
    filter(tpswrecap_programmation == "Réalisé") %>% 
    filter(!is.na(tpswrecap_poste)) %>% 
    filter(!is.na(tpswrecap_jours)) %>%
    group_by(tpswrecap_projet) %>% 
    summarise(nb_hj_realise = sum(tpswrecap_jours),
              depenses_tps_travail_realise = sum(tpswrecap_argent)
    )
  
  # Date effective des données
  date_max_comptabilite <- 
    comptabilite %>% 
    filter(prjcompta_projet_id %in% liste_projets$id) %>% 
    filter(prjcompta_ecriture_sens == "Débit") %>% 
    filter(!grepl("^\\[Z\\]", prjcompta_ecriture_libelle)) %>% 
    arrange(desc(prjcompta_date)) %>% 
    head(1) %>% 
    select(prjcompta_date) %>% 
    pull()
  
  date_max_temps_travail <- 
    opentime %>% 
    filter(tpswot_presenceenligne == "Présent") %>% 
    arrange(desc(tpswot_date)) %>% 
    head(1) %>% 
    select(tpswot_date) %>% 
    pull()
  
  date_max <- min(date_max_comptabilite, date_max_temps_travail)
  
  ##### OpenTime #####
  opentime_attendu_v2 <-
    opentime_attendu %>% 
    filter(tpswot_domaine == "Projets") %>% 
    filter(!is.na(projet_id)) #%>% 
  # mutate(duree_j = ifelse(personnel %in% c("Valéry", "Philippe", "Stéphane"), duree_h / 7.8, duree_h / 7), .after = duree_h)
  
  opentime_realise_v2 <-
    opentime_realise %>% 
    filter(tpswot_domaine == "Projets") %>% 
    filter(!is.na(projet_id)) #%>% 
  # mutate(duree_j = ifelse(personnel %in% c("Valéry", "Philippe", "Stéphane"), duree_h / 7.8, duree_h / 7), .after = duree_h)
  
  ##### Recettes #####
  recettes <- 
    evenements %>% 
    filter(prjevnmt_projet_id %in% liste_projets$id) %>%
    filter(grepl("(?i)Autofinancement|Demande d'aide|Attribution|Solde|Acompte|acompte|Devis émis|Facture réglée", prjevnmt_type)) %>% # On ne conserve que les lignes d'autofinancement ou de demande d'aide ou de solde
    filter(prjevnmt_unite == "euros") %>% 
    left_join(mo %>% mutate(gestmo_intitulecourt = ifelse(is.na(gestmo_intitulecourt), gestmo_intitule, gestmo_intitulecourt)) %>% select(id, gestmo_intitulecourt), by = c('prjevnmt_partenaire_id' = 'id')) %>%
    mutate(prjevnmt_partenaire_id = gestmo_intitulecourt) %>%
    select(-gestmo_intitulecourt) %>% 
    group_by(prjevnmt_projet_id)
  
  #### Calcul ####
  ##### Sous-tableau état détaillé par partenaire par projet #####
  ###### Recettes ######
  recettes_attendues_par_partenaire <-
    recettes %>% 
    filter(grepl("Demande d'aide", prjevnmt_type)) %>% 
    arrange(desc(prjevnmt_date)) %>% 
    group_by(prjevnmt_projet_id, prjevnmt_partenaire_id) %>%
    slice_head() %>%
    # ungroup() %>%
    # group_by(prjevnmt_projet_id, prjevnmt_partenaire_id) %>% 
    summarise(recettes_par_partenaire_attendu = sum(prjevnmt_valeur))
  
  recettes_accordees_par_partenaire <-
    recettes %>% 
    filter(!grepl("Autofinancement|Facture réglée|Acompte|Solde|Réception d'un acompte", prjevnmt_type)) %>% 
    arrange(desc(prjevnmt_date)) %>% 
    group_by(prjevnmt_projet_id, prjevnmt_partenaire_id) %>%
    slice_head() %>%
    # ungroup() %>%
    # group_by(prjevnmt_projet_id, prjevnmt_partenaire_id) %>% 
    summarise(recettes_par_partenaire_accorde = sum(prjevnmt_valeur))
  
  recettes_realisees_par_partenaire <-
    recettes %>% 
    filter(grepl("Réception d'un acompte|Acompte|Solde|Facture réglée", prjevnmt_type)) %>% 
    group_by(prjevnmt_projet_id, prjevnmt_partenaire_id) %>% 
    summarise(recettes_realisees_par_partenaire = sum(prjevnmt_valeur)) %>% 
    rename(!!glue("recettes_realisees_par_partenaire au {date_max}") := recettes_realisees_par_partenaire)
  
  ss_tableau_bilan_par_partenaire <-
    recettes_attendues_par_partenaire %>% 
    full_join(recettes_accordees_par_partenaire, by = c("prjevnmt_projet_id", "prjevnmt_partenaire_id")) %>% 
    full_join(recettes_realisees_par_partenaire, by = c("prjevnmt_projet_id", "prjevnmt_partenaire_id"))
  
  ##### Sous-tableau bilan par projet #####
  montant_global_partie_1 <-
    # Cas des études, pour respecter le coût du dossier issu du cahier des charges (car si on ne considère que les attributions ça peut le modifier)
    recettes %>% 
    filter(!grepl("(?i)Solde|Attribution|Devis émis|Facture réglée|Acompte|acompte", prjevnmt_type)) %>% # On supprime les soldes pour calculer l'enveloppe globale
    summarise(montant_global_attendu = sum(prjevnmt_valeur))
  # Cas des devis/prestations AVEC attribution
  montant_global_partie_2 <-
    recettes %>% 
    filter(!(prjevnmt_projet_id %in% montant_global_partie_1$prjevnmt_projet_id)) %>% # On ne traite que les projets qui ne disposent encore pas d'un montant global
    filter(!grepl("Solde|Devis émis|Facture réglée", prjevnmt_type)) %>% # On supprime les soldes pour calculer l'enveloppe globale
    summarise(montant_global_attendu = sum(prjevnmt_valeur)) %>% 
    union(montant_global_partie_1)
  # Cas des devis/prestations SANS attribution
  montant_global <-
    recettes %>% 
    filter(!(prjevnmt_projet_id %in% montant_global_partie_2$prjevnmt_projet_id)) %>% # On ne traite que les projets qui ne disposent encore pas d'un montant global
    filter(!grepl("Solde", prjevnmt_type)) %>% # On supprime les soldes pour calculer l'enveloppe globale
    summarise(montant_global_attendu = sum(prjevnmt_valeur)) %>% 
    # view()
    union(montant_global_partie_2)
  
  recettes_attendues <-
    recettes_attendues_par_partenaire %>% 
    group_by(prjevnmt_projet_id) %>% 
    summarise(recettes_attendu = sum(recettes_par_partenaire_attendu))
  
  recettes_accordees <-
    recettes_accordees_par_partenaire %>% 
    group_by(prjevnmt_projet_id) %>% 
    summarise(recettes_accordees = sum(recettes_par_partenaire_accorde))
  
  recettes_realisees <-
    recettes_realisees_par_partenaire %>% 
    ungroup() %>% 
    rename(valeur = 3) %>% 
    group_by(prjevnmt_projet_id) %>%
    summarise(recettes_realisees_par_projet = sum(valeur))
  
  depenses_hors_tps_travail <-
    recapitulatif %>% 
    filter(tpswrecap_projet %in% liste_projets$id) %>% 
    filter(is.na(tpswrecap_poste)) %>% 
    filter(is.na(tpswrecap_personnel)) %>% 
    filter(is.na(tpswrecap_jours)) %>% 
    filter(!grepl("Forfait impression|Forfait déplacement", tpswrecap_detail)) %>% # On supprime les forfaits (déplacement, impression, etc.)
    # Ajout des valeurs d'argent incomplètes dans le cas de valeurs attendues si estimations basées sur cout_type_prestation
    left_join(cout_type_prestation %>% select(gesttyppresta_libelle, gestctunit_coutunitaire), by = c('tpswrecap_detail' = 'gesttyppresta_libelle')) %>%
    mutate(tpswrecap_argent = ifelse(is.na(tpswrecap_argent) & tpswrecap_programmation == "Attendu", gestctunit_coutunitaire*tpswrecap_quantite, tpswrecap_argent)) %>%
    select(-gestctunit_coutunitaire) %>% 
    group_by(tpswrecap_projet)
  
  depenses_attendues_hors_tps_travail <-
    depenses_hors_tps_travail %>% 
    filter(tpswrecap_programmation == "Attendu") %>% 
    summarise(depenses_hors_tps_travail_attendu = sum(tpswrecap_argent))
  
  nb_hj_attendus <-
    recapitulatif %>% 
    filter(tpswrecap_projet %in% liste_projets$id) %>% 
    filter(!is.na(tpswrecap_poste)) %>% 
    filter(!is.na(tpswrecap_jours)) %>% 
    filter(tpswrecap_programmation == "Attendu") %>% 
    group_by(tpswrecap_projet) %>% 
    summarise(nb_hj_attendu = sum(tpswrecap_jours))
  
  ##### OpenTime attendu #####
  ###### Dépenses attendues temps de travail ######
  depenses_opentime_tps_travail_attendu <-
    opentime_attendu_v2 %>% 
    filter(is.na(depenses_externes)) %>% 
    group_by(projet_id) %>%
    summarise(depenses_opentime_tps_travail_attendu = sum(montant))
  
  ###### Dépenses attendues hors temps de travail ######
  depenses_opentime_hors_tps_travail_attendu <-
    opentime_attendu_v2 %>% 
    filter(!is.na(depenses_externes)) %>% 
    group_by(projet_id) %>%
    summarise(depenses_opentime_hors_tps_travail_attendu = sum(montant))
  
  ###### Dépenses attendues hors temps de travail ######
  nb_hh_opentime_attendu <-
    opentime_attendu_v2 %>% 
    filter(is.na(depenses_externes)) %>% 
    group_by(projet_id) %>%
    summarise(nb_hh_opentime_attendu = sum(duree_h))
  
  ##### OpenTime réalisé #####
  ###### Dépenses réalisées temps de travail ######
  depenses_opentime_tps_travail_realise <-
    opentime_realise_v2 %>% 
    # filter(is.na(depenses_externes)) %>% 
    group_by(projet_id) %>%
    summarise(depenses_opentime_tps_travail_realise = sum(montant))
  
  ###### Dépenses réalisées hors temps de travail ######
  # depenses_opentime_hors_tps_travail_realise <-
  #   opentime_realise_v2 %>% 
  #   filter(!is.na(depenses_externes)) %>% 
  #   group_by(projet_id) %>%
  #   summarise(depenses_opentime_hors_tps_travail_realise = sum(montant))
  
  ###### Dépenses attendues hors temps de travail ######
  nb_hh_opentime_realise <-
    opentime_realise_v2 %>% 
    # filter(is.na(depenses_externes)) %>% 
    group_by(projet_id) %>%
    summarise(nb_hh_opentime_realise = sum(duree_h))
  
  #### Regroupement ####
  ss_tableau_bilan_par_projet <-
    liste_projets %>% 
    left_join(montant_global, by = c("id" = "prjevnmt_projet_id")) %>% 
    left_join(recettes_attendues, by = c("id" = "prjevnmt_projet_id")) %>% 
    left_join(recettes_accordees, by = c("id" = "prjevnmt_projet_id")) %>% 
    left_join(depenses_attendues_hors_tps_travail, by = c("id" = "tpswrecap_projet")) %>% 
    mutate(depenses_tps_travail_attendu = montant_global_attendu - depenses_hors_tps_travail_attendu) %>%
    left_join(nb_hj_attendus, by = c("id" = "tpswrecap_projet")) %>% 
    mutate(taux_financement_attendu = round(recettes_attendu/montant_global_attendu, 4)) %>% 
    mutate(resultat_attendu = recettes_attendu - depenses_hors_tps_travail_attendu) %>% 
    left_join(depenses_realisees_tps_travail, by = c("id" = "tpswrecap_projet")) %>% 
    left_join(depenses_realisees_hors_tps_travail, by = c("id" = "prjcompta_projet_id")) %>% 
    left_join(recettes_realisees, by = c("id" = "prjevnmt_projet_id")) %>% 
    left_join(depenses_opentime_tps_travail_attendu, by = c("id" = "projet_id")) %>% 
    left_join(depenses_opentime_tps_travail_realise, by = c("id" = "projet_id")) %>% 
    left_join(depenses_opentime_hors_tps_travail_attendu, by = c("id" = "projet_id")) %>% 
    left_join(nb_hh_opentime_attendu, by = c("id" = "projet_id")) %>% 
    left_join(nb_hh_opentime_realise, by = c("id" = "projet_id")) %>% 
    mutate(taux_financement_realise = round(recettes_realisees_par_projet/montant_global_attendu, 4), .after = taux_financement_attendu) %>% 
    rowwise() %>% 
    mutate(depenses_realise = sum(depenses_tps_travail_realise, depenses_hors_tps_travail_realise, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(depenses_realise = ifelse(depenses_realise == 0, NA, depenses_realise)) %>% 
    mutate(resultat_realise = recettes_realisees_par_projet - depenses_hors_tps_travail_realise) %>% 
    mutate(reste_a_realiser = montant_global_attendu - depenses_realise) #%>% 
  # mutate(montant_global_attendu = ifelse(is.na(montant_global_attendu), montant_global_attendu, glue("{montant_global_attendu} €"))) %>% 
  # mutate(recettes_attendu = ifelse(is.na(recettes_attendu), recettes_attendu, glue("{recettes_attendu} €"))) %>% 
  # mutate(depenses_hors_tps_travail_attendu = ifelse(is.na(depenses_hors_tps_travail_attendu), depenses_hors_tps_travail_attendu, glue("{depenses_hors_tps_travail_attendu} €"))) %>% 
  # mutate(taux_financement_attendu = ifelse(is.na(taux_financement_attendu), taux_financement_attendu, glue("{taux_financement_attendu} %"))) %>% 
  # mutate(resultat_attendu = ifelse(is.na(resultat_attendu), resultat_attendu, glue("{resultat_attendu} €"))) %>% 
  # mutate(depenses_realise = ifelse(is.na(depenses_realise), depenses_realise, glue("{depenses_realise} €"))) %>% 
  # mutate(reste_a_realiser = ifelse(is.na(reste_a_realiser), reste_a_realiser, glue("{reste_a_realiser} €")))
  
  ss_tableau_bilan_par_projet <-
    ss_tableau_bilan_par_projet %>% 
    rename(!!glue("depenses_realise au {date_max}") := depenses_realise)
  
  ## Jointure ##
  tableau_bilan_par_projet <-
    ss_tableau_bilan_par_projet %>% 
    left_join(ss_tableau_bilan_par_partenaire, by = c("id" = "prjevnmt_projet_id")) %>% 
    ## Ordre des colonnes ##
    relocate(recettes_accordees, .after = recettes_attendu) %>% 
    relocate(recettes_realisees_par_projet, .after = recettes_accordees) %>% 
    relocate(depenses_hors_tps_travail_realise, .after = depenses_hors_tps_travail_attendu) %>%
    relocate(depenses_tps_travail_attendu, .after = depenses_hors_tps_travail_realise) %>%
    relocate(depenses_tps_travail_realise, .after = depenses_tps_travail_attendu) %>%
    relocate(nb_hj_attendu, .after = depenses_tps_travail_realise) %>%
    relocate(nb_hj_realise, .after = nb_hj_attendu) %>%
    relocate(depenses_opentime_hors_tps_travail_attendu, .after = nb_hj_realise) %>%
    relocate(depenses_opentime_tps_travail_attendu, .after = reste_a_realiser) %>%
    relocate(depenses_opentime_tps_travail_realise, .after = depenses_opentime_tps_travail_attendu) %>%
    relocate(nb_hh_opentime_attendu, .after = depenses_opentime_tps_travail_realise) %>%
    relocate(nb_hh_opentime_realise, .after = nb_hh_opentime_attendu) %>%
    mutate(nb_hh_opentime_realise = ifelse(is.na(nb_hh_opentime_realise) & !is.na(nb_hh_opentime_attendu), 0, nb_hh_opentime_realise)) %>%
    mutate(nb_hh_opentime_ecart = nb_hh_opentime_attendu - nb_hh_opentime_realise, .after = nb_hh_opentime_realise) %>%
    relocate(resultat_realise, .after = resultat_attendu) %>%
    relocate(contains("taux_financement"), .after = montant_global_attendu) %>%
    relocate(contains("resultat"), .after = taux_financement_realise) %>%
    relocate(contains("opentime"), .after = recettes_realisees_par_projet) %>%
    relocate(reste_a_realiser, .before = depenses_opentime_hors_tps_travail_attendu) %>%
    relocate(!!glue("depenses_realise au {date_max}"), .after = depenses_opentime_hors_tps_travail_attendu) %>% 
    relocate(any_of(c("depenses_hors_tps_travail_attendu", "depenses_hors_tps_travail_realise", "depenses_tps_travail_attendu", "depenses_tps_travail_realise", "nb_hj_attendu", "nb_hj_realise")), .after = last_col())
  
  ## Version avec nettoyage des lignes doublonnées ##
  tableau_bilan_par_projet_nettoye <-
    tableau_bilan_par_projet %>% 
    mutate(prjlst_projet2 = ifelse(lag(prjlst_projet) == prjlst_projet, NA, prjlst_projet)) %>% 
    mutate(prjlst_projet2 = ifelse(row_number() == 1, prjlst_projet , prjlst_projet2)) %>%
    mutate(prjlst_projet = prjlst_projet2) %>%
    select(-prjlst_projet2) %>% 
    mutate(id = ifelse(is.na(prjlst_projet), NA, id)) %>% 
    mutate(prjlst_structure = ifelse(is.na(prjlst_projet), NA, prjlst_structure)) %>% 
    mutate(prjlst_etat = ifelse(is.na(prjlst_projet), NA, prjlst_etat)) %>%
    mutate(prjlst_natureprojet = ifelse(is.na(prjlst_projet), NA, prjlst_natureprojet)) %>% 
    mutate(prjlst_pilote = ifelse(is.na(prjlst_projet), NA, prjlst_pilote)) %>% 
    mutate(prjlst_priorite = ifelse(is.na(prjlst_projet), NA, prjlst_priorite)) %>% 
    mutate(prjlst_datedebutattendue = ifelse(is.na(prjlst_projet), NA, as.character(prjlst_datedebutattendue))) %>%
    mutate(prjlst_datedebutattendue = ymd(prjlst_datedebutattendue)) %>%
    mutate(prjlst_datefinattendue = ifelse(is.na(prjlst_projet), NA, as.character(prjlst_datefinattendue))) %>%
    mutate(prjlst_datefinattendue = ymd(prjlst_datefinattendue)) %>%
    mutate(montant_global_attendu = ifelse(is.na(prjlst_projet), NA, montant_global_attendu)) %>% 
    mutate(recettes_attendu = ifelse(is.na(prjlst_projet), NA, recettes_attendu)) %>% 
    mutate(recettes_accordees = ifelse(is.na(prjlst_projet), NA, recettes_accordees)) %>% 
    mutate(depenses_hors_tps_travail_attendu = ifelse(is.na(prjlst_projet), NA, depenses_hors_tps_travail_attendu)) %>% 
    mutate(depenses_tps_travail_attendu = ifelse(is.na(prjlst_projet), NA, depenses_tps_travail_attendu)) %>% 
    mutate(nb_hj_attendu = ifelse(is.na(prjlst_projet), NA, nb_hj_attendu)) %>% 
    mutate(taux_financement_attendu = ifelse(is.na(prjlst_projet), NA, taux_financement_attendu)) %>% 
    mutate(taux_financement_realise = ifelse(is.na(prjlst_projet), NA, taux_financement_realise)) %>% 
    mutate(resultat_attendu = ifelse(is.na(prjlst_projet), NA, resultat_attendu)) %>% 
    mutate(resultat_realise = ifelse(is.na(prjlst_projet), NA, resultat_realise)) %>% 
    mutate(nb_hj_realise = ifelse(is.na(prjlst_projet), NA, nb_hj_realise)) %>% 
    mutate(depenses_opentime_tps_travail_attendu = ifelse(is.na(prjlst_projet), NA, depenses_opentime_tps_travail_attendu)) %>% 
    mutate(depenses_opentime_tps_travail_realise = ifelse(is.na(prjlst_projet), NA, depenses_opentime_tps_travail_realise)) %>% 
    mutate(depenses_opentime_hors_tps_travail_attendu = ifelse(is.na(prjlst_projet), NA, depenses_opentime_hors_tps_travail_attendu)) %>% 
    mutate(nb_hh_opentime_attendu = ifelse(is.na(prjlst_projet), NA, nb_hh_opentime_attendu)) %>% 
    mutate(nb_hh_opentime_realise = ifelse(is.na(prjlst_projet), NA, nb_hh_opentime_realise)) %>% 
    mutate(nb_hh_opentime_ecart = ifelse(is.na(prjlst_projet), NA, nb_hh_opentime_ecart)) %>% 
    mutate(depenses_tps_travail_realise = ifelse(is.na(prjlst_projet), NA, depenses_tps_travail_realise)) %>% 
    mutate(depenses_hors_tps_travail_realise = ifelse(is.na(prjlst_projet), NA, depenses_hors_tps_travail_realise)) %>% 
    ## Renommage dynamique ##
    rename(!!glue("recettes_realisees au {date_max}") := recettes_realisees_par_projet) %>% 
    rename(temporaire = !!glue("recettes_realisees au {date_max}")) %>% 
    mutate(temporaire = ifelse(is.na(prjlst_projet), NA, temporaire)) %>% 
    rename(!!glue("recettes_realisees au {date_max}") := temporaire) %>% 
    rename(temporaire = !!glue("depenses_realise au {date_max}")) %>% 
    mutate(temporaire = ifelse(is.na(prjlst_projet), NA, temporaire)) %>% 
    rename(!!glue("depenses_realise au {date_max}") := temporaire) %>% 
    mutate(reste_a_realiser = ifelse(is.na(prjlst_projet), NA, reste_a_realiser))
  
  #### Export ####
  if(save == TRUE){
  ##### Nom des feuilles #####
  feuille_1 <- glue('Synthèse')
  feuille_2 <- glue('Synthèse_filtrable')
  
  ##### Création d'un classeur avec les données #####
  tableau_synthese_excel <-
    wb_workbook() %>% 
    wb_add_worksheet(sheet = feuille_1) %>% # Ajout des feuilles
    wb_add_worksheet(sheet = feuille_2) %>% # Ajout des feuilles
    wb_add_data(sheet = feuille_1, x = tableau_bilan_par_projet_nettoye, col_names = T, with_filter = T, na.strings = "") %>% 
    wb_add_data(sheet = feuille_2, x = tableau_bilan_par_projet, col_names = T, with_filter = T, na.strings = "")
  
  ##### Bloquage des panneaux #####
  tableau_synthese_excel <-
    tableau_synthese_excel %>% 
    wb_freeze_pane(feuille_1, first_active_row = 2, first_active_col = 4) %>% 
    wb_freeze_pane(feuille_2, first_active_row = 2, first_active_col = 4)
  
  ##### Style ligne de titre #####
  # tableau_synthese_excel <-
  #   tableau_synthese_excel %>% 
  #   wb_add_cell_style(sheet = feuille_1, dims = wb_dims(rows = 1, cols = 1:ncol(tps_travail_synthese)), horizontal = "center", vertical = "center", wrap_text = T) %>%
  #   wb_add_font(bold = T, dims = wb_dims(rows = 1, cols = 1:ncol(tps_travail_synthese)))
  
  ##### Style lignes : pourcentage/dates/monétaire #####
  # col_date <- c(8, 9, 10)
  col_pourcentage <- grep("taux", colnames(tableau_bilan_par_projet_nettoye))
  # col_pourcentage <- tableau_bilan_par_projet_nettoye %>% ungroup() %>% select(contains("taux")) %>% colnames() # avec le nom à proprement parler
  
  # col_date <- c(8, 9, 10)
  col_date <- grep("date", colnames(tableau_bilan_par_projet_nettoye))
  # col_date <- tableau_bilan_par_projet_nettoye %>% ungroup() %>% select(contains("date")) %>% colnames()
  
  col_monetaire <- grep("montant|resultat|depenses|recettes|reste_a_realiser", colnames(tableau_bilan_par_projet_nettoye))
  
  col_hh <- grep("nb_hh", colnames(tableau_bilan_par_projet_nettoye))
  col_hj <- grep("nb_hj", colnames(tableau_bilan_par_projet_nettoye))
  
  tableau_synthese_excel <-
    tableau_synthese_excel %>%
    wb_add_numfmt(sheet = feuille_1, dims = wb_dims(rows = 2:nrow(tableau_bilan_par_projet_nettoye), cols = col_pourcentage), "0 %") %>% # Style pourcentage
    wb_add_numfmt(sheet = feuille_2, dims = wb_dims(rows = 2:nrow(tableau_bilan_par_projet_nettoye), cols = col_pourcentage), "0 %") %>% # Style pourcentage
    wb_add_numfmt(sheet = feuille_1, dims = wb_dims(rows = 2:nrow(tableau_bilan_par_projet_nettoye), cols = col_date), "dd/mm/yyyy") %>%  # Style date
    wb_add_numfmt(sheet = feuille_2, dims = wb_dims(rows = 2:nrow(tableau_bilan_par_projet_nettoye), cols = col_date), "dd/mm/yyyy") %>%  # Style date
    wb_add_numfmt(sheet = feuille_1, dims = wb_dims(rows = 2:nrow(tableau_bilan_par_projet_nettoye), cols = col_monetaire), "#,##0.00 €") %>% # Style monétaire
    wb_add_numfmt(sheet = feuille_2, dims = wb_dims(rows = 2:nrow(tableau_bilan_par_projet_nettoye), cols = col_monetaire), "#,##0.00 €") %>% # Style monétaire
    wb_add_numfmt(sheet = feuille_1, dims = wb_dims(rows = 2:nrow(tableau_bilan_par_projet_nettoye), cols = col_hh), "0") %>% # Arrondi simple
    wb_add_numfmt(sheet = feuille_2, dims = wb_dims(rows = 2:nrow(tableau_bilan_par_projet_nettoye), cols = col_hh), "0") %>% # Arrondi simple
    wb_add_numfmt(sheet = feuille_1, dims = wb_dims(rows = 2:nrow(tableau_bilan_par_projet_nettoye), cols = col_hj), "0.0") %>%  # Arrondi simple
    wb_add_numfmt(sheet = feuille_2, dims = wb_dims(rows = 2:nrow(tableau_bilan_par_projet_nettoye), cols = col_hj), "0.0") # Arrondi simple
  
  ##### Style mise en forme automatique avec rampes de couleurs #####
  tableau_synthese_excel <-
    tableau_synthese_excel %>%
    wb_add_dxfs_style(name = "negStyle", font_color = wb_color(hex = "FF9C0006"), bg_fill = wb_color(hex = "FFFFC7CE")) %>% 
    wb_add_dxfs_style(name = "neuStyle", font_color = wb_color(hex = "000000"), bg_fill = wb_color(hex = "FFFFFF")) %>% 
    wb_add_dxfs_style(name = "posStyle", font_color = wb_color(hex = "FF006100"), bg_fill = wb_color(hex = "FFC6EFCE")) %>% 
    #   wb_add_conditional_formatting(dims = wb_dims(rows = 2:nrow(tps_travail_synthese), cols = 21), type = "colorScale", style = c("#50B9F3", "#BEF584", "orange")) %>% # temps_prop_realisé_total
    #   wb_add_conditional_formatting(dims = wb_dims(rows = 2:nrow(tps_travail_synthese), cols = 25), type = "colorScale", style = c("#50B9F3", "#BEF584", "orange")) %>% # temps_théorie_prop_2025
    #   wb_add_conditional_formatting(dims = wb_dims(rows = 2:nrow(tps_travail_synthese), cols = 27), type = "colorScale", style = c("#BEF584", "orange")) %>% # temps_restant_2025
    #   wb_add_conditional_formatting(dims = wb_dims(rows = 2:nrow(tps_travail_synthese), cols = 30), type = "colorScale", style = c("orange", "#BEF584")) # temps_ecart_avancement_reel_2025
    wb_add_conditional_formatting(sheet = feuille_1, dims = wb_dims(rows = 2:nrow(tableau_bilan_par_projet_nettoye), cols = 13), type = "expression", rule = 'M2=""', style = "neuStyle") %>% # taux attendu inconnu
    wb_add_conditional_formatting(sheet = feuille_2, dims = wb_dims(rows = 2:nrow(tableau_bilan_par_projet_nettoye), cols = 13), type = "expression", rule = 'M2=""', style = "neuStyle") %>% # taux attendu inconnu
    wb_add_conditional_formatting(sheet = feuille_1, dims = wb_dims(rows = 2:nrow(tableau_bilan_par_projet_nettoye), cols = 13), type = "expression", rule = 'M2<0.8', style = "negStyle") %>% # taux attendu faible
    wb_add_conditional_formatting(sheet = feuille_2, dims = wb_dims(rows = 2:nrow(tableau_bilan_par_projet_nettoye), cols = 13), type = "expression", rule = 'M2<0.8', style = "negStyle") %>% # taux attendu faible
    wb_add_conditional_formatting(sheet = feuille_1, dims = wb_dims(rows = 2:nrow(tableau_bilan_par_projet_nettoye), cols = 13), type = "expression", rule = 'M2>=1', style = "posStyle") %>% # taux attendu très important
    wb_add_conditional_formatting(sheet = feuille_2, dims = wb_dims(rows = 2:nrow(tableau_bilan_par_projet_nettoye), cols = 13), type = "expression", rule = 'M2>=1', style = "posStyle") %>% # taux attendu très important
    wb_add_conditional_formatting(sheet = feuille_1, dims = wb_dims(rows = 2:nrow(tableau_bilan_par_projet_nettoye), cols = 14), type = "expression", rule = 'N2=""', style = "neuStyle") %>% # taux réalisé inconnu
    wb_add_conditional_formatting(sheet = feuille_2, dims = wb_dims(rows = 2:nrow(tableau_bilan_par_projet_nettoye), cols = 14), type = "expression", rule = 'N2=""', style = "neuStyle") %>% # taux réalisé inconnu
    wb_add_conditional_formatting(sheet = feuille_1, dims = wb_dims(rows = 2:nrow(tableau_bilan_par_projet_nettoye), cols = 14), type = "expression", rule = "N2>M2", style = "posStyle") %>% # taux réalisé meilleur qu'attendu
    wb_add_conditional_formatting(sheet = feuille_2, dims = wb_dims(rows = 2:nrow(tableau_bilan_par_projet_nettoye), cols = 14), type = "expression", rule = "N2>M2", style = "posStyle") %>% # taux réalisé meilleur qu'attendu
    wb_add_conditional_formatting(sheet = feuille_1, dims = wb_dims(rows = 2:nrow(tableau_bilan_par_projet_nettoye), cols = 25), type = "colorScale", style = c("#63BE7B", "#FFEB84", "#F8696B")) %>% # nb hh attendu rampe
    wb_add_conditional_formatting(sheet = feuille_2, dims = wb_dims(rows = 2:nrow(tableau_bilan_par_projet_nettoye), cols = 25), type = "colorScale", style = c("#63BE7B", "#FFEB84", "#F8696B")) %>% # nb hh attendu rampe
    wb_add_conditional_formatting(sheet = feuille_1, dims = wb_dims(rows = 2:nrow(tableau_bilan_par_projet_nettoye), cols = 26), type = "expression", rule = 'Z2=""', style = "neuStyle") %>% # nb hh réalisé neutre
    wb_add_conditional_formatting(sheet = feuille_2, dims = wb_dims(rows = 2:nrow(tableau_bilan_par_projet_nettoye), cols = 26), type = "expression", rule = 'Z2=""', style = "neuStyle") %>% # nb hh réalisé neutre
    wb_add_conditional_formatting(sheet = feuille_1, dims = wb_dims(rows = 2:nrow(tableau_bilan_par_projet_nettoye), cols = 26), type = "expression", rule = "Z2>Y2", style = "negStyle") %>% # nb hh réalisé trop important
    wb_add_conditional_formatting(sheet = feuille_2, dims = wb_dims(rows = 2:nrow(tableau_bilan_par_projet_nettoye), cols = 26), type = "expression", rule = "Z2>Y2", style = "negStyle") %>%  # nb hh réalisé trop important
    wb_add_conditional_formatting(sheet = feuille_1, dims = wb_dims(rows = 2:nrow(tableau_bilan_par_projet_nettoye), cols = 27), type = "colorScale", style = c("#63BE7B", "#FFEB84", "#F8696B")) %>% # nb hh écart rampe
    wb_add_conditional_formatting(sheet = feuille_2, dims = wb_dims(rows = 2:nrow(tableau_bilan_par_projet_nettoye), cols = 27), type = "colorScale", style = c("#63BE7B", "#FFEB84", "#F8696B")) # nb hh écart rampe
  # wb_add_conditional_formatting(dims = wb_dims(rows = 2:nrow(tableau_bilan_par_projet_nettoye), cols = 13), type = "expression", rule = 'ET(M2>L2;M2<>"")', style = "posStyle") # taux réalisé meilleur qu'attendu
  # wb_add_conditional_formatting(dims = wb_dims(rows = 2:nrow(tableau_bilan_par_projet_nettoye), cols = 13), type = "expression", rule = 'ET(M2>L2;SI_NON_VIDE(M2))', style = "posStyle") # taux réalisé meilleur qu'attendu
  # wb_add_conditional_formatting(dims = wb_dims(rows = 2:nrow(tableau_bilan_par_projet_nettoye), cols = 13), type = "expression", rule = "ET(M2>L2;NON(ESTVIDE(M2)))", style = "posStyle") # taux réalisé meilleur qu'attendu
  
  ##### Largeurs de cellules #####
  tableau_synthese_excel <-
    tableau_synthese_excel %>% 
    wb_set_col_widths(sheet = feuille_1, cols = 1, widths = 4) %>% # Colonne d'id de la structure
    wb_set_col_widths(sheet = feuille_2, cols = 1, widths = 4) %>% 
    wb_set_col_widths(sheet = feuille_1, cols = 2:(ncol(tableau_bilan_par_projet_nettoye) - 6), widths = "auto") %>% 
    wb_set_col_widths(sheet = feuille_2, cols = 2:(ncol(tableau_bilan_par_projet_nettoye) - 6), widths = "auto") %>% 
    wb_set_col_widths(sheet = feuille_1, cols = 3, widths = 100) %>% # Colonne d'intitulé du projet
    wb_set_col_widths(sheet = feuille_2, cols = 3, widths = 100) %>% 
    wb_set_col_widths(sheet = feuille_1, cols = (ncol(tableau_bilan_par_projet_nettoye) - 5):ncol(tableau_bilan_par_projet_nettoye), widths = "auto", hidden = TRUE) %>% 
    wb_set_col_widths(sheet = feuille_2, cols = (ncol(tableau_bilan_par_projet_nettoye) - 5):ncol(tableau_bilan_par_projet_nettoye), widths = "auto", hidden = TRUE)
  
  ##### Enregistrement du classeur #####
  fichier <- glue('{today()}_Récapitulatif_avancement_dossiers.xlsx')
  tableau_synthese_excel %>% wb_save(fichier, overwrite = TRUE)
  }
  
  #### Sortie ####
  return(tableau_bilan_par_projet)
  
  #### Affichage ####
  if(interactive() & save == TRUE) tableau_synthese_excel$open()

} # Fin de la fonction
