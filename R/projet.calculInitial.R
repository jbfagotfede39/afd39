#' Calcul initial des coûts de projet
#'
#' Cette fonction permet de calculer les coûts d'un projet au moment de son montage. Elle s'appuie sur le format saisi dans la table tpstravail_recapitulatif et réalise les conversions à partir des volumes unitaires, et ordonne les sujets
#' @name projet.calculInitial
#' @param NomProjet Nom du dataframe contenant les données "attendues" à traiter, dans le format de tpstravail_recapitulatif
#' @importFrom dplyr select
#' @import tidyverse
#' @export
#' @examples
#' RecapDataToAdd <- projet.calculInitial(NomProjet)

projet.calculInitial <- function(
  NomProjet
)
{
  #### Vérifications ####
  if(dim(NomProjet)[1] == 0) stop("Saisir un dataframe en entrée")
  if(dim(distinct(NomProjet, tpswrecap_projet))[1] != 1) stop("Plusieurs projets dans le dataframe d'entrée")
  if(dim(distinct(NomProjet, tpswrecap_programmation))[1] != 1) stop("Plusieurs statuts de programmation dans le dataframe d'entrée")
  
  dbD <- BDD.ouverture("Data")
  RecapTpsW <- tbl(dbD, in_schema("fd_production", "tpstravail_recapitulatif")) %>% collect()
  CoutAnnuel <- tbl(dbD, in_schema("fd_referentiels", "gestion_coutsannuels")) %>% collect()
  CoutTypePrestation <- tbl(dbD, in_schema("fd_referentiels", "gestion_coutsunitaires")) %>% collect()
  Poste <- tbl(dbD, in_schema("fd_referentiels", "gestion_postes")) %>% collect()
  Operateurs <- tbl(dbD, in_schema("fd_referentiels", "gestion_operateurs")) %>% collect()
  Projets <- tbl(dbD, in_schema("fd_production", "tpstravail_projets")) %>% collect() %>% arrange(tpswprj_etat, tpswprj_datelancement)
  TypologiePrestation <- tbl(dbD, in_schema("fd_referentiels", "gestion_typologieprestation")) %>% collect()
  
  DBI::dbDisconnect(dbD)
  
  if(all(colnames(NomProjet) != colnames(RecapTpsW))) stop("Dataframe d'entrée différent de tpstravail_recapitulatif")
  if(CoutAnnuel %>% filter(gestctan_annee == year(now())) %>% filter(gestctan_type == "Estimé N-1") %>% select(gestctan_poste_id, gestctan_coutjournaliermajore) %>% nrow() == 0) stop(paste0("Pas de coûts annuels estimés pour l'année ",year(now())))
  
  ##### Préparation ####
  NomProjet <-
    NomProjet %>% 
    # On remplace les libellés de prestations par leur identifiant
    left_join((TypologiePrestation %>% select(id, gesttyppresta_libelle) %>% rename(tpswrecap_detail_id = id)), by = c("tpswrecap_detail" = "gesttyppresta_libelle")) %>% 
    mutate(tpswrecap_detail = tpswrecap_detail_id) %>% 
    select(-tpswrecap_detail_id, -contains("_modif")) %>% 
    # On remplace les libellés de poste par leur identifiant
    left_join((Poste %>% select(id, gestpost_poste_libelle) %>% rename(tpswrecap_poste_id = id)), by = c("tpswrecap_poste" = "gestpost_poste_libelle")) %>% 
    mutate(tpswrecap_poste = tpswrecap_poste_id) %>% 
    select(-tpswrecap_poste_id, -contains("_modif")) %>% 
    # On remplace les libellés des opérateurs par leur identifiant
    left_join((Operateurs %>% select(id, gestop_prenom, gestop_nom) %>% mutate(gestop_libelle = paste0(gestop_prenom, " ", gestop_nom)) %>% rename(tpswrecap_personnel_id = id)), by = c("tpswrecap_personnel" = "gestop_libelle")) %>% 
    mutate(tpswrecap_personnel = tpswrecap_personnel_id) %>% 
    select(-tpswrecap_personnel_id, -contains("_modif"), -contains("gestop"))
  
  if(NomProjet %>% filter(is.na(tpswrecap_detail)) %>% nrow() > 0) stop("Il y a des tpswrecap_detail non identifiés par leur identifiant")
  
  NomProjetFJPPMA <-
    NomProjet %>% 
    filter(tpswrecap_moe == "FJPPMA")
  
  ##### Calculs ####
  RecapDataToAdd <-
    NomProjetFJPPMA %>% 
    select(-tpswrecap_coutunitaire) %>% 
    # On prend en premier les tâches déjà définies dans CoutTypePrestation #
    bind_rows(CoutTypePrestation %>% filter(gestctunit_prestation_id %in% NomProjetFJPPMA$tpswrecap_detail) %>% rename(tpswrecap_detail = gestctunit_prestation_id) %>% rename(Temps = gestctunit_temps) %>% select(-id,-contains("_modif")) %>% left_join(NomProjetFJPPMA %>% filter(is.na(tpswrecap_argent)) %>% select(tpswrecap_detail, tpswrecap_quantite, -contains("_modif")), by = "tpswrecap_detail")) %>%
    # On joint avec les coûts unitaires par poste/opérateur via une clé #
    mutate(clepostepersonnel = paste0(tpswrecap_poste, "-", tpswrecap_personnel)) %>% 
    left_join(CoutAnnuel %>% filter(gestctan_annee == year(now())) %>% filter(gestctan_type == "Estimé N-1") %>% mutate(clepostepersonnel = paste0(gestctan_poste_id, "-", gestctan_operateur_id)) %>% select(clepostepersonnel, gestctan_coutjournaliermajore) %>% rename(CoutUnitaire = gestctan_coutjournaliermajore), by = "clepostepersonnel") %>%
    # Cas du coût unitaire des agents de développement, différents en fonction du personnel : on prend la valeur max
    mutate(CoutUnitaire = ifelse(tpswrecap_poste == 1 & is.na(tpswrecap_personnel), CoutAnnuel %>% filter(gestctan_annee == year(now()) & gestctan_type == "Estimé N-1" & gestctan_poste_id == 1) %>% summarise(CoutUnitaire = max(gestctan_coutjournaliermajore)) %>% pull(), CoutUnitaire)) %>% 
    # On regroupe les coûts unitaires #
    mutate(CoutUnitaire = ifelse(is.na(CoutUnitaire), gestctunit_coutunitaire, CoutUnitaire)) %>%
    select(-gestctunit_coutunitaire) %>% 
    # Différents tests de cohérence #
    mutate(tpswrecap_programmation = ifelse(dim(distinct(NomProjetFJPPMA, tpswrecap_programmation))[1] == 1, as.character(distinct(NomProjetFJPPMA, tpswrecap_programmation)), "STOP")) %>% 
    mutate(tpswrecap_natureprojet = ifelse(dim(distinct(NomProjetFJPPMA, tpswrecap_natureprojet))[1] == 1, as.character(distinct(NomProjetFJPPMA, tpswrecap_natureprojet)), "STOP")) %>% 
    mutate(tpswrecap_moe = ifelse(dim(distinct(NomProjetFJPPMA, tpswrecap_moe))[1] == 1, as.character(distinct(NomProjetFJPPMA, tpswrecap_moe)), "STOP")) %>% 
    mutate(tpswrecap_projet = ifelse(dim(distinct(NomProjetFJPPMA, tpswrecap_projet))[1] == 1, as.character(distinct(NomProjetFJPPMA, tpswrecap_projet)), "STOP")) %>% 
    filter(!(!is.na(tpswrecap_quantite) & is.na(CoutUnitaire) & is.na(tpswrecap_jours) & is.na(tpswrecap_argent))) %>%
    mutate(tpswrecap_quantitepersonnel = ifelse(is.na(tpswrecap_argent) & is.na(tpswrecap_quantitepersonnel) & !is.na(tpswrecap_jours), 1, tpswrecap_quantitepersonnel)) %>% # Si tpswrecap_argent et tpswrecap_quantitepersonnel vides et tpswrecap_jours rempli, alors un seul bonhomme
    mutate(tpswrecap_argent = ifelse(is.na(tpswrecap_argent), tpswrecap_jours * CoutUnitaire * tpswrecap_quantitepersonnel * tpswrecap_quantite, tpswrecap_argent)) %>% # pour les tâches avec quantité
    mutate(tpswrecap_argent = ifelse(is.na(tpswrecap_argent), tpswrecap_jours * CoutUnitaire * tpswrecap_quantitepersonnel, tpswrecap_argent)) %>% # pour les tâches sans quantité
    mutate(tpswrecap_argent = ifelse(is.na(tpswrecap_argent), tpswrecap_quantite * CoutUnitaire, tpswrecap_argent)) %>% # pour les objets sans personnel
    mutate(Temps = ifelse(!is.na(tpswrecap_jours) & !is.na(tpswrecap_quantite) & !is.na(tpswrecap_quantitepersonnel), tpswrecap_jours * tpswrecap_quantite * tpswrecap_quantitepersonnel, NA)) %>% 
    mutate(Temps = ifelse(!is.na(tpswrecap_jours) & is.na(tpswrecap_quantite) & !is.na(tpswrecap_quantitepersonnel), tpswrecap_jours * tpswrecap_quantitepersonnel, Temps)) %>% 
    # Cas d'un item présent en prestation et ajouté en quantité dans le projet -> rebascule en nb de jours + suppression de la quantité
    mutate(tpswrecap_jours = ifelse(is.na(tpswrecap_jours) & !is.na(tpswrecap_poste) & !is.na(tpswrecap_quantite) & tpswrecap_moe == "FJPPMA", tpswrecap_quantite, tpswrecap_jours)) %>%
    mutate(tpswrecap_quantite = ifelse(!is.na(tpswrecap_jours) & !is.na(tpswrecap_poste) & !is.na(tpswrecap_quantite) & tpswrecap_moe == "FJPPMA" & tpswrecap_jours == tpswrecap_quantite, NA, tpswrecap_quantite)) %>%
    # On remet ce qui n'est pas MOE FJPPMA
    rename(tpswrecap_coutunitaire = CoutUnitaire) %>% 
    select(-clepostepersonnel, -gestctunit_remarques, -gestctunit_poste_id, -gestctunit_quantitepersonnel, -Temps) %>% 
    mutate(tpswrecap_projet = as.numeric(tpswrecap_projet)) %>% 
    dplyr::union(
      # On met les coûts unitaires et les sous-totaux pour ce qui n'est pas MOE FJPPMA
      NomProjet %>% filter(tpswrecap_programmation == "Attendu") %>% filter(tpswrecap_moe != "FJPPMA") %>% 
        left_join(CoutTypePrestation %>% select(gestctunit_prestation_id, gestctunit_coutunitaire), by = c('tpswrecap_detail' = 'gestctunit_prestation_id')) %>% 
        mutate(tpswrecap_coutunitaire = gestctunit_coutunitaire) %>% select(-gestctunit_coutunitaire) %>% 
        mutate(tpswrecap_argent = tpswrecap_coutunitaire * tpswrecap_quantite) #%>% view()
      ) %>% 
    mutate("_modif_utilisateur" = NA) %>% 
    mutate("_modif_type" = NA) %>% 
    mutate("_modif_date" = NA) %>% 
    select(match(colnames(RecapTpsW),names(.))) %>% 
    mutate(id = NA) %>% 
    
    # On remet les personnels en clair
    left_join((Operateurs %>% select(id, gestop_prenom, gestop_nom) %>% mutate(tpswrecap_personneltemporaire = paste0(gestop_prenom, " ", gestop_nom)) %>% rename(tpswrecap_personnel_id = id)), by = c("tpswrecap_personnel" = "tpswrecap_personnel_id")) %>% 
    mutate(tpswrecap_personnel = tpswrecap_personneltemporaire) %>% 
    select(-tpswrecap_personneltemporaire, -gestop_prenom, -gestop_nom) %>% 
    # On remet les postes en clair
    left_join(Poste %>% select(id, gestpost_poste_libelle), by = c("tpswrecap_poste" = "id")) %>% 
    mutate(tpswrecap_poste = gestpost_poste_libelle) %>% 
    select(-gestpost_poste_libelle) %>% 
    # On remet les prestation en clair + tri
    left_join(TypologiePrestation %>% select(id, gesttyppresta_libelle, gesttyppresta_ordre), by = c("tpswrecap_detail" = "id")) %>% 
    mutate(tpswrecap_detail = gesttyppresta_libelle) %>% 
    select(-gesttyppresta_libelle) %>% 
    arrange(gesttyppresta_ordre, tpswrecap_detail, tpswrecap_moe, desc(tpswrecap_poste)) %>% 
    select(-gesttyppresta_ordre) %>% 
    # Arrondi des valeurs
    mutate(tpswrecap_argent = round(tpswrecap_argent,2))
  
  if(length(which(RecapDataToAdd == "STOP")) != 0) stop("Problème : vérification nécessaire")
  
return(RecapDataToAdd)

} # Fin de la fonction
