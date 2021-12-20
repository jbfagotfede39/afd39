#' Calcul initial des coûts de projet
#'
#' Cette fonction permet de calculer les coûts d'un projet au moment de son montage. Elle s'appuie sur le format saisi dans la table tpstravail_recapitulatif et réalise les conversions à partir des volumes unitaires, et ordonne les sujets
#' @name projet.calculInitial
#' @param nom_projet Nom du dataframe contenant les données "attendues" à traiter, dans le format de tpstravail_recapitulatif
#' @importFrom dplyr select
#' @import glue
#' @import tidyverse
#' @export
#' @examples
#' recap_data_to_add <- projet.calculInitial(nom_projet)
#' tbl(dbD, dbplyr::in_schema("fd_production", "tpstravail_recapitulatif")) %>% filter(tpswrecap_projet == 212) %>% collect() %>% projet.calculInitial()

projet.calculInitial <- function(
  nom_projet
)
{
  #### Vérifications ####
  if(dim(nom_projet)[1] == 0) stop("Saisir un dataframe en entrée")
  if(dim(distinct(nom_projet, tpswrecap_projet))[1] != 1) stop("Plusieurs projets dans le dataframe d'entrée")
  if(dim(distinct(nom_projet, tpswrecap_programmation))[1] != 1) stop("Plusieurs statuts de programmation dans le dataframe d'entrée")
  
  dbD <- BDD.ouverture("Data")
  recap_tps_w <- tbl(dbD, in_schema("fd_production", "tpstravail_recapitulatif")) %>% head() %>% collect() %>% filter(row_number() == 0)
  cout_annuel <- tbl(dbD, in_schema("fd_referentiels", "gestion_coutsannuels")) %>% collect()
  cout_type_prestation <- tbl(dbD, in_schema("fd_referentiels", "gestion_coutsunitaires")) %>% collect()
  # poste <- tbl(dbD, in_schema("fd_referentiels", "gestion_postes")) %>% collect()
  # operateurs <- tbl(dbD, in_schema("fd_referentiels", "gestion_operateurs")) %>% collect()
  # projets <- tbl(dbD, in_schema("fd_production", "projets_liste")) %>% collect() %>% arrange(prjlst_etat, prjlst_datelancement)
  typologie_prestation <- tbl(dbD, in_schema("fd_referentiels", "gestion_typologieprestation")) %>% collect()
  
  DBI::dbDisconnect(dbD)
  
  if(all(colnames(nom_projet) != colnames(recap_tps_w))) stop("Dataframe d'entrée différent de tpstravail_recapitulatif")
  
  if(!grepl("10|11|12", month(today()))) annee <- year(today())
  if(grepl("10|11|12", month(today()))) annee <- year(today()) + 1
  cout_annuel <-
    cout_annuel %>% 
    filter(gestctan_annee == annee) %>%
    filter(gestctan_type == "Estimé N-1")
    
  if(cout_annuel %>% nrow() == 0) stop(glue("Pas de coûts annuels estimés pour l'année {annee}"))
  
  ##### Préparation ####
  nom_projet <-
    nom_projet %>% 
    # On remplace les libellés de prestations par leur identifiant
    left_join((typologie_prestation %>% select(id, gesttyppresta_libelle) %>% rename(tpswrecap_detail_id = id)), by = c("tpswrecap_detail" = "gesttyppresta_libelle")) %>% 
    mutate(tpswrecap_detail = tpswrecap_detail_id) %>% 
    select(-tpswrecap_detail_id, -contains("_modif"))
  
  if(nom_projet %>% filter(is.na(tpswrecap_detail)) %>% nrow() > 0) stop("Il y a des tpswrecap_detail non identifiés par leur identifiant")
  
  nom_projet_fjppma <-
    nom_projet %>% 
    filter(tpswrecap_moe == "FJPPMA")
  
  ##### Calculs ####
  recap_data_to_add <-
    nom_projet_fjppma %>% 
    select(-tpswrecap_coutunitaire) %>% 
    # On prend en premier les tâches déjà définies dans cout_type_prestation #
    bind_rows(cout_type_prestation %>% filter(gestctunit_prestation_id %in% nom_projet_fjppma$tpswrecap_detail) %>% rename(tpswrecap_detail = gestctunit_prestation_id) %>% rename(Temps = gestctunit_temps) %>% select(-id,-contains("_modif")) %>% left_join(nom_projet_fjppma %>% filter(is.na(tpswrecap_argent)) %>% select(tpswrecap_detail, tpswrecap_quantite, -contains("_modif")), by = "tpswrecap_detail")) %>%
    # On joint avec les coûts unitaires par poste/opérateur via une clé #
    mutate(clepostepersonnel = paste0(tpswrecap_poste, "-", tpswrecap_personnel)) %>% 
    left_join(cout_annuel %>% mutate(clepostepersonnel = paste0(gestctan_poste_id, "-", gestctan_operateur_id)) %>% select(clepostepersonnel, gestctan_coutjournaliermajore) %>% rename(cout_unitaire = gestctan_coutjournaliermajore), by = "clepostepersonnel") %>%
    # Cas du coût unitaire des agents de développement, différents en fonction du personnel : on prend la valeur max
    mutate(cout_unitaire = ifelse(tpswrecap_poste == 1 & is.na(tpswrecap_personnel), cout_annuel %>% filter(gestctan_poste_id == 1) %>% summarise(cout_unitaire = max(gestctan_coutjournaliermajore)) %>% pull(), cout_unitaire)) %>% 
    # On regroupe les coûts unitaires #
    mutate(cout_unitaire = ifelse(is.na(cout_unitaire), gestctunit_coutunitaire, cout_unitaire)) %>%
    select(-gestctunit_coutunitaire) %>% 
    # Différents tests de cohérence #
    mutate(tpswrecap_programmation = ifelse(dim(distinct(nom_projet_fjppma, tpswrecap_programmation))[1] == 1, as.character(distinct(nom_projet_fjppma, tpswrecap_programmation)), "STOP")) %>% 
    # mutate(tpswrecap_natureprojet = ifelse(dim(distinct(nom_projet_fjppma, tpswrecap_natureprojet))[1] == 1, as.character(distinct(nom_projet_fjppma, tpswrecap_natureprojet)), "STOP")) %>% 
    mutate(tpswrecap_moe = ifelse(dim(distinct(nom_projet_fjppma, tpswrecap_moe))[1] == 1, as.character(distinct(nom_projet_fjppma, tpswrecap_moe)), "STOP")) %>% 
    mutate(tpswrecap_projet = ifelse(dim(distinct(nom_projet_fjppma, tpswrecap_projet))[1] == 1, as.character(distinct(nom_projet_fjppma, tpswrecap_projet)), "STOP")) %>% 
    filter(!(!is.na(tpswrecap_quantite) & is.na(cout_unitaire) & is.na(tpswrecap_jours) & is.na(tpswrecap_argent))) %>%
    mutate(tpswrecap_quantitepersonnel = ifelse(is.na(tpswrecap_argent) & is.na(tpswrecap_quantitepersonnel) & !is.na(tpswrecap_jours), 1, tpswrecap_quantitepersonnel)) %>% # Si tpswrecap_argent et tpswrecap_quantitepersonnel vides et tpswrecap_jours rempli, alors un seul bonhomme
    mutate(tpswrecap_argent = ifelse(is.na(tpswrecap_argent), tpswrecap_jours * cout_unitaire * tpswrecap_quantitepersonnel * tpswrecap_quantite, tpswrecap_argent)) %>% # pour les tâches avec quantité
    mutate(tpswrecap_argent = ifelse(is.na(tpswrecap_argent), tpswrecap_jours * cout_unitaire * tpswrecap_quantitepersonnel, tpswrecap_argent)) %>% # pour les tâches sans quantité
    mutate(tpswrecap_argent = ifelse(is.na(tpswrecap_argent), tpswrecap_quantite * cout_unitaire, tpswrecap_argent)) %>% # pour les objets sans personnel
    mutate(Temps = ifelse(!is.na(tpswrecap_jours) & !is.na(tpswrecap_quantite) & !is.na(tpswrecap_quantitepersonnel), tpswrecap_jours * tpswrecap_quantite * tpswrecap_quantitepersonnel, NA)) %>% 
    mutate(Temps = ifelse(!is.na(tpswrecap_jours) & is.na(tpswrecap_quantite) & !is.na(tpswrecap_quantitepersonnel), tpswrecap_jours * tpswrecap_quantitepersonnel, Temps)) %>% 
    # Cas d'un item présent en prestation et ajouté en quantité dans le projet -> rebascule en nb de jours + suppression de la quantité
    mutate(tpswrecap_jours = ifelse(is.na(tpswrecap_jours) & !is.na(tpswrecap_poste) & !is.na(tpswrecap_quantite) & tpswrecap_moe == "FJPPMA", tpswrecap_quantite, tpswrecap_jours)) %>%
    mutate(tpswrecap_quantite = ifelse(!is.na(tpswrecap_jours) & !is.na(tpswrecap_poste) & !is.na(tpswrecap_quantite) & tpswrecap_moe == "FJPPMA" & tpswrecap_jours == tpswrecap_quantite, NA, tpswrecap_quantite)) %>%
    # On filtre les éventuelles prestations qui contiennent également du tps de travail FD mais qui sortent ici en tant que prestation du fait d'une jointure
    filter(!(is.na(tpswrecap_argent) & tpswrecap_detail == 76)) %>% 
    # On remet ce qui n'est pas MOE FJPPMA
    rename(tpswrecap_coutunitaire = cout_unitaire) %>% 
    select(-clepostepersonnel, -gestctunit_remarques, -gestctunit_poste_id, -gestctunit_quantitepersonnel, -Temps) %>% 
    mutate(tpswrecap_projet = as.numeric(tpswrecap_projet)) %>% 
    dplyr::union(
      # On met les coûts unitaires et les sous-totaux pour ce qui n'est pas MOE FJPPMA
      nom_projet %>% filter(tpswrecap_programmation == "Attendu") %>% filter(tpswrecap_moe != "FJPPMA") %>% 
        left_join(cout_type_prestation %>% select(gestctunit_prestation_id, gestctunit_coutunitaire), by = c('tpswrecap_detail' = 'gestctunit_prestation_id')) %>% 
        mutate(tpswrecap_coutunitaire = gestctunit_coutunitaire) %>% select(-gestctunit_coutunitaire) %>% 
        mutate(tpswrecap_argent = ifelse(is.na(tpswrecap_argent), tpswrecap_coutunitaire * tpswrecap_quantite, tpswrecap_argent)) # Afin de préserver d'éventuels coûts unitaires saisis manuellement dans la table récap
      ) %>% 
    mutate("_modif_utilisateur" = NA) %>% 
    mutate("_modif_type" = NA) %>% 
    mutate("_modif_date" = NA) %>% 
    select(match(colnames(recap_tps_w),names(.))) %>% 
    mutate(id = NA) %>% 
    
    # On remet les prestations en clair + tri
    left_join(typologie_prestation %>% select(id, gesttyppresta_libelle, gesttyppresta_ordre), by = c("tpswrecap_detail" = "id")) %>% 
    mutate(tpswrecap_detail = gesttyppresta_libelle) %>% 
    select(-gesttyppresta_libelle) %>% 
    arrange(gesttyppresta_ordre, tpswrecap_detail, tpswrecap_moe, desc(tpswrecap_poste)) %>% 
    select(-gesttyppresta_ordre) %>% 
    # Arrondi des valeurs
    mutate(tpswrecap_argent = round(tpswrecap_argent, 2))
  
  if(length(which(recap_data_to_add == "STOP")) != 0) stop("Problème : vérification nécessaire")
  if(recap_data_to_add %>% filter(is.na(tpswrecap_argent)) %>% nrow() != 0) stop("Problème : il y a des lignes sans valeur pécuniaire")
  
  
return(recap_data_to_add)

} # Fin de la fonction
