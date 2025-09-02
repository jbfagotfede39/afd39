#' Calcul initial des coûts de projet
#'
#' Cette fonction permet de calculer les coûts d'un projet au moment de son montage. Elle s'appuie sur le format saisi dans la table tpstravail_recapitulatif et réalise les conversions à partir des volumes unitaires, et ordonne les sujets
#' @name projet.calculInitial
#' @param nom_projet Nom du dataframe contenant les données "attendues" à traiter, dans le format de tpstravail_recapitulatif
#' @param mois_modif_couts_annuels Mois de l'année à partir duquel il faut prendre les coûts annuels N+1 (\code{12} par défaut)
#' @importFrom dplyr select
#' @import glue
#' @import tidyverse
#' @export
#' @examples
#' recap_data_to_add <- projet.calculInitial(nom_projet)
#' tbl(dbD, dbplyr::in_schema("fd_production", "tpstravail_recapitulatif")) %>% filter(tpswrecap_projet == 212) %>% collect() %>% projet.calculInitial()

projet.calculInitial <- function(
  nom_projet,
  mois_modif_couts_annuels = 12
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
  postes <- tbl(dbD, in_schema("fd_referentiels", "gestion_postes")) %>% select(-contains('_modif')) %>% collect() %>% rename(id_poste = id)
  operateurs <- tbl(dbD, in_schema("fd_referentiels", "gestion_operateurs")) %>% filter(gestop_type == "Salarié" & gestop_mo == 3 & gestop_activite == TRUE) %>% select(id, gestop_qualite, gestop_prenom, gestop_nom) %>% collect() %>% rename(id_operateur = id)
  # projets <- tbl(dbD, in_schema("fd_production", "projets_liste")) %>% collect() %>% arrange(prjlst_etat, prjlst_datelancement)
  typologie_prestation <- tbl(dbD, in_schema("fd_referentiels", "gestion_typologieprestation")) %>% collect()
  
  DBI::dbDisconnect(dbD)
  
  if(all(colnames(nom_projet) != colnames(recap_tps_w))) stop("Dataframe d'entrée différent de tpstravail_recapitulatif")
  if(all(month(today()) < mois_modif_couts_annuels & cout_annuel %>% filter(gestctan_annee == year(today())) %>% filter(gestctan_type == "Estimé N-1") %>% select(gestctan_poste_id, gestctan_coutjournaliermajore) %>% nrow() == 0)) stop(glue("Pas de coûts annuels estimés pour l'année {year(today())} en base de données"))
  if(all(month(today()) >= mois_modif_couts_annuels & cout_annuel %>% filter(gestctan_annee == 1+year(today())) %>% filter(gestctan_type == "Estimé N-1") %>% select(gestctan_poste_id, gestctan_coutjournaliermajore) %>% nrow() == 0)) stop(glue("Pas de coûts annuels estimés pour l'année {1+year(today())} en base de données"))
  
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
    bind_rows(cout_type_prestation %>% filter(gestctunit_prestation_id %in% nom_projet_fjppma$tpswrecap_detail) %>% rename(tpswrecap_detail = gestctunit_prestation_id) %>% rename(Temps = gestctunit_temps) %>% filter(!grepl("Externalisé", gestctunit_remarques)) %>% select(-id,-contains("_modif")) %>% left_join(nom_projet_fjppma %>% filter(is.na(tpswrecap_argent)) %>% select(tpswrecap_detail, tpswrecap_quantite, -contains("_modif")), by = "tpswrecap_detail")) %>%
    # view()
    # On rebascule les postes/temps de travail nécessaire à partir des données cout_type_prestation
    mutate(tpswrecap_poste = ifelse(is.na(tpswrecap_poste) & !is.na(gestctunit_poste_id), gestctunit_poste_id, tpswrecap_poste)) %>%
    mutate(tpswrecap_jours = ifelse(is.na(tpswrecap_jours) & !is.na(Temps), Temps, tpswrecap_jours)) %>%
    mutate(tpswrecap_quantitepersonnel = ifelse(is.na(tpswrecap_quantitepersonnel) & !is.na(gestctunit_quantitepersonnel), gestctunit_quantitepersonnel, tpswrecap_quantitepersonnel)) %>%
    # On ajoute le personnel concerné s'il n'est défini que par son poste :
    left_join(operateurs %>% filter(gestop_qualite != "Chargé de développement" & gestop_qualite != "Ingénieur hydrobiologiste") %>% left_join(postes, by = c("gestop_qualite" = "gestpost_poste_libelle")), by = c("tpswrecap_poste" = "id_poste")) %>% # On élude les chargés de développement/ingénieurs car traités ultérieurement, et pas d'autres cas avec plusieurs personnels sur un même poste
    mutate(tpswrecap_personnel = ifelse(is.na(tpswrecap_personnel) & !is.na(tpswrecap_poste), id_operateur, tpswrecap_personnel)) %>%
    select(-id_operateur, -contains("gestop_"), -gestpost_remarques) %>%
    # On joint avec les coûts unitaires par poste/opérateur via une clé #
    mutate(clepostepersonnel = glue("{tpswrecap_poste}-{tpswrecap_personnel}")) %>% 
    {if(month(today()) < mois_modif_couts_annuels) left_join(., cout_annuel %>% filter(gestctan_annee == year(today())) %>% filter(gestctan_type == "Estimé N-1") %>% mutate(clepostepersonnel = glue("{gestctan_poste_id}-{gestctan_operateur_id}")) %>% select(clepostepersonnel, gestctan_coutjournaliermajore) %>% rename(cout_unitaire = gestctan_coutjournaliermajore), by = "clepostepersonnel") else .} %>%
    {if(month(today()) >= mois_modif_couts_annuels) left_join(., cout_annuel %>% filter(gestctan_annee == 1+year(today())) %>% filter(gestctan_type == "Estimé N-1") %>% mutate(clepostepersonnel = glue("{gestctan_poste_id}-{gestctan_operateur_id}")) %>% select(clepostepersonnel, gestctan_coutjournaliermajore) %>% rename(cout_unitaire = gestctan_coutjournaliermajore), by = "clepostepersonnel") else .} %>%
    # Cas du coût unitaire des chargés de développement, différents en fonction du personnel, personnel qui n'est pas individuellement précisé : on prend la valeur max
    mutate(cout_unitaire = ifelse(tpswrecap_poste == 1 & is.na(tpswrecap_personnel), cout_annuel %>% filter(gestctan_annee == year(now()) & gestctan_type == "Estimé N-1" & gestctan_poste_id == 1) %>% summarise(cout_unitaire = max(gestctan_coutjournaliermajore)) %>% pull(), cout_unitaire)) %>% 
    # On regroupe les coûts unitaires #
    mutate(cout_unitaire = ifelse(is.na(cout_unitaire), gestctunit_coutunitaire, cout_unitaire)) %>%
    select(-gestctunit_coutunitaire) %>% 
    # Différents tests de cohérence #
    mutate(tpswrecap_programmation = ifelse(dim(distinct(nom_projet_fjppma, tpswrecap_programmation))[1] == 1, as.character(distinct(nom_projet_fjppma, tpswrecap_programmation)), "STOP")) %>% 
    # mutate(tpswrecap_natureprojet = ifelse(dim(distinct(nom_projet_fjppma, tpswrecap_natureprojet))[1] == 1, as.character(distinct(nom_projet_fjppma, tpswrecap_natureprojet)), "STOP")) %>% 
    mutate(tpswrecap_moe = ifelse(dim(distinct(nom_projet_fjppma, tpswrecap_moe))[1] == 1, as.character(distinct(nom_projet_fjppma, tpswrecap_moe)), "STOP")) %>% 
    mutate(tpswrecap_projet = ifelse(dim(distinct(nom_projet_fjppma, tpswrecap_projet))[1] == 1, as.character(distinct(nom_projet_fjppma, tpswrecap_projet)), "STOP")) %>% 
    # On supprime des lignes générées en trop au moment des cout_type_prestation et unifiées avec le regroupement des coûts unitaires
    filter(!(!is.na(tpswrecap_quantite) & is.na(cout_unitaire) & is.na(tpswrecap_jours) & is.na(tpswrecap_argent))) %>%
    # Rebascule de données dans les champs appropriés
    mutate(tpswrecap_quantitepersonnel = ifelse(is.na(tpswrecap_argent) & is.na(tpswrecap_quantitepersonnel) & !is.na(tpswrecap_jours), 1, tpswrecap_quantitepersonnel)) %>% # Si tpswrecap_argent et tpswrecap_quantitepersonnel vides et tpswrecap_jours rempli, alors un seul bonhomme
    mutate(tpswrecap_argent = ifelse(is.na(tpswrecap_argent), tpswrecap_jours * cout_unitaire * tpswrecap_quantitepersonnel * tpswrecap_quantite, tpswrecap_argent)) %>% # pour les tâches avec quantité
    mutate(tpswrecap_argent = ifelse(is.na(tpswrecap_argent), tpswrecap_jours * cout_unitaire * tpswrecap_quantitepersonnel, tpswrecap_argent)) %>% # pour les tâches sans quantité
    mutate(tpswrecap_argent = ifelse(is.na(tpswrecap_argent), tpswrecap_quantite * cout_unitaire, tpswrecap_argent)) %>% # pour les objets sans personnel
    mutate(Temps = ifelse(!is.na(tpswrecap_jours) & !is.na(tpswrecap_quantite) & !is.na(tpswrecap_quantitepersonnel), tpswrecap_jours * tpswrecap_quantite * tpswrecap_quantitepersonnel, NA)) %>% 
    mutate(Temps = ifelse(!is.na(tpswrecap_jours) & is.na(tpswrecap_quantite) & !is.na(tpswrecap_quantitepersonnel), tpswrecap_jours * tpswrecap_quantitepersonnel, Temps)) %>% 
    # Cas d'un item présent en prestation et ajouté en quantité dans le projet -> rebascule en nb de jours + suppression de la quantité
    mutate(tpswrecap_jours = ifelse(is.na(tpswrecap_jours) & !is.na(tpswrecap_poste) & !is.na(tpswrecap_quantite) & tpswrecap_moe == "FJPPMA", tpswrecap_quantite, tpswrecap_jours)) %>%
    mutate(tpswrecap_quantite = ifelse(!is.na(tpswrecap_jours) & !is.na(tpswrecap_poste) & !is.na(tpswrecap_quantite) & tpswrecap_moe == "FJPPMA" & tpswrecap_jours == tpswrecap_quantite, NA, tpswrecap_quantite)) %>%
    # On remet ce qui n'est pas MOE FJPPMA
    rename(tpswrecap_coutunitaire = cout_unitaire) %>% 
    select(-clepostepersonnel, -gestctunit_remarques, -gestctunit_poste_id, -gestctunit_quantitepersonnel, -Temps) %>% 
    mutate(tpswrecap_projet = as.numeric(tpswrecap_projet)) %>% 
    dplyr::union(
      # On met les coûts unitaires et les sous-totaux pour ce qui n'est pas MOE FJPPMA
      nom_projet %>% filter(tpswrecap_programmation == "Attendu") %>% filter(tpswrecap_moe != "FJPPMA") %>% 
        left_join(cout_type_prestation %>% select(gestctunit_prestation_id, gestctunit_coutunitaire), by = c('tpswrecap_detail' = 'gestctunit_prestation_id')) %>% 
        mutate(tpswrecap_coutunitaire = gestctunit_coutunitaire) %>% select(-gestctunit_coutunitaire) %>% 
        mutate(tpswrecap_argent = ifelse(is.na(tpswrecap_argent), tpswrecap_coutunitaire * tpswrecap_quantite, tpswrecap_argent)) # Pour ne pas considérer les lignes où la valeur financière a déjà été intégrée manuellement, sans référence dans les coûts unitaire ou bien si on veut la zapper
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
  
return(recap_data_to_add)

} # Fin de la fonction
