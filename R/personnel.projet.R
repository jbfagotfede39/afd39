#' Extraction des données de temps de travail par projet
#'
#' Extrait au format excel les données plus ou moins détaillées des coûts du personnel par projet
#' @name personnel.projet
#' @keywords personnel
#' @param projet Intitulé du projet que l'on souhaite calculer
#' @param projection Si \code{TRUE} (par défaut), va chercher les données projetées.
#' @param arrondi Valeur d'arrondi des journées : demi-journée \code{0.5} (par défaut)
#' @param anciennebase Si \code{TRUE}, va chercher les données détaillées également dans l'ancienne base en plus d'Opentime
#' @param envoi Si \code{TRUE}, envoie les données calculées dans la table fd_production.tpstravail_recapitulatif (\code{FALSE} par défaut)
#' @param export Si \code{TRUE} (par défaut), exporte les résultats
#' @import glue
#' @import openxlsx
#' @import RPostgreSQL
#' @import stringr
#' @import tidyverse
#' @export
#' @examples
#' personnel.projet("Étude Valouse")
#' personnel.projet("Convention-cadre AERMC-FD39-2021")

###### À faire #####
# Déplacer moe/client de tpswrecap vers tpswprj et ajouter moa (maître d'oeuvre vs maître d'ouvrage)
# 
####################

personnel.projet <- function(
  projet = NA_character_,
  projection = TRUE,
  arrondi = 0.5,
  anciennebase = FALSE,
  envoi = FALSE,
  export = TRUE
)
{
  
  ##### Récupération des données #####
  ## Connexion à la BDD ##
  dbD <- BDD.ouverture("Data")
  
  ## Récupération des données ##
  if(anciennebase == T){TpsW <- tbl(dbD, dbplyr::in_schema("fd_production", "tpstravail_detail")) %>% filter(tpswdetail_projet == projet) %>% collect(n = Inf)}
  TpsWOT <- tbl(dbD, dbplyr::in_schema("fd_production", "tpstravail_opentime")) %>% collect(n = Inf)
  if(is.numeric(projet)){Projets <- tbl(dbD, dbplyr::in_schema("fd_production", "projets_liste")) %>% filter(id == projet) %>% collect(n = Inf)} # Si le projet est recherché via son id
  if(!is.numeric(projet)){Projets <- tbl(dbD, dbplyr::in_schema("fd_production", "projets_liste")) %>% filter(prjlst_projet == projet) %>% collect(n = Inf)} # Si le projet est recherché via son intitulé
  RecapTpsW <- tbl(dbD, dbplyr::in_schema("fd_production", "tpstravail_recapitulatif")) %>% filter(tpswrecap_projet %in% !! Projets$id) %>% collect(n = Inf)
  CoutsAnnuels <- tbl(dbD, dbplyr::in_schema("fd_referentiels", "gestion_coutsannuels")) %>% filter(gestctan_projet_id %in% !! Projets$id) %>% collect(n = Inf)
  Postes <- tbl(dbD, dbplyr::in_schema("fd_referentiels", "gestion_postes")) %>% select(id, gestpost_poste_libelle) %>% collect(n = Inf) %>% rename(idposte = id)
  Personnels <- tbl(dbD, dbplyr::in_schema("fd_referentiels", "gestion_operateurs")) %>% filter(gestop_mo == 3 & gestop_type == "Salarié") %>% filter(!(gestop_prenom %in% c("Collectif", "recruter", "Admin"))) %>% select(id:gestop_qualite) %>% collect(n = Inf) %>% left_join(Postes, by = c("gestop_qualite" = "gestpost_poste_libelle")) %>% mutate(gestop_qualite = idposte) %>% select(-idposte)
  if(Personnels %>% filter(is.na(gestop_qualite)) %>% nrow() != 0) stop("Présence de personnels sans poste de travail clairement défini")
  
  id_max <- as.numeric(tbl(dbD,in_schema("fd_production", "tpstravail_recapitulatif")) %>% summarise(max = max(id, na.rm = TRUE)) %>% collect())
  projet_id <- Projets %>% select(id) %>% pull()
  projet_libelle <- Projets %>% select(prjlst_projet) %>% pull()
  
  ##### Vérification de l'existence de ce projet #####
  if(dim(Projets)[1] == 0) stop("Projet non répertorié")
  if(anciennebase == T){
    if(dim(TpsW)[1] == 0) stop("Pas de données détaillées ancienne base pour ce projet")
  } 
  if(projection == T & dim(RecapTpsW)[1] == 0) stop("Pas de données récapitulatives (au moins projetées) pour ce projet")
  
  #### Calcul des données élaborées si absentes ####
  nRecapTpsW <- 0
  if(nrow(RecapTpsW) != 0) nRecapTpsW <- nrow(RecapTpsW %>% filter(tpswrecap_programmation == "Réalisé"))
  # if(nRecapTpsW <= 5){ # On en conserve 5 dans le cas de la présence de réalisé sur le matériel/prestation # Suppression temporaire pour test le 2021-10-27, en attente de voir si ça fonctionne bien sans
    
    ### Extraction des coûts unitaires ###
    if(projection == T){
    CoutPersonnelAttendu <-
      RecapTpsW %>%
      filter(tpswrecap_programmation == "Attendu") %>%
      filter(!is.na(tpswrecap_poste)) %>%
      filter(tpswrecap_poste != "") %>%
      distinct(tpswrecap_poste, tpswrecap_personnel, tpswrecap_coutunitaire)
    } # Fin de if(projection = T)
    
    ## Recherche de coûts annuels réalisés génériques si absence de coûts réalisés propres à un projet précis :
    if(projection == F){
      
      if(nrow(CoutsAnnuels) == 0){ # On conserve ce fonctionnement avec un chargement initial propre au projet, qui semble prioritaire, puis on charge le paquet général si le paquet propre au projet est vide
        anneesrecherches <- year(today()) - 5
        CoutsAnnuelsGeneriques <- tbl(dbD, dbplyr::in_schema("fd_referentiels", "gestion_coutsannuels")) %>% filter(gestctan_annee >= anneesrecherches) %>% collect(n = Inf)
        CoutsAnnuelsReelsGeneriques <- CoutsAnnuelsGeneriques %>% filter(gestctan_type == "Réalisé N") %>% collect(n = Inf)
        # Pour ajouter l'année en cours si elle ne dispose pas de coûts réels (cas le plus fréquent) :
        if(CoutsAnnuelsReelsGeneriques %>% filter(gestctan_annee == year(today())) %>% nrow() == 0){CoutsAnnuelsReelsGeneriques <- CoutsAnnuelsReelsGeneriques %>% union(CoutsAnnuelsGeneriques %>% filter(gestctan_annee == year(today())) %>% filter(gestctan_type == "Estimé N-1"))}
        # listeannees <- unique(CoutsAnnuelsReelsGeneriques$gestctan_annee)
        # # anneeachercher <- tcltk::tk_select.list(sort(listeannees), title = "Année de référence")
        # anneeachercher <- select.list(sort(listeannees), title = glue("Année de référence pour le projet {Projets$id} - {Projets$prjlst_projet} :"))
        CoutsAnnuels <- CoutsAnnuelsReelsGeneriques
      }
      
      # if(nrow(CoutsAnnuels) == 0){ # Version qui offre le choix manuel de l'année
      #   anneesrecherches <- year(today()) - 5
      #   CoutsAnnuelsGeneriques <- tbl(dbD, dbplyr::in_schema("fd_referentiels", "gestion_coutsannuels")) %>% filter(gestctan_annee >= anneesrecherches) %>% collect(n = Inf)
      #   CoutsAnnuelsReelsGeneriques <- CoutsAnnuelsGeneriques %>% filter(gestctan_type == "Réalisé N") %>% collect(n = Inf)
      #   # Pour ajouter l'année en cours si elle ne dispose pas de coûts réels (cas le plus fréquent) :
      #   if(CoutsAnnuelsReelsGeneriques %>% filter(gestctan_annee == year(today())) %>% nrow() == 0){CoutsAnnuelsReelsGeneriques <- CoutsAnnuelsReelsGeneriques %>% union(CoutsAnnuelsGeneriques %>% filter(gestctan_annee == year(today())) %>% filter(gestctan_type == "Estimé N-1"))}
      #   listeannees <- unique(CoutsAnnuelsReelsGeneriques$gestctan_annee)
      #   # anneeachercher <- tcltk::tk_select.list(sort(listeannees), title = "Année de référence")
      #   anneeachercher <- select.list(sort(listeannees), title = glue("Année de référence pour le projet {Projets$id} - {Projets$prjlst_projet} :"))
      #   CoutsAnnuels <- CoutsAnnuelsReelsGeneriques %>% filter(gestctan_annee == anneeachercher)
      # }
    }
    
    if(nrow(CoutsAnnuels) != 0){
    CoutPersonnelReel <- 
      CoutsAnnuels %>% 
      {if(CoutsAnnuels %>% filter(gestctan_type == "Réalisé N") %>% nrow() != 0) filter(., gestctan_type == "Réalisé N") else .} # On filtre sur le réalisé que s'il en existe, comme ça on peut conserver l'attendu pour l'année en cours (qui ne contient pas de réalisé N)
      if(CoutPersonnelReel %>% filter(gestctan_annee == year(today())) %>% nrow() == 0){CoutPersonnelReel <- CoutPersonnelReel %>% union(CoutsAnnuels %>% filter(gestctan_annee == year(today())) %>% filter(gestctan_type == "Estimé N-1"))}
    CoutPersonnelReel <-
      CoutPersonnelReel %>% 
      filter(!is.na(gestctan_poste_id)) %>% 
      filter(gestctan_poste_id != "") %>% 
      # left_join(Personnels %>% mutate(tpswrecap_personnel = glue("{gestop_prenom} {gestop_nom}")) %>% rename(tpswrecap_poste = gestop_qualite) %>% select(id, tpswrecap_personnel, tpswrecap_poste), by = c("gestctan_operateur_id" = "id")) %>% 
      # rename_at(vars(contains("gestctan_")), list( ~ str_replace(., "gestctan_", "tpswrecap_"))) %>%
      rename(tpswrecap_annee = gestctan_annee) %>%
      rename(tpswrecap_poste = gestctan_poste_id) %>%
      rename(tpswrecap_personnel = gestctan_operateur_id) %>%
      rename(tpswrecap_coutunitaire = gestctan_coutjournaliermajore) %>%
      distinct(tpswrecap_annee, tpswrecap_poste, tpswrecap_personnel, tpswrecap_coutunitaire) %>% 
      select(tpswrecap_annee, tpswrecap_poste, tpswrecap_personnel, tpswrecap_coutunitaire) %>% 
      mutate(cle_cout_poste_personnel = glue('{tpswrecap_annee}-{tpswrecap_poste}-{tpswrecap_personnel}')) %>% 
      select(-tpswrecap_annee)
    }
    
    if(nrow(CoutPersonnelReel) != 0) CoutPersonnel <- CoutPersonnelReel
    if(projection == T){
      if(exists(CoutPersonnel)){
        if(nrow(CoutPersonnel) == 0) CoutPersonnel <- CoutPersonnelAttendu
      }
    }
    
    if(nrow(CoutPersonnelReel) == 0) stop("Pas de coûts unitaires réalisés <-> N+1")
    
    ### Recherche et éventuel regroupement de données détaillées
    if(!grepl("AERMC", Projets$prjlst_projet)){ # Cas où ce n'est pas une convention avec l'AE
      TpsWOTfiltre <- 
        TpsWOT %>% 
        mutate(tpswot_activite = ifelse(grepl("APD Bonlieu", tpswot_activite), "APD Bonlieu 2018", tpswot_activite)) %>% 
        mutate(tpswot_activite = ifelse(grepl("Suivi piscicole Ilay, Val", tpswot_activite), "19 - Suivi piscicole Ilay, Val", tpswot_activite)) %>% 
        mutate(tpswot_activite = ifelse(grepl("Echantillonnage Piscicole Gravières Bienne", tpswot_activite), "80 - Gravières de Jeurre et Lavancia - Suivi piscicole avant travaux", tpswot_activite)) %>% 
        mutate(tpswot_activite = ifelse(grepl("ABC CCAPS", tpswot_activite), "52 - Atlas de la Biodiversité Communale - Communauté de Communes Arbois Poligny Salins", tpswot_activite)) %>%
        mutate(tpswot_activite = ifelse(grepl("Atlas Piscicole BFC", tpswot_activite), "187 - Atlas piscicole Bourgogne-Franche-Comté", tpswot_activite)) %>%
        mutate(tpswot_activite = ifelse(grepl("AC AE 3.1 Restauration morphologique du ruisseau de Fontaine Chambon", tpswot_activite), "86 - Aménagements du ruisseau de Fontaine Chambon - Études de dimensionnement et d’incidence, rédaction du dossier loi sur l’eau ", tpswot_activite)) %>%
        # mutate(tpswot_activite = ifelse(grepl("", tpswot_activite), "", tpswot_activite)) %>% # Ligne vierge pour complément ultérieur
        mutate(tpswot_activite = ifelse(grepl("Suivi qualité temps réel", tpswot_activite) & tpswot_date <= ymd("2021-04-15"), "Suivi thermique Vouglans - Volet 2018-2019", tpswot_activite)) %>%
        mutate(tpswot_projet_id = str_split(tpswot_activite, " ", simplify = TRUE)[, 1]) %>% # Pour extraire la première chaîne de caractères avant le premier espace
        mutate(tpswot_projet_id = str_extract(tpswot_projet_id, "[:digit:]+[-]??[:digit:]+")) %>% # pour n'extraire que les chiffres et les éventuels tirets (exemple : 2020-002 ou 2020-03)
        mutate(tpswot_projet_id = ifelse(is.na(tpswot_projet_id), str_extract(tpswot_activite, "DEV[:digit:]+[-]??[:digit:]+"), tpswot_projet_id)) %>% # Pour les projets qui ont DEV2020-10 par exemple à la fin de l'intitulé
        mutate(tpswot_projet_id = str_extract(tpswot_projet_id, "[:digit:]+[-]??[:digit:]+")) # pour n'extraire que les chiffres et les éventuels tirets (exemple : 2020-002 ou 2020-03) # Pour les projets qui ont DEV2020-10 par exemple à la fin de l'intitulé
      
      # On peut manquer des projets qui sont rentrés dans OpenTime sous deux intitulés : leur numéro de projet (60) et sous leur numéro de devis (2020-001) : il faut donc rechercher sur ces deux intitulés pour ne pas rater de lignes
      TpsWOTfiltre_temporaire <-
        TpsWOTfiltre %>%
        filter(tpswot_projet_id %in% projet_id)
      
      if(!is.na(Projets$prjlst_refdevis)){
        TpsWOTfiltre <- 
          TpsWOTfiltre_temporaire %>% 
          union(TpsWOTfiltre %>% 
                  filter(tpswot_projet_id %in% Projets$prjlst_refdevis))
      }
      if(is.na(Projets$prjlst_refdevis)){
        TpsWOTfiltre <- 
          TpsWOTfiltre_temporaire
      }
      
      if(dim(TpsWOTfiltre)[1] == 0) stop(glue('Pas de données détaillées OpenTime au niveau tpswot_activite pour le projet {projet_id}')) # Sans doute nécessaire d'ajouter ultérieurement un sélecteur pour affiner la recherche dans ce scénario
      
      TpsW <-
        TpsWOTfiltre %>% 
        rename_at(vars(contains("tpswot_")), list( ~ str_replace(., "tpswot_", "tpswdetail_"))) %>%
        left_join(Personnels %>% mutate(tpswdetail_personnelcomplet = glue("{gestop_prenom} {gestop_nom}")), by = c("tpswdetail_personnel" = "id")) %>%
        rename(tpswdetail_poste = gestop_qualite) %>%
        select(-contains("gestop")) %>% 
        # mutate(tpswdetail_personnel = tpswdetail_personnelcomplet) %>% 
        select(-tpswdetail_personnelcomplet) %>% 
        mutate(tpswdetail_poste = ifelse(tpswdetail_poste == "Responsable du Pôle Technique & Développement", "Ingénieur responsable du pôle technique", tpswdetail_poste)) %>% 
        rename(tpswdetail_projet = tpswdetail_activite) %>% 
        # rename(tpswdetail_detail = tpswdetail_remarques) %>% # Ancienne version avant 2021-04-14
        mutate(tpswdetail_detail = glue("{tpswdetail_sous_projet} - {tpswdetail_remarques}")) %>% # Nouvelle version avant 2021-04-14
        mutate(tpswdetail_detail = str_replace(tpswdetail_detail, " - NA", "")) %>% # Nouvelle version avant 2021-04-14
        mutate(tpswdetail_detail = str_replace(tpswdetail_detail, "NA - ", "")) %>% # Nouvelle version avant 2021-04-14
        mutate(tpswdetail_temps = tpswdetail_duree/7) %>% 
        mutate(tpswdetail_temps = ifelse(grepl("Recouvreux|Millet|Mougin", tpswdetail_personnel), tpswdetail_duree/7.8, NA)) %>% # Car base de 39 heures rémunérées 39h, alors que Stéphane base 39h rémunées 35h + récupérations et les autres personnels = base 35h
        mutate(tpswdetail_temps = ifelse(!grepl("Recouvreux|Millet|Mougin", tpswdetail_personnel), tpswrecap_jours/7, tpswrecap_jours)) %>% # Car base de 39 heures rémunérées 39h, alors que Stéphane base 39h rémunées 35h + récupérations et les autres personnels = base 35h
        rename(tpswdetail_id = id) %>% 
        mutate(tpswdetail_statut = NA_character_) %>% 
        mutate(tpswdetail_ecosysteme = NA_character_) %>% 
        mutate(tpswdetail_massedeau = NA_character_) %>% 
        mutate(tpswdetail_projet = projet_id) %>% 
        mutate(tpswdetail_actionaermc = NA_character_) %>% 
        mutate(tpswdetail_sousactionaermc = NA_character_) %>% 
        mutate(tpswdetail_priorite = NA_character_) %>% 
        mutate(tpswdetail_natureprojet = NA_character_) %>% 
        mutate(tpswdetail_duree = NA) %>% 
        mutate(tpswdetail_priorite = NA) %>% 
        mutate(tpswdetail_completude = NA_character_) %>% 
        mutate(tpswdetail_remarques = NA_character_) %>% 
        mutate(tpswdetail_modif_utilisateur = NA_character_) %>% 
        mutate(tpswdetail_modif_type = NA_character_) %>% 
        mutate(tpswdetail_modif_date = NA) %>% 
        select(tpswdetail_id, tpswdetail_date, tpswdetail_personnel, tpswdetail_poste, tpswdetail_statut, tpswdetail_ecosysteme, tpswdetail_massedeau, tpswdetail_projet, tpswdetail_detail, tpswdetail_actionaermc, tpswdetail_sousactionaermc, tpswdetail_natureprojet, tpswdetail_temps, tpswdetail_duree, tpswdetail_priorite, tpswdetail_completude, tpswdetail_remarques, contains("tpswdetail_modif_")) %>% 
        {if(anciennebase == T) union(., TpsW) else .}
      
    if(projection == T) warning("Attention aux éventuels doublons de CoutPersonnel pour Chargé de développement ")
      
      ### Calcul à proprement parler
  DataToAdd <- 
    TpsW %>% 
    filter(tpswdetail_projet == projet_id) %>% 
    filter(!is.na(tpswdetail_temps)) %>% 
    mutate(tpswdetail_annee = year(tpswdetail_date)) %>% 
    group_by(tpswdetail_projet, tpswdetail_annee, tpswdetail_poste, tpswdetail_personnel, tpswdetail_detail) %>%
    summarise(tpswrecap_jours = sum(tpswdetail_temps, na.rm = T)) %>% 
    mutate(cle_cout_poste_personnel = glue('{tpswdetail_annee}-{tpswdetail_poste}-{tpswdetail_personnel}'), .before = 'tpswdetail_detail') %>% 
    # mutate(tpswrecap_jours = round(tpswrecap_jours / 0.5) * 0.5) %>% # Arrondi strict à la demi-journée
    # mutate(tpswrecap_jours = round(tpswrecap_jours / 0.25) * 0.25) %>% # Arrondi strict au quart de journée
    mutate(tpswrecap_jours = round(tpswrecap_jours / arrondi) * arrondi) %>%
    ungroup() %>% 
    select(-tpswdetail_annee) %>% 
    # {if(projection == T) left_join(., CoutPersonnel %>% select(-tpswrecap_poste), by = c("tpswdetail_personnel" = "tpswrecap_personnel")) else .} %>% 
    # {if(projection == F) mutate(., tpswrecap_coutunitaire = NA_real_) else .} %>% 
    # left_join(., CoutPersonnel %>% select(-tpswrecap_poste), by = c("tpswdetail_personnel" = "tpswrecap_personnel")) %>% 
    left_join(., CoutPersonnel %>% select(-tpswrecap_personnel, -tpswrecap_poste), by = "cle_cout_poste_personnel") %>% 
    select(-cle_cout_poste_personnel) %>% 
    mutate(tpswrecap_programmation = "Réalisé") %>% 
    mutate(tpswrecap_natureprojet = as.character(NA)) %>% 
    mutate(tpswrecap_moe = as.character(NA)) %>% 
    mutate(tpswrecap_client = as.integer(NA)) %>% 
    mutate(tpswrecap_actionaermc = as.character(NA)) %>%  
    mutate(tpswrecap_sousactionaermc = as.character(NA)) %>%  
    mutate(tpswrecap_argent = tpswrecap_coutunitaire * tpswrecap_jours) %>% 
    mutate(tpswrecap_quantite = as.numeric(NA)) %>%  
    mutate(tpswrecap_quantitepersonnel = as.numeric(NA))
  }
  
  ### Calcul AC AE
  if(grepl("AERMC", Projets$prjlst_projet)){ # Cas où c'est une convention de l'AE
    ## Calcul à proprement parler ##
    DataToAdd <-
      TpsWOT %>% 
      personnel.formatAC(., projet_libelle) %>% 
      mutate(tpswdetail_ecosysteme = NA_character_) %>% 
      mutate(tpswdetail_massedeau = NA_character_) %>% 
      # mutate(tpswdetail_projet = projet_id) %>% 
      mutate(tpswdetail_temps = NA_character_) %>% 
      mutate(tpswdetail_annee = year(tpswot_date)) %>% 
      mutate(cle_cout_poste_personnel = glue('{tpswdetail_annee}-{tpswdetail_poste}-{tpswot_personnel}'), .before = 'tpswdetail_detail') %>% 
      select(tpswot_date, cle_cout_poste_personnel, tpswdetail_personnel, tpswdetail_poste, tpswot_personnel, tpswdetail_statut, tpswdetail_ecosysteme, tpswdetail_massedeau, tpswdetail_projet, tpswdetail_detail, tpswdetail_actionaermc, tpswdetail_sousactionaermc, tpswdetail_temps, tpswot_duree) %>% # Remise en ordre
      rename(tpswot_personnel_id = tpswot_personnel) %>% 
      rename_all(~sub("tpswot", 'tpswdetail', .x)) %>%
      group_by(tpswdetail_projet, tpswdetail_actionaermc, tpswdetail_sousactionaermc, cle_cout_poste_personnel, tpswdetail_poste, tpswdetail_personnel, tpswdetail_personnel_id) %>%
      summarise(tpswrecap_jours = sum(tpswdetail_duree, na.rm = T)) %>%
      mutate(tpswrecap_jours = ifelse(tpswdetail_personnel_id %in% c(6, 7, 11), tpswrecap_jours/7.8, tpswrecap_jours)) %>% # Car base de 39 heures rémunérées 39h, alors que Stéphane base 39h rémunées 35h + récupérations et les autres personnels = base 35h
      mutate(tpswrecap_jours = ifelse(!(tpswdetail_personnel_id %in% c(6, 7, 11)), tpswrecap_jours/7, tpswrecap_jours)) %>% # Car base de 39 heures rémunérées 39h, alors que Stéphane base 39h rémunées 35h + récupérations et les autres personnels = base 35h
      # mutate(tpswrecap_jours = round(tpswrecap_jours / 0.5) * 0.5) %>% # Arrondi strict à la demi-journée
      # mutate(tpswrecap_jours = round(tpswrecap_jours / 0.25) * 0.25) %>% # Arrondi strict au quart de journée
      mutate(tpswrecap_jours = round(tpswrecap_jours / arrondi) * arrondi) %>%
      ungroup() %>% 
      # left_join(CoutPersonnel %>% select(-tpswrecap_poste), by = c("tpswdetail_personnel_id" = "tpswrecap_personnel")) %>% 
      left_join(., CoutPersonnel %>% select(-tpswrecap_personnel, -tpswrecap_poste), by = "cle_cout_poste_personnel") %>% 
      select(-cle_cout_poste_personnel, -tpswdetail_personnel) %>% 
      rename(tpswdetail_personnel = tpswdetail_personnel_id) %>% 
      mutate(tpswrecap_programmation = "Réalisé") %>% 
      mutate(tpswrecap_natureprojet = "Convention") %>% 
      mutate(tpswrecap_moe = NA_character_) %>% 
      mutate(tpswrecap_client = NA_integer_) %>% 
      mutate(tpswdetail_detail = NA_character_) %>% 
      mutate(tpswrecap_argent = tpswrecap_coutunitaire * tpswrecap_jours) %>% 
      mutate(tpswrecap_quantite = NA_integer_) %>%  
      mutate(tpswrecap_quantitepersonnel = NA_integer_)
      
      bug <- TpsWOT %>% personnel.formatAC(., projet_libelle) %>% filter(is.na(tpswdetail_sousactionaermc))
      if(nrow(bug) != 0) stop(glue('Il y a des actions/sous-actions AERMC non reconnues : id {bug %>% distinct(id)}'))
  }
  
  colnames(DataToAdd) <- 
    DataToAdd %>% 
    colnames() %>% 
    str_replace("tpswdetail", "tpswrecap")

  if(nrow(RecapTpsW) == 0){
    RecapTpsW <- structure(list(id = integer(0), tpswrecap_programmation = character(0), 
                                tpswrecap_moe = character(0), tpswrecap_client = integer(0), 
                                tpswrecap_actionaermc = character(0), tpswrecap_sousactionaermc = character(0), 
                                tpswrecap_projet = integer(0), tpswrecap_detail = character(0), 
                                tpswrecap_poste = integer(0), tpswrecap_personnel = integer(0), 
                                tpswrecap_argent = numeric(0), tpswrecap_jours = numeric(0), 
                                tpswrecap_coutunitaire = numeric(0), tpswrecap_quantite = integer(0), 
                                tpswrecap_quantitepersonnel = numeric(0), `_modif_utilisateur` = character(0), 
                                `_modif_type` = character(0), `_modif_date` = structure(numeric(0), tzone = "", class = c("POSIXct", 
                                                                                                                          "POSIXt")), tpswprj_remarques = character(0)), row.names = integer(0), class = c("tbl_df", 
                                                                                                                                                                                                           "tbl", "data.frame"))
  }
  
DataToAdd <-
  DataToAdd %>% 
  left_join(Projets %>% ungroup() %>% select(id, prjlst_projet, prjlst_natureprojet), by = c("tpswrecap_projet" = "id")) %>% # 
  mutate(tpswrecap_natureprojet = prjlst_natureprojet) %>% 
  mutate(tpswrecap_sousactionaermc = as.character(tpswrecap_sousactionaermc)) %>% 
  # mutate(tpswrecap_projet = id) %>% 
  mutate(id = row_number() + id_max) %>%  # Pour incrémenter les id à partir du dernier
  mutate(tpswprj_remarques = NA_character_) %>% 
  mutate('_modif_utilisateur' = NA) %>% 
  mutate('_modif_date' = NA) %>% 
  mutate('_modif_type' = NA) %>% 
  select(match(names(RecapTpsW), colnames(.)))

# Écriture des données #
if(envoi == T){
RPostgreSQL::dbWriteTable(conn = dbD,
                          name = c("fd_production","tpstravail_recapitulatif"),
                          value = DataToAdd,
                          #overwrite=F,
                          append=T,
                          row.names=FALSE)

# Mise à jour de la séquence des id #
dbGetQuery(dbD, "
SELECT setval('fd_production.tpstravail_recapitulatif_id_seq', COALESCE((SELECT MAX(id)+1 FROM fd_production.tpstravail_recapitulatif), 1), false);
           ")
  } # Fin de if(envoi == T)
#} # fin de if(nRecapTpsW <= 5) = Calcul des données élaborées si absentes - Suppression temporaire pour test le 2021-10-27, en attente de voir si ça fonctionne bien sans

#### Extraction des données élaborées ####
### Données réalisées ###
if(envoi == TRUE){
RecapTpsW <- 
    tbl(dbD, dbplyr::in_schema("fd_production", "tpstravail_recapitulatif")) %>% 
    filter(tpswrecap_projet %in% !! Projets$id) %>% 
    {if(!grepl("AERMC", Projets$prjlst_projet)) filter(., tpswrecap_programmation == "Réalisé") else .} %>%
    collect(n = Inf)
  }
if(envoi == FALSE){
    # Matériel
    materiel_realise <-
      tbl(dbD, dbplyr::in_schema("fd_production", "tpstravail_recapitulatif")) %>% 
      filter(tpswrecap_projet %in% !! Projets$id) %>% 
      filter(tpswrecap_programmation == "Réalisé" & is.na(tpswrecap_personnel)) %>%
      collect(n = Inf)
    RecapTpsW <- DataToAdd %>% union(materiel_realise) %>% mutate('_modif_date' = now())
  }

### Données attendues ###
recap_tps_w_attendu <- 
  tbl(dbD, dbplyr::in_schema("fd_production", "tpstravail_recapitulatif")) %>% 
  filter(tpswrecap_projet %in% !! Projets$id) %>% 
  filter(tpswrecap_programmation == "Attendu") %>% 
  collect()

RecapTpsW <-
  RecapTpsW %>% 
  union(recap_tps_w_attendu)

#### Extraction accord-cadre AERMC ####
if(grepl("AERMC", Projets$prjlst_projet)){
  Recapitulatif <- 
    RecapTpsW %>% 
    filter(tpswrecap_projet %in% !! Projets$id) %>% 
    mutate(tpswrecap_argent = round(tpswrecap_argent, 2))

  if(export == T){
  ## Remise en forme 
  Temps <-
    Recapitulatif %>% 
    left_join(Postes, by = c("tpswrecap_poste" = "idposte")) %>% 
    mutate(tpswrecap_poste = gestpost_poste_libelle) %>% 
    select(-gestpost_poste_libelle) %>% 
    filter(!is.na(tpswrecap_jours)) %>% # pour supprimer les valeurs d'achat sans journées associées
    group_by(tpswrecap_programmation, tpswrecap_sousactionaermc, tpswrecap_poste) %>%
    summarise(tpswrecap_jours = sum(tpswrecap_jours)) %>% 
    pivot_wider(
      names_from = c(tpswrecap_programmation, tpswrecap_poste), 
      values_from = tpswrecap_jours
    ) %>% 
    select(tpswrecap_sousactionaermc, sort(tidyselect::peek_vars())) %>% # Pour trier les colonnes par ordre alphabétique, ce qui était fait par reshape2 mais pas par pivot_wider
    rename_all(~ paste0(., "_temps")) %>%
    rename(tpswrecap_sousactionaermc = tpswrecap_sousactionaermc_temps)
    
  Argent <-
    Recapitulatif %>% 
    left_join(Postes, by = c("tpswrecap_poste" = "idposte")) %>% 
    mutate(tpswrecap_poste = gestpost_poste_libelle) %>% 
    select(-gestpost_poste_libelle) %>% 
    mutate(tpswrecap_poste = ifelse(is.na(tpswrecap_poste), "Matériel", tpswrecap_poste)) %>% 
    group_by(tpswrecap_programmation, tpswrecap_sousactionaermc, tpswrecap_poste) %>%
    summarise(tpswrecap_argent = sum(tpswrecap_argent)) %>% 
    pivot_wider(
      names_from = c(tpswrecap_programmation, tpswrecap_poste), 
      values_from = tpswrecap_argent
    ) %>% 
    select(tpswrecap_sousactionaermc, sort(tidyselect::peek_vars())) %>% # Pour trier les colonnes par ordre alphabétique, ce qui était fait par reshape2 mais pas par pivot_wider
    rename_all(~ paste0(., "_argent")) %>%
    rename(tpswrecap_sousactionaermc = tpswrecap_sousactionaermc_argent)
    
  Recapitulatif <- 
    Temps %>% 
    left_join(Argent, by="tpswrecap_sousactionaermc") %>% 
    select(tpswrecap_sousactionaermc, 
            # contains("Attendu_Responsable adminis"), contains("Attendu_Ingénieur resp"), contains("Attendu_Ingénieur hy"), contains("Attendu_Charg"), contains("Attendu_Technicien quali"), contains("Attendu_Mat"), # Sans apprenti
            # contains("Attendu_Responsable adminis"), contains("Attendu_Ingénieur resp"), contains("Attendu_Ingénieur hy"), contains("Attendu_Charg"), contains("Attendu_Technicien quali"), contains("Attendu_Appren"), contains("Attendu_Mat"), # Avec apprenti
            contains("Attendu_Direc"), contains("Attendu_Responsable adminis"), contains("Attendu_Ingénieur resp"), contains("Attendu_Responsable"), contains("Attendu_Ingénieur hy"), contains("Attendu_Charg"), contains("Attendu_Technicien quali"), contains("Attendu_Appren"), contains("Attendu_Mat"), # Avec apprenti
            # contains("Réalisé_Responsable adminis"), contains("Réalisé_Ingénieur resp"), contains("Réalisé_Ingénieur hy"), contains("Réalisé_Charg"), contains("Réalisé_Technicien quali"), contains("Réalisé_Mat") # Sans apprenti
            # contains("Réalisé_Responsable adminis"), contains("Réalisé_Ingénieur resp"), contains("Réalisé_Ingénieur hy"), contains("Réalisé_Charg"), contains("Réalisé_Technicien quali"), contains("Réalisé_Appren"), contains("Réalisé_Intérim"), contains("Réalisé_Mat") # Avec apprenti
            contains("Réalisé_Direc"), contains("Réalisé_Responsable adminis"), contains("Réalisé_Ingénieur resp"), contains("Réalisé_Responsable"), contains("Réalisé_Ingénieur hy"), contains("Réalisé_Charg"), contains("Réalisé_Technicien quali"), contains("Réalisé_Appren"), contains("Réalisé_Intérim"), contains("Réalisé_Mat") # Avec apprenti
    )
  
  ## Renommage des colonnes
  # if(ncol(Recapitulatif) == 11) colnames(Recapitulatif) <- c("Thème", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses")
  # if(ncol(Recapitulatif) == 22) colnames(Recapitulatif) <- c("Thème", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Dépenses", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses") # Sans apprenti
  # if(ncol(Recapitulatif) == 25) colnames(Recapitulatif) <- c("Thème", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Dépenses", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Dépenses") # Avec apprenti avec matériel
  # if(ncol(Recapitulatif) == 26) colnames(Recapitulatif) <- c("Thème", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Dépenses", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses") # Avec apprenti
  if(ncol(Recapitulatif) == 27) colnames(Recapitulatif) <- c("Thème", 
                                                             "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Dépenses", # Fin du attendu
                                                             "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Nb h/j", "Dépenses", "Dépenses") # Fin du réalisé

  ## Création d'un classeur
  tempsprojet <- createWorkbook()
  ## Ajout d'une feuille
  addWorksheet(tempsprojet, sheetName = glue('Récapitulatif_AC_{str_extract(Projets$prjlst_projet, "[[:digit:]]+$")}'))
  ## Ajout des données
  writeData(tempsprojet, 1, Recapitulatif, startCol = 1, startRow = 3, colNames = T) # writing content on the left-most column to be merged
  ## Ajout de cellules fusionnées
  mergeCells(tempsprojet, 1, cols = 2:14, rows = 1)
  mergeCells(tempsprojet, 1, cols = 15:27, rows = 1)
  writeData(tempsprojet, 1, "Attendu", startCol = 2, startRow = 1)
  writeData(tempsprojet, 1, "Réalisé", startCol = 15, startRow = 1)
  mergeCells(tempsprojet, 1, cols = 2:3, rows = 2)
  mergeCells(tempsprojet, 1, cols = 4:5, rows = 2)
  mergeCells(tempsprojet, 1, cols = 6:7, rows = 2)
  mergeCells(tempsprojet, 1, cols = 8:9, rows = 2)
  mergeCells(tempsprojet, 1, cols = 10:11, rows = 2)
  mergeCells(tempsprojet, 1, cols = 12:13, rows = 2)
  mergeCells(tempsprojet, 1, cols = 15:16, rows = 2)
  mergeCells(tempsprojet, 1, cols = 17:18, rows = 2)
  mergeCells(tempsprojet, 1, cols = 19:20, rows = 2)
  mergeCells(tempsprojet, 1, cols = 21:22, rows = 2)
  mergeCells(tempsprojet, 1, cols = 23:24, rows = 2)
  mergeCells(tempsprojet, 1, cols = 25:26, rows = 2)
  writeData(tempsprojet, 1, "Dir.", startCol = 2, startRow = 2)
  writeData(tempsprojet, 1, "Dir.", startCol = 15, startRow = 2)
  writeData(tempsprojet, 1, "Resp. admin. et fin.", startCol = 4, startRow = 2)
  writeData(tempsprojet, 1, "Resp. admin. et fin.", startCol = 17, startRow = 2)
  writeData(tempsprojet, 1, "Resp. techn.", startCol = 6, startRow = 2)
  writeData(tempsprojet, 1, "Resp. techn.", startCol = 19, startRow = 2)
  writeData(tempsprojet, 1, "Resp. dev.", startCol = 8, startRow = 2)
  writeData(tempsprojet, 1, "Resp. dev.", startCol = 21, startRow = 2)
  writeData(tempsprojet, 1, "Ing. hydr.", startCol = 10, startRow = 2)
  writeData(tempsprojet, 1, "Ing. hydr.", startCol = 23, startRow = 2)
  writeData(tempsprojet, 1, "Chargés de dével.", startCol = 12, startRow = 2)
  writeData(tempsprojet, 1, "Chargés de dével.", startCol = 25, startRow = 2)
  # writeData(tempsprojet, 1, "Appr.", startCol = 10, startRow = 2)
  # writeData(tempsprojet, 1, "Appr.", startCol = 21, startRow = 2)
  writeData(tempsprojet, 1, "Mat.", startCol = 14, startRow = 2)
  writeData(tempsprojet, 1, "Mat.", startCol = 27, startRow = 2)
  # writeData(tempsprojet, 1, "Inter.", startCol = 10, startRow = 2)
  # writeData(tempsprojet, 1, "Inter.", startCol = 23, startRow = 2)
  ## Centrage des cellules fusionnées
  centerStyle <- createStyle(halign = "center")
  addStyle(tempsprojet, 1, centerStyle, rows = 1:2, cols = 1:25, gridExpand = TRUE)
  ## Enregistrement du classeur
  saveWorkbook(tempsprojet, glue('{today()}_{projet_libelle}_récapitulatif_coût_personnel.xlsx'), overwrite = T) 
  } # fin de export = T
  if(export == F) return(Recapitulatif)
}

##### Extraction autre dossier que accord-cadre AERMC ####
if(!grepl("AERMC", Projets$prjlst_projet)){
### Extraction des données de synthèse par poste ###
SynthesePoste <- 
  RecapTpsW %>% 
  filter(tpswrecap_projet %in% !! Projets$id) %>% 
  filter(!is.na(tpswrecap_jours)) %>% 
  group_by(tpswrecap_projet, tpswrecap_detail,tpswrecap_personnel) %>% 
  summarise(Jours = sum(tpswrecap_jours)
  ) %>% 
  arrange(tpswrecap_detail,tpswrecap_personnel) %>% 
  rename(Projet = tpswrecap_projet) %>% 
  rename(Detail = tpswrecap_detail) %>% 
  rename(Personnel = tpswrecap_personnel)
  
SynthesePoste <- as.data.frame(SynthesePoste)
  
### Extraction des données de synthèse par personnel ###
SynthesePersonnel <- 
  RecapTpsW %>% 
  filter(tpswrecap_projet %in% !! Projets$id) %>% 
  filter(!is.na(tpswrecap_jours)) %>% 
  group_by(tpswrecap_projet, tpswrecap_personnel) %>%
  summarise(Journées = sum(tpswrecap_jours)) %>% 
  rename(Projet = tpswrecap_projet) %>% 
  rename(Personnel = tpswrecap_personnel)
  
SynthesePersonnel <- as.data.frame(SynthesePersonnel)
  
### Extraction des données détaillées par personnel ###
  Detail <- 
    TpsW %>% 
    filter(tpswdetail_projet == projet_id) %>% 
    select(tpswdetail_personnel, tpswdetail_date, tpswdetail_poste, tpswdetail_statut, tpswdetail_projet, tpswdetail_detail, tpswdetail_ecosysteme, tpswdetail_temps) %>% 
    arrange(tpswdetail_personnel, tpswdetail_date) %>% 
    filter(!is.na(tpswdetail_temps)) %>% 
    select(-tpswdetail_detail) %>% 
    rename(Personnel = tpswdetail_personnel) %>% 
    rename(Date = tpswdetail_date) %>% 
    rename(Poste = tpswdetail_poste) %>% 
    rename(Statut = tpswdetail_statut) %>% 
    rename(Temps = tpswdetail_temps) %>% 
    rename(Projet = tpswdetail_projet) %>% 
    select(-tpswdetail_ecosysteme)
  
  Detail$Date <- as.character(Detail$Date)
  Detail <- as.data.frame(Detail)
  
  ### Extraction des données théoriques/réalisées ###
  if(RecapTpsW %>% nrow() != 0){
    Comparaison <-
      RecapTpsW %>% 
      group_by(tpswrecap_programmation, tpswrecap_poste, tpswrecap_detail) %>% 
      summarise(tpswrecap_jours = sum(tpswrecap_jours)) %>% 
      pivot_wider(
        names_from = c(tpswrecap_programmation), 
        values_from = tpswrecap_jours,
        values_fn = mean
      )
  }

  if(export == T){
  ### Écriture du fichier excel ###
  tempsprojet <- createWorkbook() # Création d'un classeur
  ## Ajout des feuilles
  addWorksheet(tempsprojet, sheetName = "SynthèsePersonnel")
  addWorksheet(tempsprojet, sheetName = "SynthèsePoste")
  addWorksheet(tempsprojet, sheetName = "Détail")
  addWorksheet(tempsprojet, sheetName = "Comparaison")
  ## Ajout des données
  writeData(tempsprojet, 1, SynthesePersonnel, startCol = 1, startRow = 1, colNames = T) # writing content on the left-most column to be merged
  writeData(tempsprojet, 2, SynthesePoste, startCol = 1, startRow = 1, colNames = T) # writing content on the left-most column to be merged
  writeData(tempsprojet, 3, Detail, startCol = 1, startRow = 1, colNames = T) # writing content on the left-most column to be merged
  writeData(tempsprojet, 4, Comparaison, startCol = 1, startRow = 1, colNames = T) # writing content on the left-most column to be merged
  ## Écriture du classeur
  saveWorkbook(tempsprojet, glue('{today()}_{projet_libelle}_récapitulatif_coût_personnel.xlsx'), overwrite = T)
  } # fin de export = T
  
  DBI::dbDisconnect(dbD)
  
  # if(export == F){return(SynthesePersonnel)}
  if(export == F){return(RecapTpsW)}
  
} # Fin de if(!grepl("AERMC", Projets$prjlst_projet))
  
} # Fin de la fonction
