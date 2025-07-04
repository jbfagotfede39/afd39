#' Extraction et réencodage données OpenTime pour AC AE
#'
#' Permet l'extraction et le réencodage des données OpenTime pour les accords-cadre avec l'Agence de l'eau
#' @name personnel.formatAC
#' @param data Jeu de données à formater
#' @param projet Accord-cadre concerné
#' @keywords personnel
#' @import glue
#' @import tidyverse
#' @export
#' @examples
#' personnel.formatAC(data, "Convention-cadre AERMC-FD39-2020")

personnel.formatAC <- function(
  data,
  projet = NA_character_
)
{
  
  #### Données de référence ####
  ## Connexion à la BDD ##
  dbD <- BDD.ouverture("Data")
  
  annee_ac <- str_replace(projet, "Convention-cadre AERMC-FD39-", "")
  date_debut_ac <- glue("{annee_ac}-01-01")
  date_fin_ac <- glue("{annee_ac}-12-31")
  
  tps_w_ot <- structure(list(id = integer(0), tpswot_date = structure(numeric(0), class = "Date"), 
                           tpswot_domaine = character(0), tpswot_activite = character(0), 
                           tpswot_sous_projet = character(0), tpswot_personnel = integer(0), 
                           tpswot_heuredebut = character(0), tpswot_heurefin = character(0), 
                           tpswot_duree = numeric(0), tpswot_validation = character(0), 
                           tpswot_remarques = character(0), tpswot_presenceenligne = character(0), 
                           `_modif_utilisateur` = character(0), `_modif_type` = character(0), 
                           `_modif_date` = structure(numeric(0), tzone = "", class = c("POSIXct", 
                                                                                       "POSIXt"))), row.names = integer(0), class = c("tbl_df", 
                                                                                                                                      "tbl", "data.frame"))
  postes <- tbl(dbD, dbplyr::in_schema("fd_referentiels", "gestion_postes")) %>% select(id, gestpost_poste_libelle) %>% collect(n = Inf) %>% rename(idposte = id)
  personnels <- tbl(dbD, dbplyr::in_schema("fd_referentiels", "gestion_operateurs")) %>% filter(gestop_mo == 3 & gestop_type == "Salarié") %>% filter(!(gestop_prenom %in% c("Collectif", "recruter", "Admin"))) %>% select(id:gestop_qualite, gestop_datefinactivite) %>% collect(n = Inf) %>% filter(is.na(gestop_datefinactivite)|(gestop_datefinactivite <= ymd(date_fin_ac)+years(1))) %>% select(-gestop_datefinactivite) %>% left_join(postes, by = c("gestop_qualite" = "gestpost_poste_libelle")) %>% mutate(gestop_qualite = idposte) %>% select(-idposte)
  if(personnels %>% filter(is.na(gestop_qualite)) %>% nrow() != 0) stop("Présence de personnels sans poste de travail clairement défini")
  
  if(is.na(projet)) stop("Pas de projet spécifié")
  if(is.numeric(projet)){projets <- tbl(dbD, dbplyr::in_schema("fd_production", "projets_liste")) %>% filter(id == projet) %>% collect(n = Inf)} # Si le projet est recherché via son id
  if(!is.numeric(projet)){projets <- tbl(dbD, dbplyr::in_schema("fd_production", "projets_liste")) %>% filter(prjlst_projet == projet) %>% collect(n = Inf)} # Si le projet est recherché via son intitulé
  
  DBI::dbDisconnect(dbD)
  
  #### Tests ####
  if(grepl("Convention-cadre AERMC-FD39-", projets$prjlst_projet) == FALSE) stop("Le projet spécifié n'est pas de la forme Convention-cadre AERMC-FD39-")
  if(all(colnames(data) %in% colnames(tps_w_ot)) == FALSE) stop("Données en entrée pas au format de la table tpstravail_opentime")
  
  #### Réencodage ####
  projet_id <- projets %>% select(id) %>% pull()
  annee_ac <- str_replace(projets$prjlst_projet, "Convention-cadre AERMC-FD39-", "")
  pattern_1 <- c("^AC AE")
  pattern_2 <- c(glue("{projets$id} - AC AE"))
  data_v2 <-
    data %>% 
    filter(year(tpswot_date) == annee_ac) %>% 
    filter(tpswot_domaine == "Projets") %>% 
    filter(str_detect(tpswot_activite, pattern_1) | str_detect(tpswot_activite, pattern_2)) %>% 
    # filter() %>% 
    # union(data %>% 
    #         filter(year(tpswot_date) == annee_ac) %>% 
    #         filter(tpswot_domaine == "Projets") %>% 
    #         filter(grepl("1.1|1.2|1.3|2\\.1|2\\.2|2.3|2.4|2.5|3.1|4.1|4.2", tpswot_sous_projet))
    # ) %>% 
    filter(!is.na(tpswot_duree)) %>% 
    filter(tpswot_presenceenligne == "Présent") %>% 
    mutate(tpswdetail_projet = projet_id) %>% 
    # rowwise() %>%
    mutate(tpswdetail_sousactionaermc = NA_character_) %>% 
    mutate(tpswdetail_sousactionaermc = ifelse(str_detect(tpswot_activite, "1.1"), 1.1, tpswdetail_sousactionaermc)) %>%
    mutate(tpswdetail_sousactionaermc = ifelse(is.na(tpswdetail_sousactionaermc) & str_detect(tpswot_sous_projet, "1.1"), 1.1, tpswdetail_sousactionaermc)) %>%
    mutate(tpswdetail_sousactionaermc = ifelse(is.na(tpswdetail_sousactionaermc) & str_detect(tpswot_activite, "AC AE") & tpswot_sous_projet == "Administratif", 1.1, tpswdetail_sousactionaermc)) %>% # 3 lignes en 2020 où il n'y avait que "AC AE 2019" ou "AC AE 2018" en activité et "Administratif" en sous-projet
    mutate(tpswdetail_sousactionaermc = ifelse(str_detect(tpswot_activite, "1.2"), 1.2, tpswdetail_sousactionaermc)) %>%
    mutate(tpswdetail_sousactionaermc = ifelse(is.na(tpswdetail_sousactionaermc) & str_detect(tpswot_sous_projet, "1.2"), 1.2, tpswdetail_sousactionaermc)) %>%
    mutate(tpswdetail_sousactionaermc = ifelse(str_detect(tpswot_activite, "1.3"), 1.3, tpswdetail_sousactionaermc)) %>%
    mutate(tpswdetail_sousactionaermc = ifelse(is.na(tpswdetail_sousactionaermc) & str_detect(tpswot_sous_projet, "1.3"), 1.3, tpswdetail_sousactionaermc)) %>%
    mutate(tpswdetail_sousactionaermc = ifelse(str_detect(tpswot_activite, "2\\.1"), 2.1, tpswdetail_sousactionaermc)) %>% # Il faut un échappement du point car sinon il cherche 2*1, soit par ex 2019
    mutate(tpswdetail_sousactionaermc = ifelse(is.na(tpswdetail_sousactionaermc) & str_detect(tpswot_sous_projet, "2\\.1"), 2.1, tpswdetail_sousactionaermc)) %>% # Il faut un échappement du point car sinon il cherche 2*1, soit par ex 2019
    mutate(tpswdetail_sousactionaermc = ifelse(str_detect(tpswot_activite, "2\\.2"), 2.2, tpswdetail_sousactionaermc)) %>% # Il faut un échappement du point car sinon il cherche 2*2, soit par ex 2020
    mutate(tpswdetail_sousactionaermc = ifelse(is.na(tpswdetail_sousactionaermc) & str_detect(tpswot_sous_projet, "2\\.2"), 2.2, tpswdetail_sousactionaermc)) %>% # Il faut un échappement du point car sinon il cherche 2*2, soit par ex 2020
    mutate(tpswdetail_sousactionaermc = ifelse(str_detect(tpswot_activite, "2.3"), 2.3, tpswdetail_sousactionaermc)) %>%
    mutate(tpswdetail_sousactionaermc = ifelse(is.na(tpswdetail_sousactionaermc) & str_detect(tpswot_sous_projet, "2.3"), 2.3, tpswdetail_sousactionaermc)) %>% # Il faut un échappement du point car sinon il cherche 2*2, soit par ex 2020
    mutate(tpswdetail_sousactionaermc = ifelse(str_detect(tpswot_activite, "2\\.4"), 2.4, tpswdetail_sousactionaermc)) %>%
    mutate(tpswdetail_sousactionaermc = ifelse(is.na(tpswdetail_sousactionaermc) & str_detect(tpswot_sous_projet, "2\\.4"), 2.4, tpswdetail_sousactionaermc)) %>% # Il faut un échappement du point car sinon il cherche 2*4, soit par ex l'identifiant de projet 224
    mutate(tpswdetail_sousactionaermc = ifelse(str_detect(tpswot_activite, "2\\.5"), 2.5, tpswdetail_sousactionaermc)) %>%
    mutate(tpswdetail_sousactionaermc = ifelse(is.na(tpswdetail_sousactionaermc) & str_detect(tpswot_sous_projet, "2\\.5"), 2.5, tpswdetail_sousactionaermc)) %>%
    mutate(tpswdetail_sousactionaermc = ifelse(str_detect(tpswot_activite, "3\\.1"), 3.1, tpswdetail_sousactionaermc)) %>%
    mutate(tpswdetail_sousactionaermc = ifelse(is.na(tpswdetail_sousactionaermc) & str_detect(tpswot_sous_projet, "3\\.1"), 3.1, tpswdetail_sousactionaermc)) %>%
    mutate(tpswdetail_sousactionaermc = ifelse(str_detect(tpswot_activite, "4\\.1"), 4.1, tpswdetail_sousactionaermc)) %>%
    mutate(tpswdetail_sousactionaermc = ifelse(is.na(tpswdetail_sousactionaermc) & str_detect(tpswot_sous_projet, "4\\.1"), 4.1, tpswdetail_sousactionaermc)) %>%
    mutate(tpswdetail_sousactionaermc = ifelse(str_detect(tpswot_activite, "4\\.2"), 4.2, tpswdetail_sousactionaermc)) %>%
    mutate(tpswdetail_sousactionaermc = ifelse(is.na(tpswdetail_sousactionaermc) & str_detect(tpswot_sous_projet, "4\\.2"), 4.2, tpswdetail_sousactionaermc)) %>%
    mutate(tpswdetail_sousactionaermc = ifelse(is.na(tpswdetail_sousactionaermc) & str_detect(tpswot_activite, "AC AE 3.1"), 3.1, tpswdetail_sousactionaermc)) %>% # "AC AE 3.1 Hydrologie Lac des Rousses" mystérieusement non traité par phrase classique plus haut
    mutate(tpswdetail_actionaermc = str_sub(tpswdetail_sousactionaermc, 1, 1)) %>% # Déduit à partir de la sous-action
    left_join(personnels %>% mutate(tpswdetail_personnel = glue("{gestop_prenom} {gestop_nom}")), by = c("tpswot_personnel" = "id")) %>% 
    rename(tpswdetail_poste = gestop_qualite) %>% 
    select(-contains("gestop")) %>% 
    mutate(tpswdetail_statut = case_when(.$tpswdetail_poste %in% c(2, 3, 4, 5, 6, 7, 8) ~ "Expertise",
                                         .$tpswdetail_poste %in% c(1, 9, 10, 13) ~ "Surveillance")) %>% 
    mutate(tpswdetail_detail = NA_character_) %>% 
    mutate(tpswdetail_detail = ifelse(is.na(.$tpswot_sous_projet) & is.na(.$tpswot_remarques), tpswot_activite, tpswdetail_detail)) %>% 
    mutate(tpswdetail_detail = ifelse(is.na(.$tpswot_remarques), glue('{tpswot_activite} - {tpswot_sous_projet}'), tpswdetail_detail)) %>% 
    mutate(tpswdetail_detail = ifelse(is.na(.$tpswot_sous_projet), glue('{tpswot_activite} - {tpswot_remarques}'), tpswdetail_detail)) %>% 
    mutate(tpswdetail_detail = ifelse(is.na(.$tpswdetail_detail), glue('{tpswot_domaine} - {tpswot_activite} - {tpswot_remarques}'), tpswdetail_detail))
    
  #### Sortie des données ####
  return(data_v2)
  
} # Fin de la fonction
  