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

###### À faire #####
# 
# 
####################

personnel.formatAC <- function(
  data,
  projet = NA_character_
)
{
  
  #### Données de référence ####
  TpsWOT <- structure(list(id = integer(0), tpswot_date = structure(numeric(0), class = "Date"), 
                           tpswot_domaine = character(0), tpswot_activite = character(0), 
                           tpswot_sous_projet = character(0), tpswot_personnel = integer(0), 
                           tpswot_heuredebut = character(0), tpswot_heurefin = character(0), 
                           tpswot_duree = numeric(0), tpswot_validation = character(0), 
                           tpswot_remarques = character(0), tpswot_presenceenligne = character(0), 
                           `_modif_utilisateur` = character(0), `_modif_type` = character(0), 
                           `_modif_date` = structure(numeric(0), tzone = "", class = c("POSIXct", 
                                                                                       "POSIXt"))), row.names = integer(0), class = c("tbl_df", 
                                                                                                                                      "tbl", "data.frame"))
  Personnels <- tbl(dbD, dbplyr::in_schema("fd_referentiels", "gestion_operateurs")) %>% filter(gestop_mo == 3 & gestop_type == "Salarié") %>% select(id:gestop_qualite) %>% collect(n = Inf)
  
  #### Tests ####
  if(is.na(projet)) stop("Pas de projet spécifié")
  if(grepl("Convention-cadre AERMC-FD39-", projet) == FALSE) stop("Le projet spécifié n'est pas de la forme Convention-cadre AERMC-FD39-")
  if(all(colnames(data) %in% colnames(TpsWOT)) == FALSE) stop("Données en entrée pas au format de la table tpstravail_opentime")
  
  #### Réencodage ####
  anneeAC <- str_replace(projet, "Convention-cadre AERMC-FD39-", "")
  data <-
    data %>% 
    filter(year(tpswot_date) == anneeAC) %>% 
    filter(tpswot_domaine == "Projets") %>% 
    filter(str_detect(tpswot_activite, "^AC AE")) %>% 
    union(data %>% 
            filter(year(tpswot_date) == anneeAC) %>% 
            filter(tpswot_domaine == "Projets") %>% 
            filter(grepl("1.1|1.2|1.3|2\\.1|2\\.2|2.3|2.4|2.5|3.1|4.1|4.2", tpswot_sous_projet))
    ) %>% 
    filter(!is.na(tpswot_duree)) %>% 
    filter(tpswot_presenceenligne == "Présent") %>% 
    mutate(tpswdetail_projet = projet) %>% 
    mutate(tpswdetail_sousactionaermc = ifelse(str_detect(tpswot_activite, "1.1"), 1.1, NA_character_)) %>%
    mutate(tpswdetail_sousactionaermc = ifelse(str_detect(tpswot_sous_projet, "1.1"), 1.1, tpswdetail_sousactionaermc)) %>%
    mutate(tpswdetail_sousactionaermc = ifelse(str_detect(tpswot_activite, "AC AE") & tpswot_sous_projet == "Administratif", 1.1, tpswdetail_sousactionaermc)) %>% # 3 lignes en 2020 où il n'y avait que "AC AE 2019" ou "AC AE 2018" en activité et "Administratif" en sous-projet
    mutate(tpswdetail_sousactionaermc = ifelse(str_detect(tpswot_activite, "1.2"), 1.2, tpswdetail_sousactionaermc)) %>%
    mutate(tpswdetail_sousactionaermc = ifelse(str_detect(tpswot_sous_projet, "1.2"), 1.2, tpswdetail_sousactionaermc)) %>%
    mutate(tpswdetail_sousactionaermc = ifelse(str_detect(tpswot_activite, "1.3"), 1.3, tpswdetail_sousactionaermc)) %>%
    mutate(tpswdetail_sousactionaermc = ifelse(str_detect(tpswot_sous_projet, "1.3"), 1.3, tpswdetail_sousactionaermc)) %>%
    mutate(tpswdetail_sousactionaermc = ifelse(str_detect(tpswot_activite, "2\\.1"), 2.1, tpswdetail_sousactionaermc)) %>% # Il faut un échappement du point car sinon il cherche 2*1, soit par ex 2019
    mutate(tpswdetail_sousactionaermc = ifelse(str_detect(tpswot_sous_projet, "2\\.1"), 2.1, tpswdetail_sousactionaermc)) %>% # Il faut un échappement du point car sinon il cherche 2*1, soit par ex 2019
    mutate(tpswdetail_sousactionaermc = ifelse(str_detect(tpswot_activite, "2\\.2"), 2.2, tpswdetail_sousactionaermc)) %>% # Il faut un échappement du point car sinon il cherche 2*2, soit par ex 2020
    mutate(tpswdetail_sousactionaermc = ifelse(str_detect(tpswot_sous_projet, "2\\.2"), 2.2, tpswdetail_sousactionaermc)) %>% # Il faut un échappement du point car sinon il cherche 2*2, soit par ex 2020
    mutate(tpswdetail_sousactionaermc = ifelse(str_detect(tpswot_activite, "2.3"), 2.3, tpswdetail_sousactionaermc)) %>%
    mutate(tpswdetail_sousactionaermc = ifelse(str_detect(tpswot_sous_projet, "2.3"), 2.3, tpswdetail_sousactionaermc)) %>%
    mutate(tpswdetail_sousactionaermc = ifelse(str_detect(tpswot_activite, "2.4"), 2.4, tpswdetail_sousactionaermc)) %>%
    mutate(tpswdetail_sousactionaermc = ifelse(str_detect(tpswot_sous_projet, "2.4"), 2.4, tpswdetail_sousactionaermc)) %>%
    mutate(tpswdetail_sousactionaermc = ifelse(str_detect(tpswot_activite, "2.5"), 2.5, tpswdetail_sousactionaermc)) %>%
    mutate(tpswdetail_sousactionaermc = ifelse(str_detect(tpswot_sous_projet, "2.5"), 2.5, tpswdetail_sousactionaermc)) %>%
    mutate(tpswdetail_sousactionaermc = ifelse(str_detect(tpswot_activite, "3.1"), 3.1, tpswdetail_sousactionaermc)) %>%
    mutate(tpswdetail_sousactionaermc = ifelse(str_detect(tpswot_sous_projet, "3.1"), 3.1, tpswdetail_sousactionaermc)) %>%
    mutate(tpswdetail_sousactionaermc = ifelse(str_detect(tpswot_activite, "4.1"), 4.1, tpswdetail_sousactionaermc)) %>%
    mutate(tpswdetail_sousactionaermc = ifelse(str_detect(tpswot_sous_projet, "4.1"), 4.1, tpswdetail_sousactionaermc)) %>%
    mutate(tpswdetail_sousactionaermc = ifelse(str_detect(tpswot_activite, "4.2"), 4.2, tpswdetail_sousactionaermc)) %>%
    mutate(tpswdetail_sousactionaermc = ifelse(str_detect(tpswot_sous_projet, "4.2"), 4.2, tpswdetail_sousactionaermc)) %>%
    mutate(tpswdetail_sousactionaermc = ifelse(str_detect(tpswot_activite, "AC AE 3.1"), 3.1, tpswdetail_sousactionaermc)) %>% # "AC AE 3.1 Hydrologie Lac des Rousses" mystérieusement non traité par phrase classique plus haut
    mutate(tpswdetail_actionaermc = str_sub(tpswdetail_sousactionaermc, 1, 1)) %>% # Déduit à partir de la sous-action
    left_join(Personnels %>% mutate(tpswdetail_personnel = glue("{gestop_prenom} {gestop_nom}")), by = c("tpswot_personnel" = "id")) %>% 
    rename(tpswdetail_poste = gestop_qualite) %>% 
    select(-contains("gestop")) %>% 
    mutate(tpswdetail_poste = ifelse(tpswdetail_poste == "Responsable du Pôle Technique & Développement", "Ingénieur responsable du pôle technique", tpswdetail_poste)) %>% 
    mutate(tpswdetail_statut = case_when(.$tpswdetail_poste == "Ingénieur responsable du pôle technique" ~ "Expertise",
                                         .$tpswdetail_poste == "Responsable administrative et financière" ~ "Expertise",
                                         .$tpswdetail_poste == "Technicien qualifié PDPG" ~ "Expertise",
                                         .$tpswdetail_poste == "Ingénieur hydrobiologiste" ~ "Expertise",
                                         .$tpswdetail_poste == "Chargé de développement" ~ "Surveillance")) %>% 
    mutate(tpswdetail_detail = NA_character_) %>% 
    mutate(tpswdetail_detail = ifelse(is.na(.$tpswot_sous_projet) & is.na(.$tpswot_remarques), tpswot_activite, tpswdetail_detail)) %>% 
    mutate(tpswdetail_detail = ifelse(is.na(.$tpswot_remarques), glue('{tpswot_activite} - {tpswot_sous_projet}'), tpswdetail_detail)) %>% 
    mutate(tpswdetail_detail = ifelse(is.na(.$tpswot_sous_projet), glue('{tpswot_activite} - {tpswot_remarques}'), tpswdetail_detail)) %>% 
    mutate(tpswdetail_detail = ifelse(is.na(.$tpswdetail_detail), glue('{tpswot_domaine} - {tpswot_activite} - {tpswot_remarques}'), tpswdetail_detail))
    
  #### Sortie des données ####
  return(data)
  
} # Fin de la fonction
  