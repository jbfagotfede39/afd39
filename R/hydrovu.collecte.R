#' Collecte des données de l'API HydroVu
#'
#' Cette fonction permet de collecter les données disponibles l'API HydroVu
#' @name hydrovu.collecte
#' @param id Id du capteur recherché sur l'API
#' @param chsta_coderhj Station concernées par l'\code{id} recherché sur l'API
#' @param modem Modem concerné par l'\code{id} recherché sur l'API
#' @param capteur Capteur concerné par l'\code{id} recherché sur l'API
#' @param time_end Timecode de fin de recherche (\code{now()} par défaut)
#' @param profondeur Profondeur temporelle recherchée (en jours)
#' @param intervalle Intervalle entre deux séquences de collecte (en heures)
#' @keywords chronique
#' @import aquatools
#' @import tidyverse
#' @export 
#' @examples
#' hydrovu.collecte("5327553842264847", "BONlac", "852311", "794849")

hydrovu.collecte <- function(
    id = NA_character_,
    chsta_coderhj = NA_character_,
    modem = NA_character_,
    capteur = NA_character_,
    time_end = now(),
    profondeur = 10,
    intervalle = 6
  )
{

  #### Évaluation des choix ####
  if(is.na(id)) stop("Un identifiant HydroVu doit forcément être fourni")
  if(is.na(chsta_coderhj)) stop("Un chsta_coderhj doit forcément être fourni")
  if(is.na(modem)) stop("Un identifiant de modem doit forcément être fourni")
  if(is.na(capteur)) stop("Un identifiant de capteur doit forcément être fourni")
  
  #### Contexte ####
  id_machine <- general.identification()
  
  #### Identification ####
  if(id_machine == "serveur"){
    fileName <- 'conf.txt'
    motmystere <- readChar(fileName, file.info(fileName)$size-1)
    keyring_unlock(password = motmystere)
  }
  
  motdepasse <- keyring::key_get('hydrovu', "jbf")
  token <- hydrovu.authentification("jbf", motdepasse)
  
  #### Collecte des données ####
  ##### Création de la liste des requêtes #####
  liste_creneaux <-
    seq(floor_date(time_end-days(profondeur), unit = "hours"), floor_date(time_end, unit = "hours"), by = glue("{intervalle} hour")) %>% 
    as_tibble() %>% 
    rename(time = value) %>% 
    mutate(chmes_coderhj = id, .before = time)
  
  ##### Collecte des nouvelles données #####
  data_to_add_v1 <-
    liste_creneaux %>% 
    group_split(chmes_coderhj, time) %>%
    map(~ hydrovu.extraction(token, .$chmes_coderhj, .$time)) %>%
    list_rbind() %>% 
    distinct()

  #### Données de référence ####
  mesures <- mesures_structure
  
  if(data_to_add_v1 %>% nrow() == 0) data_sortie <- mesures_structure
  if(data_to_add_v1 %>% nrow() != 0){
    #### Nettoyage & reformatage ####
    data_to_add_v2 <- 
      data_to_add_v1 %>% 
      mutate(chmes_coderhj = chsta_coderhj) %>% 
      mutate(chmes_date = ymd(chmes_date)) %>%
      mutate(chmes_valeur = as.numeric(chmes_valeur)) %>%
      mutate(chmes_capteur = case_when(
        chmes_typemesure == "Barométrie" ~ modem,
        chmes_typemesure == "Niveau de batterie" ~ modem,
        chmes_typemesure == "Piézométrie compensée" ~ capteur,
        chmes_typemesure == "Thermie piézométrique" ~ capteur,
        chmes_typemesure == "Thermie" ~ capteur,
        chmes_typemesure == "Nitrates" ~ capteur,
        chmes_typemesure == "Conductivité" ~ capteur
      )
      ) %>% 
      mutate(chmes_typemesure = ifelse(chmes_typemesure == "Piézométrie compensée" & chmes_unite == "m", "Piézométrie NGF", chmes_typemesure)) %>% 
      mutate(chmes_unite = ifelse(chmes_typemesure == "Piézométrie NGF", "NGF", chmes_unite)) %>% 
      mutate(chmes_unite = ifelse(chmes_unite == "uS/cm", "μS/cm", chmes_unite)) %>% 
      mutate(chmes_validation = "À valider") %>% 
      mutate(chmes_mode_acquisition = "Mesuré") %>% 
      mutate(chmes_mode_integration = "Ajout automatique") %>% 
      # mutate(chmes_mode_integration = "Ajout manuel") %>% 
      mutate(chmes_referentiel_temporel = NA_character_)
    
    data_to_add_v3 <- 
      data_to_add_v2 %>% 
      mutate(id = NA_integer_) %>% mutate(`_modif_utilisateur` = NA) %>% mutate(`_modif_type` = NA) %>% mutate(`_modif_date` = NA) %>% 
      select(match(colnames(mesures), names(.))) %>% 
      distinct() %>% 
      arrange(desc(chmes_date), desc(chmes_heure))
    
    data_sortie <- data_to_add_v3
    
    #### Vérification ####
    test <- data_sortie %>% filter(is.na(chmes_capteur))
    if(test %>% nrow() != 0) stop("Attention : il y a des lignes sans capteur attribué")
  }
  
  #### Sortie ####
  return(data_sortie)
  
} # Fin de la fonction
