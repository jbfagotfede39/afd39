#' Reformatage de noms propres mal orthographiés
#'
#' Cette fonction permet de reformater des noms propres (écosystèmes, communes, etc.) mal orthographiés
#' @name formatage.noms_propres
#' @param data Dataframe contenant les données à transformer
#' @param Operation Type de transformation que l'on souhaite réaliser : \code{Nettoyage} par défaut (sortie sans article), \code{Développement} possible (sortie avec article)
#' @param ColonneEntree Champ contenant la donnée d'entrée
#' @param ColonneSortie Champ recevant la donnée de sortie (peut être identique au champs d'entrée si on le souhaite)
#' @import glue
#' @import tidyverse
#' @importFrom dplyr select
#' @export
#' @examples
#' formatage.noms_propres(data, Operation = "Nettoyage", ColonneEntree = "chsta_diccor_valeur_incorrecte", ColonneSortie = "chsta_ecosysteme")
#' formatage.noms_propres(data, Operation = "Développement", ColonneEntree = "chsta_diccor_valeur_incorrecte", ColonneSortie = "chsta_ecosysteme")

formatage.noms_propres <- function(
  data,
  Operation = c("Nettoyage", "Développement"),
  ColonneEntree = NA_character_,
  ColonneSortie = NA_character_
  )
  {

  ## Évaluation des choix
  Operation <- match.arg(Operation)
  
  ## Tests ##
if(nchar(ColonneEntree) == 0) stop("Pas de champs en entrée")
if(nchar(ColonneSortie) == 0) stop("Pas de champs de sortie")
if(ColonneEntree %in% names(data) == FALSE) stop(paste0("Le champs ", ColonneEntree, " est absent du dataframe d'entrée"))

  #### Chargement des données de référence ####
  dbD <- BDD.ouverture("Data")
  
  dictionnaire_correction <-
    tbl(dbD, in_schema("fd_referentiels", "dictionnaire_correction")) %>% 
    select(id:diccor_remarques) %>% 
    collect()
  
  dictionnaire_articles <-
    tbl(dbD, in_schema("fd_referentiels", "dictionnaire_articles")) %>% 
    select(id:dicar_remarques) %>% 
    collect()
  
  dictionnaire_correction_complete <-
    dictionnaire_correction %>% 
    union(dictionnaire_correction %>% distinct(diccor_valeur_correcte) %>% mutate(id = NA_integer_, .before = "diccor_valeur_correcte") %>% mutate(diccor_valeur_incorrecte = diccor_valeur_correcte, .before = "diccor_valeur_correcte") %>% mutate(diccor_remarques = NA_character_)) %>% # Afin de générer les valeurs propres directement, afin qu'elles ne soient pas ignorées
    left_join(dictionnaire_articles, by = c("diccor_valeur_correcte" = "dicar_valeur")) %>% 
    select(-contains("id"), -contains("remarques")) %>% 
    mutate(diccor_valeur_correcte_avec_article = glue("{dicar_article}{diccor_valeur_correcte}"))
  
    ## Fermeture de la BDD ##
    DBI::dbDisconnect(dbD)
    
##### Nettoyage #####
if(Operation == "Nettoyage"){

  data_nom_colonnes <- data
  
  data <-
    data %>% 
    rename(diccor_valeur_incorrecte := !!ColonneEntree) %>% 
    mutate(diccor_valeur_incorrecte = str_trim(diccor_valeur_incorrecte)) %>% # Afin de retirer d'éventuels espaces en début et fin
    mutate(diccor_valeur_incorrecte = str_to_title(diccor_valeur_incorrecte)) %>% # Afin de mettre la première lettre de chaque mot en majuscule si elle ne l'était pas, afin de limiter les cas ensuite
    left_join(dictionnaire_correction_complete, by = c("diccor_valeur_incorrecte" = "diccor_valeur_incorrecte")) %>% 
    rename(!!ColonneEntree := diccor_valeur_incorrecte) %>%
    rename(Sortie = diccor_valeur_correcte)
  
}
  
  ##### Développement #####
if(Operation == "Développement"){
  
  data_nom_colonnes <- data
  
  data <-
    data %>% 
    rename(diccor_valeur_incorrecte := !!ColonneEntree) %>% 
    left_join(dictionnaire_correction_complete, by = c("diccor_valeur_incorrecte" = "diccor_valeur_incorrecte")) %>% 
    rename(!!ColonneEntree := diccor_valeur_incorrecte) %>%
    rename(Sortie = diccor_valeur_correcte_avec_article)
  
}

#### Test de complétude ####
test <- 
    data %>% 
    dplyr::filter(is.na(Sortie)) %>% 
    rename(diccor_valeur_incorrecte := !!ColonneEntree)
if(nrow(test) == 1) warning(paste0("Présence d'un nom propre impossible à corriger : "), glue_collapse(unique(test$diccor_valeur_incorrecte), ", ", last = " et "))
if(nrow(test) > 1) warning(paste0("Présence de noms propres impossibles à corriger : "), glue_collapse(unique(test$diccor_valeur_incorrecte), ", ", last = " et "))
  
#### Renommage final ####
if(ColonneEntree == ColonneSortie){data <- data %>% select(-contains("diccor_"), -contains("dicar_")) %>% select(-matches(ColonneEntree)) %>% rename(!!ColonneSortie := Sortie)}
if(ColonneEntree != ColonneSortie){
  if(ColonneSortie %in% colnames(data) == TRUE){data <- data %>% select(-matches(ColonneSortie))} # Attention l'ordre de ces deux étapes est important, car on fait disparaître ColonneSortie qui est donc ensuite généré car absent
  if(!(ColonneSortie %in% colnames(data))){data <- data %>% rename(!!ColonneSortie := Sortie)} # Attention l'ordre de ces deux étapes est important
}
  
#### Ré-ordonnancement ####
if(ColonneSortie %in% colnames(data_nom_colonnes)){data <- data %>% select(match(colnames(data_nom_colonnes),names(.)))}
if(!(ColonneSortie %in% colnames(data_nom_colonnes))){data <- data %>% select(match(colnames(data_nom_colonnes), names(.)), matches(ColonneSortie))}

  
#### Retour du tableau complet ####
return(data)
  
} # Fin de la fonction