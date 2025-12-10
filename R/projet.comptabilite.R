#' Affectation des projet à la comptabilité
#'
#' Cette fonction permet d'affecter aux opérations comptables les projets associés par nettoyage
#' @name projet.comptabilite
#' @param fichier Fichier txt à traiter, extrait du logiciel Ciel Compta (grand livre analytique)
#' @import readr
#' @import tidyverse
#' @export
#' @examples
#' "2024 Grand livre analytique.txt" %>% projet.comptabilite()

projet.comptabilite <- function(
    fichier = NA_character_
)
{
  
  #### Test de cohérence ####
  ##### Suppression d'une éventuelle saisie vide de la forme "" #####
  test_id <- fichier %>% nchar()
  if(!(test_id %>% is.na())) {
    if(test_id == 0) fichier <- NA_character_}
  
  ##### Test #####
  if(is.na(fichier)) stop("Aucun fichier à traiter")
  
  #### Collecte des données ####
  ##### Récupération des données #####
  dbD <- BDD.ouverture("Data")
  projets <- tbl(dbD, dbplyr::in_schema("fd_production", "projets_liste")) %>% collect()
  DBI::dbDisconnect(dbD)
  
  ##### Ouverture des données #####
  data_to_add_brutes <-
    fichier %>% 
    read_tsv(col_names = c("num_mvt", "journal", "date", "num_piece", "libelle_ecriture", "S", "debit_montant", "lett", "credit_montant", "solde_cumule"), skip = 12, locale = locale(encoding = "latin1", date_format = "%d/%m/%Y"))
  
  #### Nettoyage & reformatage ####
  ##### Changement de formats #####
  data_nettoyees <-
    data_to_add_brutes %>% 
    # Suppression des lignes vides
    filter(!is.na(num_mvt)) %>% 
    # Valeurs numériques #
    # mutate(debit_montant_bis = parse_number(debit_montant, locale = locale(decimal_mark = ",", grouping_mark = " ")), .after = debit_montant) %>% 
    mutate(debit_montant = parse_number(debit_montant, locale = locale(decimal_mark = ",", grouping_mark = " "))) %>% 
    mutate(credit_montant = parse_number(credit_montant, locale = locale(decimal_mark = ",", grouping_mark = " "))) %>% 
    mutate(solde_cumule = parse_number(solde_cumule, locale = locale(decimal_mark = ",", grouping_mark = " "))) %>% 
    # Marqueur #
    mutate(rownumber = row_number(), .before = "num_mvt") # Afin de conserver un repère fixe par ligne
  
  ##### Attribution des projets ##### 
  ###### Définition de limites de projets ######
  decoupage_1 <-
    data_nettoyees %>% 
    filter(grepl("P0|P1", num_mvt)) %>% # On conserve uniquement les lignes dont num_mvt contiennent P0, comme P0003 Accord Cadre 2018, pour le début du projet
    mutate(projet_id = str_split(num_mvt, " ", simplify = TRUE)[, 1], .before = "num_mvt") %>% # On conserve le premier mot de la chaîne de caractères
    mutate(projet_id = stringr::str_replace(projet_id, "P[0]+", "")) %>% # Retravailler P0099 pour ne conserver que 99, ou  P0103 pour ne conserver que 103
    filter(!is.na(projet_id)) %>% # Afin de ne pas attribuer de projet_id aux lignes qui arrivent après le total du dernier projet
    mutate(projet_id = ifelse(projet_id == "TOTAL", NA, projet_id)) %>% 
    fill(projet_id, .direction = "down") %>% # Afin de propager la limite de projet
    mutate(projet_id = as.numeric(projet_id)) # Afin de pouvoir réaliser un tri numérique et non lexicographique : 69 après 7
  
  ###### Définition des projets à partir de ces limites ######
  data_groupees_1 <-
    data_nettoyees %>% 
    left_join(decoupage_1 %>% filter(projet_id != "TOTAL"), by = c("rownumber", "num_mvt", "journal", "date", "num_piece", "libelle_ecriture", "S", "debit_montant", "lett", "credit_montant", "solde_cumule")) %>% 
    select(rownumber, projet_id, everything()) %>% 
    filter(rownumber <= max(decoupage_1$rownumber)) %>% 
    fill(projet_id, .direction = "down") # Pour compléter le numéro de projet sur toutes les lignes où la valeur est absente
  
  ###### Attribution des comptes ######
  ## Définition de limites de comptes ##
  decoupage_2 <-
    data_nettoyees %>% 
    filter(is.na(date) & str_starts(num_mvt, "2|6|7|TOTAL COMPTE")) %>% # On conserve les lignes qui concernent les débuts (Comptes "6" - dépenses et "7" - recettes et compte "2" - immobilisations - et fins de compte ("TOTAL COMPTE")
    filter(!str_starts(num_mvt, "TOTAL CODE") & !str_starts(num_mvt, "P")) %>% # Car il en restant des mauvaises, je ne sais pas pourquoi
    mutate(compte_id = str_split(num_mvt, " ", simplify = TRUE)[, 1], .before = "num_mvt") # On conserve le premier mot de la chaîne de caractères
  
  ## Définition des comptes à partir de ces limites ##
  data_groupees_2 <-
    data_groupees_1 %>% 
    left_join(decoupage_2 %>% filter(compte_id != "TOTAL"), by = c("rownumber", "num_mvt", "journal", "date", "num_piece", "libelle_ecriture", "S", "debit_montant", "lett", "credit_montant", "solde_cumule")) %>% 
    select(rownumber, projet_id, compte_id, everything()) %>% 
    group_by(projet_id) %>% 
    fill(compte_id, .direction = "down") %>% # Pour compléter le numéro de compte sur toutes les lignes où la valeur est absente, mais où cela est pertient (rôle du group)
    ungroup()
  
  ### Filtrage des lignes sans opérations ###
  data_to_add <-
    data_groupees_2 %>% 
    filter(!is.na(date)) %>% 
    select(-rownumber)
  
  ### Filtrage des lignes liées à des projets ###
  data_to_add <-
    data_to_add %>% 
    filter(projet_id %in% projets$id)
  
  ##### Transformation des formats #####
  data_to_add_2 <-
    data_to_add %>% 
    relocate(lett, .after = S) %>% 
    select(-solde_cumule)
  
  ##### Tri #####
  data_to_add_3 <-
    data_to_add_2 %>% 
    arrange(projet_id, date)
  
  #### Sortie ####
  return(data_to_add_3)
  
} # Fin de la fonction
