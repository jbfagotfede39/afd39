#' Extraction du réseau hydrographique selon le SDAGE
#'
#' Cette fonction permet d'extraire les masses d'eau au sens du SDAGE
#' @name hydrographie.SDAGE
#' @param export Forme du dataframe de export - \code{FALSE} (par défault) ou \code{TRUE}
#' @keywords donnees
#' @import glue
#' @import lubridate
#' @import sf
#' @import tidyverse
#' @export
#' @examples
#' hydrographie.SDAGE()
#' MDO <- hydrographie.SDAGE()
#' Resultats <- hydrographie.SDAGE(export = "TRUE")

hydrographie.SDAGE <- function(
  export = FALSE
)
  {
  
  ## Évaluation des choix ##
  # export <- match.arg(export)
  
  ## Ouverture de la BDD ##
  dbD <- BDD.ouverture("Data")
  
  ##### Récupération des données #####
  Departement <- 
    sf::st_read(dbD, query = "SELECT * FROM fd_referentiels.topographie_departement;")
  BV  <- 
    sf::st_read(dbD, query = "SELECT * FROM fd_referentiels.hydrographie_bvsdage2016;")
  CDO  <- 
    sf::st_read(dbD, query = "SELECT * FROM fd_referentiels.hydrographie_coursdeausdage2016;")
  PLDO  <- 
    sf::st_read(dbD, query = "SELECT * FROM fd_referentiels.hydrographie_plandeausdage2016;")
  
  ## Fermeture de la BDD ##
  DBI::dbDisconnect(dbD)
  
  ##### Filtrage résultats #####
  MDORecherchees <-
    CDO %>% 
    bind_rows(PLDO) %>% 
    mutate(MDO_code = ifelse(!is.na(hymdorivsdage_coursdeau_code), hymdorivsdage_coursdeau_code, NA_character_)) %>% 
    mutate(MDO_code = ifelse(!is.na(hymdopldosdage_plandeau_code), hymdopldosdage_plandeau_code, MDO_code)) %>%
    mutate(MDO_libelle = ifelse(!is.na(hymdorivsdage_coursdeau_libelle), hymdorivsdage_coursdeau_libelle, NA_character_)) %>% 
    mutate(MDO_libelle = ifelse(!is.na(hymdopldosdage_plandeau_libelle), hymdopldosdage_plandeau_libelle, MDO_libelle)) %>%
    mutate(BV_code = ifelse(!is.na(hymdorivsdage_bassinversant_code), hymdorivsdage_bassinversant_code, NA_character_)) %>%
    mutate(BV_code = ifelse(!is.na(hymdopldosdage_bassinversant_code), hymdopldosdage_bassinversant_code, BV_code)) %>%
    mutate(BV_libelle = ifelse(!is.na(hymdorivsdage_bassinversant_libelle), hymdorivsdage_bassinversant_libelle, NA_character_)) %>%
    mutate(BV_libelle = ifelse(!is.na(hymdopldosdage_bassinversant_libelle), hymdopldosdage_bassinversant_libelle, BV_libelle)) %>%
    mutate(MDO_nature = ifelse(!is.na(hymdorivsdage_massedeau_nature), hymdorivsdage_massedeau_nature, NA_character_)) %>%
    mutate(MDO_nature = ifelse(!is.na(hymdopldosdage_massedeau_nature), hymdopldosdage_massedeau_nature, MDO_nature)) %>%
    select(MDO_code, MDO_libelle, BV_code, BV_libelle, MDO_nature, everything()) %>% 
    st_join(Departement) %>% 
    filter(!is.na(tpcomm_departement_insee))
  
  #### Sortie des résultats ####
  # Export shp
  if(export == T){
    SIG.export(MDORecherchees, glue("{today()}_Masses_d'eau_SDAGE_2016"), shp = F, kml = F, excel = F)
  }
  
  return(MDORecherchees)
  
} # Fin de la fonction