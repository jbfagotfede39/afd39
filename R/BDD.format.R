#' Mise au format des données pour ajout BDD
#'
#' Cette fonction permet de formater les données pour les ajouter aux bases de données respectives
#' @name BDD.format
#' @param data Jeu de données à mettre au format de la base de données
#' @keywords data
#' @import DBI
#' @import glue
#' @import lubridate
#' @import sf
#' @import testit
#' @import tidyverse
#' @export
#' @examples
#' BDD.format(data)

###### À faire #####
# Ajout d'un test pour les suivis de chronique : si le champ fonctionnement contient perdue, alors le champ action ne peut être que disparue, sinon stop et signalement
# 
####################

BDD.format <- function(
  data = data,
  traitementforce = FALSE,
  Type = c("MI", "Chroniques", "PC", "Temps de travail")
  )
{
  
  ##### Évaluation des choix #####
  Type <- match.arg(Type)

  ###### Contexte ######
  if(traitementforce == "TRUE"){Type <- match.arg(Type)} # Évaluation des choix
  if(Type == "MI") warning("Attention le type par défaut est MI")
  
  ###### MI ######
  Testtraitementforce <- 0
  if(traitementforce == TRUE & Type == "MI") Testtraitementforce <- 1
  if(traitementforce == FALSE & Type == "MI") Testtraitementforce <- 1
  if(Testtraitementforce == 1){
    
  ## Connexion à la BDD ##
  dbD <- BDD.ouverture("Data")
  
  ## Récupération des données ##
  Habitats <- structure(list(id = integer(0), mihab_miop_id = integer(0), mihab_mihabref_id = integer(0), 
                             mihab_recouvrement = numeric(0), mihab_margdom = character(0), 
                             mihab_remarques = character(0), `_modif_utilisateur` = character(0), 
                             `_modif_type` = character(0), `_modif_date` = structure(numeric(0), tzone = "", class = c("POSIXct", 
                                                                                                                       "POSIXt"))), row.names = integer(0), class = c("tbl_df", 
                                                                                                                                                                      "tbl", "data.frame"))
  Prelevements <- structure(list(id = integer(0), miprlvt_miop_id = integer(0), 
                                 miprlvt_numech_dce = integer(0), miprlvt_numech_mag20 = integer(0), 
                                 miprlvt_numech_commun = integer(0), miprlvt_mihabref_id_substrat = integer(0), 
                                 miprlvt_mihabref_id_vitesse = integer(0), miprlvt_mihabref_id_hauteur = integer(0), 
                                 miprlvt_phasedce = character(0), miprlvt_ibgn = logical(0), 
                                 miprlvt_intensitecolmatage = numeric(0), miprlvt_stabilite = character(0), 
                                 miprlvt_naturevegetation = character(0), miprlvt_micapt_abondancevegetation = numeric(0), 
                                 miprlvt_remarques = character(0), `_modif_utilisateur` = character(0), 
                                 `_modif_type` = character(0), `_modif_date` = structure(numeric(0), tzone = "", class = c("POSIXct", 
                                                                                                                           "POSIXt"))), row.names = integer(0), class = c("tbl_df", 
                                                                                                                                                                          "tbl", "data.frame"))
  Captures <- structure(list(id = integer(0), micapt_miprlvt_id = integer(0), 
                             micapt_taxon = character(0), micapt_abondance = numeric(0), 
                             micapt_typeabondance = character(0), micapt_volumeabondance = character(0), 
                             micapt_stade = character(0), micapt_sexe = character(0), 
                             micapt_remarques = character(0), `_modif_utilisateur` = character(0), 
                             `_modif_type` = character(0), `_modif_date` = structure(numeric(0), tzone = "", class = c("POSIXct", 
                                                                                                                       "POSIXt"))), row.names = integer(0), class = c("tbl_df", 
                                                                                                                                                                      "tbl", "data.frame"))
  
  # Travail sur les habitats #
  if(all(colnames(data) %in% colnames(Habitats))) {
    
    # Ajout des ID
    data <-
      data %>% 
      mutate(id = row_number() + as.numeric(dbGetQuery(dbD, "SELECT MAX(id) FROM fd_production.macroinvertebres_habitats;")))
  }
  
  # Travail sur les prélèvements #
  if(all(colnames(data) %in% colnames(Prelevements))) {

    # Transformation des formats
    data <- 
      data %>% 
      mutate(id = as.integer(id)) %>% 
      mutate(miprlvt_miop_id = as.integer(miprlvt_miop_id)) %>% 
      mutate(miprlvt_numech_dce = as.integer(miprlvt_numech_dce)) %>% 
      mutate(miprlvt_numech_mag20 = as.integer(miprlvt_numech_mag20)) %>% 
      mutate(miprlvt_numech_commun = as.integer(miprlvt_numech_commun))
    
    # Ajout des ID
    data <-
      data %>% 
      mutate(id = row_number() + as.numeric(dbGetQuery(dbD, "SELECT MAX(id) FROM fd_production.macroinvertebres_prelevements;")))
  }
  
  # Travail sur les captures #
  if(all(colnames(data) %in% colnames(Captures))) {

    # Transformation des formats
    data <- 
      data %>% 
      mutate(id = as.integer(id)) %>% 
      mutate(micapt_miprlvt_id = as.integer(micapt_miprlvt_id)) %>% 
      mutate(micapt_abondance = as.integer(micapt_abondance))
    
    # Ajout des ID
    data <-
      data %>% 
      mutate(id = row_number() + as.numeric(dbGetQuery(dbD, "SELECT MAX(id) FROM fd_production.macroinvertebres_captures;")))
  } # Fin de travail sur les captures
  DBI::dbDisconnect(dbD)
  } # Fin de travail sur les MI
  
  ###### Chroniques ######
  Testtraitementforce <- 0
  if(traitementforce == TRUE & Type == "Chroniques") Testtraitementforce <- 1
  if(traitementforce == FALSE & Type == "Chroniques") Testtraitementforce <- 1
  if(Testtraitementforce == 1){
  
  ## Création des données type ##
    Stations <- structure(list(id = integer(0), chsta_coderhj = character(0), 
                   chsta_codemo = character(0), chsta_codesie = character(0), 
                   chsta_mo = character(0), chsta_milieu = character(0), chsta_milieucodehydro = logical(0), 
                   chsta_bassin = character(0), chsta_sousbassin = character(0), 
                   chsta_commune = character(0), chsta_departement = character(0), 
                   chsta_codecontextepdpg = character(0), chsta_pays = character(0), 
                   chsta_coord_x = numeric(0), chsta_coord_y = numeric(0), chsta_coord_type = character(0), 
                   chsta_fonctionnement = character(0), chsta_transmission = character(0), 
                   chsta_suivithermie = character(0), chsta_reseauthermietype = character(0), 
                   chsta_suivipiezo = character(0), chsta_suivihydro = character(0), 
                   chsta_suivio2 = character(0), chsta_suivipluvio = character(0), 
                   chsta_altitude = numeric(0), chsta_distancesource = numeric(0), 
                   chsta_distancesource_confluencedrainprincipal = numeric(0), 
                   chsta_temperaturemax = integer(0), chsta_sectionmouillee = integer(0), 
                   chsta_durete = integer(0), chsta_largeurlitmineur = integer(0), 
                   chsta_largeurlitetiage = integer(0), chsta_pente = integer(0), 
                   chsta_typetheorique = character(0), chsta_surfacebassinversant = numeric(0), 
                   chsta_carteign = character(0), chsta_rive = character(0), 
                   chsta_ancrage = character(0), chsta_acces = character(0), 
                   chsta_detailsloc = character(0), chsta_description = character(0), 
                   chsta_url = character(0), chsta_remarques = character(0), 
                   chsta_ordretournee = integer(0), chsta_impacts = character(0), 
                   chsta_profsonde = numeric(0), chsta_substrats = character(0), 
                   chsta_distberge = numeric(0), chsta_numphoto = character(0), 
                   chsta_zcapteur = numeric(0), chsta_zbouchon = numeric(0), 
                   chsta_typez = numeric(0), chsta_hcapteurbouchon = character(0), 
                   chsta_module = numeric(0), chsta_qmna5 = numeric(0), chsta_q2 = numeric(0), 
                   chsta_q5 = numeric(0), chsta_q10 = numeric(0), chsta_q20 = numeric(0), 
                   chsta_q30 = numeric(0), chsta_q50 = numeric(0), chsta_q100 = numeric(0), 
                   chsta_q300 = numeric(0), `_modif_utilisateur` = character(0), 
                   `_modif_type` = character(0), `_modif_date` = structure(numeric(0), tzone = "", class = c("POSIXct", 
                                                                                                             "POSIXt")), chsta_ombrage = character(0), chsta_facies = character(0), 
                   chsta_sprep = character(0), geom = structure(list(), class = c("sfc_GEOMETRY", 
                                                                                  "sfc"), precision = 0, bbox = structure(c(xmin = NA_real_, 
                                                                                                                            ymin = NA_real_, xmax = NA_real_, ymax = NA_real_), crs = structure(list(
                                                                                                                              input = NA_character_, wkt = NA_character_), class = "crs"), class = "bbox"), crs = structure(list(
                                                                                                                                input = "EPSG:2154", wkt = "PROJCRS[\"RGF93 / Lambert-93\",\n    BASEGEOGCRS[\"RGF93\",\n        DATUM[\"Reseau Geodesique Francais 1993\",\n            ELLIPSOID[\"GRS 1980\",6378137,298.257222101,\n                LENGTHUNIT[\"metre\",1]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        ID[\"EPSG\",4171]],\n    CONVERSION[\"Lambert-93\",\n        METHOD[\"Lambert Conic Conformal (2SP)\",\n            ID[\"EPSG\",9802]],\n        PARAMETER[\"Latitude of false origin\",46.5,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8821]],\n        PARAMETER[\"Longitude of false origin\",3,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8822]],\n        PARAMETER[\"Latitude of 1st standard parallel\",49,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8823]],\n        PARAMETER[\"Latitude of 2nd standard parallel\",44,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8824]],\n        PARAMETER[\"Easting at false origin\",700000,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8826]],\n        PARAMETER[\"Northing at false origin\",6600000,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8827]]],\n    CS[Cartesian,2],\n        AXIS[\"easting (X)\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1]],\n        AXIS[\"northing (Y)\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1]],\n    USAGE[\n        SCOPE[\"unknown\"],\n        AREA[\"France\"],\n        BBOX[41.15,-9.86,51.56,10.38]],\n    ID[\"EPSG\",2154]]"), class = "crs"), classes = character(0), n_empty = 0L)), row.names = integer(0), class = c("sf", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              "data.frame"), sf_column = "geom", agr = structure(c(id = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   chsta_coderhj = NA_integer_, chsta_codemo = NA_integer_, chsta_codesie = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   chsta_mo = NA_integer_, chsta_milieu = NA_integer_, chsta_milieucodehydro = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   chsta_bassin = NA_integer_, chsta_sousbassin = NA_integer_, chsta_commune = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   chsta_departement = NA_integer_, chsta_codecontextepdpg = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   chsta_pays = NA_integer_, chsta_coord_x = NA_integer_, chsta_coord_y = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   chsta_coord_type = NA_integer_, chsta_fonctionnement = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   chsta_transmission = NA_integer_, chsta_suivithermie = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   chsta_reseauthermietype = NA_integer_, chsta_suivipiezo = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   chsta_suivihydro = NA_integer_, chsta_suivio2 = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   chsta_suivipluvio = NA_integer_, chsta_altitude = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   chsta_distancesource = NA_integer_, chsta_distancesource_confluencedrainprincipal = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   chsta_temperaturemax = NA_integer_, chsta_sectionmouillee = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   chsta_durete = NA_integer_, chsta_largeurlitmineur = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   chsta_largeurlitetiage = NA_integer_, chsta_pente = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   chsta_typetheorique = NA_integer_, chsta_surfacebassinversant = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   chsta_carteign = NA_integer_, chsta_rive = NA_integer_, chsta_ancrage = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   chsta_acces = NA_integer_, chsta_detailsloc = NA_integer_, chsta_description = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   chsta_url = NA_integer_, chsta_remarques = NA_integer_, chsta_ordretournee = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   chsta_impacts = NA_integer_, chsta_profsonde = NA_integer_, chsta_substrats = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   chsta_distberge = NA_integer_, chsta_numphoto = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   chsta_zcapteur = NA_integer_, chsta_zbouchon = NA_integer_, chsta_typez = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   chsta_hcapteurbouchon = NA_integer_, chsta_module = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   chsta_qmna5 = NA_integer_, chsta_q2 = NA_integer_, chsta_q5 = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   chsta_q10 = NA_integer_, chsta_q20 = NA_integer_, chsta_q30 = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   chsta_q50 = NA_integer_, chsta_q100 = NA_integer_, chsta_q300 = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   `_modif_utilisateur` = NA_integer_, `_modif_type` = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   `_modif_date` = NA_integer_, chsta_ombrage = NA_integer_, chsta_facies = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   chsta_sprep = NA_integer_), .Label = c("constant", "aggregate", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "identity"), class = "factor")) %>% 
    mutate(chsta_sprep = NA_character_) %>% # Car absente de la table originale dans Postgres
    mutate(chsta_ombrage = NA_character_) %>% # Car absente de la table originale dans Postgres
    mutate(chsta_facies = NA_character_) # Car absente de la table originale dans Postgres
  
  Capteurs <- structure(list(id = 13L, chcap_proprietaire = "CD39", chcap_typecapteur = "Thermie", 
                             chcap_modelecapteur = NA_character_, chcap_numerocapteur = "10316505", 
                             chcap_etat = NA_character_, chcap_projet = "RDP", chcap_originecapteur = "Achat", 
                             chcap_datedebut = NA_character_, chcap_datefin = NA_character_, 
                             chcap_remarques = NA_character_, `_modif_utilisateur` = NA_character_, 
                             `_modif_type` = NA_character_, `_modif_date` = structure(NA_real_, class = c("POSIXct", 
                                                                                                          "POSIXt"), tzone = "")), row.names = 1L, class = c("tbl_df", 
                                                                                                                                                             "tbl", "data.frame"))
  Mesures <- structure(list(id = 32968L, chmes_coderhj = "BON", chmes_capteur = "350938", 
                            chmes_date = structure(15126, class = "Date"), chmes_heure = "14:00:00", 
                            chmes_valeur = 11.09, chmes_unite = "°C", chmes_typemesure = "Thermie", 
                            chmes_validation = "Validé", chmes_mode_acquisition = "Mesuré", 
                            chmes_mode_integration = "Ajout manuel", `_modif_utilisateur` = "JB", 
                            `_modif_type` = "I", `_modif_date` = structure(1541660441.85543, class = c("POSIXct", 
                                                                                                       "POSIXt"), tzone = "")), row.names = 1L, class = c("tbl_df", 
                                                                                                                                                          "tbl", "data.frame"))
  SuiviTerrain <- structure(list(id = 1151L, chsvi_mo = "SMISA", chsvi_coderhj = "BCB4-1amont", 
                                 chsvi_typesuivi = "Thermie", chsvi_operateurs = "PIZZETTI", 
                                 chsvi_date = "2018-03-26", chsvi_heure = "10:20:00", chsvi_capteur = "20136662", 
                                 chsvi_valeur = 7.4, chsvi_profondeur = 0.3, chsvi_unite = "°C", chsvi_action = "Relève", 
                                 chsvi_fonctionnement = "OK", chsvi_qualite = NA_character_, 
                                 chsvi_actionafaire = NA_character_, chsvi_remarques = "Piles impossible à changer problème paramétrage ordi et impossible de mettre en place nouvelles sonde car impossible d'en lancer une suite à ce problème", 
                                 `_modif_utilisateur` = "JB", `_modif_type` = "I", `_modif_date` = structure(1537779116.5002, class = c("POSIXct", 
                                                                                                                                        "POSIXt"), tzone = "")), row.names = 1L, class = c("tbl_df", 
                                                                                                                                                                                           "tbl", "data.frame"))
  
  # MesuresGroupees
  MesuresGroupees <- structure(list(id = integer(0), chmesgr_coderhj_id = integer(0), 
                                    chmesgr_capteur_id = integer(0), chmesgr_date = structure(numeric(0), class = "Date"), 
                                    chmesgr_periodicite = character(0), chmesgr_typeagregation = character(0), 
                                    chmesgr_valeur = numeric(0), chmesgr_unite = character(0), 
                                    chmesgr_typemesure = character(0), chmesgr_validation = character(0), 
                                    chmesgr_mode_acquisition = character(0), chmesgr_mode_integration = character(0), 
                                    chmesgr_source = character(0), chmesgr_remarques = character(0), 
                                    `_modif_utilisateur` = character(0), `_modif_type` = character(0), 
                                    `_modif_date` = structure(numeric(0), tzone = "", class = c("POSIXct", 
                                                                                                "POSIXt"))), row.names = integer(0), class = c("tbl_df", 
                                                                                                                                               "tbl", "data.frame"))
  
  # Mesures #
  if(all(colnames(data) %in% colnames(Mesures))) {
    
    # Arrondi des valeurs
    data$chmes_valeur <- round(as.numeric(data$chmes_valeur),3) # On arrondi à 3 chiffres après la virgule
    
    # Complément des heures #
    data$chmes_heure <- ifelse(nchar(data$chmes_heure) == 5, paste0(data$chmes_heure, ":00"), data$chmes_heure) 
    
    # Correction des unités #
    data$chmes_unite <- ifelse(data$chmes_typemesure == "Piézométrie NGF", "NGF", data$chmes_unite)
    
    # Transformation des formats
    data$id <- as.integer(data$id)
    data$chmes_date <- as.character(data$chmes_date)
    
    # Ajout des ID
    data$id <- row_number(data$chmes_valeur) + as.numeric(tbl(dbD,in_schema("fd_production", "chroniques_mesures")) %>% summarise(max = max(id, na.rm = TRUE)) %>% collect()) # Pour incrémenter les id à partir du dernier
    if(dim(filter(data, is.na(id)))[1] > 0 & dim(filter(data, is.na(chmes_validation)))[1] == 0) data$id <- row_number(data$chmes_validation) + as.numeric(tbl(dbD,in_schema("fd_production", "chroniques_mesures")) %>% summarise(max = max(id, na.rm = TRUE)) %>% collect())
    if(dim(filter(data, is.na(id)))[1] > 0) stop("Tous les id ne sont pas complétés")
  }
  
  # Mesures groupées #
  if(all(colnames(data) %in% colnames(MesuresGroupees))) {
    
    # Ajout des ID
    data$id <- row_number(data$chmesgr_coderhj_id) + as.numeric(dbGetQuery(dbD, "SELECT MAX(id) FROM fd_production.chroniques_mesuresgroupees;")) # Pour incrémenter les id à partir du dernier
    data <- data %>% arrange(id)
  }
  
  # SuiviTerrain #
  if(all(colnames(data) %in% colnames(SuiviTerrain))) {
    
    # Travail sur les MO #
    data <-
      data %>% 
      mutate(chsvi_mo = ifelse(chsvi_mo == "Fédé 39", "FJPPMA", chsvi_mo)) %>% 
      mutate(chsvi_mo = ifelse(chsvi_mo == "FD39", "FJPPMA", chsvi_mo)) %>% 
      mutate(chsvi_mo = ifelse(chsvi_mo == "Fédé39", "FJPPMA", chsvi_mo)) %>% 
      mutate(chsvi_mo = ifelse(chsvi_mo == "PNR HJ", "PNRHJ", chsvi_mo))
    
    # Travail sur les stations #
    data$chsvi_coderhj <- str_replace(data$chsvi_coderhj, " ", "") # On efface les espaces en trop dans les noms de station
    data$chsvi_coderhj <- str_to_upper(data$chsvi_coderhj, locale = "fr") # On met les noms de station en majuscules
    data <- 
      data %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "VOUCHARTREUSE", "VOUchartreuse", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "VOUGRINGALETPORT", "VOUgringaletport", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "VOUMERCANTINE", "VOUmercantine", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "VOUPATORNAY", "VOUpatornay", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "VOUSURCHAUFFANT", "VOUsurchauffant", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "VOUBELLECIN", "VOUbellecin", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "BONSEUIL", "BONseuil", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "BONZH", "BONzh", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "GCLVANNE", "GCLvanne", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "GCLPLONGEOIR", "GCLplongeoir", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "GCLZHAVAL", "GCLzhaval", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "GCLZHENTRE2", "GCLzhentre2", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "ILABLOC", "ILAbloc", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "LEM-2-2", "LEM2-2", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "NCZ6-2TRÉMONTAGNE", "NCZ6-2", chsvi_coderhj)) %>% 
      mutate(chsvi_coderhj = stringr::str_replace(chsvi_coderhj, "BIS$", "bis")) %>% # Remplacement en fin de station de BIS par bis
      mutate(chsvi_coderhj = stringr::str_replace(chsvi_coderhj, "LAC$", "lac")) %>% # Remplacement en fin de station de LAC par lac
      mutate(chsvi_coderhj = stringr::str_replace(chsvi_coderhj, "BARO$", "baro")) %>% # Remplacement en fin de station de BARO par baro
      mutate(chsvi_coderhj = stringr::str_replace(chsvi_coderhj, "AMONT$", "amont")) %>% # Remplacement en fin de station de AMONT par amont
      mutate(chsvi_coderhj = stringr::str_replace(chsvi_coderhj, "AVAL$", "aval")) %>% # Remplacement en fin de station de AVAL par aval
      mutate(chsvi_coderhj = stringr::str_replace(chsvi_coderhj, "ATMO$", "atmo")) # Remplacement en fin de station de ATMO par atmo
      #mutate(chsvi_coderhj = ifelse(chsvi_coderhj == "", "", chsvi_coderhj)) %>% 
    
    # Travail sur les dates #
    if(!(mean(nchar(data$chsvi_date)) >= 4.5 & mean(nchar(data$chsvi_date)) <= 5.5)){ # Cas où les dates ressemblent vraiment à des dates, pas 43020 (issu du format de champ Date de excel)
      if(testit::has_warning(ymd(data$chsvi_date)) == TRUE & testit::has_warning(dmy(data$chsvi_date)) == FALSE) data$chsvi_date <- as.character(format(dmy(data$chsvi_date), format="%Y-%m-%d"))
      if(testit::has_warning(ymd(data$chsvi_date)) == TRUE & testit::has_warning(dmy(data$chsvi_date)) == TRUE){ # dans le cas où les formats de date sont mélangés
        data <-
          data %>% 
          mutate(chsvi_datebis = chsvi_date) %>% 
          mutate(chsvi_date = format(ymd(data$chsvi_date), format="%Y-%m-%d")) %>% 
          mutate(chsvi_date = ifelse(is.na(chsvi_date), format(dmy(data$chsvi_date), format="%Y-%m-%d"), chsvi_date)) %>% 
          select(-chsvi_datebis)
      }
    }
    if(mean(nchar(data$chsvi_date)) >= 4.5 & mean(nchar(data$chsvi_date)) <= 5.5){ # Cas où les dates ne ressemblent pas à des dates, comme 43020 (issu du format de champ Date de excel)
      data <-
        data %>% 
        mutate(chsvi_date = ymd("1899-12-30") + as.numeric(chsvi_date))
    }
    
    # Travail sur les heures #
    if(any(!is.na(data$chsvi_heure))){
    data <- 
      data %>% 
      mutate(chsvi_heure = str_replace(chsvi_heure, "h", ":")) %>% # On remplace le h par :
      mutate(chsvi_heure = str_replace(chsvi_heure, "H", ":")) %>% # On remplace le H par :
      mutate(chsvi_heure = ifelse(grepl("Oubli", chsvi_heure), NA_character_, chsvi_heure)) %>% 
      mutate(chsvi_heure = ifelse(grepl("oubli", chsvi_heure), NA_character_, chsvi_heure)) %>% 
      mutate(count = str_count(.$chsvi_heure, ":")) %>% 
      mutate(chsvi_heure = ifelse(!is.na(chsvi_heure) & count == 1, paste0(chsvi_heure, ":00"), chsvi_heure)) %>% 
      select(-count) %>% 
      rowwise() %>% 
      mutate(chsvi_heure = ifelse(any(class(.$chsvi_heure) == "POSIXct"), format(chsvi_heure, format="%H:%M:%S"), chsvi_heure)) %>% 
      ungroup()
    if(testit::has_warning(format(ymd_hms(paste(data$chsvi_date,"-",data$chsvi_heure)), format="%H:%M:%S")) == FALSE) data$chsvi_heure <- format(ymd_hms(paste(data$chsvi_date,"-",data$chsvi_heure)), format="%H:%M:%S") # Afin de ré-écrire les heures proprement
    if(testit::has_warning(format(ymd_hms(data$chsvi_heure), format="%H:%M:%S")) == FALSE) data$chsvi_heure <- format(ymd_hms(data$chsvi_heure), format="%H:%M:%S") # Afin de ré-écrire les heures proprement
    }
    
    # Transformation des numéros de capteurs
    data <- 
      data %>% 
      mutate(chsvi_capteur = str_replace(chsvi_capteur, "\\..*", "")) %>% # On supprime d'éventuels .0 à la fin
      mutate(chsvi_capteur = str_replace(chsvi_capteur, "O", "0")) %>%  # On supprime d'éventuels O par des 0
      mutate(chsvi_capteur = str_replace(chsvi_capteur, "\\?", NA_character_)) # On supprime d'éventuels ?
    
    # Travail sur les valeurs manuelles #
    data <-
      data %>% 
      mutate(chsvi_valeur = str_replace(chsvi_valeur, "°C", "")) %>% # On efface le °C
      mutate(chsvi_valeur = str_replace(chsvi_valeur, "°c", "")) %>% # On efface le °c
      mutate(chsvi_valeur = str_replace(chsvi_valeur, "°", "")) %>% # On efface le °
      mutate(chsvi_valeur = ifelse(chsvi_valeur == "-", NA, chsvi_valeur)) %>% # On met des NA pour les valeurs absentes
      mutate(chsvi_valeur = ifelse(chsvi_valeur == "Impossible", NA, chsvi_valeur)) %>% # On met des NA pour les valeurs absentes
      mutate(chsvi_valeur = as.numeric(sub(",", ".", .$chsvi_valeur))) %>%
      mutate(chsvi_valeur = round(as.numeric(chsvi_valeur), 2)) # On arrondi à 1 chiffre après la virgule

    # Transformation des unités
    data <-
      data %>% 
      mutate(chsvi_unite = str_replace(chsvi_unite, "degré Celsius", "°C"))

    # Transformation des actions
    data <-
      data %>% 
      mutate(chsvi_action = str_to_sentence(chsvi_action)) %>% # Pour n'avoir une majuscule qu'au début
      mutate(chsvi_action = dplyr::recode(chsvi_action,
                                       # "disparue" = "Disparue",
                                       "Sonde disparue" = "Disparue",
                                       "releve" = "Relève",
                                       # "relève" = "Relève",
                                       "Relève et repose" = "Relève",
                                       "Relevé" = "Relève",
                                       # "pose" = "Pose",
                                       "Repose" = "Pose"
                                       # "dépose" = "Dépose"
                                       )
             )

    # Vérification des types d'action
    if(dim(filter(data, chsvi_action == "changement de pile"))[1] > 0){
      data <- 
        data %>% 
        mutate(chsvi_remarques = ifelse(chsvi_action == "changement de pile", paste0(chsvi_remarques, " - Changement de pile"), chsvi_remarques)) %>% 
        mutate(chsvi_action = ifelse(chsvi_action == "changement de pile", "Relève", chsvi_action)) %>% 
        mutate(chsvi_remarques = ifelse(chsvi_remarques == "NA - Changement de pile", "Changement de pile", chsvi_remarques))
    }
    if(dim(filter(data, grepl("Dépose", chsvi_remarques)))[1] > 0){
    data <- 
      data %>% 
      mutate(chsvi_action = ifelse(grepl("Dépose", chsvi_remarques), "Dépose", chsvi_action))
    }
    if(dim(filter(data, !(chsvi_action == "Disparue"|chsvi_action == "Pose"|chsvi_action == "Dépose"|chsvi_action == "Relève"|chsvi_action == "Mesure manuelle"|chsvi_action == "Entretien")))[1] > 0) stop("Action saisie de type inconnu")
    
    # Transformation des formats
    data$id <- as.integer(data$id)
    data$chsvi_date <- as.character(data$chsvi_date)
    
    # Ajout des ID
    data$id <- row_number(data$chsvi_coderhj) + as.numeric(dbGetQuery(dbD, "SELECT MAX(id) FROM fd_production.chroniques_suiviterrain;")) # Pour incrémenter les id à partir du dernier
    data <- data %>% arrange(id)
    
    # Données générales et ordre des colonnes
    data <- 
      data %>% 
      mutate(`_modif_type` = "I") %>% 
      mutate(`_modif_date` = now()) %>% 
      select(match(colnames(SuiviTerrain),names(.)))

  }
  
  # Capteurs #
  if(all(colnames(data) %in% colnames(Capteurs))) {
    data$id <- row_number(data$chcap_numerocapteur) + as.numeric(dbGetQuery(dbD, "SELECT MAX(id) FROM fd_production.chroniques_capteurs;")) # Pour incrémenter les id à partir du dernier
  }
  
  # Stations #
  if(all(colnames(data) %in% colnames(Stations))) {
    data <-
      data %>% 
      mutate(id = row_number(data$chsta_coderhj) + as.numeric(dbGetQuery(dbD, "SELECT MAX(id) FROM fd_production.chroniques_stations;"))) %>% # Pour incrémenter les id à partir du dernier
      #arrange(id) %>% # Pour conserver le même ordre que celui dans le fichier de saisie
      mutate(chsta_mo = ifelse(chsta_mo == "FD39", "FJPPMA", chsta_mo)) %>% 
      mutate(chsta_mo = ifelse(chsta_mo == "CD39", "CD39_CR_Ain", chsta_mo)) %>% 
      mutate(chsta_mo = ifelse(chsta_mo == "CD39-FJPPMA", "CD39_CR_Ain - FJPPMA", chsta_mo)) %>% 
      mutate(chsta_mo = ifelse(chsta_mo == "ONEMA", "OFB", chsta_mo)) %>% 
      mutate(chsta_mo = ifelse(chsta_mo == "AFB", "OFB", chsta_mo)) %>% 
      mutate(chsta_mo = ifelse(chsta_mo == "CD39_CR_Ain - ONEMA", "CD39_CR_Ain - OFB", chsta_mo)) %>% 
      mutate(chsta_mo = ifelse(chsta_mo == "CD39_CR_Ain - AFB", "CD39_CR_Ain - OFB", chsta_mo)) %>% 
      mutate(chsta_profsonde = str_replace(chsta_profsonde, "m", "")) %>% # On efface le m
      mutate(chsta_profsonde = as.numeric(sub(",", ".", .$chsta_profsonde))) %>% # On efface le m
      mutate(chsta_distberge = str_replace(chsta_distberge, "m", "")) %>% # On efface le m
      mutate(chsta_distberge = as.numeric(sub(",", ".", .$chsta_distberge))) %>% # On efface le m
      mutate(chsta_numphoto = ifelse(is.na(chsta_numphoto), "acompleter.png", chsta_numphoto)) %>% 
      mutate(`_modif_type` = "I") %>% 
      mutate(`_modif_date` = now()) %>% 
      select(match(colnames(Stations),names(.)))
  }
  
  # Résultats #
  if(length(colnames(data)) > 22) {
    if(colnames(data)[46] == "Percentile90diurneAB"){
      data <- 
        data %>% 
        rename_all(list(~ stringi::stri_trans_general(., "latin-ascii"))) %>% # Pour remplacer les caractères accentués par les mêmes sans accents
        rename_all(list(~ paste0("chres_", .))) %>%
        rename_all(list(~ gsub("[[:punct:]]", "_", .))) %>%
        rename_all(list(~ tolower(.))) %>% 
        mutate(chres_aquatoolsversion = packageVersion("aquatools") %>% as.character()) %>% 
        mutate(id = row_number() + as.numeric(dbGetQuery(dbD, "SELECT MAX(id) FROM fd_production.chroniques_resultats"))) %>% # Pour incrémenter les id à partir du dernier
        mutate(`_modif_utilisateur` = NA_character_) %>% 
        mutate(`_modif_type` = "I") %>% 
        mutate(`_modif_date` = now()) %>% 
        select(id, everything(), `_modif_utilisateur`, `_modif_type`,`_modif_date`)
    }
  } # fin de travail sur les résultats
  } # Fin de travail sur les chroniques
  
  ##### PC #####
  Testtraitementforce <- 0
  if(traitementforce == TRUE & Type == "PC") Testtraitementforce <- 1
  if(traitementforce == FALSE & Type == "PC") Testtraitementforce <- 1
  if(Testtraitementforce == 1){
  ## Connexion à la BDD ##
  dbD <- BDD.ouverture("Data")
  
  ## Récupération des données ##
  PC <- tbl(dbD, in_schema("fd_production", "physicochimie_mesures")) %>% collect(n = 10)
  Operations <- tbl(dbD, in_schema("fd_production", "physicochimie_suiviterrain")) %>% collect(n = 10)

  ## Travail sur les mesures de PC ##
  if(all(colnames(data) %in% colnames(PC))) {
    
    # Transformation des formats
    data <-
      data %>% 
      mutate(pcmes_date = format(ymd(data$pcmes_date), format="%Y-%m-%d")) %>% # Car sinon transformation automatique des formats de date
    # Ajout des ID
      mutate(id = row_number() + as.numeric(dbGetQuery(dbD, "SELECT MAX(id) FROM fd_production.physicochimie_mesures;")))
  } # Fin de travail sur les mesures de PC
  } # Fin de travail sur PC
  
  ##### Temps de travail ####
  Testtraitementforce <- 0
  if(traitementforce == TRUE & Type == "Temps de travail") Testtraitementforce <- 1
  if(traitementforce == FALSE & Type == "Temps de travail") Testtraitementforce <- 1
  if(Testtraitementforce == 1){
    ## Connexion à la BDD ##
    dbD <- BDD.ouverture("Data")
    
    ## Récupération des données ##
    TpsWOpenTime <- structure(list(jour = structure(numeric(0), tzone = "UTC", class = c("POSIXct", 
                                                                                 "POSIXt")), domaine = character(0), activité = character(0), 
                           `sous-projet` = character(0), statut = character(0), `date de lancement` = character(0), 
                           `date de clôture` = character(0), métier = character(0), 
                           Utilisateur = character(0), `numéro de matricule` = logical(0), 
                           `temps (h)` = numeric(0), commentaire = character(0), Validé = character(0), 
                           `code analytique (domaine)` = logical(0), `code analytique` = logical(0), 
                           `financeur(s)...16` = logical(0), `financeur(s)...17` = logical(0), 
                           `coût horaire (direct)` = numeric(0), `coût horaire (production)` = numeric(0), 
                           description = logical(0), de = character(0), à = character(0)), row.names = integer(0), class = c("tbl_df", 
                                                                                                                             "tbl", "data.frame"))
    TpsW <- tbl(dbD, dbplyr::in_schema("fd_production", "tpstravail_detail")) %>% collect(n = 1)
    RecapTpsW <- tbl(dbD, dbplyr::in_schema("fd_production", "tpstravail_recapitulatif")) %>% collect(n = 1)
    Projets <- tbl(dbD, dbplyr::in_schema("fd_production", "tpstravail_projets")) %>% collect(n = Inf)
    Personnels <- tbl(dbD, dbplyr::in_schema("fd_referentiels", "gestion_operateurs")) %>% filter(gestop_mo == 3 & gestop_type == "Salarié") %>% select(id:gestop_qualite) %>% collect(n = Inf)
    
    ## Travail sur les données OpenTime ##
    if(all(colnames(data) %in% colnames(TpsWOpenTime))) {
      
      # Transformation des formats
      data <-
        data %>% 
          # nettoyage
          rename(date = jour, poste = `métier`, personnel = Utilisateur, duree = `temps (h)`, remarques = commentaire, validation = `Validé`, heuredebut = de, heurefin = `à`) %>% 
          select(-statut, -`date de lancement`, -`date de clôture`, -`numéro de matricule`, -poste) %>% 
          select(date:`sous-projet`, personnel, heuredebut, heurefin, duree, validation, remarques) %>%
          mutate(date = as_date(date)) %>% 
          mutate(personnel = ifelse(personnel == "Fagot Jean Baptiste", "Fagot Jean-Baptiste", personnel)) %>% # Ajout du tiret manquant
          mutate(validation = str_to_title(validation)) %>% # Ajout d'une majuscule à oui/non
          # Complément/modification
          left_join(Personnels %>% mutate(personnel = glue("{gestop_nom} {gestop_prenom}")), by = c("personnel")) %>% 
          mutate(personnel = id) %>% 
          select(-contains("gestop"), -id) %>% 
          # mise en forme 
          rename_all(list(~ stringi::stri_trans_general(., "latin-ascii"))) %>% # Pour remplacer les caractères accentués par les mêmes sans accents
          rename_all(list(~ paste0("tpswot_", .))) %>%
          rename_all(list(~ gsub("[[:punct:]]", "_", .))) %>%
          rename_all(list(~ tolower(.))) %>% 
          mutate(id = row_number() + as.numeric(dbGetQuery(dbD, "SELECT nextval('fd_production.tpstravail_opentime_id_seq');"))) %>% # Pour incrémenter les id à partir du dernier
          mutate(`_modif_utilisateur` = NA_character_) %>% 
          mutate(`_modif_type` = NA_character_) %>% 
          mutate(`_modif_date` = NA) %>% 
          select(id, everything(), `_modif_utilisateur`, `_modif_type`,`_modif_date`)
    } # Fin de travail sur les mesures de temps de travail
  } # Fin de travail sur Temps de travail
  
  ##### Commun #####
data <- as.data.frame(data)

  return(data)
  
} # Fin de la fonction
