#' Création d'un fond de carte
#'
#' Cette fonction permet de créer un fond de carte départemental ou local au format ggplot2
#' @name carte.territoire
#' @param emprise Objet sf (polygone)d'emprise de la carte à représenter
#' @param occupation_sols Affichage de l'occupation des sols en couleur (\code{TRUE} par défaut)
#' @param routes_principales Affichage des routes principales (\code{TRUE} par défaut)
#' @param voies_ferrees Affichage des voies ferrées (\code{TRUE} par défaut)
#' @param communes_principales Affichage des communes principales (\code{FALSE} par défaut)
#' @param altitude Affichage d'un fond altitudinal (\code{TRUE} par défaut)
#' @param zoom Niveau de zoom du MNT (\code{8} par défaut)
#' @param position_nord Position de la flèche du nord (\code{tl} par défaut)
#' @param position_echelle Position de l'échelle (\code{tr} par défaut)
#' @keywords cartographie
#' @import elevatr
#' @import ggspatial
#' @import metR
#' @import sf
#' @import tidyverse
#' @export
#' @examples
#' carte.territoire()
#' carte.territoire(emprise, altitude = F)
#' carte.territoire(routes_principales = F, voies_ferrees = F)

carte.territoire <- function(
    emprise = NA,
    occupation_sols = T,
    routes_principales = T,
    voies_ferrees = T,
    communes_principales = F,
    altitude = T,
    zoom = 8,
    position_nord = "tl",
    position_echelle = "tr"
)
{
  
  ## Évaluation des choix ##
  # Sortie <- match.arg(Sortie)
  # periode <- match.arg(periode)
  
  #### Test de cohérence ####
  if(length(emprise) == 1) emprise_definie <- T # Enveloppe sf
  if(length(emprise) != 1) emprise_definie <- F # Objet bbox <-> length == 4
  if(emprise_definie == F) warning("Pas d'emprise de définie")
  
  #### Collecte des données ####
  ## Ouverture de la BDD ##
  dbD <- BDD.ouverture("Data")
  
  ## Emprise ##
  emprise_wgs84 <- emprise %>% st_transform(4326)
  emprise_wgs84_bbox_df <- data.frame(x = runif(6,min=sf::st_bbox(emprise_wgs84)$xmin, 
                                           max=sf::st_bbox(emprise_wgs84)$xmax),
                                 y = runif(6,min=sf::st_bbox(emprise_wgs84)$ymin, 
                                           max=sf::st_bbox(emprise_wgs84)$ymax))
  
  ## Occupation des sols ##
  if(occupation_sols == T) occupation <- sf::st_read(dbD, query = "SELECT clc.usgclc_code_simplifie, st_intersection(clc.geom, dep.geom) as geom FROM fd_referentiels.usages_corinelandcover_2018 clc, fd_referentiels.topographie_departement dep WHERE st_intersects(clc.geom, dep.geom);")
  
  ## Transports ##
  if(routes_principales == T) routes_ppales <- sf::st_read(dbD, query = "select rte.trprte_type_route, st_intersection(rte.geom, dep.geom) as geom from fd_referentiels.transports_routes_ppales rte, fd_referentiels.topographie_departement dep WHERE st_intersects(rte.geom, dep.geom);")
  if(voies_ferrees == T) voies_fer <- sf::st_read(dbD, query = "select st_intersection(vf.geom, dep.geom) as geom from fd_referentiels.transports_voies_ferrees vf, fd_referentiels.topographie_departement dep WHERE st_intersects(vf.geom, dep.geom);")
  
  ## Hydrographie ##
  plansdeau <- sf::st_read(dbD, query = "select * from fd_referentiels.hydrographie_plansdeau;")
  # coursdeau <- sf::st_read(dbD, query = "select * from fd_referentiels.hydrographie_bdtopagecoursdeau;")
  coursdeau <- sf::st_read(dbD, query = "select * from fd_referentiels.hydrographie_coursdeaugeopeche;")
  
  ## Communes ##
  if(communes_principales == T) communes_ppales <- sf::st_read(dbD, query = "select * from fd_referentiels.topographie_communes_principales;")
  
  ## Département ##
  departement <- sf::st_read(dbD, query = "select * from fd_referentiels.topographie_departement;")
  departement_wgs84 <- departement %>% st_transform(4326)
  
  ## MNT ##
  if(altitude == T){
    if(emprise_definie == T) elevations <- elevatr::get_elev_raster(locations = emprise_wgs84_bbox_df, z = zoom, prj = st_crs(departement_wgs84), clip = 'locations')
    # if(emprise_definie == T) elevations <- elevatr::get_elev_raster(locations = emprise_wgs84, z = zoom, clip = 'locations')
    if(emprise_definie == F) elevations <- elevatr::get_elev_raster(locations = departement_wgs84, z = zoom, clip = 'locations')
    elevations <- raster::projectRaster(elevations,
                                        crs = raster::crs(departement))
    # Convert elevation data to dataframe
    elevations <- raster::as.data.frame(elevations, xy = TRUE)
    colnames(elevations)[3] <- 'elevation'
    # Remove rows with one or more NA's using complete.cases
    elevations <- elevations[complete.cases(elevations), ]
  }
  
  ## Fermeture de la BDD ##
  DBI::dbDisconnect(dbD)
  
  #### Calcul ####
  if(emprise_definie == T){
    # emprise_bbox <- emprise %>% st_bbox()
    # gg <- gg + st_crop(xlim = c(emprise_bbox$xmin, emprise_bbox$xmax), ylim = c(emprise_bbox$ymin, emprise_bbox$ymax))
    if(occupation_sols == T) occupation <- occupation %>% st_crop(st_bbox(emprise))
    if(routes_principales == T) routes_ppales <- routes_ppales %>% st_crop(st_bbox(emprise))
    if(voies_ferrees == T) voies_fer <- voies_fer %>% st_crop(st_bbox(emprise))
    plansdeau <- plansdeau %>% st_crop(st_bbox(emprise))
    coursdeau <- coursdeau %>% st_crop(st_bbox(emprise))
    if(communes_principales == T) communes_ppales <- communes_ppales %>% st_crop(st_bbox(emprise))
    departement <- departement %>% st_crop(st_bbox(emprise))
    # departement <- departement %>% st_crop(st_bbox(emprise))
  }
  
  #### Représentation ####
  ##### Chartes colorimétriques #####
  ###### CLC ######
  cols_clc <- c("1" = "#eee2f0", "2" = "#ffe601", "3" = "#33a02c", "4" = "#56a2ee", "5" = "#56a2ee")
  size_ce <- c("0" = "0.25", "1" = "0.3")
  
  ##### Construction générales #####
  gg <- ggplot()
  if(altitude == T) gg <- gg + geom_relief(data = elevations,
                                           aes(x = x, y = y, z = elevation, light = 'white', dark = 'grey20'), 
                                           raster = FALSE, interpolate = TRUE, sun.angle = 60, alpha = 0.25)
  if(occupation_sols == T) gg <- gg + geom_sf(data = occupation, aes(fill = usgclc_code_simplifie), color = NA) + scale_fill_manual(values = alpha(cols_clc, 0.25))
  gg <- gg + geom_sf(data = coursdeau, color = "#0978ab", aes(linewidth = hycdogeop_rank), alpha = 0.5) + scale_linewidth(range = c(0.25, 0.55))
  gg <- gg + geom_sf(data = plansdeau, color = alpha("#0978ab", 0.5), fill = "#0978ab", alpha = 0.5)
  if(voies_ferrees == T) gg <- gg + geom_sf(data = voies_fer, color = "#787878", linewidth = 1, alpha = 0.25)
  if(voies_ferrees == T) gg <- gg + geom_sf(data = voies_fer, color = "#ffffff", linetype = "11", linewidth = 0.6, alpha = 0.25)
  if(routes_principales == T) gg <- gg + geom_sf(data = routes_ppales %>% filter(trprte_type_route == "Autoroute"), color = "#787878", linewidth = 1, alpha = 0.25)
  if(routes_principales == T) gg <- gg + geom_sf(data = routes_ppales %>% filter(trprte_type_route == "Autoroute"), color = "#ffe601", linewidth = 0.6, alpha = 0.25)
  if(routes_principales == T) gg <- gg + geom_sf(data = routes_ppales %>% filter(trprte_type_route == "Nationale"), color = "#787878", linewidth = 0.85, alpha = 0.25)
  if(routes_principales == T) gg <- gg + geom_sf(data = routes_ppales %>% filter(trprte_type_route == "Nationale"), color = "#ffffff", linewidth = 0.55, alpha = 0.25)
  # gg <- gg + geom_sf(data = routes_ppales %>% filter(trprte_type_route == "Départementale"), color = "#787878", linewidth = 0.70)
  # gg <- gg + geom_sf(data = routes_ppales %>% filter(trprte_type_route == "Départementale"), color = "#ffffff", linewidth = 0.50)
  if(communes_principales == T) gg <- gg + geom_sf(data = communes_ppales, alpha = 0.25)
  # gg 
  
  ##### Décorations et thème ##### 
  gg <- gg + theme_void() # Pas d'axes de coordonnées
  gg <- gg + theme(legend.position = "none") # Pas de légende
  gg <- gg + annotation_scale(location = position_echelle)
  gg <- gg + annotation_north_arrow(location = position_nord, which_north = "true", height = unit(0.75, "cm"), width = unit(0.75, "cm"))
  gg
  
  return(gg)
} # Fin de la fonction