#' Création d'une carte départementale
#'
#' Cette fonction permet de créer un fond de carte départementale au format ggplot2
#' @name carte.departement
#' @param occupation_sols Affichage de l'occupation des sols en couleur (\code{TRUE} par défaut)
#' @param routes_principales Affichage des routes principales (\code{TRUE} par défaut)
#' @param voies_ferrees Affichage des voies ferrées (\code{TRUE} par défaut)
#' @param communes_principales Affichage des communes principales (\code{FALSE} par défaut)
#' @param zoom Niveau de zoom du MNT (\code{8} par défaut)
#' @keywords cartographie
#' @import elevatr
#' @import ggspatial
#' @import metR
#' @import raster
#' @import sf
#' @import tidyverse
#' @export
#' @examples
#' carte.departement()
#' carte.departement(routes_principales = F, voies_ferrees = F)

carte.departement <- function(
    occupation_sols = T,
    routes_principales = T,
    voies_ferrees = T,
    communes_principales = F,
    zoom = 8
)
  {
  
  ## Évaluation des choix ##
  # Sortie <- match.arg(Sortie)
  # periode <- match.arg(periode)
  
  ## Ouverture de la BDD ##
  dbD <- BDD.ouverture("Data")
  
  ##### Collecte des données #####
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
  elevations <- elevatr::get_elev_raster(locations = departement_wgs84, z = zoom, clip = 'locations')
  elevations <- projectRaster(elevations,
                              crs = crs(departement))
  # Convert elevation data to dataframe
  elevations <- raster::as.data.frame(elevations, xy = TRUE)
  colnames(elevations)[3] <- 'elevation'
  # Remove rows with one or more NA's using complete.cases
  elevations <- elevations[complete.cases(elevations), ]

  ## Fermeture de la BDD ##
  DBI::dbDisconnect(dbD)
  
  #### Représentation ####
  ##### Chartes colorimétriques #####
  ###### CLC ######
  cols_clc <- c("1" = "#eee2f0", "2" = "#ffe601", "3" = "#33a02c", "4" = "#56a2ee", "5" = "#56a2ee")
  size_ce <- c("0" = "0.25", "1" = "0.3")
  
  ##### Construction générales #####
  gg <- ggplot()
  gg <- gg + geom_relief(data = elevations,
                         aes(x = x, y = y, z = elevation, light = 'white', dark = 'grey20'), 
                         raster = FALSE, interpolate = TRUE, sun.angle = 60)
  if(occupation_sols == T) gg <- gg + geom_sf(data = occupation, aes(fill = usgclc_code_simplifie), color = NA) + scale_fill_manual(values = alpha(cols_clc, 0.25))
  gg <- gg + geom_sf(data = coursdeau, color = "#0978ab", aes(linewidth = hycdogeop_rank)) + scale_linewidth(range = c(0.25, 0.55))
  gg <- gg + geom_sf(data = plansdeau, color = "#0978ab", fill = "#0978ab")
  if(voies_ferrees == T) gg <- gg + geom_sf(data = voies_fer, color = "#787878", linewidth = 1)
  if(voies_ferrees == T) gg <- gg + geom_sf(data = voies_fer, color = "#ffffff", linetype = "11", linewidth = 0.6)
  if(routes_principales == T) gg <- gg + geom_sf(data = routes_ppales %>% filter(trprte_type_route == "Autoroute"), color = "#787878", linewidth = 1)
  if(routes_principales == T) gg <- gg + geom_sf(data = routes_ppales %>% filter(trprte_type_route == "Autoroute"), color = "#ffe601", linewidth = 0.6)
  if(routes_principales == T) gg <- gg + geom_sf(data = routes_ppales %>% filter(trprte_type_route == "Nationale"), color = "#787878", linewidth = 0.85)
  if(routes_principales == T) gg <- gg + geom_sf(data = routes_ppales %>% filter(trprte_type_route == "Nationale"), color = "#ffffff", linewidth = 0.55)
  # gg <- gg + geom_sf(data = routes_ppales %>% filter(trprte_type_route == "Départementale"), color = "#787878", linewidth = 0.70)
  # gg <- gg + geom_sf(data = routes_ppales %>% filter(trprte_type_route == "Départementale"), color = "#ffffff", linewidth = 0.50)
  if(communes_principales == T) gg <- gg + geom_sf(data = communes_ppales)
  # gg 
  
  ##### Décorations et thème ##### 
  gg <- gg + theme_void() # Pas d'axes de coordonnées
  gg <- gg + theme(legend.position="none") # Pas de légende
  gg <- gg + annotation_scale(location = "tr")
  gg <- gg + annotation_north_arrow(location = "tl", which_north = "true", height = unit(0.75, "cm"), width = unit(0.75, "cm"))
  gg

  return(gg)
} # Fin de la fonction