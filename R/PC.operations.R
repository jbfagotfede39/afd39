#' Collecte des opérations de physico-chimie
#'
#' Cette fonction permet de collecter les opérations de physico-chimie
#' @name PC.operations
#' @param station Station recherchée (pas de filtrage si vide)
#' @param date Date recherchée (pas de filtrage si vide)
#' @param sortie Forme du dataframe de sortie - \code{Propre} (par défault) ou \code{Simple} ou \code{Complet}
#' @keywords pc
#' @import DBI
#' @import sf
#' @import tidyverse
#' @export
#' @examples
#' PC.operations()
#' PC.operations(station = "SOR10-2")
#' PC.operations("SOR10-2", sortie = "Propre")
#' PC.operations("Lac de Chalain", date = "2010-01-01", sortie = "Propre")

PC.operations <- function(
  station = "", 
  date = "",
  sortie = c("Propre","Simple","Complet")
)
{

  ## Évaluation des choix
  sortie <- match.arg(sortie)
  
  ## Connexion à la BDD
  dbD <- BDD.ouverture("Data")

  ## Récupération des données ##
  operations <- 
    tbl(dbD, in_schema("fd_production", "physicochimie_suiviterrain")) %>% 
    {if(nchar(station) != 0) filter(., pcsvi_coderhj == station) else .} %>% 
    {if(nchar(date) != 0) filter(., pcsvi_date == date) else .} %>% 
    {if(sortie == "Simple") select(., pcsvi_mo, pcsvi_coderhj, pcsvi_date) else .} %>% 
    {if(sortie == "Propre") select(., everything(), -id, -contains("_modif")) else .} %>% 
    arrange(desc(pcsvi_date), pcsvi_coderhj) %>% 
    collect(n = Inf)
  
  ## Fermeture de la BDD ##
  DBI::dbDisconnect(dbD)
  
  ## Suppression de la géométrie ##
if(sortie != "Complet"){
  operations_v2 <-
    operations %>% 
    st_drop_geometry()
}    
  ## Renommage ##
if(sortie != "Complet"){
  renommage <- c(MO = "pcsvi_mo",
                 Station = "pcsvi_coderhj",
                 `Code SIE` = "pcsvi_codesie",
                 Date = "pcsvi_date",
                 Heure = "pcsvi_heure",
                 `Coordonnée X` = "pcsvi_coord_x",
                 `Coordonnée Y` = "pcsvi_coord_y",
                 Projection = "pcsvi_coord_type",
                 Précision = "pcsvi_coord_precision",
                 Opérateurs = "pcsvi_operateurs",
                 Matériel = "pcsvi_materiel",
                 Météo = "pcsvi_meteo",
                 Hydrologie = "pcsvi_hydrologie",
                 Remarques = "pcsvi_remarques"
                 ) 
  
  operations_v3 <- 
    operations_v2 %>% 
    rename(any_of(renommage))
}
  
  ## Vérifications ##
  if(nrow(operations_v3) == 0)
    warning("Attention : aucune opération correspondante dans la base de données")

  return(operations_v3)
  
} # Fin de la fonction