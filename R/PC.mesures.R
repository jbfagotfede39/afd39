#' Collecte des mesures de physico-chimie
#'
#' Cette fonction permet de collecter les mesures de physico-chimie
#' @name PC.mesures
#' @param station Station recherchée (pas de filtrage si vide)
#' @param date Date recherchée (pas de filtrage si vide)
#' @param sortie Forme du dataframe de sortie - \code{Propre} (par défault) ou \code{Simple} ou \code{Complet}
#' @keywords pc
#' @import DBI
#' @import tidyverse
#' @export
#' @examples
#' PC.mesures()
#' PC.mesures(station = "SOR10-2")
#' PC.mesures("SOR10-2", sortie = "Propre")
#' PC.mesures("Lac de Chalain", date = "2010-01-01", sortie = "Propre")

PC.mesures <- function(
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
  mesures <- 
    tbl(dbD, in_schema("fd_production", "physicochimie_mesures")) %>% 
    {if(nchar(station) != 0) filter(., pcmes_coderhj == station) else .} %>% 
    {if(nchar(date) != 0) filter(., pcmes_date == date) else .} %>% 
    {if(sortie == "Simple") select(., pcmes_coderhj, pcmes_date, pcmes_heure, pcmes_parametresandre, pcmes_parametrenom, pcmes_valeur, contains("unite"), contains("support"), pcmes_coderemarque, contains("lacustre")) else .} %>%
    {if(sortie == "Propre") select(., everything(), -id, -contains("_modif")) else .} %>%
    arrange(desc(pcmes_date), pcmes_coderhj, pcmes_parametrenom) %>% 
    collect(n = Inf)
  
  ## Fermeture de la BDD ##
  DBI::dbDisconnect(dbD)

  ## Renommage ##
if(sortie != "Complet"){
  renommage <- c(Station = "pcmes_coderhj",
                 `Code SIE` = "pcmes_codesie",
                 Milieu = "pcmes_milieu",
                 Date = "pcmes_date",
                 Heure = "pcmes_heure",
                 `Paramètre code` = "pcmes_parametresandre",
                 `Paramètre libellé` = "pcmes_parametrenom",
                 Valeur = "pcmes_valeur",
                 `Unité libellé` = "pcmes_unitenom",
                 `Unité code` = "pcmes_unitesandre",
                 `Support libellé` = "pcmes_supportnom",
                 `Support code` = "pcmes_supportsandre",
                 `Qualification libellé` = "pcmes_qualificationnom",
                 `Qualification code` = "pcmes_qualificationsandre",
                 `Validation libellé` = "pcmes_validationnom",
                 `Validation code` = "pcmes_validationsandre",
                 `Vraisemblance code` = "pcmes_vraisemblancesandre",
                 `Profondeur plan d'eau` = "pcmes_profondeurlacustre",
                 `Zone plan d'eau` = "pcmes_zonelacustre",
                 `Laboratoire libellé` = "pcmes_laboratoirenom",
                 `Laboratoire SIRET` = "pcmes_laboratoiresiret",
                 `Fraction libellé` = "pcmes_fractionnom",
                 `Fraction code` = "pcmes_fractionsandre",
                 `Code remarque` = "pcmes_coderemarque",
                 `Code in situ` = "pcmes_codeinsitu",
                 `Limite quantification` = "pcmes_limitequantification",
                 `Limite détection` = "pcmes_limitedetection",
                 `Méthode libellé` = "pcmes_methodenom",
                 `Méthode code` = "pcmes_methodesandre",
                 `Accréditation` = "pcmes_accreditation",
                 `Producteur libellé` = "pcmes_producteurnom",
                 `Producteur code` = "pcmes_producteursandre",
                 `Origine producteur` = "pcmes_origineproducteursandre",
                 Remarques = "pcmes_remarques"
                 ) 
  
  mesures_v2 <- 
    mesures %>% 
    rename(any_of(renommage))
}
  
  ## Vérifications ##
  if(nrow(mesures_v2) == 0)
    warning("Attention : aucune mesure correspondante dans la base de données")

  return(mesures_v2)
  
} # Fin de la fonction