#' Application suivi temps de travail
#'
#' Cette fonction permet de lancer l'application shiny de suivi du temps de travail
#' 
#' @name app.tps_travail
#' @export
#' @examples
#' app.tps_travail()

app.tps_travail <- function(){
  shiny::runApp(
    adresse.switch("/FD_Perso/Scripts_JB/Temps de travail_scripts/00_Application_shiny_suivi_tps_travail/") ,
    launch.browser = TRUE,  # Ouvre dans le navigateur
    port = 3838             # Port spécifique
  )
  
} # Fin de la fonction