#' Identification de la machine
#'
#' Cette fonction permet d'identifier la machine sur laquelle le code est exécuté
#' 
#' @name general.identification
#' @import glue
#' @export

general.identification <- function(
    )
{

  #### Calcul ####
  id_machine <- "inconnu"
  if(system('uname -n', intern=T) == "MBPdeJeBaptiste") id_machine <- "jb_pro"
  if(system('uname -n', intern=T) == "r-ftp") id_machine <- "serveur"

  #### Vérification ####
  if(id_machine == "inconnu") warning(glue("La machine {system('uname -n', intern=T)} n'est pas listée dans la fonction general.identification du package afd39"))
  
  #### Sortie ####
  return(id_machine)
  
}
