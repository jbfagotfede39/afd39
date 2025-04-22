#' Permuter la localisation de fichier
#'
#' Cette fonction permet de retrouver automatiquement l'adresse d'un fichier entre différentes machines
#' 
#' @name adresse.switch
#' @param url Adresse du fichier à ouvrir
#' @import glue
#' @export
#' @examples
#' adresse.switch("NAS-JB/Études/2014_Les Rousses/Résultats/Poissons/ResultatsRoussestotal-28janvier2015.xlsx")
#' adresse.switch("NAS-DATA/Poissons/PDPG/Stations PDPG_V1.xlsx")
#' fish <- read_excel(adresse.switch("NAS-JB/Études/2014_Les Rousses/Résultats/Poissons/ResultatsRoussestotal-28janvier2015.xlsx"), sheet = 2)

adresse.switch <- function(
  url = "")
  
{
  
  #### Test de la présence d'une URL ####
  if(nchar(url) == 0) url = readline(prompt = "Saisir une adresse de fichier : ")
  
  #### Transformation du format ####
  if(file.exists(glue("/Users/jean-baptistefagot/{url}"))) url <- glue("/Users/jean-baptistefagot/{url}") # #129 - Machine JB
  if(file.exists(glue("/Users/jean-baptistefagot/Nextcloud/{url}"))) url <- glue("/Users/jean-baptistefagot/Nextcloud/{url}") # #129 - Machine JB
  if(file.exists(glue("/Users/jean-baptistefagot/Nextcloud_perso/{url}"))) url <- glue("/Users/jean-baptistefagot/Nextcloud_perso/{url}") # #129 - Machine JB
  if(file.exists(glue("/Users/anne-lauretruchot/Nextcloud/{url}"))) url <- glue("/Users/anne-lauretruchot/Nextcloud/{url}") #
  if(file.exists(glue("/Users/adrienlavigne/", url))) url <- glue("/Users/adrienlavigne/{url}") # #4 - Machine Adrien

  #### Retour de l'adresse correcte ####
  return(url)
  
} # Fin de la fonction