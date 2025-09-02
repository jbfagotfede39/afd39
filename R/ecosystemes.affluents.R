#' Listage des affluents
#'
#' Cette fonction permet de lister les affluents d'un cours d'eau donné
#' @name ecosystemes.affluents
#' @param ecosysteme Nom du cours d'eau principal
#' @keywords écosystèmes
#' @import tidyverse
#' @export
#' @examples
#' ecosystemes.affluents("Ain")

##### TODO LIST #####
# Rajouter le choix de l'écosystème avec les codes (avec recherche dans le champ observation) et de même avec le code de la masse d'eau, avec un interrupteur disant où la fonction doit chercher initialement. Le reste du code reste identique.
#####################

ecosystemes.affluents <- function(
  ecosysteme="Bienne")
{
  
  ## Ouverture de la BDD ##
  dbP <- BDD.ouverture(Type = "Poissons")
  
  ## Récupération des données ##
  Ecosystemes <- tbl(dbP,"ecosystemes") %>% collect(n = Inf)
  
  ## Extraction des afférences ##
  # Test si le nom existe bien, sinon message d'erreur et arrêt de la fonction #

if(dim(Ecosystemes %>% filter(nomecosysteme == ecosysteme)
       )[1] == 0) 
  stop("Attention : nom d'écosystème absent de la base de données")
  
EcosystemeRecherche <-
  Ecosystemes %>% 
  filter(nomecosysteme == ecosysteme)

  # Extraction du Codeecosysteme du CE qui nous concerne
#EcosystemeRecherche[,1]

  # Recherche des cours d'eau qui ont un codeaffluent = à ce Codeecosysteme
EcosystemeRechercheV2 <-
  EcosystemeRecherche %>% 
  full_join(Ecosystemes %>% 
  filter(codeaffluent == EcosystemeRecherche[,1] %>% pull()), by = c("codeecosysteme", "nomecosysteme", "typeecosysteme", "codeaffluent", "nbstations", "nbinventaires", "observations", "coderdt", "abbreviation", "codesandre", "codepvempoissonnement", "categorie", "_creationdate", "_modificationdate", "__last_user_id")
  )

# 1ère itération pour remonter d'un niveau
EcosystemeRechercheV3 <- EcosystemeRechercheV2
for(i in 1:dim(EcosystemeRechercheV2)[1]){
EcosystemeRechercheV3 <-
  EcosystemeRechercheV3 %>% 
  full_join(Ecosystemes %>% 
              filter(codeaffluent == EcosystemeRechercheV2$codeecosysteme[i]), by = c("codeecosysteme", "nomecosysteme", "typeecosysteme", "codeaffluent", "nbstations", "nbinventaires", "observations", "coderdt", "abbreviation", "codesandre", "codepvempoissonnement", "categorie", "_creationdate", "_modificationdate", "__last_user_id")
  )
}

# 2ème itération pour remonter d'un niveau
EcosystemeRechercheV4 <- EcosystemeRechercheV3
for(i in 1:dim(EcosystemeRechercheV3)[1]){
  EcosystemeRechercheV4 <-
    EcosystemeRechercheV4 %>% 
    full_join(Ecosystemes %>% 
                filter(codeaffluent == EcosystemeRechercheV3$codeecosysteme[i]), by = c("codeecosysteme", "nomecosysteme", "typeecosysteme", "codeaffluent", "nbstations", "nbinventaires", "observations", "coderdt", "abbreviation", "codesandre", "codepvempoissonnement", "categorie", "_creationdate", "_modificationdate", "__last_user_id")
    )
}

# 3ème itération pour remonter d'un niveau
EcosystemeRechercheV5 <- EcosystemeRechercheV4
for(i in 1:dim(EcosystemeRechercheV4)[1]){
  EcosystemeRechercheV5 <-
    EcosystemeRechercheV5 %>% 
    full_join(Ecosystemes %>% 
                filter(codeaffluent == EcosystemeRechercheV4$codeecosysteme[i]), by = c("codeecosysteme", "nomecosysteme", "typeecosysteme", "codeaffluent", "nbstations", "nbinventaires", "observations", "coderdt", "abbreviation", "codesandre", "codepvempoissonnement", "categorie", "_creationdate", "_modificationdate", "__last_user_id")
    )
}

# 4ème itération pour remonter d'un niveau
EcosystemeRechercheV6 <- EcosystemeRechercheV5
for(i in 1:dim(EcosystemeRechercheV5)[1]){
  EcosystemeRechercheV6 <-
    EcosystemeRechercheV6 %>% 
    full_join(Ecosystemes %>% 
                filter(codeaffluent == EcosystemeRechercheV5$codeecosysteme[i]), by = c("codeecosysteme", "nomecosysteme", "typeecosysteme", "codeaffluent", "nbstations", "nbinventaires", "observations", "coderdt", "abbreviation", "codesandre", "codepvempoissonnement", "categorie", "_creationdate", "_modificationdate", "__last_user_id")
    )
}
  
return(EcosystemeRechercheV6)

} # Fin de la fonction