#' Envoi de mail
#'
#' Cette fonction permet d'envoyer des mails
#' 
#' @name general.mail
#' @param destinataire Destinataire du mail
#' @param objet Objet à inclure
#' @param message Message à inclure
#' @param expediteur Expéditeur du mail
#' @param expediteur_mdp Mot de passe du compte d'expédition
#' @import DBI
#' @import emayili
#' @import htmltools
#' @import tidyverse
#' @export

general.mail <- function(
    destinataire = NA_character_,
    objet = NA_character_,
    message = NA_character_,
    expediteur = NA_character_,
    expediteur_mdp = NA_character_
    )
{
  
  #### Évaluation des choix ####
  # Recherche <- match.arg(Recherche)
  
  #### Test de cohérence ####
  if(is.na(destinataire)) stop("Absence de destinataire")
  if(nchar(destinataire) == 0) stop("Absence de destinataire")
  if(is.na(objet)) stop("Absence d'objet")
  if(nchar(objet) == 0) stop("Absence d'objet")
  if(is.na(message)) stop("Absence de message")
  if(nchar(message) == 0) stop("Absence de message")
  if(is.na(expediteur)) stop("Absence d'expediteur")
  if(nchar(expediteur) == 0) stop("Absence d'expediteur")
  if(is.na(expediteur_mdp)) stop("Absence de mot de passe du compte expéditeur")
  if(nchar(expediteur_mdp) == 0) stop("Absence de mot de passe du compte expéditeur")
  
  #### Calcul ####
  ## Connexion smtp ##
  smtp <- server(host = "ssl0.ovh.net",
                 port = 465,
                 username = expediteur,
                 password = expediteur_mdp)
  
  # Création du message #
  email <- 
    envelope() %>%
    from(expediteur) %>%
    to(destinataire) %>%
    subject(objet) %>%
    text(message)
    # html(message)
  
  # Envoi du message #
  smtp(email, verbose = TRUE)
  
}
