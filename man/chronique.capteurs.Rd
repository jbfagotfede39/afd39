% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chronique.capteurs.R
\name{chronique.capteurs}
\alias{chronique.capteurs}
\title{Listage des capteurs de chroniques}
\usage{
chronique.capteurs(
  x = "CD39",
  Recherche = c("Propriétaire", "Type", "Modèle", "Numéro", "État", "Projet"),
  Sortie = c("Complet", "Propre", "Simple")
)
}
\arguments{
\item{x}{Nom recherché}

\item{Recherche}{Type de données recherchées}

\item{Sortie}{Forme du dataframe de sortie - \code{Complet} (par défault - tous les champs), \code{Propre} ou \code{Simple}}
}
\description{
Cette fonction permet de lister les capteurs de la BDD Chroniques
}
\examples{
chronique.capteurs("CD39","Propriétaire")
chronique.capteurs("Thermie","Type")
chronique.capteurs("Hobo UA-001-64","Modèle")
chronique.capteurs("10165890","Numéro")
chronique.capteurs("PDPG","Projet")
chronique.capteurs("OK","État")
SuiviCapteurs <- listeCapteurs$chmes_capteur \%>\% map_dfr(~ chronique.capteurs(., Recherche = "Numéro"))
}
\keyword{capteurs}
\keyword{chronique}
