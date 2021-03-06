% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chronique.resultats.R
\name{chronique.resultats}
\alias{chronique.resultats}
\title{Listage des résultats de chroniques}
\usage{
chronique.resultats(
  x = NA_character_,
  Recherche = c("Station", "Année biologique")
)
}
\arguments{
\item{Recherche}{Type de données recherchées}

\item{Nom}{Nom recherché}
}
\description{
Cette fonction permet de lister les résultats de la BDD Chroniques
}
\examples{
chronique.resultats("HER0-6","Station")
chronique.resultats("2017","Année biologique")
Resultats <- Stations$chsta_coderhj \%>\% map_dfr(~ chronique.resultats(., Recherche = "Station"))
}
\keyword{chronique}
