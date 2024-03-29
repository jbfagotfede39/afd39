% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/poissons.stations.R
\name{poissons.stations}
\alias{poissons.stations}
\title{Extraction de données de stations}
\usage{
poissons.stations(poi_coderhj = NA_character_)
}
\arguments{
\item{poi_coderhj}{Nom de la station}
}
\description{
Cette fonction permet d'extraire les données complètes de l'ensemble des stations piscicoles (par défaut) ou d'une seule
}
\examples{
stations <- poissons.stations()
station <- poissons.stations("SOR10-2")
}
\keyword{stations}
