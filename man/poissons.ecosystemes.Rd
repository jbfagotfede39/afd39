% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/poissons.ecosystemes.R
\name{poissons.ecosystemes}
\alias{poissons.ecosystemes}
\title{Extraction de données des écosystèmes}
\usage{
poissons.ecosystemes(poi_ecosysteme = NA_character_)
}
\arguments{
\item{poi_ecosysteme}{Nom de l'écosystème}
}
\description{
Cette fonction permet d'extraire les données complètes de l'ensemble des écosystèmes (par défaut) ou d'un seul
}
\examples{
listeCE <- poissons.ecosystemes()
listeCE <- poissons.ecosystemes("Valouse")
}
\keyword{poissons}
