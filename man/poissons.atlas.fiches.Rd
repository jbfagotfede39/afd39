% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/poissons.atlas.fiches.R
\name{poissons.atlas.fiches}
\alias{poissons.atlas.fiches}
\title{Génération SIG pour fiches atlas poissons}
\usage{
poissons.atlas.fiches(
  data = data,
  projet = NA_character_,
  commentaires = F,
  export = F
)
}
\arguments{
\item{data}{Jeu de données en entrée (opérations issues de poissons.operations)}

\item{projet}{Nom du projet}

\item{commentaires}{\code{FALSE} par défaut. Permet d'extraire le commentaire associé aux opérations}

\item{export}{\code{FALSE} par défaut. Permet d'exporter les données}
}
\description{
Cette fonction permet de générer le fichier geojson nécessaire à la création des fiches de l'atlas 2021-02-01_Modèle_atlas_poissons_V1
}
\examples{
poissons.atlas.fiches(data)
poissons.atlas.fiches(data, export=T)
poissons.captures("AIN18-4") \%>\% poissons.atlas.fiches(export=T)
}
\keyword{poissons}
