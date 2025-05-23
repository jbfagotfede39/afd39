% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/poissons.IPR.R
\name{poissons.IPR}
\alias{poissons.IPR}
\title{Exportation des IPR}
\usage{
poissons.IPR(
  ListeOperations = data.frame(Station = character(0), Date = character(0)),
  sortie = c("Simple", "Propre", "Complet")
)
}
\arguments{
\item{ListeOperations}{Dataframe contenant un colonne "Station" avec le code de la station (RHJ) et une colonne "Date"}

\item{sortie}{Forme du dataframe de sortie - \code{Simple} (par défaut), \code{Propre} (format diffusable, avec stations) ou \code{Complet} (tous les champs)}
}
\description{
Cette fonction permet d'exporter les résultats IPR de pêche
}
\examples{
poissons.IPR()
poissons.IPR(listeOperations)
poissons.IPR(data.frame(Station = "CUI3-8", Date = "2024-09-18"))
poissons.IPR(data.frame(Station = "CUI3-8", Date = "2024-09-18"), sortie = "Propre")
poissons.IPR(data.frame(Station = "CUI3-8", Date = "2024-09-18"), sortie = "Complet")
}
\keyword{poissons}
