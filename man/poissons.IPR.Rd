% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/poissons.IPR.R
\name{poissons.IPR}
\alias{poissons.IPR}
\title{Exportation des IPR}
\usage{
poissons.IPR(
  ListeOperations = data.frame(Station = character(0), Date = character(0)),
  expertise = TRUE
)
}
\arguments{
\item{ListeOperations}{Dataframe contenant un colonne "Station" avec le code de la station (RHJ) et une colonne "Date"}

\item{expertise}{\code{TRUE} par défault}
}
\description{
Cette fonction permet d'exporter les résultats IPR de pêche
}
\examples{
poissons.IPR()
poissons.IPR(listeOperations)
poissons.IPR(data.frame(Station = "SOR10-2", Date = "2012-11-03"))
poissons.IPR(data.frame(Station = "SOR10-2", Date = "2012-11-03"), expertise = FALSE)
}
\keyword{poissons}
