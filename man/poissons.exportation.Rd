% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/poissons.exportation.R
\name{poissons.exportation}
\alias{poissons.exportation}
\title{Extraire les résultats de pêches d'inventaire}
\usage{
poissons.exportation(
  station = "AIN18-4",
  date = "2011-09-07",
  commentaires = FALSE
)
}
\arguments{
\item{station}{Code RHJ de la station}

\item{date}{Date de la pêche}

\item{commentaires}{\code{FALSE} par défault. Permet d'extraire le commentaire associé à la pêche}
}
\description{
Permet d'extraire les résultats de pêches d'inventaire
}
\examples{
poissons.exportation("SOR10-2", "2015-05-19")
poissons.exportation("SOR10-2", "2015-05-19", commentaires = T)
}
\keyword{poissons}
