% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/poissons.synthese.R
\name{poissons.synthese}
\alias{poissons.synthese}
\title{Synthèse des données piscicoles}
\usage{
poissons.synthese(
  Synthese = "Présence",
  Territoire = NA_character_,
  Maille = c("Commune", "Canton", "ComCom", "Département", "Région", "ContextePDPG",
    "HER", "GEMAPI", "MO", "Milieu", "Bassin", "Sous-bassin", "INPN")
)
}
\arguments{
\item{Synthese}{Type de synthèse. Présence par défaut (pas d'autres types pour l'instant)}

\item{Territoire}{Territoire concerné : issu de stations.territoire}

\item{Maille}{Maille de synthèse des données : commune (par défaut), canton, communauté de communes, département, région, contexte de PDPG, Hydro-écorégion, entité GEMAPI, Maître d'ouvrage, Milieu, Bassin versant, Sous-bassin versant, maille INPN}
}
\description{
Cette fonction permet de réaliser différentes synthèses des enjeux piscicoles à différentes échelles spatiales (commune, communauté de communes, contexte de PDPG ou département)
}
\examples{
poissons.synthese("Présence", Territoire, "Commune")
poissons.synthese("Présence", Territoire, "ContextePDPG")
poissons.synthese("Présence", stations.territoire("ContextePDPG"), "ContextePDPG")
poissons.synthese("Présence", stations.territoire("Milieu", c("Lac de Chalain", "Bonlieu")), "ContextePDPG")
}
