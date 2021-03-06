#' Création de fiches de résultats de pêche
#'
#' Cette fonction permet de créer une fiche de synthèse des résultats de pêche
#' @name poissons.fiche
#' @param station Code RHJ de la station
#' @param date Date de la pêche
#' @param commentaires \code{FALSE} par défault
#' @keywords poissons
#' @import DBI
#' @import knitr
#' @export
#' @examples
#' poissons.fiche("SOR10-2", "2015-05-19", commentaires = FALSE)

poissons.fiche <- function(
  station="SOR10-2",
  date="2015-05-19",
  commentaires = FALSE)
{

  ##### -------------- A FAIRE -------------- #####
  # Ajouter le code SIE de la station
  # Ajouter l'IPR, le nb d'espèces
  # Mettre le commentaire en valeur + ajouter expertise + avis court
  # -------------- A FAIRE -------------- #  
  
## Copie des logos ##
adresselogo <-
  find.package("aquatools") %>%
  paste0("/extdata/FD39aveccadre.png")
file_copy(adresselogo, "./", overwrite = TRUE)

adresseroseau <-
  find.package("aquatools") %>%
  paste0("/extdata/roseaux.jpg")
file_copy(adresseroseau, "./", overwrite = TRUE)
      
  
if(commentaires == FALSE) fileName <- system.file("extdata", "ModeleRenduPeche.Rnw", package = "aquatools") else fileName <- system.file("extdata", "ModeleRenduPecheCommente.Rnw", package = "aquatools")
ModeleRenduPeche <- readChar(fileName, file.info(fileName)$size)

date <- as.character(ymd(date))
#datejolie <- ymd_hms(date)
#datejolie <- paste(day(datejolie), month(datejolie), year(datejolie),sep="-")

ModeleRenduPeche <- str_replace_all(ModeleRenduPeche, "SOR10-2", station)
ModeleRenduPeche <- str_replace_all(ModeleRenduPeche, "2015-05-19", date)

write.table(ModeleRenduPeche,file=paste0(station, "_", date, ".rnw"), quote=F, row.names=F, col.names=F)

# knit2pdf le fichier en question en supprimant les fichiers temporaires
knit2pdf(paste0(station, "_", date, ".rnw"))

## Suppression des fichiers temporaires de compilation
liste <- c("bib", "aux", "bbl", "blg", "log", "out", "rnw", "xml", "tex")

for(i in 1:length(liste)){
  temp <- list.files(path=".", pattern = (paste0(liste[i],"$")))
  file.remove(temp)
}

## Suppression des logos ##
file_delete("./FD39aveccadre.png")
file_delete("./roseaux.jpg")

} # Fin de la fonction