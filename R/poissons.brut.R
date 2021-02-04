#' Exportation des résultats bruts de pêche
#'
#' Cette fonction permet d'exporter les résultats bruts et élaborés de pêche au format excel et png
#' @name poissons.brut
#' @param station Code RHJ de la station
#' @param date Date de la pêche
#' @keywords poissons
#' @import glue
#' @import gridExtra
#' @import openxlsx 
#' @import tidyverse
#' @export
#' @examples
#' poissons.brut("SOR10-2", "2015-05-19")

poissons.brut <- function(
  station="AIN18-4",
  date="2011-09-07")
{

  ##### TODO LIST #####
  # 
  #####################
  
  ## Ouverture de la BDD ##
  dbP <- BDD.ouverture(Type = "Poissons")
  
  ## Récupération des données ##
  Captures <- tbl(dbP,"captures") %>% collect(n = Inf)
  Inventaires <- tbl(dbP,"inventaires") %>% collect(n = Inf)
  Stations <- tbl(dbP,"stations") %>% collect(n = Inf)
  
  ## Fermeture de la BDD ##
  DBI::dbDisconnect(dbP)
  
  ## Synthèse des données ##
  Captures <- merge(Captures, Inventaires, by = c("codeinventaire"))
  Captures <- merge(Captures, Stations, by = c("codestation"))
  
  ## Format de dates ##
  Captures$datedebut <- ymd(Captures$datedebut)
  
  ## Simplification ##
  Captures <- 
    Captures %>%
    dplyr::select(nom, datedebut, numerodepassage, codeespece, tailleminimum, taillemaximum, nombre, poids) %>% 
    filter(nom == station, datedebut == date) %>% 
    rename(Station = nom, Date = datedebut, Passage = numerodepassage, Espece = codeespece) %>% 
    arrange(Passage, Espece, nombre)
  Captures[Captures == 0] <- ""
  
  ##### Sorties des résultats traités au format Excel #####
  Resultats <- poissons.resultats.BDD() # Avec Aquatools
  Resultats$datedebut.x <- ymd(Resultats$datedebut.x)
  
  ## Résultats bruts
  Bruts <-
    Resultats %>%
    filter(nom == station, datedebut.x == date) %>%
    dplyr::select(nom, datedebut.x, codeespece, n_sommecapturepassage1, n_sommecapturepassage2, n_sommecapturepassage3, nombretotalcaptures, densitenumeriquebrute, biomassetotalecapturee, densiteponderalebrute) %>%
    arrange(codeespece)
  
  Bruts$densitenumeriquebrute <- round(Bruts$densitenumeriquebrute,1)
  Bruts$densiteponderalebrute <- round(Bruts$densiteponderalebrute,1)
  
  temporaire <-
    Bruts %>% 
    summarise(codeespece = n(),
              n_sommecapturepassage1 = sum(n_sommecapturepassage1),
              n_sommecapturepassage2 = sum(n_sommecapturepassage2),
              n_sommecapturepassage3 = sum(n_sommecapturepassage3),
              nombretotalcaptures = sum(nombretotalcaptures),
              densitenumeriquebrute = sum(densitenumeriquebrute),
              biomassetotalecapturee = sum(biomassetotalecapturee),
              densiteponderalebrute = sum(densiteponderalebrute)
    )
  temporaire$nom <- "TOTAL"
  Bruts <- merge(Bruts, temporaire, all=T)
  
  Bruts <- 
    Bruts %>% 
    dplyr::select(nom, datedebut.x, codeespece, n_sommecapturepassage1, n_sommecapturepassage2, n_sommecapturepassage3, nombretotalcaptures, densitenumeriquebrute, biomassetotalecapturee, densiteponderalebrute) %>%
    dplyr::rename(Date = datedebut.x) %>% 
    mutate(biomassetotalecapturee = round(biomassetotalecapturee/1000, 1)) %>% # Passage en kg, arrondi 1 chiffre après la virgule
    mutate(densiteponderalebrute = round(densiteponderalebrute/1000, 1)) # Passage en kg, arrondi 1 chiffre après la virgule

  colnames(Bruts) <- c("Station", "Date","Espèce","P1","P2","P3","Nb total","Ind/10a", "Biomasse (kg)", "kg/ha")
  
  # Suppression des valeurs d'effectifs si toutes les valeurs du P2 sont nulles
  if(length(unique(Bruts$P2)) == 1){
    if(unique(Bruts$P2) == 0) Bruts$P2 <- ""
  }
  
  # Suppression des valeurs d'effectifs si toutes les valeurs du P3 sont nulles
  if(length(unique(Bruts$P3)) == 1){
    if(unique(Bruts$P3) == 0) Bruts$P3 <- ""
  }
  
  ## Résultats élaborés
  Elabores <-
    Resultats %>%
    filter(nom == station, datedebut.x == date) %>%
    dplyr::select(nom, datedebut.x, codeespece, n_sommecapturepassage1, n_sommecapturepassage2, n_sommecapturepassage3, estimationeffectifnumerique, densitenumeriqueestimee, intervalleconfiancedensitenum, estimationeffectifponderal, densiteponderaleestimee, intervalleconfiancedensitepond, coteabondancenumerique, coteabondanceponderale) %>%
    arrange(codeespece)
  
  Elabores$densitenumeriqueestimee <- round(Elabores$densitenumeriqueestimee,1)
  Elabores$intervalleconfiancedensitenum <- round(Elabores$intervalleconfiancedensitenum,1)
  Elabores$densiteponderaleestimee <- round(Elabores$densiteponderaleestimee,1)
  Elabores$intervalleconfiancedensitepond <- round(Elabores$intervalleconfiancedensitepond,1)
  
  temporaire <-
    Elabores %>% 
    summarise(codeespece = n(),
              n_sommecapturepassage1 = sum(n_sommecapturepassage1),
              n_sommecapturepassage2 = sum(n_sommecapturepassage2),
              n_sommecapturepassage3 = sum(n_sommecapturepassage3),
              estimationeffectifnumerique = sum(estimationeffectifnumerique),
              densitenumeriqueestimee = sum(densitenumeriqueestimee),
              estimationeffectifponderal = sum(estimationeffectifponderal),
              densiteponderaleestimee = sum(densiteponderaleestimee)
              )
  temporaire$nom <- "TOTAL"
  Elabores <- merge(Elabores, temporaire, all=T)
  
  # Modification des unités
  Elabores <- 
    Elabores %>% 
    mutate(estimationeffectifponderal = round(estimationeffectifponderal/1000, 1)) %>%  # Passage en kg, arrondi 1 chiffre après la virgule
    mutate(densiteponderaleestimee = round(densiteponderaleestimee/1000, 1)) %>% # Passage en kg, arrondi 1 chiffre après la virgule
    mutate(intervalleconfiancedensitepond = round(intervalleconfiancedensitepond/1000, 3)) # Passage en kg, arrondi 3 chiffre après la virgule

  # Remise en ordre des colonnes et renommage #
  Elabores <- 
    Elabores %>% 
    dplyr::select(nom, datedebut.x, codeespece, n_sommecapturepassage1, n_sommecapturepassage2, n_sommecapturepassage3, estimationeffectifnumerique, densitenumeriqueestimee, intervalleconfiancedensitenum, estimationeffectifponderal, densiteponderaleestimee, intervalleconfiancedensitepond, coteabondancenumerique, coteabondanceponderale) %>% 
    dplyr::rename(Date = datedebut.x)
  colnames(Elabores)<-c("Station", "Date","Espèce","P1","P2","P3","Effectif estimé","Ind/10a","IC Ind/10a","Biomasse estimée (kg)","kg/ha","IC kg/ha", "CAN", "CAP")
  
  # Suppression des valeurs d'effectifs si toutes les valeurs du P2 sont nulles
  if(length(unique(Elabores$P2)) == 1){
    if(unique(Elabores$P2) == 0) Elabores$P2 <- ""
  }
  
  # Suppression des valeurs d'effectifs si toutes les valeurs du P3 sont nulles
  if(length(unique(Elabores$P3)) == 1){
    if(unique(Elabores$P3) == 0) Elabores$P3 <- ""
  }
  
  ###### Écriture des fichiers ######
  ## Captures excel ##
  captures <- createWorkbook() # Création d'un classeur
  addWorksheet(captures, sheetName = glue('{station} {date}')) # Ajout d'une feuille
  writeData(captures, 1, Captures, startCol = 1, startRow = 1, colNames = T) # Ajout des données
  setColWidths(captures, sheet = 1, cols = 2, widths = 10) # Largeur de colonne pour la date
  saveWorkbook(captures, glue('{station}_{date}_captures.xlsx'), overwrite = T) # save workbook
  
  ## Résultats calculés excel ##
  SortieResultats <- createWorkbook() # Création d'un classeur
  addWorksheet(SortieResultats, sheetName = "Bruts") # Ajout d'une feuille
  addWorksheet(SortieResultats, sheetName = "Calculés") # Ajout d'une feuille
  writeData(SortieResultats, 1, Bruts, startCol = 1, startRow = 1, colNames = T) # Ajout des données
  writeData(SortieResultats, 2, Elabores, startCol = 1, startRow = 1, colNames = T) # Ajout des données
  setColWidths(SortieResultats, sheet = 1, cols = 2, widths = 10) # Largeur de colonne pour la date
  setColWidths(SortieResultats, sheet = 2, cols = 2, widths = 10) # Largeur de colonne pour la date
  saveWorkbook(SortieResultats, glue('{station}_{date}_résultats.xlsx'), overwrite = T) # save workbook
  
  ## Résultats calculés png ##
  Elabores <- 
    Elabores %>% 
    mutate(Date = format(Date, format="%Y-%m-%d")) %>% 
    mutate_all(funs(replace(., is.na(.), ""))) %>% 
    mutate(Station = ifelse(row_number() == 1 | row_number() == max(row_number()), Station, ""))%>% 
    mutate(Date = ifelse(row_number() == 1, Date, ""))
  
  matriceGrasItalique <- matrix(c("plain", "plain", "plain", "plain", "plain", "plain", "plain", "bold", "plain", "plain", "bold", "plain", "bold", "bold"), ncol = ncol(Elabores), nrow = nrow(Elabores), byrow = TRUE)
  matriceGrasItalique[nrow(Elabores),] <- "bold.italic"
  
  tt1 <- 
    ttheme_minimal(
      core = list(
        bg_params = list(fill = blues9[1:2], col=NA),
        fg_params = list(fontface = matriceGrasItalique)
      ),
      colhead = list(fg_params=list(col="navyblue", fontface=4L)),
      rowhead = list(fg_params=list(col="white", fontface=3L))
    )
  
  png(paste0(station, "_", date, "_résultats.png", sep=""), height = 25*nrow(Elabores), width = 60*ncol(Elabores))
  grid.table(Elabores, theme = tt1, rows = NULL)
  dev.off()
  
} # Fin de la fonction