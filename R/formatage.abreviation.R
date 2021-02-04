#' Exportation d'abréviations
#'
#' Cette fonction permet d'exporter la définition des acronymes
#' @name formatage.abreviation
#' @keywords glossaire
#' @param thematique Thématique recherchée : Complet (par défault), Écosystème, Espèce, Biologie, Chronique, Physico-chimie, Morphologie, Document, Institution, Usage
#' @param formatage Type de formatage de la sortie : \code{Aucun} (par défault), \code{Propre} ou \code{Latex}
#' @param export Si \code{TRUE}, exporte le glossaire. Si \code{FALSE} (par défault), ne l'exporte pas.
#' @param exportformat Format d'exportation des acronymes : \code{.xlsx} (par défault), \code{.pdf} ou \code{.tex}.
#' @import DBI
#' @import dbplyr
#' @import glue
#' @import stringi
#' @import tidyverse
#' @export
#' @examples
#' formatage.abreviation()
#' formatage.abreviation(formatage = "Latex", export = T)
#' formatage.abreviation(thematique = "Physico-chimie", formatage = "Propre", export = T)
#' 
#' NomsColonnes <- colnames(Operations)
#' acronymes <- formatage.abreviation()
#' Operations <-
#'   Operations %>%
#'   left_join(acronymes, by = c(Station = "Acronyme")) %>%
#'   select(-Station) %>%
#'   rename(Station = Definition)

#' Operations <-
#'   Operations %>%
#'   select(match(NomsColonnes,names(.)))

##### TODO LIST #####
# Ajouter un export propre sous forme de pdf
# Le dédoublonnage n'est pas réalisé, mais car par cas ? acronymes$dicglo_codeunique[duplicated(acronymes$dicglo_codeunique)]
#####################

formatage.abreviation <- function(
  thematique = c("Complet", "Écosystème", "Espèce", "Chronique", "Biologie", "Physico-chimie", "Morphologie", "Document", "Institution", "Usage"),
  formatage = c("Aucun","Propre", "Latex"),
  export = F,
  exportformat = c(".xlsx",".pdf", ".tex")
){

  #### Évaluation des choix ####
  thematique <- match.arg(thematique)
  formatage <- match.arg(formatage)
  
  if(formatage == "Latex"){
    export <- T
    exportformat <- ".tex"
    warning("Le formatage latex implique forcément un export au format .tex")
  }
  
  #### Importation ####
  db <- BDD.ouverture(Type = "Poissons")
  dbD <- BDD.ouverture("Data")
  Donnees <- tbl(dbD, in_schema("fd_referentiels", "dictionnaire_glossaire")) %>% collect()
  acronymesEcosystemes <- tbl(db, "ecosystemes") %>% dplyr::select(coderdt, nomecosysteme) %>% filter(!is.na(coderdt)) %>% collect(n = Inf) %>% dplyr::rename(dicglo_codeunique = coderdt, dicglo_nomcomplet = nomecosysteme) %>% mutate(dicglo_type = "Écosystème") %>% mutate(dicglo_acronyme = dicglo_codeunique)
  acronymesEsp <- tbl(db, "especes") %>% dplyr::select(codeespece, nomfrancais, nomfrancaispluriel) %>% filter(!is.na(codeespece)) %>% filter(codeespece != "N/A") %>% collect(n = Inf) %>% dplyr::rename(dicglo_codeunique = codeespece, dicglo_nomcomplet = nomfrancais, dicglo_nomcompletpluriel = nomfrancaispluriel) %>% mutate(dicglo_type = "Espèce") %>% mutate(dicglo_acronyme = dicglo_codeunique)
  DBI::dbDisconnect(db)
  DBI::dbDisconnect(dbD)
  
  #### Fusion des trois morceaux ####
  acronymes <- 
    Donnees %>% 
    bind_rows(acronymesEcosystemes) %>% 
    bind_rows(acronymesEsp)
  
  #### Thématique ####
  if(thematique != "Complet"){
    acronymes <-
      acronymes %>% 
      filter(dicglo_type == thematique)
  }
  
  #### Formatage ####
  if(formatage == "Propre"){
    acronymes <- 
      acronymes %>% 
      select(dicglo_acronyme, dicglo_nomcomplet, dicglo_definition) %>% 
      rename(Abréviation = dicglo_acronyme,
             Définition = dicglo_nomcomplet,
             Complément = dicglo_definition
      )
  }
  
  if(formatage == "Latex"){
    acronymes <-
      acronymes %>% 
      mutate(latex = ifelse(is.na(dicglo_nomcompletpluriel), paste0('\\newacronym{', dicglo_codeunique, '}{', dicglo_acronyme, '}{', dicglo_nomcomplet, '}'), paste0('\\newacronym[longplural=', dicglo_nomcompletpluriel, ']{', dicglo_codeunique, '}{', dicglo_acronyme, '}{', dicglo_nomcomplet, '}'))) %>% 
      select(latex) %>% 
      pull()
  }
  
  #### Export ####
  if(export == T){
        thematique <- 
          thematique %>% 
          stri_trans_general("latin-ascii") %>% 
          str_to_lower()
        
    acronymes %>% 
      {if(exportformat == ".xlsx") openxlsx::write.xlsx(., "Glossaire.xlsx", sheetName = "Glossaire", row.names = F, showNA = F, colWidths="auto") else .} %>% 
      {if(exportformat == ".tex") write(., file = glue("acronymes-{thematique}.tex")) else .}
  }
  
  #### Retour du tableau complet ####
  if(export == FALSE){
  return(acronymes)
  }
  
} # Fin de la fonction
