#' Attribution de classes de qualité
#'
#' Cette fonction permet d'attribuer des classes de qualité à des valeurs physico-chimiques
#' @name PC.classes
#' @param PC Jeu de données
#' @param Referentiel Referentiel de qualité : NV (par défaut), SEQ-EAU, Quebec ou Arrete4aout2006tableau4
#' @param Categorie Catégorie piscicole (1 par défaut)
#' @import tidyverse
#' @import reshape2
#' @export
#' @examples
#' PC.classes(PC)
#' PC.classes(PC,Referentiel="NV")
#' PC.classes(PC,Referentiel="SEQ-EAU")
#' PC.classes(PC,Referentiel="Quebec")
#' PC.classes(PC,Referentiel="SEQ-EAU",Categorie = 2)

##### TODO LIST #####
# Voir pour intégrer un passage par PC.agregation avant attribution des classes de qualité ?
# Ajout d'un filtre si inférieur au seuil de detection avec commentaire en fonction de la valeur de ce seuil/toxicité
# Notion d'unité : à faire avec la clé : parametre-Matrice-Unite
# Option permettant différentes sorties (couleurs/valeurs etc.) = Ajout d'une option permettant de choisir le mode de sortie (en l'état avec 2 colonnes de + ou bien différents types de matrices de synthèse (valeurs, ClasseQualite, couleurs) comme dans fichier PC_V3.R)
# Complément base de données (ETM SEQ-Eau et Québec ok)
# Intégration base de données seuils dans base de données PC
# Il faudrait qu'il n'y ait pas de NA dans les colonnes ClasseQualite et Referentiel suite au traitement
#####################

PC.classes <- function(
  PC,
  Referentiel = c("NV", "SEQ-EAU", "Quebec", "Arrete4aout2006tableau4"),
  Categorie = 1
  )
{
  
  ## Évaluation des choix
  Referentiel <- match.arg(Referentiel)
  
  ##### Chargement des Referentiels #####
  data(Seuils_PC) # Pour charger les seuils de qualité
   
###### Nisbet et Verneaux #####
if(Referentiel == "NV") {
  
  ## Création des seuils ##
  Seuils <- 
    data %>% 
    filter(Referentiel == "Nisbet - Verneaux") %>% 
    tidyr::unite(Seuil, c(Seuil, ClasseQualite), remove=T, sep = "-") %>% 
    dcast(Referentiel + pcmes_parametresandre + pcmes_supportsandre ~ Seuil, value.var = "ValeurSeuil") %>% 
    tidyr::unite(Cl, c(pcmes_parametresandre, pcmes_supportsandre), remove=T, sep = "-")
  
  ## Attribution des classes de qualité ##
  ClasseQualites <-
    PC %>% 
    tidyr::unite(Cl, c(pcmes_parametresandre, pcmes_supportsandre), remove=F, sep = "-") %>% 
    left_join(Seuils, by = c("Cl" = "Cl")) %>% 
    mutate(pcmes_valeur = as.numeric(sub(",", ".", pcmes_valeur))) %>% 
    mutate(ClasseQualite = NA) %>% 
    mutate(ClasseQualite = ifelse(is.na(`Minimum-1`) & is.na(`Minimum-2`) & is.na(`Minimum-3`) & is.na(`Minimum-4`) & is.na(`Minimum-5`) & is.na(`Minimum-6`) & is.na(`Minimum-7`), # Pour le cas où il y a 8 classes = 7 seuils <=> SANDRE 1305
                                  case_when(.$pcmes_valeur < .$`Maximum-1` ~ "Classe 1",
                                            .$pcmes_valeur >= .$`Maximum-1` & .$pcmes_valeur < .$`Maximum-2` ~ "Classe 2",
                                            .$pcmes_valeur >= .$`Maximum-2` & .$pcmes_valeur < .$`Maximum-3` ~ "Classe 3",
                                            .$pcmes_valeur >= .$`Maximum-3` & .$pcmes_valeur < .$`Maximum-4` ~ "Classe 4",
                                            .$pcmes_valeur >= .$`Maximum-4` & .$pcmes_valeur < .$`Maximum-5` ~ "Classe 5",
                                            .$pcmes_valeur >= .$`Maximum-5` & .$pcmes_valeur < .$`Maximum-6` ~ "Classe 6",
                                            .$pcmes_valeur >= .$`Maximum-6` & .$pcmes_valeur < .$`Maximum-7` ~ "Classe 7",
                                            .$pcmes_valeur >= .$`Minimum-8` ~ "Classe 8",
                                            TRUE ~ "Pas de classe"),  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
                                  ClasseQualite)
    ) %>% 
    mutate(ClasseQualite = ifelse(Cl == "1312-3", # Pour le cas où SANDRE 1312 <-> 6 cas
                                  case_when(.$pcmes_valeur < .$`Maximum-6` ~ "Classe 6",
                                            .$pcmes_valeur >= .$`Minimum-5` & .$pcmes_valeur < .$`Minimum-4` ~ "Classe 5",
                                            .$pcmes_valeur >= .$`Minimum-4` & .$pcmes_valeur < .$`Minimum-3` ~ "Classe 4",
                                            .$pcmes_valeur >= .$`Minimum-3` & .$pcmes_valeur < .$`Minimum-2` ~ "Classe 3",
                                            .$pcmes_valeur >= .$`Minimum-2` & .$pcmes_valeur < .$`Minimum-1` ~ "Classe 2",
                                            .$pcmes_valeur >= .$`Minimum-1` ~ "Classe 1",
                                            TRUE ~ "Pas de classe"),  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
                                  ClasseQualite)
    ) %>% 
    mutate(ClasseQualite = ifelse(Cl == "1313-3", # Pour le cas où SANDRE 1313 <-> 4 cas
                                  case_when(.$pcmes_valeur < .$`Maximum-1` ~ "Classe 1",
                                            .$pcmes_valeur >= .$`Maximum-1` & .$pcmes_valeur < .$`Maximum-2` ~ "Classe 2",
                                            .$pcmes_valeur >= .$`Maximum-2` & .$pcmes_valeur < .$`Maximum-3` ~ "Classe 3",
                                            .$pcmes_valeur >= .$`Minimum-4` ~ "Classe 4",
                                            TRUE ~ "Pas de classe"),  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
                                  ClasseQualite)
    ) %>% 
    mutate(ClasseQualite = ifelse(Cl == "1335-3", # Pour le cas où SANDRE 1335 <-> 3 cas
                                  case_when(.$pcmes_valeur < .$`Maximum-1` ~ "Classe 1",
                                            .$pcmes_valeur >= .$`Maximum-1` & .$pcmes_valeur < .$`Minimum-3` ~ "Classe 2",
                                            .$pcmes_valeur >= .$`Minimum-3` ~ "Classe 3",
                                            TRUE ~ "Pas de classe"),  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
                                  ClasseQualite)
    ) %>% 
    mutate(ClasseQualite = ifelse(Cl == "1337-3" | Cl == "1338-3" , # Pour le cas où SANDRE 1337 et 1338 <-> 7 cas
                                  case_when(.$pcmes_valeur < .$`Maximum-1` ~ "Classe 1",
                                            .$pcmes_valeur >= .$`Maximum-1` & .$pcmes_valeur < .$`Maximum-2` ~ "Classe 2",
                                            .$pcmes_valeur >= .$`Maximum-2` & .$pcmes_valeur < .$`Maximum-3` ~ "Classe 3",
                                            .$pcmes_valeur >= .$`Maximum-3` & .$pcmes_valeur < .$`Maximum-4` ~ "Classe 4",
                                            .$pcmes_valeur >= .$`Maximum-4` & .$pcmes_valeur < .$`Maximum-5` ~ "Classe 5",
                                            .$pcmes_valeur >= .$`Maximum-5` & .$pcmes_valeur < .$`Maximum-6` ~ "Classe 6",
                                            .$pcmes_valeur >= .$`Minimum-7` ~ "Classe 7",
                                            TRUE ~ "Pas de classe"),  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
                                  ClasseQualite)
    ) %>% 
    mutate(ClasseQualite = ifelse(Cl == "1339-3", # Pour le cas où SANDRE 1339 <-> 4 cas
                                  case_when(.$pcmes_valeur < .$`Maximum-1` ~ "Classe 1",
                                            .$pcmes_valeur >= .$`Maximum-1` & .$pcmes_valeur < .$`Maximum-2` ~ "Classe 2",
                                            .$pcmes_valeur >= .$`Maximum-2` & .$pcmes_valeur < .$`Maximum-3` ~ "Classe 3",
                                            .$pcmes_valeur >= .$`Minimum-4` ~ "Classe 4",
                                            TRUE ~ "Pas de classe"),  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
                                  ClasseQualite)
    ) %>% 
    mutate(ClasseQualite = ifelse(Cl == "1333-3" | Cl == "1340-3" , # Pour le cas où SANDRE 1333 et 1340 <-> 6 cas
                                  case_when(.$pcmes_valeur < .$`Maximum-1` ~ "Classe 1",
                                            .$pcmes_valeur >= .$`Maximum-1` & .$pcmes_valeur < .$`Maximum-2` ~ "Classe 2",
                                            .$pcmes_valeur >= .$`Maximum-2` & .$pcmes_valeur < .$`Maximum-3` ~ "Classe 3",
                                            .$pcmes_valeur >= .$`Maximum-3` & .$pcmes_valeur < .$`Maximum-4` ~ "Classe 4",
                                            .$pcmes_valeur >= .$`Maximum-4` & .$pcmes_valeur < .$`Maximum-5` ~ "Classe 5",
                                            .$pcmes_valeur >= .$`Minimum-6` ~ "Classe 6",
                                            TRUE ~ "Pas de classe"),  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
                                  ClasseQualite)
    ) %>% 
    mutate(ClasseQualite = ifelse(pcmes_coderemarque == 10 & !is.na(`Referentiel`), "< seuil quantification", ClasseQualite)) %>%  # Pour compléter les cas inférieurs au seuil de quantification
    mutate(ClasseQualite = ifelse(pcmes_coderemarque == 2 & !is.na(`Referentiel`), "< seuil detection", ClasseQualite)) %>%  # Pour compléter les cas inférieurs au seuil de detection 
    select(-(`Maximum-1`:`Minimum-8`), -Cl) %>% 
    mutate(Couleur = case_when(.$ClasseQualite == "Classe 1" ~ "Bleu",
                               .$ClasseQualite == "Classe 2" ~ "Vert",
                               .$ClasseQualite == "Classe 3" ~ "Vert clair",
                               .$ClasseQualite == "Classe 4" ~ "Jaune",
                               .$ClasseQualite == "Classe 5" ~ "Orange",
                               .$ClasseQualite == "Classe 6" ~ "Rouge",
                               .$ClasseQualite == "Classe 7" ~ "Violet",
                               .$ClasseQualite == "Classe 8" ~ "Noir",
                               .$ClasseQualite == "< seuil detection" ~ "Gris clair",
                               .$ClasseQualite == "< seuil quantification" ~ "Gris",
                               TRUE ~ "Pas de classe"))  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
}
  
###### SEQ-EAU #####
if(Referentiel == "SEQ-EAU") {
  
  ## Création des seuils ##
  Seuils <- 
    data %>% 
    filter(Referentiel == "SEQ-Eau par alteration" & pcmes_parametresandre != "1301") %>% # On enlève la température de l'eau
    filter(!(pcmes_supportsandre == 3 & (pcmes_parametresandre == "1382" | pcmes_parametresandre == "1383" | pcmes_parametresandre == "1386" | pcmes_parametresandre == "1388" | pcmes_parametresandre == "1389" | pcmes_parametresandre == "1392"))) %>% # Afin d'éliminer certains ETM car dépendants de la dureté donc traitement à part
    bind_rows(filter(data, Referentiel == "SEQ-Eau par alteration" & (pcmes_parametresandre_condition == 2 & Valeur_condition == Categorie))) %>% 
    tidyr::unite(Seuil, c(Seuil, ClasseQualite), remove=T, sep = "-") %>% 
    dcast(Referentiel + pcmes_parametresandre + pcmes_supportsandre ~ Seuil, value.var = "ValeurSeuil") %>% 
    tidyr::unite(Cl, c(pcmes_parametresandre, pcmes_supportsandre), remove=T, sep = "-")
  
  # Calcul de la dureté par operation ##
  Durete <-
    PC %>%
    filter(pcmes_parametresandre == "1345") %>%
    mutate(pcmes_valeur = as.numeric( sub(",", ".", pcmes_valeur))) %>%
    distinct(pcmes_coderhj, pcmes_date, pcmes_valeur) %>%
    rename(Durete = pcmes_valeur)
  PC <-
    PC %>%
    left_join(Durete, by = c("pcmes_coderhj", "pcmes_date"))
  
  # Seuils dureté pour cuivre
  SeuilsETMH2O <-
    data %>% 
    filter(pcmes_supportsandre == 3 & (pcmes_parametresandre == "1382" | pcmes_parametresandre == "1383" | pcmes_parametresandre == "1386" | pcmes_parametresandre == "1388" | pcmes_parametresandre == "1389" | pcmes_parametresandre == "1392")) %>% # Afin de ne considérer que certains métaux sur l'eau
    mutate(Durete = case_when(Seuil_condition == "Maximum" & Valeur_condition == 5 ~ "DureteFaible",
                                     Seuil_condition == "Maximum" & Valeur_condition == 20 ~ "DureteMoyenne",
                                     Seuil_condition == "Minimum" & Valeur_condition == 20 ~ "DureteForte")
    ) %>% 
    tidyr::unite(Seuil, c(Durete, Seuil, ClasseQualite), remove=T, sep = "-") %>% 
    dcast(Referentiel + pcmes_parametresandre + pcmes_supportsandre ~ Seuil, value.var = "ValeurSeuil") %>% 
    tidyr::unite(Cl, c(pcmes_parametresandre, pcmes_supportsandre), remove=T, sep = "-") %>% 
    rename(Referentie = Referentiel)

  ## Attribution des classes de qualité ##
  ClasseQualites <-
    PC %>% 
    tidyr::unite(Cl, c(pcmes_parametresandre, pcmes_supportsandre), remove=F, sep = "-") %>% 
    left_join(Seuils, by = c("Cl" = "Cl")) %>% 
    left_join(SeuilsETMH2O, by = c("Cl" = "Cl")) %>% # Afin de coller les seuils de qualité pour les ETM qui sont fonction de la dureté
    mutate(Referentiel = ifelse(is.na(Referentie), Referentiel, Referentie)) %>% # Afin de remettre la colonne référentiel en une seule commune aux deux jeux de données de seuils
    select(-Referentie) %>% # Afin d'effacer la colonne temporaire
    mutate(pcmes_valeur = as.numeric( sub(",", ".", pcmes_valeur))) %>% 
    #mutate(Durete = as.numeric( sub(",", ".", Durete))) %>% 
    mutate(ClasseQualite = NA) %>% 
    mutate(ClasseQualite = ifelse(is.na(`Maximum-4`) & !is.na(`Maximum-1`), # Pour le cas où il n'y a que 3 seuils - Avec exclusion des na en max-1 pour 1311 et 1312 traités ensuite
                           case_when(.$pcmes_valeur < .$`Maximum-1` ~ "Classe 1",
                                     .$pcmes_valeur >= .$`Maximum-1` & .$pcmes_valeur < .$`Maximum-2` ~ "Classe 2",
                                     .$pcmes_valeur >= .$`Maximum-2` & .$pcmes_valeur < .$`Maximum-3` ~ "Classe 3",
                                     .$pcmes_valeur >= .$`Maximum-3` ~ "Classe 4", # S'il n'existe que 3 limites
                                     #.$pcmes_valeur >= .$`Maximum-3` & .$pcmes_valeur < .$`Maximum-Classe 4` ~ "Classe 4", # S'il existe 4 limites
                                     #.$pcmes_valeur >= .$`Maximum-4` ~ "Classe 5", # S'il existe 4 limites
                                     TRUE ~ "Pas de classe"),  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
                           ClasseQualite)
    ) %>% 
    mutate(ClasseQualite = ifelse(!is.na(`Maximum-4`), # Pour le cas où il y a 4 seuils
                           case_when(.$pcmes_valeur < .$`Maximum-1` ~ "Classe 1",
                                     .$pcmes_valeur >= .$`Maximum-1` & .$pcmes_valeur < .$`Maximum-2` ~ "Classe 2",
                                     .$pcmes_valeur >= .$`Maximum-2` & .$pcmes_valeur < .$`Maximum-3` ~ "Classe 3",
                                     #.$pcmes_valeur >= .$`Maximum-3` ~ "Classe 4", # S'il n'existe que 3 limites
                                     .$pcmes_valeur >= .$`Maximum-3` & .$pcmes_valeur < .$`Maximum-4` ~ "Classe 4", # S'il existe 4 limites
                                     .$pcmes_valeur >= .$`Maximum-4` ~ "Classe 5", # S'il existe 4 limites
                                     TRUE ~ "Pas de classe"),  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
                           ClasseQualite)
    ) %>% 
    mutate(ClasseQualite = ifelse(Cl == "1311-3" | Cl == "1312-3", # Pour le cas où il y a 4 seuils mais seulement pour 1311 et 1312 (car par minimum et non par maximum)
                                  case_when(.$pcmes_valeur < .$`Minimum-4` ~ "Classe 5",
                                            .$pcmes_valeur >= .$`Minimum-4` & .$pcmes_valeur < .$`Minimum-3` ~ "Classe 4",
                                            .$pcmes_valeur >= .$`Minimum-3` & .$pcmes_valeur < .$`Minimum-2` ~ "Classe 3",
                                            .$pcmes_valeur >= .$`Minimum-2` & .$pcmes_valeur < .$`Minimum-1` ~ "Classe 2",
                                            .$pcmes_valeur >= .$`Minimum-1` ~ "Classe 1",
                                            TRUE ~ "Pas de classe"),  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
                                  ClasseQualite)
    ) %>% 
    mutate(ClasseQualite = ifelse(Cl == "1382-3" | Cl == "1383-3" | Cl == "1386-3" | Cl == "1388-3" | Cl == "1389-3" | Cl == "1392-3", # Pour le cas où il y a 4 seuils mais seulement pour quelques ETM
                                  case_when(Durete < 5 & pcmes_valeur < `DureteFaible-Maximum-1` ~ "Classe 1",
                                            Durete < 5 & pcmes_valeur >= `DureteFaible-Maximum-1` & pcmes_valeur < `DureteFaible-Maximum-2` ~ "Classe 2",
                                            Durete < 5 & pcmes_valeur >= `DureteFaible-Maximum-2` & pcmes_valeur < `DureteFaible-Maximum-3` ~ "Classe 3",
                                            Durete < 5 & pcmes_valeur >= `DureteFaible-Maximum-3` & pcmes_valeur < `DureteFaible-Maximum-4` ~ "Classe 4", # S'il existe 4 limites
                                            Durete < 5 & pcmes_valeur >= `DureteFaible-Maximum-4` ~ "Classe 5", # S'il existe 4 limites
                                            Durete >= 5 & Durete < 20 & pcmes_valeur < `DureteMoyenne-Maximum-1` ~ "Classe 1",
                                            Durete >= 5 & Durete < 20 & pcmes_valeur >= `DureteMoyenne-Maximum-1` & pcmes_valeur < `DureteMoyenne-Maximum-2` ~ "Classe 2",
                                            Durete >= 5 & Durete < 20 & pcmes_valeur >= `DureteMoyenne-Maximum-2` & pcmes_valeur < `DureteMoyenne-Maximum-3` ~ "Classe 3",
                                            Durete >= 5 & Durete < 20 & pcmes_valeur >= `DureteMoyenne-Maximum-3` & pcmes_valeur < `DureteMoyenne-Maximum-4` ~ "Classe 4", # S'il existe 4 limites
                                            Durete >= 5 & Durete < 20 & pcmes_valeur >= `DureteMoyenne-Maximum-4` ~ "Classe 5", # S'il existe 4 limites
                                            Durete > 20 & pcmes_valeur < `DureteForte-Maximum-1` ~ "Classe 1",
                                            Durete > 20 & pcmes_valeur >= `DureteForte-Maximum-1` & pcmes_valeur < `DureteForte-Maximum-2` ~ "Classe 2",
                                            Durete > 20 & pcmes_valeur >= `DureteForte-Maximum-2` & pcmes_valeur < `DureteForte-Maximum-3` ~ "Classe 3",
                                            Durete > 20 & pcmes_valeur >= `DureteForte-Maximum-3` & pcmes_valeur < `DureteForte-Maximum-4` ~ "Classe 4", # S'il existe 4 limites
                                            Durete > 20 & pcmes_valeur >= `DureteForte-Maximum-4` ~ "Classe 5", # S'il existe 4 limites
                                            is.na(Durete) ~ "Pas de durete",
                                            TRUE ~ "Pas de classe"),  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
                                  ClasseQualite)
    ) %>%
    mutate(ClasseQualite = ifelse(pcmes_coderemarque == 10 & !is.na(`Referentiel`), "< seuil quantification", ClasseQualite)) %>%  # Pour compléter les cas inférieurs au seuil de quantification
    mutate(ClasseQualite = ifelse(pcmes_coderemarque == 2 & !is.na(`Referentiel`), "< seuil detection", ClasseQualite)) %>%  # Pour compléter les cas inférieurs au seuil de detection 
    select(-(`Maximum-1`:`Minimum-4`), -Cl, -Durete,-(`DureteFaible-Maximum-1`:`DureteMoyenne-Maximum-4`)) %>% 
    mutate(Couleur = case_when(.$ClasseQualite == "Classe 1" ~ "Bleu",
                               .$ClasseQualite == "Classe 2" ~ "Vert",
                               .$ClasseQualite == "Classe 3" ~ "Jaune",
                               .$ClasseQualite == "Classe 4" ~ "Orange",
                               .$ClasseQualite == "Classe 5" ~ "Rouge",
                               .$ClasseQualite == "< seuil detection" ~ "Gris clair",
                               .$ClasseQualite == "< seuil quantification" ~ "Gris",
                               .$ClasseQualite == "Pas de durete" ~ "Pas de classe",
                               TRUE ~ "Pas de classe"))  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
}

###### Sédiments Québec #####
  
if(Referentiel == "Quebec") {
  
  ## Création des seuils ##
Seuils <- 
  data %>% 
  filter(Referentiel == "Criteres pour l'evaluation de la qualite des sediments au Quebec") %>% 
  tidyr::unite(Seuil, c(Seuil, ClasseQualite), remove=T, sep = "-") %>% 
  dcast(Referentiel + pcmes_parametresandre + pcmes_supportsandre ~ Seuil, value.var = "ValeurSeuil") %>% 
  tidyr::unite(Cl, c(pcmes_parametresandre, pcmes_supportsandre), remove=T, sep = "-")
  
  ## Attribution des classes de qualité ##
ClasseQualites <-
  PC %>% 
  tidyr::unite(Cl, c(pcmes_parametresandre, pcmes_supportsandre), remove=F, sep = "-") %>% 
  left_join(Seuils, by = c("Cl" = "Cl")) %>% 
  mutate(pcmes_valeur = as.numeric( sub(",", ".", pcmes_valeur))) %>% 
  mutate(ClasseQualite = NA) %>% 
  mutate(ClasseQualite = ifelse(is.na(`Minimum-CEF`) & is.na(`Minimum-CEP`) & is.na(`Minimum-CER`) & is.na(`Minimum-CSE`), # Pour le cas où il y a 1 seuil
                             case_when(.$pcmes_valeur < .$`Minimum-CEO` ~ "< Concentration effets occasionnels",
                                       #.$pcmes_valeur >= .$`Minimum-CER` & .$pcmes_valeur < .$`Minimum-CSE` ~ "Concentration effets rares",
                                       #.$pcmes_valeur >= .$`Minimum-CSE` & .$pcmes_valeur < .$`Minimum-CEO` ~ "Concentration seuil produisant un effet",
                                       .$pcmes_valeur >= .$`Minimum-CEO` ~ "Concentration effets occasionnels",
                                       #.$pcmes_valeur >= .$`Minimum-CEP` & .$pcmes_valeur < .$`Minimum-CEF` ~ "Concentration produisant un effet probable",
                                       #.$pcmes_valeur >= .$`Minimum-CEF` ~ "Concentration effets frequents",
                                       TRUE ~ "Pas de classe"),  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
                             ClasseQualite)
  ) %>% 
  mutate(ClasseQualite = ifelse(!is.na(`Minimum-CEF`), # Pour le cas où il y a 5 seuils
                             case_when(.$pcmes_valeur < .$`Minimum-CER` ~ "Concentration sans effet",
                                       .$pcmes_valeur >= .$`Minimum-CER` & .$pcmes_valeur < .$`Minimum-CSE` ~ "Concentration effets rares",
                                       .$pcmes_valeur >= .$`Minimum-CSE` & .$pcmes_valeur < .$`Minimum-CEO` ~ "Concentration seuil produisant un effet",
                                       .$pcmes_valeur >= .$`Minimum-CEO` & .$pcmes_valeur < .$`Minimum-CEP` ~ "Concentration effets occasionnels",
                                       .$pcmes_valeur >= .$`Minimum-CEP` & .$pcmes_valeur < .$`Minimum-CEF` ~ "Concentration produisant un effet probable",
                                       .$pcmes_valeur >= .$`Minimum-CEF` ~ "Concentration effets frequents",
                                       TRUE ~ "Pas de classe"),  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
                             ClasseQualite)
      ) %>% 
  mutate(ClasseQualite = ifelse(pcmes_coderemarque == 10 & !is.na(`Referentiel`), "< seuil quantification", ClasseQualite)) %>%  # Pour compléter les cas inférieurs au seuil de quantification
  mutate(ClasseQualite = ifelse(pcmes_coderemarque == 2 & !is.na(`Referentiel`), "< seuil detection", ClasseQualite)) %>%  # Pour compléter les cas inférieurs au seuil de detection 
  select(-(`Minimum-CEF`:`Minimum-CSE`), -Cl) %>% 
  mutate(Couleur = case_when(.$ClasseQualite == "Concentration sans effet" ~ "Bleu",
                                 .$ClasseQualite == "Concentration effets rares" ~ "Vert",
                                 .$ClasseQualite == "Concentration seuil produisant un effet" ~ "Jaune",
                                 .$ClasseQualite == "Concentration effets occasionnels" ~ "Orange",
                                 .$ClasseQualite == "Concentration produisant un effet probable" ~ "Rouge",
                                 .$ClasseQualite == "Concentration effets frequents" ~ "Violet",
                                 .$ClasseQualite == "< seuil detection" ~ "Gris clair",
                                 .$ClasseQualite == "< seuil quantification" ~ "Gris",
                                 .$ClasseQualite == "< Concentration effets occasionnels" ~ "Vert clair",
                                 TRUE ~ "Pas de classe"))  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
    
}
  
  ###### Sédiments Arrete4aout2006tableau4 #####
  
  if(Referentiel == "Arrete4aout2006tableau4") {
    
    ## Création des seuils ##
    Seuils <- 
      data %>% 
      filter(Referentiel == "Arrete4aout2006tableau4") %>% 
      tidyr::unite(Seuil, c(Seuil, ClasseQualite), remove=T, sep = "-") %>% 
      dcast(Referentiel + pcmes_parametresandre + pcmes_supportsandre + pcmes_unitesandre ~ Seuil, value.var = "ValeurSeuil") %>% 
      tidyr::unite(Cl, c(pcmes_parametresandre, pcmes_supportsandre, pcmes_unitesandre), remove=T, sep = "-")
    
    ## Attribution des classes de qualité ##
    ClasseQualites <-
      PC %>% 
      ## Transformation des unités ##
      mutate(pcmes_unitenom = ifelse(pcmes_unitenom == "mg(Cd)/kg MS", "mg/kg MS", pcmes_unitenom)) %>% 
      mutate(pcmes_unitesandre = ifelse(pcmes_unitesandre == "294", "160", pcmes_unitesandre)) %>% 
      mutate(pcmes_unitenom = ifelse(pcmes_unitenom == "mg(Cu)/kg MS", "mg/kg MS", pcmes_unitenom)) %>% 
      mutate(pcmes_unitesandre = ifelse(pcmes_unitesandre == "371", "160", pcmes_unitesandre)) %>% 
      mutate(pcmes_unitenom = ifelse(pcmes_unitenom == "mg(Hg)/kg MS", "mg/kg MS", pcmes_unitenom)) %>% 
      mutate(pcmes_unitesandre = ifelse(pcmes_unitesandre == "312", "160", pcmes_unitesandre)) %>% 
      mutate(pcmes_unitenom = ifelse(pcmes_unitenom == "mg(Ni)/kg MS", "mg/kg MS", pcmes_unitenom)) %>% 
      mutate(pcmes_unitesandre = ifelse(pcmes_unitesandre == "396", "160", pcmes_unitesandre)) %>% 
      mutate(pcmes_unitenom = ifelse(pcmes_unitenom == "mg(Pb)/kg MS", "mg/kg MS", pcmes_unitenom)) %>% 
      mutate(pcmes_unitesandre = ifelse(pcmes_unitesandre == "336", "160", pcmes_unitesandre)) %>% 
      mutate(pcmes_unitenom = ifelse(pcmes_unitenom == "mg(Zn)/kg MS", "mg/kg MS", pcmes_unitenom)) %>% 
      mutate(pcmes_unitesandre = ifelse(pcmes_unitesandre == "350", "160", pcmes_unitesandre)) %>% 
      mutate(pcmes_valeur = as.numeric( sub(",", ".", pcmes_valeur))) %>% 
      mutate(pcmes_valeur = ifelse(pcmes_unitesandre == "132", pcmes_valeur/1000, pcmes_valeur)) %>% 
      mutate(pcmes_unitesandre = ifelse(pcmes_unitesandre == "132", "160", pcmes_unitesandre)) %>% 
      mutate(pcmes_unitenom = ifelse(pcmes_unitenom == "µg/kg MS", "mg/kg MS", pcmes_unitenom)) %>% 
      tidyr::unite(Cl, c(pcmes_parametresandre, pcmes_supportsandre, pcmes_unitesandre), remove=F, sep = "-") %>% 
      left_join(Seuils, by = c("Cl" = "Cl")) %>% 
      mutate(ClasseQualite = NA_character_) %>% # Création préalable nsc pour le ifelse en-dessous
      mutate(ClasseQualite = ifelse(!is.na(`Maximum-1`),
                                    case_when(.$pcmes_valeur < .$`Maximum-1` ~ "Autorisé",
                                              .$pcmes_valeur >= .$`Maximum-1` ~ "Non autorisé",
                                              TRUE ~ "Pas de classe"),  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
                                    ClasseQualite)
      ) %>%
      mutate(ClasseQualite = ifelse(pcmes_coderemarque == 10 & !is.na(`Referentiel`), "< seuil quantification", ClasseQualite)) %>%  # Pour compléter les cas inférieurs au seuil de quantification
      mutate(ClasseQualite = ifelse(pcmes_coderemarque == 2 & !is.na(`Referentiel`), "< seuil detection", ClasseQualite)) %>%  # Pour compléter les cas inférieurs au seuil de detection 
      select(-(`Maximum-1`), -Cl) %>% 
      mutate(Couleur = case_when(.$ClasseQualite == "Autorisé" ~ "Vert",
                                 .$ClasseQualite == "Non autorisé" ~ "Rouge",
                                 .$ClasseQualite == "< seuil detection" ~ "Gris clair",
                                 .$ClasseQualite == "< seuil quantification" ~ "Gris",
                                 TRUE ~ "Pas de classe"))  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
    
  }
  
  #### Mise en forme ####
  ClasseQualites <- 
    ClasseQualites %>% 
    arrange(pcmes_parametrenom, factor(Couleur, levels = c("Noir", "Violet", "Rouge", "Orange", "Jaune", "Vert clair", "Vert", "Bleu", "Gris", "Gris clair")), desc(pcmes_valeur))
  
#### Retour des données ####
  return(ClasseQualites)
  
} # Fin de la fonction
