# NEWS - afd39

## 0.0.28 - 2025-05-17
### Ajouts
- `chronique.traitement` : 
  * ajout de la gestion des nouveaux champs de stations introduits via TIGRE 2 (oubli de la version `0.0.27 - 2025-05-14`)
  
### Modifications
- `BDD.format` : modification des jeux de données de référence de chroniques vers `chronique_structure`

### Corrections
- `chronique.DCE` : 
  * ajout de l'export du référentiel horaire suite à évolution format 2025 (oubli de la version `0.0.27 - 2025-05-14`)
  * correction de coquilles de sélection de colonnes (erreurs de la version `0.0.27 - 2025-05-14`)
  * rustine temporaire pour effacer le suivi qui ne correspond pas à la station : code à reprendre en profondeur

## 0.0.27 - 2025-05-14
### Ajouts
- `PC.mesures` : ajout d'un exemple d'export en masse
- `chronique.DCE` : ajout de l'export du MO du point de suivi et du référentiel horaire suite à évolution format 2025
- `chronique.traitement` : ajout d'une vérification de l'absence de doublons dans les commentaires, avec message d'erreur associé

### Modifications
- `chronique.suivi` : modification de l'appel vers le jeu de données de référence suite à harmonisation des intitulés

### Corrections
- `PC.mesures` : correction de l'absence d'export si `sortie = "Complet"`

## 0.0.26 - 2025-04-22
### Ajouts
- `personnel.projection` : ajout d'un arrondi

### Modifications
- `adresse.switch` : suppression de la transformation de la session de Malidé
- `poissons.atlas.fiches` : inversion de l'ordre des paramètres `commentaires` et `export`

### Corrections
- `poissons.brut` : ajout de la dépendance à `chromote`, car bug lors de l'exécution de `gtsave` suite à update chrome > v128 (https://github.com/rstudio/chromote/issues/170)
- `poissons.IPR` : retrait d'un paramètre qui n'était plus utilisé et qui restait dans le code

## 0.0.25 - 2025-03-25
### Ajouts
- `BDD.format` : ajout d'un arrondi de la durée de travail dans les données OpenTime
- `formatage.mo.libelle` : création
- `formatage.personnel.prenom` : création
- `opentime.ouverture` : création
- `opentime.projet` : création
- `personnel.projection` : création

### Corrections
- `BDD.format` : fermeture de connexions à la BDD après collecte des données nécessaires

## 0.0.24 - 2025-02-13
### Ajouts
- `carte.territoire` : 
  * ajout d'un affichage à un niveau plus local si nécessaire
  * ajout de l'option `altitude` qui permet de supprimer le MNT en fond
  * ajout de l'option `position_echelle` qui permet de régler la position de l'échelle
  * ajout de l'option `position_nord` qui permet de régler la position de la flèche du nord
- `general.mail` : création

### Modifications
- `adresse.switch` : refactoring et ajout de la machine d'AL
- `carte.territoire` : également un affichage à un niveau plus local si nécessaire
  * réglages transparences du fond de carte
  * léger refactoring

## 0.0.23 - 2025-01-16
### Ajouts
- `carte.departement` : création
  
### Modifications
- `poissons.IPR` : 
  * refactoring partiel
  * modification du format de sortie, afin de permettre l'extraction des métriques tout en laissant ça harmonisé
  
## 0.0.22 - 2024-12-24
### Ajouts
- Ajout d'une `licence` et d'un `readme`
- Ajout de dépendances manquantes : `aquatools`, `gt`, `gtsummary`
- `chronique.traitement` :
  * Ajout de la création automatique du répertoire `log`

### Modifications
- `chronique.traitement` :
  * Recodage de la section liée aux `Informations de session` : noms de variables et remplacement de `paste0` par `glue`
  * Recodage de la section liée au `Zippage` : remplacement de `paste0` par `glue`

## 0.0.21 - 2024-11-19
### Ajouts
- `formatage.noms_propres` :
  * Nettoyage amélioré avec retrait des espaces (début/fin) et ajout de majuscules à tous les mots, afin de limiter les cas minuscules/majuscules
  * Ajout d'un union qui permet de reprendre les noms déjà propres, afin qu'on les retrouve directement en sortie
  * Ajout d'une fermeture de l'accès à la base de données, qui était absent
- `poissons.atlas.fiches` : collecte d'un champs complémentaire (`gestionnaire`) + nettoyage

### Corrections
- `chronique.DCE` : suppression de la dépendance à `tcltk`
- `chronique.traitement` : suppression de la dépendance à `tcltk`
- `MI.captures` : suppression de la colonne de géométrie issue des prélèvements, qui posait problème ensuite
- `MI.operations` : fermeture de la connexion à la base de données
- `MI.systematique` : Ré-écriture complète
- `MI.systematique.presence` : Suppression d'un test de cohérence avec le format de `MI.captures` en entrée
- `stations.territoire` : suppression de la dépendance à `tcltk`

### Modifications
- `formatage.sapl` : nettoyage
- `projet.calculInitial` : modification du critère du mois à partir duquel il faut prendre les coûts annuels N+1 sous la forme d'un paramètre `mois_modif_couts_annuels`
- `poissons.brut` : 
  * Nettoyage
  * Modification de la forme du tableau (passage au format `gt`)

## 0.0.20 - 2024-06-17
### Ajouts
- `BDD.format` : ajout d'un traitement manquant pour les heures de suivi de chroniques
- `stations.territoire` : création du traitement `GEMAPI`

### Corrections
- `personnel.formatAC` : correction d'un filtre un peu trop restrictif (cas de Germain)
- `personnel.projet` : 
  * Modification du calcul de conversion des heures en jours car chargés de dvpt à 7,8 heures par jour
  * Ajout d'un paramètre `arrondi` permettant de gérer la valeur d'arrondi souhaitée
- `poissons.atlas.fiches` : 
  * Nettoyage du code
  * Renommage de variables
  * Correction d'une colonne en doublon (`codesiermc`) qui bloquait l'export en `geojson`
- `poissons.ecosystemes` : 
  * Corrections de l'en-tête
  * Passage en snake_case des noms de variables
  * Correction d'un nom de champ de la base de données qui ne fonctionnait pas
  * Filtrage directement dans la requête SQL afin d'optimiser les chargements de tables
- `poissons.stations` : recodage du test initial, qui ne générait un bug s'il n'y avait pas de guillemets entre les parenthèses

## 0.0.19 - 2024-01-11
### Ajouts
- `topographie.mesures` : création

### Modifications
- `chronique.mesures` : nettoyage du code
- `poissons.stations` :
  * Reformatage et versionnage des noms de variables
  * Remplacement des paramètres par défaut par des `NA_character`
  * Filtrage directement dans la requête SQL afin d'optimiser les chargements de tables

## 0.0.18 - 2024-01-09
### Ajouts
- `BDD.format` : 
  * Ajout d'un cas de formatage du PNRHJ pour le suivi de terrain des chroniques
  * Ajout d'un cas de remplace d'un `_` par un `-` dans les noms de station dans le suivi de terrain des chroniques
  * Amélioration du traitement des actions du suivi de terrain, pour affichage direct des modalités qui ne conviennent pas
  
- `chronique.traitement` : ajout de paramètres permettant l'utilisation directe des seuils de `chronique.analyse`

- `PC.classes` : ajout d'un tri en fonction de la classe de qualité établie (du pire au meilleur)
- `PC.mesures` : création
- `PC.operations` : création

- `stations.territoire` : ajout d'une liste automatique par BV à partir des contextes PDPG (cas de la Bienne et de l'Orbe ici)

### Corrections
- `chronique.traitement` : correction de l'ordre du paramètre `projet` dans la documentation
- `MI.systematique` : correction d'un bug de jointure suite à l'évolution de la structure de la base de données
  
### Modifications
- `personnel.formatAC` : 
  * Mise en snake case de variables
  * Amélioration des personnels concernés
  
- `personnel.projet` : 
  * Amélioration des personnels concernés
  * Correction d'un bug entraînant une impossible d'`union` à cause de datatypes différents
  * Harmonisation du code entre les versions de la variable `projet` qui est divisée en `projet_id` et `projet_libelle`
  * Correction de l'export au format excel pour l'accord-cadre, en modifiant les postes et intégration du coût du matériel saisi manuellement

## 0.0.17 - 2023-05-23
### Ajouts
- `DESCRIPTION` :
  * Ajout des champs `Maintainer` et `Depends`
  * Mise à jour des dépendances
- `adresse.switch` : 
  * Ajout d'une modalité incluant le répertoire `Nextcloud` sur le poste de JB
- `BDD.format` : 
  * Ajout d'un traitement des données de prélèvements AEP
  * Ajout d'un 0 en début de '9:00:00' pour le suivi des chroniques
- `chronique.traitement` :
  * Ajout de paramètres permettant d'indiquer les localisations de fichiers sources directement, sans applet
  * Ajout du paramètre `style` dans la documentation, qui était absent bien qu'utilisé ensuite
- `formatage.abreviation` 
  * Ajout d'une modalité `Général`, qui était présente en base mais pas dans les sorties potentielles
- `projet.calculInitial` :
  * Calcul des coûts pour l'année N+1 si réalisé au mois de décembre
  * Complément du test de disponibilité des coûts annuels N+1 si calcul au mois de décembre
  * Ajout de commentaires afin d'expliciter quelques parties de codes

### Corrections
- `chronique.suivi` : ajout d'un format de sortie permettant les jointures s'il n'y a pas de données collectées (car format de date incompatible : `date` vs `character`)

### Modifications
- `adresse.switch` : 
  * Remplacement de `paste0()` par des `glue()`
- `projet.calculInitial` :
  * Remplacement de `paste0()` par des `glue()`
  * Prise en compte de scénarios potentiels comme existence d'un coût type IBL en prestation, mais qu'on souhaite le réaliser en régie (le coût prestation était quand même ajouté)

## 0.0.16 - 2022-09-21
### Ajouts
- `BDD.format` : 
  * Ajout d'un traitement pour les opérations de suivi physico-chimique
  * Intégration de la structure des bases de données de référence en dur dans le code concernant la physico-chimie, sans passer par une requête de chargement de ces tables depuis la base de données.
- `projet.calculInitial` : ajout du calcul des actions avec coût unitaire défini uniquement en temps de travail (MAG20 par exemple)

### Corrections
- `chronique.capteurs` : ajout en sortie d'un dataframe vide si aucune donnée trouvée (à partir de `data("chronique_structure")`), afin de ne pas casser la jointure dans le cas d'une utilisation dans un `map_dfr`.
- `chronique.commentaires` : ajout en sortie d'un dataframe vide si aucune donnée trouvée (à partir de `data("chronique_structure")`), afin de ne pas casser la jointure dans le cas d'une utilisation dans un `map_dfr`.
- `chronique.DCE` : 
  * Suppression de la géométrie des stations qui faisaient planter l'export excel + très léger refactoring avec `glue` à la place de `paste0`.
  * Ajout du calcul de la géométrie des stations dans le cas de données externes, qui sera ensuite supprimée (point précédent)
- `chronique.mesures` : ajout en sortie d'un dataframe vide si aucune donnée trouvée (à partir de `data("chronique_structure")`), afin de ne pas casser la jointure dans le cas d'une utilisation dans un `map_dfr`.
- `chronique.resultats` : ajout en sortie d'un dataframe vide si aucune donnée trouvée (à partir de `data("chronique_structure")`), afin de ne pas casser la jointure dans le cas d'une utilisation dans un `map_dfr`.
- `formatage.noms_propres` : correction d'une coquille dans la description d'un paramètre
- `poissons.operations` : correction d'une coquille dans un nom de variable dans un scénario de traitement
- `projet.calculInitial` : court-circuit des valeurs unitaires pour les coûts type si une valeur est ajoutée manuellement directement dans le récapitulatif théorique

## 0.0.15 - 2022-05-31
### Ajouts
- `formatage.noms_propres` : création
- `personnel.formatAC` : extraction de l'id du projet concerné, afin de pouvoir ensuite indiquer l'information exacte que l'on souhaite (id ou intitulé)
- `personnel.projet` : extraction de l'id du projet concerné, afin de pouvoir ensuite indiquer l'information exacte que l'on souhaite (id ou intitulé)

### Corrections
- `chronique.mesures` : modification du format de sortie en cas d'absence de lignes, afin de permettre un union avec d'autres jeux de données, car il y avait une différence de datatypes
- `BDD.format` : petite correction suite à évolution format d'entrée concernant les données OpenTime
- `personnel.projet` : correction du calcul suite à l'utilisation d'un identifiant pour les postes et plus l'intitulé complet

### Modifications
- `chronique.traitement` : 
  * très léger refactoring du formatage du nom de projet
  * correction de quelques appels de commande générant des bugs
- `personnel.formatAC` : 
  * ajout d'un filtrage sur les personnels seulement en activité
  * précision du filtre pour l'AC AE (modification ancienne mais pas intégrée au git par oubli)
  * nettoyage
- `personnel.projet` : 
  * léger refactoring de l'extraction des données attendues au niveau de l'exportation de synthèse
  * intégration partielle du traitement des apprentis
  
### Suppression
- `poissons.poids` : retiré car déplacé dans `aquatools`

## 0.0.14 - 2022-05-04
### Ajouts
- `NEWS.md` : création
- `BDD.format` : 
  * ajout de la topographie
  * ajout du traitement des profondeurs dans le suivi des chroniques
- `stations.territoire` : création du code de traitement des acteurs GEMAPI
- `chronique.traitement` : 
  * ajout d'une possibilité de log de déroulement de la fonction, avec choix possible de son caractère plus ou moins verbeux
  * correction d'une url de répertoire de sortie
  * ajout de paramètres permettant de désactiver l'export de figures classiques ou de synthèse (utile pour certains paramètres comme l'oxygénation, pour lesquels certaines figures n'ont pas de sens)
  * intégration de `chronique.figure.classescalendaires`
  * extraction des valeurs agrégées précédemment calculées pour l'export afin de pouvoir les réutiliser ailleurs dans la fonction

### Corrections
- `formatage.abreviation` : 
  * correction d'une syntaxe afin d'être moins restrictif et de supprimer un warning
  * modification d'un nom de paramètre qui était deprecated dans une dépendance

### Modifications
- `BDD.format` : ré-écriture partielle du traitement des dates dans le suivi de chroniques
- `adresse.switch` : ajout de la nouvelle machine de JB

## 0.0.13 - 2022-02-08
### Modifications
- Correction des noms de champs des tables relatives aux macroinvertébrés afin de basculer de la base de données `sqlite` à la base `PostGIS`.