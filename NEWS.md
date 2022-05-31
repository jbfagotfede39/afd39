# NEWS - afd39

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