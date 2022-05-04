# NEWS - afd39

## 0.0.14
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

## 0.0.13
### Modifications 
- Correction des noms de champs des tables relatives aux macroinvertébrés afin de basculer de la base de données `sqlite` à la base `PostGIS`.