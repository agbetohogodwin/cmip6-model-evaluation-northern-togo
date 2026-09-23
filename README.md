# cmip6-model-evaluation-northern-togo

Évaluation de 32 modèles climatiques NEX-GDDP-CMIP6 sur 4 stations du Nord-Togo pour la période historique (1983 - 2014). L'objectif est de sélectionner les modèles les plus performants pour les projections climatiques futures sous SSP2-4.5 et SSP5-8.5.

## Zone d'étude

Régions : Savanes et Kara
Stations : Dapaong, Mango, Kara, Niamtougou
Variables : Précipitations, Tmax, Tmin
Données de référence : Données de météo des stations

## Méthode d'évaluation

4 métriques calculées par station et par variable :

BIAS : biais moyen

RMSE : erreur quadratique moyenne

Pearson R : corrélation linéaire

Willmott d : indice d'accord

Classement final par mesure de notation complète (RM / Comprehensive Rating Metric). Seuil de sélection : RM supérieur à 0,5.
Modèles retenus


## 18 modèles sélectionnés sur 32 évalués :

UKESM1-0-LL
INM-CM4-8
FGOALS-g3
KACE-1-0-G
HadGEM3-GC31-MM
MIROC-ES2L
INM-CM5-0
HadGEM3-GC31-LL
CanESM5
BCC-CSM2-MR
ACCESS-ESM1-5
GISS-E2-1-G
ACCESS-CM2
GFDL-ESM4
MIROC6
GFDL-CM4_gr2
KIOST-ESM
IPSL-CM6A-LR


## Outils

Langage : R
Téléchargement des données : Google Earth Engine (JavaScript)
Visualisation : diagrammes de Taylor (package plotrix)
Données : NASA NEX-GDDP-CMIP6, Données des précipitaions et des températures des stations météo 

## Structure du dépôt

### ├── scripts/

          ├── evaluation_cmip6.R
          
          └── download_cmip6_gee.js


### └── README.md

## Statut

Analyse complète. Résultats intégrés dans une thèse (pas encore soutenue) sur la vulnérabilité de la chaîne de valeur karité face aux changements climatiques au Nord-Togo.
