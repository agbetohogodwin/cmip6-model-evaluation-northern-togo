# cmip6-model-evaluation-northern-togo
Évaluation de 26 modèles NEX-GDDP-CMIP6 sur 4 stations du Nord-Togo (régions Savanes et Kara : Dapaong, Mango, Kara, Niamtougou) pour les précipitations, Tmax et Tmin. Classement final par mesure de notation complète (RM, Comprehensive Rating Metric) à partir de 4 métriques : BIAS, RMSE, Pearson R et Willmott d.

Évaluation des modèles CMIP6 pour le Nord-Togo

Évaluation de 26 modèles climatiques NEX-GDDP-CMIP6 sur 4 stations du Nord-Togo pour la période historique (1983 - 2014). L'objectif est de sélectionner les modèles les plus performants pour les projections climatiques futures sous SSP2-4.5 et SSP5-8.5.

Zone d'étude

Régions : Savanes et Kara
Stations : Dapaong, Mango, Kara, Niamtougou
Variables : Précipitations, Tmax, Tmin
Données de référence : Données de météo des stations

Méthode d'évaluation

4 métriques calculées par station et par variable :

BIAS : biais moyen

RMSE : erreur quadratique moyenne

Pearson R : corrélation linéaire

Willmott d : indice d'accord

Classement final par mesure de notation complète (RM / Comprehensive Rating Metric). Seuil de sélection : RM supérieur à 0,5.
Modèles retenus

Diagrammes de Taylor

### Précipitations (PR)

![Dapaong](https://raw.githubusercontent.com/agbetohogodwin/cmip6-model-evaluation-northern-togo/main/outputs/Taylor_PR_Dapaong.png)
![Global](https://raw.githubusercontent.com/agbetohogodwin/cmip6-model-evaluation-northern-togo/main/outputs/Taylor_PR_Global.png)
![Kara](https://raw.githubusercontent.com/agbetohogodwin/cmip6-model-evaluation-northern-togo/main/outputs/Taylor_PR_Kara.png)
![Mango](https://raw.githubusercontent.com/agbetohogodwin/cmip6-model-evaluation-northern-togo/main/outputs/Taylor_PR_Mango.png)
![Niamtougou](https://raw.githubusercontent.com/agbetohogodwin/cmip6-model-evaluation-northern-togo/main/outputs/Taylor_PR_Niamtougou.png)

### Température maximale (TASMAX)

![Dapaong](https://raw.githubusercontent.com/agbetohogodwin/cmip6-model-evaluation-northern-togo/main/outputs/Taylor_TASMAX_Dapaong.png)
![Global](https://raw.githubusercontent.com/agbetohogodwin/cmip6-model-evaluation-northern-togo/main/outputs/Taylor_TASMAX_Global.png)
![Kara](https://raw.githubusercontent.com/agbetohogodwin/cmip6-model-evaluation-northern-togo/main/outputs/Taylor_TASMAX_Kara.png)
![Mango](https://raw.githubusercontent.com/agbetohogodwin/cmip6-model-evaluation-northern-togo/main/outputs/Taylor_TASMAX_Mango.png)
![Niamtougou](https://raw.githubusercontent.com/agbetohogodwin/cmip6-model-evaluation-northern-togo/main/outputs/Taylor_TASMAX_Niamtougou.png)

### Température minimale (TASMIN)

![Dapaong](https://raw.githubusercontent.com/agbetohogodwin/cmip6-model-evaluation-northern-togo/main/outputs/Taylor_TASMIN_Dapaong.png)
![Global](https://raw.githubusercontent.com/agbetohogodwin/cmip6-model-evaluation-northern-togo/main/outputs/Taylor_TASMIN_Global.png)
![Kara](https://raw.githubusercontent.com/agbetohogodwin/cmip6-model-evaluation-northern-togo/main/outputs/Taylor_TASMIN_Kara.png)
![Mango](https://raw.githubusercontent.com/agbetohogodwin/cmip6-model-evaluation-northern-togo/main/outputs/Taylor_TASMIN_Mango.png)
![Niamtougou](https://raw.githubusercontent.com/agbetohogodwin/cmip6-model-evaluation-northern-togo/main/outputs/Taylor_TASMIN_Niamtougou.png)


14 modèles sélectionnés sur 26 évalués :

UKESM1-0-LL,
INM-CM4-8,
FGOALS-g3,
KACE-1-0-G,
MIROC-ES2L,
HadGEM3-GC31-MM
INM-CM5-0,
CanESM5,
HadGEM3-GC31-LL,
BCC-CSM2-MR,
ACCESS-ESM1-5,
GISS-E2-1-G,
ACCESS-CM2,
GFDL-ESM4

Outils

Langage : R
Téléchargement des données : Google Earth Engine (JavaScript)
Visualisation : diagrammes de Taylor (package plotrix)
Données : NASA NEX-GDDP-CMIP6, Données des précipitaions et des températures des stations météo 

Structure du dépôt

├── scripts/
│   ├── evaluation_cmip6.R
│   └── download_cmip6_gee.js
├── outputs/
│   ├── Taylor_PR_*.png
│   ├── Taylor_TASMAX_*.png
│   └── Taylor_TASMIN_*.png
└── README.md

Statut

Analyse complète. Résultats intégrés dans une thèse (pas encore soutenue) sur la vulnérabilité de la chaîne de valeur karité face aux changements climatiques au Nord-Togo.
