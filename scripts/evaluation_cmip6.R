# =============================================================
# Étape 1 : Répertoire de travail et packages
# =============================================================

library(tidyverse)
library(lubridate)
library(hydroGOF)
library(openxlsx)
library(plotrix)
library(ggplot2)
library(scales)
library(readxl)

setwd("D:/TAF/KOTCHADJO/New evaluation/CMIP6_evaluation")

list.files()
list.files("CMIP6")
list.files("Observations")

# =============================================================
# Étape 2 : Charger les observations (déjà mensuelles)
# =============================================================

obs_mensuel <- read_excel("Observations/Météo.xlsx") %>%
  mutate(year = as.integer(year),
         month = as.integer(month))

glimpse(obs_mensuel)


# =============================================================
# Étape 3 : Charger les fichiers CMIP6 historiques (journaliers)
# =============================================================

models <- c(
  'ACCESS-CM2', 'ACCESS-ESM1-5', 'BCC-CSM2-MR', 'CanESM5',
  'CNRM-CM6-1', 'CNRM-ESM2-1', 'EC-Earth3', 'EC-Earth3-Veg-LR',
  'FGOALS-g3', 'GFDL-CM4', 'GFDL-ESM4', 'GISS-E2-1-G',
  'HadGEM3-GC31-LL', 'HadGEM3-GC31-MM', 'INM-CM4-8', 'INM-CM5-0',
  'IPSL-CM6A-LR', 'KACE-1-0-G', 'MIROC-ES2L', 'MIROC6',
  'MPI-ESM1-2-HR', 'MPI-ESM1-2-LR', 'MRI-ESM2-0',
  'NorESM2-LM', 'NorESM2-MM', 'UKESM1-0-LL'
)

cmip6_hist <- map_dfr(models, function(m) {
  fichier <- paste0("CMIP6/", m, "_historical.csv")
  if (file.exists(fichier)) {
    read_csv(fichier, show_col_types = FALSE) %>%
      mutate(date   = as.Date(date),
             pr     = pr * 86400,
             tasmax = tasmax - 273.15,
             tasmin = tasmin - 273.15,
             model  = m) %>%
      select(-scenario)
  } else {
    message("Fichier manquant : ", fichier)
    NULL
  }
})

glimpse(cmip6_hist)
cat("Nombre de modèles chargés :", n_distinct(cmip6_hist$model), "\n")
print(unique(cmip6_hist$model))

# =============================================================
# Étape 4 : Agréger les modèles au mensuel
# =============================================================

cmip6_mensuel_mod <- cmip6_hist %>%
  mutate(year  = year(date),
         month = month(date)) %>%
  group_by(model, NAME_2, year, month) %>%
  summarise(
    pr_mod     = sum(pr,      na.rm = TRUE),
    tasmax_mod = mean(tasmax, na.rm = TRUE),
    tasmin_mod = mean(tasmin, na.rm = TRUE),
    .groups = "drop"
  )

glimpse(cmip6_mensuel_mod)
cat("Nombre de lignes :", nrow(cmip6_mensuel_mod), "\n")

# =============================================================
# Étape 5 : Fusion modèles + observations
# =============================================================

cmip6_mensuel <- cmip6_mensuel_mod %>%
  left_join(obs_mensuel, by = c("NAME_2", "year", "month")) %>%
  rename(pr_obs = precipitation) %>%
  filter(!is.na(pr_obs),
         !is.na(tmax_obs),
         !is.na(tmin_obs))

glimpse(cmip6_mensuel)
cat("Lignes après fusion :", nrow(cmip6_mensuel), "\n")
cat("Valeurs manquantes :\n")
print(colSums(is.na(cmip6_mensuel)))

# =============================================================
# Étape 6 : Calcul des métriques
# =============================================================

# Fonction calcul métriques
calc_metriques <- function(obs, sim) {
  bias <- mean(sim - obs, na.rm = TRUE)
  rmse <- sqrt(mean((sim - obs)^2, na.rm = TRUE))
  r <- cor(obs, sim, use = "complete.obs")

  # Willmott d
  num   <- sum((sim - obs)^2, na.rm = TRUE)
  denom <- sum((abs(sim - mean(obs, na.rm = TRUE)) +
                abs(obs - mean(obs, na.rm = TRUE)))^2, na.rm = TRUE)
  d <- 1 - (num / denom)

  return(c(BIAS = round(bias, 3),
           RMSE = round(rmse, 3),
           R    = round(r, 3),
           d    = round(d, 3)))
}

# Calculer les métriques par modèle x préfecture
metriques <- cmip6_mensuel %>%
  group_by(model, NAME_2) %>%
  summarise(
    # PR
    BIAS_PR     = calc_metriques(pr_obs,     pr_mod)["BIAS"],
    RMSE_PR     = calc_metriques(pr_obs,     pr_mod)["RMSE"],
    R_PR        = calc_metriques(pr_obs,     pr_mod)["R"],
    d_PR        = calc_metriques(pr_obs,     pr_mod)["d"],
    # TASMAX
    BIAS_TASMAX = calc_metriques(tmax_obs, tasmax_mod)["BIAS"],
    RMSE_TASMAX = calc_metriques(tmax_obs, tasmax_mod)["RMSE"],
    R_TASMAX    = calc_metriques(tmax_obs, tasmax_mod)["R"],
    d_TASMAX    = calc_metriques(tmax_obs, tasmax_mod)["d"],
    # TASMIN
    BIAS_TASMIN = calc_metriques(tmin_obs, tasmin_mod)["BIAS"],
    RMSE_TASMIN = calc_metriques(tmin_obs, tasmin_mod)["RMSE"],
    R_TASMIN    = calc_metriques(tmin_obs, tasmin_mod)["R"],
    d_TASMIN    = calc_metriques(tmin_obs, tasmin_mod)["d"],
    .groups = "drop"
  )

glimpse(metriques)
cat("Nombre de lignes :", nrow(metriques), "\n")

# =============================================================
# Étape 7 : Moyenne globale et classement des modèles
# =============================================================

metriques_global <- metriques %>%
  group_by(model) %>%
  summarise(
    BIAS_PR     = round(mean(BIAS_PR),     3),
    RMSE_PR     = round(mean(RMSE_PR),     3),
    R_PR        = round(mean(R_PR),        3),
    d_PR        = round(mean(d_PR),        3),
    BIAS_TASMAX = round(mean(BIAS_TASMAX), 3),
    RMSE_TASMAX = round(mean(RMSE_TASMAX), 3),
    R_TASMAX    = round(mean(R_TASMAX),    3),
    d_TASMAX    = round(mean(d_TASMAX),    3),
    BIAS_TASMIN = round(mean(BIAS_TASMIN), 3),
    RMSE_TASMIN = round(mean(RMSE_TASMIN), 3),
    R_TASMIN    = round(mean(R_TASMIN),    3),
    d_TASMIN    = round(mean(d_TASMIN),    3),
    .groups = "drop"
  )

# Classement : RMSE faible + R élevé + d élevé + BIAS proche de 0
metriques_global <- metriques_global %>%
  mutate(
    score_PR     = R_PR     + d_PR     - abs(BIAS_PR)    /max(abs(BIAS_PR))     - RMSE_PR    /max(RMSE_PR),
    score_TASMAX = R_TASMAX + d_TASMAX - abs(BIAS_TASMAX)/max(abs(BIAS_TASMAX)) - RMSE_TASMAX/max(RMSE_TASMAX),
    score_TASMIN = R_TASMIN + d_TASMIN - abs(BIAS_TASMIN)/max(abs(BIAS_TASMIN)) - RMSE_TASMIN/max(RMSE_TASMIN),
    score_global = round((score_PR + score_TASMAX + score_TASMIN) / 3, 3)
  ) %>%
  arrange(desc(score_global)) %>%
  mutate(Rang = row_number()) %>%
  select(Rang, model, everything())

print(metriques_global %>% select(Rang, model, score_global), n = 26)

# =============================================================
# Étape 8 : Export Excel des métriques
# =============================================================

prefectures_ordre <- c("Dapaong", "Mango", "Kara", "Niamtougou")

chemin <- "D:/TAF/KOTCHADJO/New evaluation/Evaluation new/Exportation"

make_wide <- function(data, variable) {
  data %>%
    select(model, NAME_2, !!sym(variable)) %>%
    pivot_wider(names_from = NAME_2, values_from = !!sym(variable)) %>%
    select(model, all_of(prefectures_ordre)) %>%
    mutate(Moyenne = round(rowMeans(select(., -model), na.rm = TRUE), 3)) %>%
    arrange(model)
}

# FICHIER 1 : Précipitations
wb <- createWorkbook()
addWorksheet(wb, "BIAS_PR");        writeData(wb, "BIAS_PR",        make_wide(metriques, "BIAS_PR"))
addWorksheet(wb, "RMSE_PR");        writeData(wb, "RMSE_PR",        make_wide(metriques, "RMSE_PR"))
addWorksheet(wb, "R_PR");           writeData(wb, "R_PR",           make_wide(metriques, "R_PR"))
addWorksheet(wb, "d_PR");           writeData(wb, "d_PR",           make_wide(metriques, "d_PR"))
addWorksheet(wb, "Classement_Global"); writeData(wb, "Classement_Global", metriques_global)
saveWorkbook(wb, paste0(chemin, "Metriques_PR.xlsx"), overwrite = TRUE)
cat("Metriques_PR.xlsx exporté\n")

# FICHIER 2 : Tmax
wb2 <- createWorkbook()
addWorksheet(wb2, "BIAS_TASMAX");   writeData(wb2, "BIAS_TASMAX",   make_wide(metriques, "BIAS_TASMAX"))
addWorksheet(wb2, "RMSE_TASMAX");   writeData(wb2, "RMSE_TASMAX",   make_wide(metriques, "RMSE_TASMAX"))
addWorksheet(wb2, "R_TASMAX");      writeData(wb2, "R_TASMAX",      make_wide(metriques, "R_TASMAX"))
addWorksheet(wb2, "d_TASMAX");      writeData(wb2, "d_TASMAX",      make_wide(metriques, "d_TASMAX"))
addWorksheet(wb2, "Classement_Global"); writeData(wb2, "Classement_Global", metriques_global)
saveWorkbook(wb2, paste0(chemin, "Metriques_TASMAX.xlsx"), overwrite = TRUE)
cat("Metriques_TASMAX.xlsx exporté\n")

# FICHIER 3 : Tmin
wb3 <- createWorkbook()
addWorksheet(wb3, "BIAS_TASMIN");   writeData(wb3, "BIAS_TASMIN",   make_wide(metriques, "BIAS_TASMIN"))
addWorksheet(wb3, "RMSE_TASMIN");   writeData(wb3, "RMSE_TASMIN",   make_wide(metriques, "RMSE_TASMIN"))
addWorksheet(wb3, "R_TASMIN");      writeData(wb3, "R_TASMIN",      make_wide(metriques, "R_TASMIN"))
addWorksheet(wb3, "d_TASMIN");      writeData(wb3, "d_TASMIN",      make_wide(metriques, "d_TASMIN"))
addWorksheet(wb3, "Classement_Global"); writeData(wb3, "Classement_Global", metriques_global)
saveWorkbook(wb3, paste0(chemin, "Metriques_TASMIN.xlsx"), overwrite = TRUE)
cat("Metriques_TASMIN.xlsx exporté\n")



# ============================================================
# SELECTION DES MODELES CMIP6 PAR COMPREHENSIVE RATING METRIC
# Methode : Chen et al. (2011) ; Kebede et al. (2026)
# Fichiers : ExportationMetriques_PR / TASMAX / TASMIN
# 26 modeles | 3 variables | 4 metriques = 12 criteres
# Critere de selection : RM > 0.5
# ============================================================

library(readxl)
library(writexl)

chemin <- "D:/TAF/KOTCHADJO/New evaluation/Evaluation new/"

# ------------------------------------------------------------
# 1. LECTURE DES MOYENNES PAR METRIQUE ET PAR VARIABLE
# Colonne "Moyenne" = colonne 6 (index 5) dans chaque feuille
# Ligne 1 = en-tete, lignes 2 a 27 = 26 modeles
# ------------------------------------------------------------

lire_moyenne <- function(fichier, feuille) {
  df <- read_excel(fichier, sheet = feuille, col_names = TRUE)
  # Renommer les colonnes
  names(df) <- c("model", "Dapaong", "Mango", "Kara", "Niamtougou", "Moyenne")
  df$Moyenne <- as.numeric(df$Moyenne)
  return(df[, c("model", "Moyenne")])
}

# Precipitation
PR    <- lire_moyenne(paste0(chemin, "ExportationMetriques_PR.xlsx"),     "BIAS_PR")
PR$BIAS  <- PR$Moyenne
PR$RMSE  <- lire_moyenne(paste0(chemin, "ExportationMetriques_PR.xlsx"),  "RMSE_PR")$Moyenne
PR$R     <- lire_moyenne(paste0(chemin, "ExportationMetriques_PR.xlsx"),  "R_PR")$Moyenne
PR$d     <- lire_moyenne(paste0(chemin, "ExportationMetriques_PR.xlsx"),  "d_PR")$Moyenne
PR$Moyenne <- NULL

# Tmax
TX    <- lire_moyenne(paste0(chemin, "ExportationMetriques_TASMAX.xlsx"), "BIAS_TASMAX")
TX$BIAS  <- TX$Moyenne
TX$RMSE  <- lire_moyenne(paste0(chemin, "ExportationMetriques_TASMAX.xlsx"), "RMSE_TASMAX")$Moyenne
TX$R     <- lire_moyenne(paste0(chemin, "ExportationMetriques_TASMAX.xlsx"), "R_TASMAX")$Moyenne
TX$d     <- lire_moyenne(paste0(chemin, "ExportationMetriques_TASMAX.xlsx"), "d_TASMAX")$Moyenne
TX$Moyenne <- NULL

# Tmin
TN    <- lire_moyenne(paste0(chemin, "ExportationMetriques_TASMIN.xlsx"), "BIAS_TASMIN")
TN$BIAS  <- TN$Moyenne
TN$RMSE  <- lire_moyenne(paste0(chemin, "ExportationMetriques_TASMIN.xlsx"), "RMSE_TASMIN")$Moyenne
TN$R     <- lire_moyenne(paste0(chemin, "ExportationMetriques_TASMIN.xlsx"), "R_TASMIN")$Moyenne
TN$d     <- lire_moyenne(paste0(chemin, "ExportationMetriques_TASMIN.xlsx"), "d_TASMIN")$Moyenne
TN$Moyenne <- NULL

modeles <- PR$model
n <- length(modeles)  # 26
m <- 12               # 4 metriques x 3 variables

cat("Modeles lus :", n, "\n")
cat("Criteres    :", m, "\n\n")

# ------------------------------------------------------------
# 2. CLASSEMENT PAR CRITERE
# BIAS et RMSE : rang 1 = valeur absolue la plus faible
# R et d       : rang 1 = valeur la plus elevee
# ------------------------------------------------------------

classer_min <- function(x) rank(abs(x), ties.method = "min")
classer_max <- function(x) rank(-x,     ties.method = "min")

rangs <- data.frame(
  model    = modeles,
  P_BIAS   = classer_min(PR$BIAS),
  P_RMSE   = classer_min(PR$RMSE),
  P_R      = classer_max(PR$R),
  P_d      = classer_max(PR$d),
  TX_BIAS  = classer_min(TX$BIAS),
  TX_RMSE  = classer_min(TX$RMSE),
  TX_R     = classer_max(TX$R),
  TX_d     = classer_max(TX$d),
  TN_BIAS  = classer_min(TN$BIAS),
  TN_RMSE  = classer_min(TN$RMSE),
  TN_R     = classer_max(TN$R),
  TN_d     = classer_max(TN$d)
)

cols_rangs <- names(rangs)[-1]  # toutes sauf "model"

# ------------------------------------------------------------
# 3. CALCUL DU RM
# Formule Chen et al. (2011) :
# RM(i) = 1 - [ (1 / (n x m)) x somme des rangs du modele i ]
# RM varie entre 0 et 1
# RM > 0.5 = modele dans la moitie superieure sur tous criteres
# ------------------------------------------------------------

somme_rangs <- rowSums(rangs[, cols_rangs])
RM          <- 1 - (somme_rangs / (n * m))
rang_RM     <- rank(-RM, ties.method = "min")

resultats <- data.frame(
  Rang_RM     = rang_RM,
  Modele      = modeles,
  Somme_rangs = somme_rangs,
  RM          = round(RM, 4),
  Retenu      = ifelse(RM > 0.5, "OUI", "NON")
)

resultats <- resultats[order(resultats$Rang_RM), ]

# ------------------------------------------------------------
# 4. AFFICHAGE
# ------------------------------------------------------------

cat("=== CLASSEMENT COMPLET PAR RM ===\n\n")
print(resultats, row.names = FALSE)

cat("\n=== MODELES RETENUS (RM > 0.5) ===\n")
retenus <- resultats[resultats$Retenu == "OUI", ]
cat("Nombre de modeles retenus :", nrow(retenus), "\n\n")
print(retenus[, c("Rang_RM", "Modele", "RM")], row.names = FALSE)

# ------------------------------------------------------------
# 5. EXPORT EXCEL
# ------------------------------------------------------------

rangs_export <- rangs[order(rang_RM), ]
rangs_export <- cbind(
  rangs_export,
  Somme_rangs = somme_rangs[order(rang_RM)],
  RM          = round(RM[order(rang_RM)], 4),
  Retenu      = ifelse(RM[order(rang_RM)] > 0.5, "OUI", "NON")
)

export_list <- list(
  Classement_RM  = resultats,
  Rangs_criteres = rangs_export
)

writexl::write_xlsx(export_list, paste0(chemin, "RM_selection_modeles.xlsx"))
cat("\nFichier exporte : RM_selection_modeles.xlsx\n")



# =============================================================
# Étape 9 : Diagrammes de Taylor
# =============================================================

library(plotrix)
library(tidyverse)

chemin_pref <- "D:/TAF/KOTCHADJO/New evaluation/Evaluation new/Exportation/Par préfecture/"

prefectures <- c("Dapaong", "Mango", "Kara", "Niamtougou")

couleurs <- c(
  "red", "blue", "green3", "orange", "purple", "cyan4", "brown",
  "hotpink", "darkblue", "darkgreen", "gold", "gray40", "tomato",
  "steelblue", "olivedrab", "coral", "navy", "seagreen", "chocolate",
  "magenta", "turquoise4", "firebrick", "dodgerblue", "darkorchid",
  "sienna", "deeppink"
)

formes <- c(15, 16, 17, 18, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
            13, 14, 15, 16, 17, 18, 1, 2, 3, 4)

dir.create(paste0(chemin_pref, "Global/"), showWarnings = FALSE, recursive = TRUE)
for (pref in prefectures) {
  dir.create(paste0(chemin_pref, pref, "/"), showWarnings = FALSE, recursive = TRUE)
}
cat("Dossiers créés\n")

plot_taylor_final <- function(variable_mod, variable_obs, titre_var, fichier_prefix, pref_cible = NULL) {

  if (is.null(pref_cible)) {
    liste <- "Global"
  } else {
    liste <- pref_cible
  }

  for (pref in liste) {

    if (pref == "Global") {
      obs_pref <- cmip6_mensuel %>%
        filter(model == models[1]) %>%
        group_by(year, month) %>%
        summarise(val = mean(.data[[variable_obs]], na.rm = TRUE), .groups = "drop") %>%
        arrange(year, month) %>% pull(val)
      dossier <- paste0(chemin_pref, "Global/")
      titre   <- paste(titre_var, "\nGlobal (8 préfectures)")
    } else {
      obs_pref <- cmip6_mensuel %>%
        filter(NAME_2 == pref, model == models[1]) %>%
        arrange(year, month) %>%
        pull(.data[[variable_obs]])
      dossier <- paste0(chemin_pref, pref, "/")
      titre   <- paste(titre_var, "\n", pref)
    }

    nom_fichier <- paste0(dossier, fichier_prefix, "_", pref, ".png")

    png(nom_fichier, width = 2400, height = 1200, res = 120)
    layout(matrix(c(1, 2), nrow = 1), widths = c(3, 1))
    par(mar = c(5, 4, 4, 1))

    if (pref == "Global") {
      mod1 <- cmip6_mensuel %>%
        filter(model == models[1]) %>%
        group_by(year, month) %>%
        summarise(val = mean(.data[[variable_mod]], na.rm = TRUE), .groups = "drop") %>%
        arrange(year, month) %>% pull(val)
    } else {
      mod1 <- cmip6_mensuel %>%
        filter(NAME_2 == pref, model == models[1]) %>%
        arrange(year, month) %>%
        pull(.data[[variable_mod]])
    }

    taylor.diagram(obs_pref, mod1,
                   col = couleurs[1], pch = formes[1],
                   main = titre,
                   xlab = "Écart-type",
                   ylab = "Écart-type",
                   ref.sd = TRUE, sd.arcs = TRUE,
                   show.gamma = TRUE,
                   normalize = FALSE,
                   cex = 1.3, cex.main = 1.2,
                   gamma.col = "darkgreen")

    for (i in 2:length(models)) {
      if (pref == "Global") {
        mod_i <- cmip6_mensuel %>%
          filter(model == models[i]) %>%
          group_by(year, month) %>%
          summarise(val = mean(.data[[variable_mod]], na.rm = TRUE), .groups = "drop") %>%
          arrange(year, month) %>% pull(val)
      } else {
        mod_i <- cmip6_mensuel %>%
          filter(NAME_2 == pref, model == models[i]) %>%
          arrange(year, month) %>%
          pull(.data[[variable_mod]])
      }

      taylor.diagram(obs_pref, mod_i,
                     col = couleurs[i], pch = formes[i],
                     add = TRUE, normalize = FALSE,
                     cex = 1.3)
    }

    sd_obs <- sd(obs_pref, na.rm = TRUE)
    points(sd_obs, 0, pch = 8, cex = 2.5, col = "black", lwd = 2)
    text(sd_obs, -sd_obs * 0.07, "OBS", cex = 0.8, font = 2)

    gamma1 <- sd_obs * 0.5
    text(sd_obs - gamma1 * cos(pi/4),
         gamma1 * sin(pi/4),
         "RMSE", cex = 0.85, font = 2,
         col = "darkgreen", srt = 45)

    par(mar = c(5, 0, 4, 1))
    plot.new()
    legend("center",
           legend = c("OBS", models),
           col    = c("black", couleurs),
           pch    = c(8, formes),
           pt.lwd = c(2, rep(1, length(models))),
           cex    = 0.75,
           ncol   = 1,
           bty    = "o",
           bg     = "white",
           title  = expression(bold("Modèles")),
           title.adj = 0.5)

    dev.off()
    cat("Exporté :", nom_fichier, "\n")
  }
}

# Par préfecture
plot_taylor_final("pr_mod",     "pr_obs",
                  "Diagramme de Taylor - Précipitations (mm/mois)",
                  "Taylor_PR", prefectures)

plot_taylor_final("tasmax_mod", "tmax_obs",
                  "Diagramme de Taylor - Tmax (°C)",
                  "Taylor_TASMAX", prefectures)

plot_taylor_final("tasmin_mod", "tmin_obs",
                  "Diagramme de Taylor - Tmin (°C)",
                  "Taylor_TASMIN", prefectures)

# Global
plot_taylor_final("pr_mod",     "pr_obs",
                  "Diagramme de Taylor - Précipitations (mm/mois)",
                  "Taylor_PR")

plot_taylor_final("tasmax_mod", "tmax_obs",
                  "Diagramme de Taylor - Tmax (°C)",
                  "Taylor_TASMAX")

plot_taylor_final("tasmin_mod", "tmin_obs",
                  "Diagramme de Taylor - Tmin (°C)",
                  "Taylor_TASMIN")

cat("Tous les diagrammes sont terminés.\n")


