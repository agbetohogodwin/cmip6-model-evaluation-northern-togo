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
library(writexl)

setwd("D:/TAF/KOTCHADJO/New evaluation/CMIP6_evaluation")

list.files()
list.files("CMIP6")
list.files("Observations")

# =============================================================
# Étape 2 : Charger les observations (déjà mensuelles)
# =============================================================

obs_mensuel <- read_excel("Observations/Météo.xlsx") %>%
  mutate(year  = as.integer(year),
         month = as.integer(month))

glimpse(obs_mensuel)

# =============================================================
# Étape 3 : Charger les fichiers CMIP6 historiques (journaliers)
# 32 modèles : GFDL-CM4 remplacé par GFDL-CM4_gr1 et GFDL-CM4_gr2
# =============================================================

models <- c(
  'ACCESS-CM2', 'ACCESS-ESM1-5', 'BCC-CSM2-MR', 'CanESM5',
  'CMCC-CM2-SR5', 'CMCC-ESM2', 'CNRM-CM6-1', 'CNRM-ESM2-1',
  'EC-Earth3', 'EC-Earth3-Veg-LR', 'FGOALS-g3', 'GFDL-CM4_gr1',
  'GFDL-CM4_gr2', 'GFDL-ESM4', 'GISS-E2-1-G', 'HadGEM3-GC31-LL',
  'HadGEM3-GC31-MM', 'INM-CM4-8', 'INM-CM5-0', 'IPSL-CM6A-LR',
  'KACE-1-0-G', 'KIOST-ESM', 'MIROC-ES2L', 'MIROC6',
  'MPI-ESM1-2-HR', 'MPI-ESM1-2-LR', 'MRI-ESM2-0', 'NESM3',
  'NorESM2-LM', 'NorESM2-MM', 'TaiESM1', 'UKESM1-0-LL'
)

cat("Nombre de modèles :", length(models), "\n")

# Les fichiers GEE exportés pour GFDL-CM4 sont nommés :
# GFDL-CM4_gr1_historical.csv et GFDL-CM4_gr2_historical.csv

cmip6_hist <- map(models, function(m) {
  fichier <- paste0("CMIP6/", m, "_historical.csv")
  if (file.exists(fichier)) {
    read_csv(fichier, show_col_types = FALSE) %>%
      mutate(date   = as.Date(date),
             pr     = pr * 86400,
             tasmax = tasmax - 273.15,
             tasmin = tasmin - 273.15,
             model  = m) %>%
      select(-any_of(c("scenario", "grid")))
  } else {
    message("Fichier manquant : ", fichier)
    NULL
  }
}) %>% list_rbind()

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

calc_metriques <- function(obs, sim) {
  bias  <- mean(sim - obs, na.rm = TRUE)
  rmse  <- sqrt(mean((sim - obs)^2, na.rm = TRUE))
  r     <- cor(obs, sim, use = "complete.obs")
  num   <- sum((sim - obs)^2, na.rm = TRUE)
  denom <- sum((abs(sim - mean(obs, na.rm = TRUE)) +
                  abs(obs - mean(obs, na.rm = TRUE)))^2, na.rm = TRUE)
  d <- 1 - (num / denom)
  return(c(BIAS = round(bias, 3),
           RMSE = round(rmse, 3),
           R    = round(r,    3),
           d    = round(d,    3)))
}

metriques <- cmip6_mensuel %>%
  group_by(model, NAME_2) %>%
  summarise(
    BIAS_PR     = calc_metriques(pr_obs,   pr_mod)["BIAS"],
    RMSE_PR     = calc_metriques(pr_obs,   pr_mod)["RMSE"],
    R_PR        = calc_metriques(pr_obs,   pr_mod)["R"],
    d_PR        = calc_metriques(pr_obs,   pr_mod)["d"],
    BIAS_TASMAX = calc_metriques(tmax_obs, tasmax_mod)["BIAS"],
    RMSE_TASMAX = calc_metriques(tmax_obs, tasmax_mod)["RMSE"],
    R_TASMAX    = calc_metriques(tmax_obs, tasmax_mod)["R"],
    d_TASMAX    = calc_metriques(tmax_obs, tasmax_mod)["d"],
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
  ) %>%
  mutate(
    score_PR     = R_PR     + d_PR     - abs(BIAS_PR)    /max(abs(BIAS_PR))     - RMSE_PR    /max(RMSE_PR),
    score_TASMAX = R_TASMAX + d_TASMAX - abs(BIAS_TASMAX)/max(abs(BIAS_TASMAX)) - RMSE_TASMAX/max(RMSE_TASMAX),
    score_TASMIN = R_TASMIN + d_TASMIN - abs(BIAS_TASMIN)/max(abs(BIAS_TASMIN)) - RMSE_TASMIN/max(RMSE_TASMIN),
    score_global = round((score_PR + score_TASMAX + score_TASMIN) / 3, 3)
  ) %>%
  arrange(desc(score_global)) %>%
  mutate(Rang = row_number()) %>%
  select(Rang, model, everything())

print(metriques_global %>% select(Rang, model, score_global), n = 32)

# =============================================================
# Étape 8 : Export Excel des métriques
# =============================================================

prefectures_ordre <- c("Dapaong", "Mango", "Kara", "Niamtougou")
chemin <- "D:/TAF/KOTCHADJO/New evaluation/Evaluation new/Exportation/"

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
addWorksheet(wb, "BIAS_PR");           writeData(wb, "BIAS_PR",           make_wide(metriques, "BIAS_PR"))
addWorksheet(wb, "RMSE_PR");           writeData(wb, "RMSE_PR",           make_wide(metriques, "RMSE_PR"))
addWorksheet(wb, "R_PR");              writeData(wb, "R_PR",              make_wide(metriques, "R_PR"))
addWorksheet(wb, "d_PR");              writeData(wb, "d_PR",              make_wide(metriques, "d_PR"))
addWorksheet(wb, "Classement_Global"); writeData(wb, "Classement_Global", metriques_global)
saveWorkbook(wb, paste0(chemin, "Metriques_PR.xlsx"), overwrite = TRUE)
cat("Metriques_PR.xlsx exporté\n")

# FICHIER 2 : Tmax
wb2 <- createWorkbook()
addWorksheet(wb2, "BIAS_TASMAX");       writeData(wb2, "BIAS_TASMAX",       make_wide(metriques, "BIAS_TASMAX"))
addWorksheet(wb2, "RMSE_TASMAX");       writeData(wb2, "RMSE_TASMAX",       make_wide(metriques, "RMSE_TASMAX"))
addWorksheet(wb2, "R_TASMAX");          writeData(wb2, "R_TASMAX",          make_wide(metriques, "R_TASMAX"))
addWorksheet(wb2, "d_TASMAX");          writeData(wb2, "d_TASMAX",          make_wide(metriques, "d_TASMAX"))
addWorksheet(wb2, "Classement_Global"); writeData(wb2, "Classement_Global", metriques_global)
saveWorkbook(wb2, paste0(chemin, "Metriques_TASMAX.xlsx"), overwrite = TRUE)
cat("Metriques_TASMAX.xlsx exporté\n")

# FICHIER 3 : Tmin
wb3 <- createWorkbook()
addWorksheet(wb3, "BIAS_TASMIN");       writeData(wb3, "BIAS_TASMIN",       make_wide(metriques, "BIAS_TASMIN"))
addWorksheet(wb3, "RMSE_TASMIN");       writeData(wb3, "RMSE_TASMIN",       make_wide(metriques, "RMSE_TASMIN"))
addWorksheet(wb3, "R_TASMIN");          writeData(wb3, "R_TASMIN",          make_wide(metriques, "R_TASMIN"))
addWorksheet(wb3, "d_TASMIN");          writeData(wb3, "d_TASMIN",          make_wide(metriques, "d_TASMIN"))
addWorksheet(wb3, "Classement_Global"); writeData(wb3, "Classement_Global", metriques_global)
saveWorkbook(wb3, paste0(chemin, "Metriques_TASMIN.xlsx"), overwrite = TRUE)
cat("Metriques_TASMIN.xlsx exporté\n")

# =============================================================
# SÉLECTION DES MODÈLES PAR COMPREHENSIVE RATING METRIC (RM)
# Méthode : Chen et al. (2011) ; Kebede et al. (2026)
# 32 modèles | 3 variables | 4 métriques = 12 critères
# Critère de sélection : RM > 0.5
# =============================================================

chemin_rm <- "D:/TAF/KOTCHADJO/New evaluation/Evaluation new/"

lire_moyenne <- function(fichier, feuille) {
  df <- read_excel(fichier, sheet = feuille, col_names = TRUE)
  names(df) <- c("model", "Dapaong", "Mango", "Kara", "Niamtougou", "Moyenne")
  df$Moyenne <- as.numeric(df$Moyenne)
  return(df[, c("model", "Moyenne")])
}

PR      <- lire_moyenne(paste0(chemin_rm, "Exportation/Metriques_PR.xlsx"),     "BIAS_PR")
PR$BIAS <- PR$Moyenne
PR$RMSE <- lire_moyenne(paste0(chemin_rm, "Exportation/Metriques_PR.xlsx"),     "RMSE_PR")$Moyenne
PR$R    <- lire_moyenne(paste0(chemin_rm, "Exportation/Metriques_PR.xlsx"),     "R_PR")$Moyenne
PR$d    <- lire_moyenne(paste0(chemin_rm, "Exportation/Metriques_PR.xlsx"),     "d_PR")$Moyenne
PR$Moyenne <- NULL

TX      <- lire_moyenne(paste0(chemin_rm, "Exportation/Metriques_TASMAX.xlsx"), "BIAS_TASMAX")
TX$BIAS <- TX$Moyenne
TX$RMSE <- lire_moyenne(paste0(chemin_rm, "Exportation/Metriques_TASMAX.xlsx"), "RMSE_TASMAX")$Moyenne
TX$R    <- lire_moyenne(paste0(chemin_rm, "Exportation/Metriques_TASMAX.xlsx"), "R_TASMAX")$Moyenne
TX$d    <- lire_moyenne(paste0(chemin_rm, "Exportation/Metriques_TASMAX.xlsx"), "d_TASMAX")$Moyenne
TX$Moyenne <- NULL

TN      <- lire_moyenne(paste0(chemin_rm, "Exportation/Metriques_TASMIN.xlsx"), "BIAS_TASMIN")
TN$BIAS <- TN$Moyenne
TN$RMSE <- lire_moyenne(paste0(chemin_rm, "Exportation/Metriques_TASMIN.xlsx"), "RMSE_TASMIN")$Moyenne
TN$R    <- lire_moyenne(paste0(chemin_rm, "Exportation/Metriques_TASMIN.xlsx"), "R_TASMIN")$Moyenne
TN$d    <- lire_moyenne(paste0(chemin_rm, "Exportation/Metriques_TASMIN.xlsx"), "d_TASMIN")$Moyenne
TN$Moyenne <- NULL

modeles <- PR$model
n <- length(modeles)  # 32
m <- 12               # 4 métriques x 3 variables

cat("Modèles lus :", n, "\n")
cat("Critères    :", m, "\n\n")

classer_min <- function(x) rank(abs(x), ties.method = "min")
classer_max <- function(x) rank(-x,     ties.method = "min")

rangs <- data.frame(
  model   = modeles,
  P_BIAS  = classer_min(PR$BIAS),
  P_RMSE  = classer_min(PR$RMSE),
  P_R     = classer_max(PR$R),
  P_d     = classer_max(PR$d),
  TX_BIAS = classer_min(TX$BIAS),
  TX_RMSE = classer_min(TX$RMSE),
  TX_R    = classer_max(TX$R),
  TX_d    = classer_max(TX$d),
  TN_BIAS = classer_min(TN$BIAS),
  TN_RMSE = classer_min(TN$RMSE),
  TN_R    = classer_max(TN$R),
  TN_d    = classer_max(TN$d)
)

cols_rangs  <- names(rangs)[-1]
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

cat("=== CLASSEMENT COMPLET PAR RM ===\n\n")
print(resultats, row.names = FALSE)

cat("\n=== MODÈLES RETENUS (RM > 0.5) ===\n")
retenus <- resultats[resultats$Retenu == "OUI", ]
cat("Nombre de modèles retenus :", nrow(retenus), "\n\n")
print(retenus[, c("Rang_RM", "Modele", "RM")], row.names = FALSE)

rangs_export <- rangs[order(rang_RM), ]
rangs_export <- cbind(
  rangs_export,
  Somme_rangs = somme_rangs[order(rang_RM)],
  RM          = round(RM[order(rang_RM)], 4),
  Retenu      = ifelse(RM[order(rang_RM)] > 0.5, "OUI", "NON")
)

writexl::write_xlsx(
  list(Classement_RM  = resultats,
       Rangs_criteres = rangs_export),
  paste0(chemin_rm, "RM_selection_modeles_32.xlsx")
)
cat("\nFichier exporté : RM_selection_modeles_32.xlsx\n")

# =============================================================
# Étape 9 : Diagrammes de Taylor
# =============================================================

chemin_pref <- "D:/TAF/KOTCHADJO/New evaluation/Evaluation new/Exportation/Par préfecture/"
prefectures <- c("Dapaong", "Mango", "Kara", "Niamtougou")

couleurs <- c(
  "red", "blue", "green3", "orange", "purple", "cyan4", "brown",
  "hotpink", "darkblue", "darkgreen", "gold", "gray40", "tomato",
  "steelblue", "olivedrab", "coral", "navy", "seagreen", "chocolate",
  "magenta", "turquoise4", "firebrick", "dodgerblue", "darkorchid",
  "sienna", "deeppink", "springgreen4", "orangered", "royalblue",
  "violetred", "tan4", "mediumorchid"
)

formes <- c(15, 16, 17, 18, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
            13, 14, 15, 16, 17, 18, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

# =============================================================
# Version resserrée de taylor.diagram (plotrix) : la fonction
# d'origine fixe toujours la marge de l'axe à 1.5x la plus
# grande dispersion (obs ou modèle). On copie la fonction pour
# pouvoir imposer une marge plus serrée (10%) via max.sd, sans
# toucher au reste de la géométrie du diagramme.
# =============================================================

taylor.diagram.tight <- function(ref, model, add = FALSE, col = "red", pch = 19,
                                  pos.cor = TRUE, xlab = "", ylab = "",
                                  main = "Taylor Diagram", show.gamma = TRUE,
                                  ngamma = 3, gamma.col = 8, sd.arcs = 0,
                                  ref.sd = FALSE, sd.method = "sample",
                                  grad.corr.lines = c(0.2, 0.4, 0.6, 0.8, 0.9),
                                  pcex = 1, cex.axis = 1, normalize = FALSE,
                                  mar = c(5, 4, 6, 6), max.sd = NULL, ...) {
  grad.corr.full <- c(0, 0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.99, 1)
  R <- cor(ref, model, use = "pairwise")
  if (is.list(ref)) ref <- unlist(ref)
  if (is.list(model)) ref <- unlist(model)
  SD <- function(x, subn) {
    meanx <- mean(x, na.rm = TRUE)
    devx  <- x - meanx
    sqrt(sum(devx * devx, na.rm = TRUE) / (length(x[!is.na(x)]) - subn))
  }
  subn <- sd.method != "sample"
  sd.r <- SD(ref, subn)
  sd.f <- SD(model, subn)
  if (normalize) { sd.f <- sd.f / sd.r; sd.r <- 1 }
  maxsd <- if (!is.null(max.sd)) max.sd else 1.5 * max(sd.f, sd.r)
  oldpar <- par("mar", "xpd", "xaxs", "yaxs")
  if (!add) {
    if (nchar(ylab) == 0) ylab <- "Standard deviation"
    par(mar = mar)
    plot(0, xlim = c(0, maxsd), ylim = c(0, maxsd), xaxs = "i", yaxs = "i",
         axes = FALSE, main = main, xlab = xlab, ylab = ylab, type = "n",
         cex = cex.axis, ...)
    if (grad.corr.lines[1]) {
      for (gcl in grad.corr.lines) lines(c(0, maxsd * gcl), c(0, maxsd * sqrt(1 - gcl^2)), lty = 3)
    }
    segments(c(0, 0), c(0, 0), c(0, maxsd), c(maxsd, 0))
    axis.ticks <- pretty(c(0, maxsd)); axis.ticks <- axis.ticks[axis.ticks <= maxsd]
    axis(1, at = axis.ticks, cex.axis = cex.axis)
    axis(2, at = axis.ticks, cex.axis = cex.axis)
    if (sd.arcs[1]) {
      if (length(sd.arcs) == 1) sd.arcs <- axis.ticks
      for (sdarc in sd.arcs) {
        xcurve <- cos(seq(0, pi/2, by = 0.03)) * sdarc
        ycurve <- sin(seq(0, pi/2, by = 0.03)) * sdarc
        lines(xcurve, ycurve, col = "blue", lty = 3)
      }
    }
    if (show.gamma[1]) {
      gamma <- if (length(show.gamma) > 1) show.gamma else pretty(c(0, maxsd), n = ngamma)[-1]
      if (gamma[length(gamma)] > maxsd) gamma <- gamma[-length(gamma)]
      labelpos <- seq(45, 70, length.out = length(gamma))
      for (gindex in seq_along(gamma)) {
        xcurve <- cos(seq(0, pi, by = 0.03)) * gamma[gindex] + sd.r
        endcurve <- which(xcurve < 0); endcurve <- ifelse(length(endcurve), min(endcurve) - 1, 105)
        ycurve <- sin(seq(0, pi, by = 0.03)) * gamma[gindex]
        maxcurve <- xcurve * xcurve + ycurve * ycurve
        startcurve <- which(maxcurve > maxsd * maxsd); startcurve <- ifelse(length(startcurve), max(startcurve) + 1, 0)
        lines(xcurve[startcurve:endcurve], ycurve[startcurve:endcurve], col = gamma.col)
        if (xcurve[labelpos[gindex]] > 0) boxed.labels(xcurve[labelpos[gindex]], ycurve[labelpos[gindex]], gamma[gindex], border = FALSE)
      }
    }
    xcurve <- cos(seq(0, pi/2, by = 0.01)) * maxsd
    ycurve <- sin(seq(0, pi/2, by = 0.01)) * maxsd
    lines(xcurve, ycurve)
    bigtickangles <- acos(seq(0.1, 0.9, by = 0.1))
    medtickangles <- acos(seq(0.05, 0.95, by = 0.1))
    smltickangles <- acos(seq(0.91, 0.99, by = 0.01))
    segments(cos(bigtickangles) * maxsd, sin(bigtickangles) * maxsd,
             cos(bigtickangles) * 0.97 * maxsd, sin(bigtickangles) * 0.97 * maxsd)
    par(xpd = TRUE)
    if (ref.sd) {
      xcurve <- cos(seq(0, pi/2, by = 0.01)) * sd.r
      ycurve <- sin(seq(0, pi/2, by = 0.01)) * sd.r
      lines(xcurve, ycurve)
    }
    points(sd.r, 0, cex = pcex)
    text(cos(c(bigtickangles, acos(c(0.95, 0.99)))) * 1.05 * maxsd,
         sin(c(bigtickangles, acos(c(0.95, 0.99)))) * 1.05 * maxsd,
         c(seq(0.1, 0.9, by = 0.1), 0.95, 0.99))
    text(maxsd * 0.8, maxsd * 0.8, "Correlation", srt = 315)
    segments(cos(medtickangles) * maxsd, sin(medtickangles) * maxsd,
             cos(medtickangles) * 0.98 * maxsd, sin(medtickangles) * 0.98 * maxsd)
    segments(cos(smltickangles) * maxsd, sin(smltickangles) * maxsd,
             cos(smltickangles) * 0.99 * maxsd, sin(smltickangles) * 0.99 * maxsd)
  }
  points(sd.f * R, sd.f * sin(acos(R)), pch = pch, col = col, cex = pcex)
  invisible(oldpar)
}

dir.create(paste0(chemin_pref, "Global/"), showWarnings = FALSE, recursive = TRUE)
for (pref in prefectures) {
  dir.create(paste0(chemin_pref, pref, "/"), showWarnings = FALSE, recursive = TRUE)
}
cat("Dossiers créés\n")

plot_taylor_final <- function(variable_mod, variable_obs, titre_var,
                              fichier_prefix, pref_cible = NULL) {
  
  liste <- if (is.null(pref_cible)) "Global" else pref_cible
  
  for (pref in liste) {
    
    if (pref == "Global") {
      obs_pref <- cmip6_mensuel %>%
        filter(model == models[1]) %>%
        group_by(year, month) %>%
        summarise(val = mean(.data[[variable_obs]], na.rm = TRUE), .groups = "drop") %>%
        arrange(year, month) %>% pull(val)
      dossier <- paste0(chemin_pref, "Global/")
      titre   <- paste(titre_var, "\nGlobal")
    } else {
      obs_pref <- cmip6_mensuel %>%
        filter(NAME_2 == pref, model == models[1]) %>%
        arrange(year, month) %>%
        pull(.data[[variable_obs]])
      dossier <- paste0(chemin_pref, pref, "/")
      titre   <- paste(titre_var, "\n", pref)
    }
    
    nom_fichier <- paste0(dossier, fichier_prefix, "_", pref, ".png")
    
    # --- Récupérer les 32 séries modélisées une seule fois, pour calculer ---
    # --- une échelle d'axe commune basée sur les données réelles         ---
    mod_list <- map(models, function(m) {
      if (pref == "Global") {
        cmip6_mensuel %>%
          filter(model == m) %>%
          group_by(year, month) %>%
          summarise(val = mean(.data[[variable_mod]], na.rm = TRUE), .groups = "drop") %>%
          arrange(year, month) %>% pull(val)
      } else {
        cmip6_mensuel %>%
          filter(NAME_2 == pref, model == m) %>%
          arrange(year, month) %>%
          pull(.data[[variable_mod]])
      }
    })
    
    # --- Tracer d'abord le modèle avec le plus grand écart-type, avec  ---
    # --- une échelle d'axe resserrée à 10% de marge (au lieu de 50%    ---
    # --- par défaut dans plotrix) pour occuper tout l'espace utile     ---
    sd_obs         <- sd(obs_pref, na.rm = TRUE)
    sd_modeles     <- sapply(mod_list, sd, na.rm = TRUE)
    sd_normalisees <- sd_modeles / sd_obs
    ordre          <- order(-sd_modeles)
    premier        <- ordre[1]
    max_sd_axe     <- max(c(1, sd_normalisees), na.rm = TRUE) * 1.10
    
    png(nom_fichier, width = 2800, height = 1400, res = 120)
    layout(matrix(c(1, 2), nrow = 1), widths = c(3, 1))
    par(mar = c(5, 4, 4, 1))
    
    taylor.diagram.tight(obs_pref, mod_list[[premier]],
                   col = couleurs[premier], pch = formes[premier],
                   main = titre,
                   xlab = "Normalized standard deviation",
                   ylab = "Normalized standard deviation",
                   ref.sd = TRUE, sd.arcs = TRUE,
                   show.gamma = TRUE, normalize = TRUE,
                   pcex = 1.5, cex.main = 1.5,
                   gamma.col = "darkgreen",
                   max.sd = max_sd_axe)
    
    for (i in ordre[-1]) {
      taylor.diagram.tight(obs_pref, mod_list[[i]],
                     col = couleurs[i], pch = formes[i],
                     add = TRUE, normalize = TRUE, pcex = 2.5)
    }
    
    # --- Après normalisation, la référence (Obs) est toujours à 1 ---
    points(1, 0, pch = 8, cex = 2.5, col = "black", lwd = 2)
    text(1, -0.05, "Obs", cex = 1.8, font = 2)
    
    gamma1 <- 0.5
    text(1 - gamma1 * cos(pi/4),
         gamma1 * sin(pi/4),
         "RMSE", cex = 0.85, font = 2, col = "darkgreen", srt = 45)
    
    par(mar = c(5, 0, 4, 1))
    plot.new()
    legend("center",
           legend    = c("Obs", models),
           col       = c("black", couleurs),
           pch       = c(8, formes),
           pt.lwd    = c(2, rep(1, length(models))),
           cex       = 1.4,
           ncol      = 1,
           bty       = "o",
           bg        = "white",
           title     = expression(bold("Models")),
           title.adj = 0.5)
    
    dev.off()
    cat("Exporté :", nom_fichier, "\n")
  }
}

# Par préfecture
plot_taylor_final("pr_mod",     "pr_obs",
                  "Taylor Diagram - Precipitation (mm/month)",
                  "Taylor_PR", prefectures)

plot_taylor_final("tasmax_mod", "tmax_obs",
                  "Taylor Diagram - Tmax (°C)",
                  "Taylor_TASMAX", prefectures)

plot_taylor_final("tasmin_mod", "tmin_obs",
                  "Taylor Diagram - Tmin (°C)",
                  "Taylor_TASMIN", prefectures)

# Global
plot_taylor_final("pr_mod",     "pr_obs",
                  "Taylor Diagram - Precipitation (mm/month)",
                  "Taylor_PR")

plot_taylor_final("tasmax_mod", "tmax_obs",
                  "Taylor Diagram - Tmax (°C)",
                  "Taylor_TASMAX")

plot_taylor_final("tasmin_mod", "tmin_obs",
                  "Taylor Diagram - Tmin (°C)",
                  "Taylor_TASMIN")

cat("Tous les diagrammes sont terminés.\n")
