# Publication sur shinyapps.io. Sans --publier, rien n'est envoyé.
#   Rscript dashboard/publier.R                          site réel, liste des fichiers
#   Rscript dashboard/publier.R --publier                site réel, envoi (met à jour ituri-surveillance)
#   Rscript dashboard/publier.R --demo                   site de démonstration, liste des fichiers
#   Rscript dashboard/publier.R --demo --publier         site de démonstration, envoi
#
# Options :
#   --nom=mon-site      autre application, sans toucher aux autres. Refusé si ce nom existe déjà.
#   --mettre-a-jour     autorise à remplacer l'application de ce nom.
#
# Liste blanche : seul ce qui est listé ici quitte le poste. Le site réel contient les données réelles et les
# résultats provisoires. Le site de démonstration ne contient que des données synthétiques et aucun logo.

args <- commandArgs(trailingOnly = TRUE)
options(repos = c(CRAN = "https://cloud.r-project.org"))
option <- function(nom) {
  x <- sub(paste0("^--", nom, "="), "", grep(paste0("^--", nom, "="), args, value = TRUE))
  if (length(x)) x[1] else NA_character_
}
demo <- "--demo" %in% args
SITE_REEL <- "ituri-surveillance"
nom <- option("nom")
nom_choisi <- !is.na(nom)
if (!nom_choisi) nom <- if (demo) "ituri-surveillance-demo" else SITE_REEL
if (!grepl("^[A-Za-z0-9_-]{4,63}$", nom)) stop("Nom invalide : lettres, chiffres, tirets, 4 à 63 caractères.", call. = FALSE)
if (demo && nom == SITE_REEL) stop("Refus : le site de démonstration ne doit pas remplacer le site réel.", call. = FALSE)

racine <- here::here()
relatif <- function(chemins) sub(paste0("^", gsub("([.()+^$])", "\\\\\\1", racine), "/"), "", normalizePath(chemins, winslash = "/"))
lister <- function(dossier, motif = NULL) list.files(file.path(racine, dossier), pattern = motif, full.names = TRUE, recursive = TRUE)

obligatoires <- c(".here", ".Renviron", "app.R", "config.yml", "dashboard/app.R", "dashboard/comptes.sqlite", "rapports/rapport_hebdomadaire.Rmd")
if (!demo) obligatoires <- c(obligatoires, "config.local.yml", "assets/modele_msf.docx", "assets/logo_msf.png")
absents <- obligatoires[!file.exists(file.path(racine, obligatoires))]
if (length(absents)) stop("Fichiers manquants : ", paste(absents, collapse = ", "), call. = FALSE)

renv_txt <- readLines(file.path(racine, ".Renviron"), warn = FALSE)
if (!any(grepl("^ITURI_COMPTES_SECRET=.+", renv_txt))) stop("Secret absent de .Renviron.", call. = FALSE)
if (any(grepl("ITURI_LOCAL|ITURI_DEMO", renv_txt))) stop(".Renviron désactiverait la connexion ou forcerait un mode.", call. = FALSE)

# Résultats correspondant exactement à la base envoyée.
modele_de <- function(empreinte) {
  modeles <- list.files(file.path(racine, "outputs/modeles"), pattern = "\\.rds$", full.names = TRUE)
  modeles <- modeles[order(file.mtime(modeles), decreasing = TRUE)]
  garde <- Filter(function(f) identical(tryCatch(readRDS(f)$sha256, error = function(e) NULL), empreinte), modeles)
  if (length(garde)) garde[1] else character(0)
}

if (demo) {
  # Pour générer ce qui manque en mode synthétique, sans jamais lire les données réelles.
  Sys.setenv(ITURI_DEMO = "1")
  for (f in lister("R", "\\.R$")) source(f, encoding = "UTF-8")
  cfg <- lire_config()
  if (!dir.exists(chemin_donnees("raw"))) generer_donnees_synthetiques(cfg)
  if (!dir.exists(chemin_donnees("raw", "historique"))) generer_historique_synthetique(cfg)
  base <- trouver_base(cfg)
  modele <- modele_de(digest::digest(file = base, algo = "sha256"))
  fichiers <- c(file.path(racine, obligatoires), lister("R", "\\.R$"), lister("dashboard/R", "\\.R$"), lister("dashboard/www"),
                lister("data/synthetic"), modele)
} else {
  # Dernière base et résultats correspondant à cette base uniquement.
  bases <- list.files(file.path(racine, "data/raw"), pattern = "^Base_DPS_Ituri_Week[0-9]+\\.xlsx$", full.names = TRUE)
  if (!length(bases)) stop("Aucune base réelle dans data/raw.", call. = FALSE)
  base <- bases[which.max(as.integer(sub(".*Week([0-9]+)\\.xlsx$", "\\1", bases)))]
  modele <- modele_de(digest::digest(file = base, algo = "sha256"))
  fichiers <- c(
    file.path(racine, obligatoires),
    lister("R", "\\.R$"),
    lister("dashboard/R", "\\.R$"),
    lister("dashboard/www"),
    base,
    lister("data/raw/historique", "^Base_DPS_Ituri_[0-9]{4}\\.xlsx$"),
    list.files(file.path(racine, "data"), pattern = "^corrections_manuelles_[0-9]{4}\\.csv$", full.names = TRUE),
    file.path(racine, "data/external", c("mve_provinces.csv", "mve_zones.csv", "rapport_s32_reference.csv", "completude_as.csv")),
    file.path(racine, "data/corrections_manuelles.csv"),
    list.files(file.path(racine, "notes"), pattern = "^S[0-9]+\\.md$", full.names = TRUE),
    modele
  )
}
if (!length(modele)) message("Aucun résultat pour cette base : la page Analyse d'impact sera vide.")
fichiers <- unique(relatif(fichiers[file.exists(fichiers)]))

interdits <- grepl("comptes_mots_de_passe|controles_reels|article\\.Rmd|anomalies|\\.pdf$|^outputs/(hebdo|article|demo)", fichiers)
interdits <- interdits | if (demo) grepl("^data/(raw|external)/|^data/corrections|^notes/|config\\.local|logo|^assets/", fichiers) else grepl("^data/synthetic", fichiers)
if (any(interdits)) stop("Fichier interdit dans l'envoi : ", paste(fichiers[interdits], collapse = ", "), call. = FALSE)
if (demo) {
  reelles <- list.files(file.path(racine, "data/raw"), pattern = "\\.xlsx$", full.names = TRUE, recursive = TRUE)
  empreintes <- function(x) vapply(x, digest::digest, character(1), file = TRUE, algo = "sha256", USE.NAMES = FALSE)
  xlsx <- file.path(racine, grep("\\.xlsx$", fichiers, value = TRUE))
  if (length(intersect(empreintes(reelles), empreintes(xlsx)))) stop("Refus : une base réelle figure dans l'envoi de démonstration.", call. = FALSE)
}

cat(sprintf("Application : %s (%s)\n", nom, if (demo) "DÉMONSTRATION, données synthétiques" else "DONNÉES RÉELLES"))
cat("Fichiers qui seraient envoyés (", length(fichiers), ") :\n", sep = "")
cat(paste0("  ", fichiers), sep = "\n")

# Le site de démonstration part d'un dossier temporaire : sans data/raw, l'application démarre seule en mode synthétique.
preparer <- function(dossier) {
  for (f in fichiers) {
    dir.create(dirname(file.path(dossier, f)), recursive = TRUE, showWarnings = FALSE)
    file.copy(file.path(racine, f), file.path(dossier, f), overwrite = TRUE)
  }
  dir.create(file.path(dossier, "assets"), showWarnings = FALSE)
  # Le modèle Word sans logo se construit avec Pandoc, cherché aux mêmes endroits que dans lancer.R.
  if (!rmarkdown::pandoc_available()) {
    for (d in c(file.path(Sys.getenv("LOCALAPPDATA"), "Pandoc"), "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools",
                "C:/Program Files/RStudio/bin/pandoc", "C:/Program Files/Pandoc")) {
      if (file.exists(file.path(d, "pandoc.exe"))) { rmarkdown::find_pandoc(dir = d); break }
    }
  }
  if (!rmarkdown::pandoc_available()) stop("Pandoc introuvable : installer RStudio ou Pandoc.", call. = FALSE)
  construire_modele(file.path(dossier, "assets/modele_msf.docx"), logo = NULL)
  invisible(c(fichiers, "assets/modele_msf.docx"))
}

dossier_verif <- option("preparer")
if (!is.na(dossier_verif)) {
  if (!demo) stop("--preparer n'existe que pour --demo.", call. = FALSE)
  preparer(dossier_verif)
  cat("\nDossier préparé, rien n'est envoyé :", dossier_verif, "\n")
  quit(save = "no")
}
if (!"--publier" %in% args) {
  cat("\nAucun envoi. Ajouter --publier pour publier.\n")
  quit(save = "no")
}
if (!nrow(rsconnect::accounts())) stop("Compte shinyapps.io non configuré : voir dashboard/README.md.", call. = FALSE)

# Le site réel se met à jour sous son nom habituel ; tout autre envoi ne remplace rien sans le demander.
existantes <- rsconnect::applications()$name
if (nom %in% existantes && (demo || nom_choisi) && !"--mettre-a-jour" %in% args) {
  stop("Refus : l'application ", nom, " existe déjà. Choisir un autre --nom, ou ajouter --mettre-a-jour pour la remplacer.", call. = FALSE)
}
titre <- if (demo) "Ituri - Démonstration" else "Ituri - Surveillance"
if (demo) {
  dossier <- tempfile("ituri_demo_")
  dir.create(dossier)
  on.exit(unlink(dossier, recursive = TRUE), add = TRUE)
  fichiers_app <- preparer(dossier)
  rsconnect::deployApp(appDir = dossier, appFiles = fichiers_app, appName = nom, appTitle = titre,
                       forceUpdate = TRUE, launch.browser = FALSE, lint = FALSE, logLevel = "normal")
  unlink(dossier, recursive = TRUE)
} else {
  rsconnect::deployApp(appDir = racine, appFiles = fichiers, appName = nom, appTitle = titre,
                       forceUpdate = TRUE, launch.browser = FALSE, lint = FALSE, logLevel = "normal")
}
