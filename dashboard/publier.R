# Publication sur shinyapps.io.
#   Rscript dashboard/publier.R              affiche seulement ce qui serait envoyé
#   Rscript dashboard/publier.R --publier    envoie
#
# Liste blanche : seul ce qui est listé ici quitte le poste. Y figurent les données réelles
# et les résultats provisoires, visibles par tout compte connecté.

args <- commandArgs(trailingOnly = TRUE)
options(repos = c(CRAN = "https://cloud.r-project.org"))
racine <- here::here()
relatif <- function(chemins) sub(paste0("^", gsub("([.()+^$])", "\\\\\\1", racine), "/"), "", normalizePath(chemins, winslash = "/"))
lister <- function(dossier, motif = NULL) list.files(file.path(racine, dossier), pattern = motif, full.names = TRUE, recursive = TRUE)

obligatoires <- c(".here", ".Renviron", "app.R", "config.yml", "config.local.yml", "dashboard/app.R", "dashboard/comptes.sqlite",
                  "rapports/rapport_hebdomadaire.Rmd", "assets/modele_msf.docx", "assets/logo_msf.png")
absents <- obligatoires[!file.exists(file.path(racine, obligatoires))]
if (length(absents)) stop("Fichiers manquants : ", paste(absents, collapse = ", "), call. = FALSE)

renv_txt <- readLines(file.path(racine, ".Renviron"), warn = FALSE)
if (!any(grepl("^ITURI_COMPTES_SECRET=.+", renv_txt))) stop("Secret absent de .Renviron.", call. = FALSE)
if (any(grepl("ITURI_LOCAL|ITURI_DEMO", renv_txt))) stop(".Renviron désactiverait la connexion ou activerait la démo.", call. = FALSE)

# Dernière base et résultats correspondant à cette base uniquement.
bases <- list.files(file.path(racine, "data/raw"), pattern = "^Base_DPS_Ituri_Week[0-9]+\\.xlsx$", full.names = TRUE)
if (!length(bases)) stop("Aucune base réelle dans data/raw.", call. = FALSE)
semaines <- as.integer(sub(".*Week([0-9]+)\\.xlsx$", "\\1", bases))
base <- bases[which.max(semaines)]
empreinte <- digest::digest(file = base, algo = "sha256")
modeles <- list.files(file.path(racine, "outputs/modeles"), pattern = "\\.rds$", full.names = TRUE)
modeles <- modeles[order(file.mtime(modeles), decreasing = TRUE)]
garde <- Filter(function(f) identical(tryCatch(readRDS(f)$sha256, error = function(e) NULL), empreinte), modeles)
modele <- if (length(garde)) garde[1] else character(0)
if (!length(modele)) message("Aucun résultat pour cette base : la page Analyse d'impact sera vide.")

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
fichiers <- unique(relatif(fichiers[file.exists(fichiers)]))

interdits <- grepl("comptes_mots_de_passe|controles_reels|article\\.Rmd|anomalies|\\.pdf$|^data/synthetic|^outputs/(hebdo|article|demo)", fichiers)
if (any(interdits)) stop("Fichier interdit dans l'envoi : ", paste(fichiers[interdits], collapse = ", "), call. = FALSE)

cat("Fichiers qui seraient envoyés (", length(fichiers), ") :\n", sep = "")
cat(paste0("  ", fichiers), sep = "\n")

if (!"--publier" %in% args) {
  cat("\nAucun envoi. Ajouter --publier pour publier.\n")
  quit(save = "no")
}
if (!nrow(rsconnect::accounts())) stop("Compte shinyapps.io non configuré : voir dashboard/README.md.", call. = FALSE)
rsconnect::deployApp(appDir = racine, appFiles = fichiers, appName = "ituri-surveillance", appTitle = "Ituri - Surveillance",
                     forceUpdate = TRUE, launch.browser = FALSE, lint = FALSE, logLevel = "normal")
