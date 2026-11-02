#!/usr/bin/env Rscript
# Point d'entrée unique.
#   Rscript run_all.R                    rapport de la dernière semaine, article, supplément, tests
#   Rscript run_all.R --cible=hebdo --semaine=32
#   Rscript run_all.R --cible=article [--forcer]
#   Rscript run_all.R --cible=tests
#   Rscript run_all.R --demo             tout, sur données synthétiques (aucune donnée réelle requise)

args <- commandArgs(trailingOnly = TRUE)
arg <- function(nom, defaut = NULL) {
  m <- grep(paste0("^--", nom, "="), args, value = TRUE)
  if (length(m)) sub(paste0("^--", nom, "="), "", m[1]) else defaut
}
cible <- arg("cible", "tout")
forcer <- "--forcer" %in% args
demo <- "--demo" %in% args
if (demo) Sys.setenv(ITURI_DEMO = "1")
sorties <- if (demo) "outputs/demo" else "outputs"

options(dplyr.summarise.inform = FALSE)
if (!rmarkdown::pandoc_available()) {
  candidats <- c(file.path(Sys.getenv("LOCALAPPDATA"), "Pandoc"), "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools",
                 "C:/Program Files/RStudio/bin/pandoc", "C:/Program Files/Pandoc")
  for (dossier in candidats) if (file.exists(file.path(dossier, "pandoc.exe"))) { rmarkdown::find_pandoc(dir = dossier); break }
}
if (!rmarkdown::pandoc_available()) stop("Pandoc introuvable : installer RStudio ou Pandoc.")

for (f in list.files(here::here("R"), full.names = TRUE)) source(f, encoding = "UTF-8")
cfg <- lire_config()

if (demo) generer_donnees_synthetiques(cfg)

# Le logo MSF n'est pas versionné : sans lui, le modèle Word est construit sans en-tête.
logo <- here::here("assets/logo_msf.png")
if (!file.exists(here::here("assets/modele_msf.docx"))) construire_modele(here::here("assets/modele_msf.docx"), logo = if (file.exists(logo)) logo)
if (!file.exists(here::here("assets/modele_article.docx"))) construire_modele(here::here("assets/modele_article.docx"))

rendre <- function(rmd, sortie, dossier, params = NULL) {
  dir.create(here::here(dossier), recursive = TRUE, showWarnings = FALSE)
  rmarkdown::render(here::here("rapports", rmd), output_file = sortie, output_dir = here::here(dossier),
                    params = params, envir = new.env(parent = globalenv()), quiet = TRUE,
                    knit_root_dir = here::here("rapports"))
  message("OK : ", file.path(dossier, sortie))
}

if (cible %in% c("tests", "tout")) {
  r <- as.data.frame(testthat::test_dir(here::here("tests/testthat"), reporter = "summary", stop_on_failure = TRUE))
  message("Tests : ", sum(r$nb), " vérifications, ", sum(r$failed), " échec(s).")
}

if (cible %in% c("hebdo", "tout")) {
  S <- as.integer(arg("semaine", nettoyer_ids(trouver_base(cfg), cfg)$semaine_max))
  rendre("rapport_hebdomadaire.Rmd", sprintf("Rapport_Ituri_S%02d.docx", S), file.path(sorties, "hebdo"), list(semaine = S))
}

if (cible %in% c("article", "tout")) {
  ids <- nettoyer_ids(trouver_base(cfg), cfg)
  invisible(resultats_article(ids, cfg, forcer = forcer))
  # Le texte du manuscrit reste local jusqu'à l'autorisation des propriétaires des données.
  if (file.exists(here::here("rapports/article.Rmd"))) rendre("article.Rmd", "Article_Ebola_Ituri.docx", file.path(sorties, "article"))
  rendre("supplement.Rmd", "Supplement_Ebola_Ituri.docx", file.path(sorties, "article"))
}

dir.create(here::here(sorties), recursive = TRUE, showWarnings = FALSE)
writeLines(utils::capture.output(utils::sessionInfo()), here::here(sorties, "session_info.txt"))
