# Appelé par Lancer_tableau_de_bord.bat : prépare l'environnement puis ouvre l'application.
args <- commandArgs(trailingOnly = TRUE)
navigateur <- !"--sans-navigateur" %in% args
port_arg <- sub("--port=", "", grep("^--port=", args, value = TRUE))
if ("--demo" %in% args) Sys.setenv(ITURI_DEMO = "1")

paquets <- c("bsicons", "bslib", "digest", "dplyr", "DT", "fixest", "flextable", "ggplot2", "here", "knitr", "mgcv", "officer", "patchwork",
             "plotly", "ragg", "reactable", "readr", "readxl", "rmarkdown", "scales", "shiny", "tibble", "tidyr", "writexl", "yaml", "zip")
manquants <- paquets[!vapply(paquets, requireNamespace, logical(1), quietly = TRUE)]
if (length(manquants)) {
  message("Installation des paquets manquants : ", paste(manquants, collapse = ", "))
  install.packages(manquants, repos = "https://cloud.r-project.org")
}

# Pandoc n'est requis que pour le bouton « Rapport Word ».
if (!rmarkdown::pandoc_available()) {
  for (dossier in c(file.path(Sys.getenv("LOCALAPPDATA"), "Pandoc"), "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools",
                    "C:/Program Files/RStudio/bin/pandoc", "C:/Program Files/Pandoc")) {
    if (file.exists(file.path(dossier, "pandoc.exe"))) { rmarkdown::find_pandoc(dir = dossier); break }
  }
}

logo <- here::here("assets/logo_msf.png")
for (f in list.files(here::here("R"), pattern = "\\.R$", full.names = TRUE)) source(f, encoding = "UTF-8")
if (!file.exists(here::here("assets/modele_msf.docx"))) construire_modele(here::here("assets/modele_msf.docx"), logo = if (file.exists(logo)) logo)

# 127.0.0.1 : l'application n'est accessible que depuis cet ordinateur.
shiny::runApp(here::here("dashboard"), host = "127.0.0.1", launch.browser = navigateur,
              port = if (length(port_arg)) as.integer(port_arg) else getOption("shiny.port"))
