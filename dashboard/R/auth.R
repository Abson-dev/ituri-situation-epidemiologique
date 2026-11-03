# Connexion : obligatoire partout, sauf lancement explicite sur ce poste par lancer.R.
# Un serveur sans base de comptes refuse de démarrer plutôt que d'afficher les données.

auth_requise <- function() !identical(Sys.getenv("ITURI_LOCAL"), "1")

chemin_comptes <- function() {
  autre <- Sys.getenv("ITURI_COMPTES_DB")
  if (nzchar(autre)) autre else here::here("dashboard/comptes.sqlite")
}

secret_comptes <- function() Sys.getenv("ITURI_COMPTES_SECRET")

verifier_auth <- function() {
  if (!auth_requise()) return(invisible(TRUE))
  if (!file.exists(chemin_comptes())) stop("Base de comptes absente : démarrage refusé. Voir dashboard/gerer_comptes.R.", call. = FALSE)
  if (!nzchar(secret_comptes())) stop("Secret ITURI_COMPTES_SECRET absent : démarrage refusé.", call. = FALSE)
  invisible(TRUE)
}

proteger_ui <- function(ui, logo = NULL) {
  if (!auth_requise()) return(ui)
  shinymanager::secure_app(ui, language = "fr", enable_admin = FALSE,
                           head_auth = shiny::tags$link(rel = "stylesheet", href = "style.css"),
                           tags_top = shiny::tagList(
                             if (!is.null(logo)) shiny::img(src = logo, alt = "Médecins Sans Frontières", class = "connexion-logo"),
                             shiny::h1(class = "connexion-titre", "Surveillance épidémiologique"),
                             shiny::p(class = "connexion-lieu", "Province de l'Ituri, RDC")
                           ),
                           tags_bottom = shiny::p(class = "connexion-pied", "Accès réservé aux personnes autorisées."))
}

proteger_serveur <- function() {
  if (!auth_requise()) return(invisible(NULL))
  shinymanager::secure_server(
    check_credentials = shinymanager::check_credentials(chemin_comptes(), passphrase = secret_comptes()),
    timeout = 30
  )
}
