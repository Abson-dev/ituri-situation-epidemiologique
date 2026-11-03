# Identité visuelle : logo, en-têtes de page, chiffres clés.

# Le logo reste hors du dépôt public (assets/ est ignoré par Git). Il n'apparaît que sur les données
# réelles : un écran de démonstration ne doit pas passer pour une production de MSF.
logo_msf_src <- function() {
  f <- here::here("assets/logo_msf.png")
  if (mode_demo() || !file.exists(f)) return(NULL)
  knitr::image_uri(f)
}

marque <- function(logo) {
  shiny::div(
    class = "marque",
    if (!is.null(logo)) shiny::img(src = logo, alt = "Médecins Sans Frontières", class = "marque-logo"),
    shiny::div(class = "marque-texte",
               shiny::span(class = "marque-titre", "Surveillance épidémiologique"),
               shiny::span(class = "marque-lieu", "Province de l'Ituri, RDC"))
  )
}

entete_page <- function(titre, texte, icone, droite = NULL) {
  shiny::div(
    class = "entete-page",
    shiny::div(class = "entete-icone", bsicons::bs_icon(icone)),
    shiny::div(class = "entete-texte", shiny::h2(titre), shiny::p(texte)),
    droite
  )
}

rendu_puce_semaine <- function(S, cfg) {
  shiny::renderUI({
    deb <- date_semaine(S(), cfg)
    shiny::div(class = "puce-semaine", bsicons::bs_icon("calendar3"),
               shiny::strong(paste0("Semaine ", S())),
               shiny::span(sprintf("%s au %s", format(deb, "%d/%m"), format(deb + 6, "%d/%m/%Y"))))
  })
}

# ton : bleu (paludisme), vert (respiratoire), orange (Ebola), rouge (décès), ambre (à vérifier), ardoise (général).
kpi <- function(titre, valeur, detail, icone, ton = "ardoise") {
  bslib::value_box(titre, valeur, shiny::div(class = "kpi-detail", detail), class = paste0("kpi ton-", ton),
                   showcase = bsicons::bs_icon(icone), showcase_layout = bslib::showcase_left_center(width = 0.22))
}

# Une hausse n'est pas colorée comme « mauvaise » en vert/rouge : une baisse peut aussi venir d'une moindre notification.
evolution_tag <- function(e, S) {
  if (is.na(e$variation)) return("Pas de comparaison possible")
  hausse <- e$variation >= 0
  shiny::tagList(
    shiny::span(class = if (hausse) "hausse" else "baisse",
                paste0(if (hausse) "▲ +" else "▼ −", pct_fr(abs(e$variation)))),
    sprintf(" par rapport à S%d, sur %d zones comparables", S - 1L, e$n_zones)
  )
}
