mod_analyse_ui <- function(id) {
  ns <- NS(id)
  page(
    div(class = "attention serieux", bsicons::bs_icon("exclamation-triangle-fill"),
        div(strong("Résultats exploratoires, non publiés et non diffusables."),
            div(class = "detail", "Sans base 2025, la saisonnalité propre à chaque zone n'est pas écartée. Une baisse peut refléter moins de consultations, moins de notifications ou moins de maladie."))),
    ligne(c(5, 7),
      carte_graphe("Effet associé à 10 cas d'Ebola / 10 000 hab. (contre 0)", plotly::plotlyOutput(ns("g_foret"), height = "100%"), 470),
      pile(
        div(class = "filtres", uiOutput(ns("filtre"))),
        carte_graphe("Étude d'événement : ratio par rapport aux semaines 16 à 19", plotly::plotlyOutput(ns("g_evenement"), height = "100%"), 380)
      )
    ),
    carte_tableau("Solidité des résultats", reactable::reactableOutput(ns("table")), 310)
  )
}

mod_analyse_server <- function(id, app) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    r <- app$modele
    placebo <- "Placebo: false break at week 10, pre-period only"
    tendance <- "Zone-specific linear trends"
    virgule <- function(x) gsub(".", ",", x, fixed = TRUE)

    resume <- reactive({
      validate(need(!is.null(r), "Résultats non calculés pour cette base : lancer  Rscript run_all.R --cible=article"))
      lapply(names(r$resultats), function(code) {
        x <- r$resultats[[code]]
        s <- x$sensibilite$table
        pl <- s[s$analyse == placebo, ]
        tr <- s[s$analyse == tendance, ]
        tibble::tibble(code = code, issue = libelle(code, app$cfg), rrr = x$principal$rrr, bas = x$principal$bas, haut = x$principal$haut,
                       p_holm = x$principal$p_holm %||% NA_real_, p_pre = x$evenement$p_pre,
                       placebo_ok = pl$bas <= 1 & pl$haut >= 1, placebo = virgule(sprintf("%.2f (%.2f–%.2f)", pl$rrr, pl$bas, pl$haut)),
                       tendance_ok = tr$haut < 1, tendance = virgule(sprintf("%.2f (%.2f–%.2f)", tr$rrr, tr$bas, tr$haut)))
      }) |> dplyr::bind_rows()
    })

    output$g_foret <- plotly::renderPlotly(pl_foret(resume()))

    output$filtre <- renderUI({
      req(!is.null(r))
      codes <- names(r$resultats)
      selectInput(ns("issue"), "Résultat", stats::setNames(codes, libelle(codes, app$cfg)), selected = "IRA", width = "320px")
    })

    output$g_evenement <- plotly::renderPlotly({
      req(input$issue)
      x <- r$resultats[[input$issue]]$evenement$table |> dplyr::mutate(milieu = (sem_deb + sem_fin) / 2, etiq = paste0("S", sem_deb, "–S", sem_fin))
      plotly::plot_ly(x, x = ~milieu, y = ~rrr, type = "scatter", mode = "markers",
                      marker = list(color = COUL$serie1, size = 9),
                      error_y = list(type = "data", symmetric = FALSE, array = ~haut - rrr, arrayminus = ~rrr - bas, color = COUL$serie1, thickness = 1.6, width = 0),
                      text = ~etiq, customdata = ~paste0(sprintf("%.2f", bas), " – ", sprintf("%.2f", haut)),
                      hovertemplate = "<b>%{text}</b><br>Ratio : %{y:.2f}<br>IC 95 % : %{customdata}<extra></extra>") |>
        pl_style(app$cfg) |>
        plotly::layout(yaxis = list(title = "Ratio (échelle log)", type = "log", tickvals = c(0.4, 0.6, 0.8, 1, 1.25, 1.5), gridcolor = COUL$grille, zeroline = FALSE, fixedrange = TRUE, rangemode = "normal"))
    })

    output$table <- reactable::renderReactable({
      p <- function(v) ifelse(is.na(v), "–", ifelse(v < 0.001, "<0,001", sub(".", ",", sprintf("%.3f", v), fixed = TRUE)))
      x <- resume() |>
        dplyr::transmute(`Résultat` = issue, `Ratio (IC 95 %)` = virgule(sprintf("%.2f (%.2f–%.2f)", rrr, bas, haut)), `p (Holm)` = p(p_holm),
                         `Tendance préalable (p)` = p(p_pre), `Placebo (IC 95 %)` = placebo,
                         `Test placebo` = ifelse(placebo_ok, "Passé", "Échoué"), `Avec tendances par zone` = tendance,
                         `Hors de 1 avec tendances` = ifelse(tendance_ok, "Oui", "Non"))
      tableau(x, lignes = 10, columns = list(`Résultat` = reactable::colDef(minWidth = 190),
        `Test placebo` = reactable::colDef(cell = function(v) span(class = paste("pastille", if (v == "Passé") "ok" else "critique"), v)),
        `Hors de 1 avec tendances` = reactable::colDef(cell = function(v) span(class = paste("pastille", if (v == "Oui") "ok" else "attention"), v))))
    })
  })
}
