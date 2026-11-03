mod_maladies_ui <- function(id) {
  ns <- NS(id)
  page(
    entete_page("Maladies", "Évolution d'une maladie, dans la province ou dans une zone", "activity", uiOutput(ns("semaine"))),
    uiOutput(ns("filtres")),
    ligne(c(8, 4),
      pile(
        carte_graphe(textOutput(ns("titre_cas"), inline = TRUE), plotly::plotlyOutput(ns("g_cas"), height = "100%"), 300),
        carte_graphe("Décès notifiés", plotly::plotlyOutput(ns("g_deces"), height = "100%"), 200)
      ),
      carte_graphe("Zones les plus touchées, 4 dernières semaines", plotly::plotlyOutput(ns("g_rang"), height = "100%"), 516)
    ),
    carte_graphe("Incidence par zone et par semaine, pour 100 000 hab. (gris : aucune ligne dans la base)", plotly::plotlyOutput(ns("g_chaleur"), height = "100%"), 760)
  )
}

mod_maladies_server <- function(id, app, S, ids_S) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$semaine <- rendu_puce_semaine(S, app$cfg)
    output$filtres <- renderUI({
      layout_columns(
        col_widths = c(5, 4, 3), class = "filtres",
        selectInput(ns("maladie"), "Maladie", choix_maladies(app), selected = "PALUDISME SUSP"),
        selectInput(ns("zone"), "Territoire", c(list(Province = stats::setNames(PROVINCE, "Toute la province")), choix_zones(app))),
        radioButtons(ns("mesure"), "Mesure", c("Cas" = "cas", "Pour 100 000 hab." = "taux"), inline = TRUE)
      )
    })
    d <- reactive(ids_S()$donnees)
    code <- reactive({ req(input$maladie); input$maladie })
    lib <- reactive(libelle(code(), app$cfg))
    zone <- reactive(input$zone %||% PROVINCE)

    serie <- reactive({
      x <- serie_cible(d(), code(), S(), zone())
      pop <- if (zone() == PROVINCE) sum(app$ids$pop$pop_ref) else app$ids$pop$pop_ref[app$ids$pop$zs == zone()]
      dplyr::mutate(x, taux = cas / pop * 1e5)
    })

    output$titre_cas <- renderText({
      lieu <- if (zone() == PROVINCE) "province" else nom_zone(zone())
      paste0(lib(), " : ", if (identical(input$mesure, "taux")) "incidence" else "cas", ", ", lieu)
    })
    output$g_cas <- plotly::renderPlotly({
      m <- input$mesure %||% "cas"
      pl_barres(serie(), m, if (m == "taux") "pour 100 000 hab." else "cas", app$cfg)
    })
    output$g_deces <- plotly::renderPlotly(pl_barres(serie(), "deces", "décès", app$cfg, COUL_DECES))
    output$g_rang <- plotly::renderPlotly({
      x <- d() |> dplyr::filter(maladie == code(), semaine > S() - 4L) |> dplyr::group_by(zs) |>
        dplyr::summarise(cas = sum(cas), .groups = "drop") |> dplyr::filter(cas > 0) |>
        dplyr::slice_max(cas, n = 12, with_ties = FALSE) |> dplyr::mutate(zone = nom_zone(zs))
      validate(need(nrow(x) > 0, "Aucun cas au cours des 4 dernières semaines."))
      pl_barres_h(x, "cas", "zone", "cas")
    })
    output$g_chaleur <- plotly::renderPlotly({
      x <- d() |> dplyr::filter(maladie == code()) |> dplyr::mutate(taux = cas / pop * 1e5) |> dplyr::select(zs, semaine, taux)
      validate(need(nrow(x) > 0, "Aucune donnée pour cette maladie."))
      x <- tidyr::complete(x, zs = app$zones, semaine = seq_len(S()))
      pl_carte_chaleur(x, "taux", "Pour 100 000", app$cfg, format_z = ".1f")
    })
  })
}
