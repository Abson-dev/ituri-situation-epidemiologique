mod_qualite_ui <- function(id) {
  ns <- NS(id)
  page(
    uiOutput(ns("kpis")),
    ligne(c(8, 4),
      carte_graphe("Rapportage par zone et par semaine (bleu : rapporté ; gris : absent)", plotly::plotlyOutput(ns("g_rapport"), height = "100%"), 760),
      carte_tableau("Couverture des zones", reactable::reactableOutput(ns("couverture")), 760)
    ),
    ligne(c(7, 5),
      carte_tableau("Journal de nettoyage (base entière)", reactable::reactableOutput(ns("journal")), 420),
      carte_tableau("Conflits à arbitrer avec la DPS", reactable::reactableOutput(ns("conflits")), 420)
    )
  )
}

mod_qualite_server <- function(id, app, S, ids_S) {
  moduleServer(id, function(input, output, session) {
    cz <- reactive({
      tibble::tibble(zs = app$zones) |>
        dplyr::left_join(rapportage_zones(ids_S()$donnees, S()), by = "zs") |>
        dplyr::mutate(sem_rapportees = dplyr::coalesce(sem_rapportees, 0L), couverture = dplyr::coalesce(couverture, 0), couverture_4s = dplyr::coalesce(couverture_4s, 0))
    })

    output$kpis <- renderUI({
      c <- cz()
      prevu <- length(app$zones) * S()
      j <- app$ids$journal
      val <- function(m) j$n[grepl(m, j$regle, fixed = TRUE)][1]
      layout_column_wrap(
        width = "190px", fill = FALSE,
        kpi("Zones-semaines rapportées", pct_fr(sum(c$sem_rapportees) / prevu), paste0(n_fr(sum(c$sem_rapportees)), " sur ", n_fr(prevu)), "broadcast"),
        kpi("Zones incomplètes, 4 semaines", n_fr(sum(c$couverture_4s < 1)), paste0("sur ", length(app$zones)), "hourglass-split"),
        kpi("Conflits d'enregistrement", n_fr(val("Conflicting")), "Base entière", "files"),
        kpi("Dates corrigées", n_fr(val("Week-start")), "Base entière", "calendar-x")
      )
    })

    output$g_rapport <- plotly::renderPlotly({
      x <- ids_S()$donnees |> dplyr::distinct(zs, semaine) |> dplyr::mutate(r = 1)
      ord <- cz() |> dplyr::arrange(couverture, zs) |> dplyr::pull(zs)
      x <- tidyr::complete(x, zs = app$zones, semaine = seq_len(S()))
      pl_carte_chaleur(x, "r", "Rapporté", app$cfg, zmax = 1, ordre = ord, format_z = ".0f") |>
        plotly::style(showscale = FALSE, hovertemplate = "<b>%{y}</b>, S%{x} : rapporté<extra></extra>")
    })

    output$couverture <- reactable::renderReactable({
      x <- cz() |> dplyr::arrange(couverture_4s, couverture, zs) |>
        dplyr::transmute(Zone = nom_zone(zs), `Semaines` = sem_rapportees, `Couverture` = pct_fr(couverture, 0), `4 sem.` = pct_fr(couverture_4s, 0),
                         `Manquantes` = dplyr::coalesce(manquantes_4s, ""))
      tableau(x, lignes = 40)
    })

    output$journal <- reactable::renderReactable({
      x <- app$ids$journal |> dplyr::filter(n > 0) |> dplyr::transmute(`Règle` = regle, Occurrences = n, Action = action)
      tableau(x, lignes = 20, columns = list(`Règle` = reactable::colDef(minWidth = 220), Occurrences = reactable::colDef(minWidth = 80, align = "right"), Action = reactable::colDef(minWidth = 220)))
    })

    output$conflits <- reactable::renderReactable({
      x <- app$ids$conflits |> dplyr::arrange(numsem, zs) |>
        dplyr::transmute(Zone = nom_zone(zs), Semaine = paste0("S", numsem), Maladie = libelle(maladie, app$cfg), `Valeurs` = valeurs)
      tableau(x, lignes = 12)
    })
  })
}
