mod_qualite_ui <- function(id) {
  ns <- NS(id)
  page(
    entete_page("Qualité des données", "Rapportage des zones et corrections appliquées à la base", "clipboard-check", uiOutput(ns("semaine"))),
    uiOutput(ns("kpis")),
    ligne(c(7, 5),
      carte_graphe("Rapportage par zone et par semaine (bleu : rapporté ; gris foncé : absent)", plotly::plotlyOutput(ns("g_rapport"), height = "100%"), 760),
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
    output$semaine <- rendu_puce_semaine(S, app$cfg)
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
      ton_si <- function(n) if (!is.na(n) && n > 0) "ambre" else "vert"
      incompletes <- sum(c$couverture_4s < 1)
      layout_column_wrap(
        width = "190px", fill = FALSE,
        kpi("Zones-semaines rapportées", pct_fr(sum(c$sem_rapportees) / prevu), paste0(n_fr(sum(c$sem_rapportees)), " sur ", n_fr(prevu)), "broadcast", "bleu"),
        kpi("Zones incomplètes, 4 semaines", n_fr(incompletes), paste0("sur ", length(app$zones)), "hourglass-split", ton_si(incompletes)),
        kpi("Conflits d'enregistrement", n_fr(val("Conflicting")), "Base entière", "files", ton_si(val("Conflicting"))),
        kpi("Dates corrigées", n_fr(val("Week-start")), "Base entière", "calendar-x", ton_si(val("Week-start")))
      )
    })

    output$g_rapport <- plotly::renderPlotly({
      x <- ids_S()$donnees |> dplyr::distinct(zs, semaine) |> dplyr::mutate(r = 1)
      ord <- cz() |> dplyr::arrange(couverture, zs) |> dplyr::pull(zs)
      x <- tidyr::complete(x, zs = app$zones, semaine = seq_len(S()), fill = list(r = 0))
      # L'information utile est l'absence : le rapporté reste clair, le manquant ressort.
      pl_carte_chaleur(x, "r", "Rapporté", app$cfg, zmax = 1, ordre = ord, echelle = list(c(0, COUL$encre2), c(1, "#a9cdf5"))) |>
        plotly::style(showscale = FALSE, hovertemplate = "<b>%{y}</b>, S%{x}<br>Rapporté : %{z:.0f} (1 = oui, 0 = non)<extra></extra>") |>
        plotly::layout(plot_bgcolor = COUL$surface)
    })

    output$couverture <- reactable::renderReactable({
      x <- cz() |> dplyr::arrange(couverture_4s, couverture, zs) |>
        dplyr::transmute(Zone = nom_zone(zs), `Sem.` = sem_rapportees, `Couverture` = couverture, `4 sem.` = couverture_4s,
                         `Absentes` = dplyr::coalesce(manquantes_4s, ""))
      tableau(x, lignes = 40, columns = list(
        Zone = col_libelle(92),
        `Sem.` = col_nombre(44),
        `Couverture` = reactable::colDef(minWidth = 84, cell = function(v) barre_cellule(pct_fr(v, 0), v, "bleu")),
        `4 sem.` = reactable::colDef(minWidth = 62, align = "center", cell = function(v) pastille(pct_fr(v, 0), if (v >= 1) "ok" else if (v >= 0.5) "attention" else "critique")),
        `Absentes` = reactable::colDef(minWidth = 72, class = "muet")
      ))
    })

    output$journal <- reactable::renderReactable({
      x <- app$ids$journal |> dplyr::filter(n > 0) |> dplyr::transmute(`Règle` = traduire_journal(regle), Occurrences = n, Action = traduire_journal(action))
      tableau(x, lignes = 20, columns = list(`Règle` = col_libelle(220), Occurrences = col_nombre(84), Action = reactable::colDef(minWidth = 220)))
    })

    output$conflits <- reactable::renderReactable({
      x <- app$ids$conflits |> dplyr::arrange(numsem, zs) |>
        dplyr::transmute(Zone = nom_zone(zs), Semaine = paste0("S", numsem), Maladie = libelle(maladie, app$cfg), `Valeurs` = valeurs)
      tableau(x, lignes = 12, columns = list(Zone = col_libelle(100), Semaine = reactable::colDef(minWidth = 66, align = "center"), `Valeurs` = reactable::colDef(align = "right", minWidth = 80)))
    })
  })
}
