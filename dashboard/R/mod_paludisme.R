mod_paludisme_ui <- function(id) {
  ns <- NS(id)
  page(
    uiOutput(ns("message")),
    ligne(c(6, 6),
      carte_graphe("Rapport confirmés / suspects, province", plotly::plotlyOutput(ns("g_pos"), height = "100%"), 300),
      carte_graphe("Paludisme confirmé : cas", plotly::plotlyOutput(ns("g_conf"), height = "100%"), 300)
    ),
    ligne(c(8, 4),
      carte_graphe("Confirmés / suspects, par zone et par semaine ; zones à vérifier en premier (gris : aucune donnée)", plotly::plotlyOutput(ns("g_chaleur"), height = "100%"), 760),
      carte_tableau("Zones à vérifier", reactable::reactableOutput(ns("ruptures")), 760)
    )
  )
}

mod_paludisme_server <- function(id, app, S, ids_S) {
  moduleServer(id, function(input, output, session) {
    output$message <- renderUI({
      pos <- positivite_province(ids_S(), S())
      p <- pos[pos$semaine == S(), ]
      n <- sum(!is.na(ids_S()$ruptures$debut_rupture))
      div(class = "attention serieux", bsicons::bs_icon("exclamation-triangle-fill"),
          div(strong(sprintf("En S%d, le rapport est de %s dans toutes les zones et de %s hors zones sans confirmation.", S(), pct_fr(p$brute, 0), pct_fr(p$corrigee, 0))),
              div(class = "detail", sprintf("%d zones ont perdu brutalement leurs cas confirmés alors que les suspects persistent. Hypothèses : rupture de TDR ou arrêt de la déclaration, à vérifier avec les stocks. Le nombre de patients testés n'étant pas dans la base, ce rapport n'est qu'un indicateur approché de la positivité.", n))))
    })

    output$g_pos <- plotly::renderPlotly({
      x <- positivite_province(ids_S(), S())
      plotly::plot_ly(x, x = ~semaine) |>
        plotly::add_lines(y = ~brute, name = "Toutes les zones", line = list(color = COUL$muet, width = 2, dash = "dot"),
                          hovertemplate = "<b>S%{x}</b><br>Toutes les zones : %{y:.0%}<extra></extra>") |>
        plotly::add_lines(y = ~corrigee, name = "Hors zones sans confirmation", line = list(color = COUL$serie1, width = 2.5),
                          hovertemplate = "<b>S%{x}</b><br>Hors zones sans confirmation : %{y:.0%}<extra></extra>") |>
        pl_style(app$cfg, legende = TRUE, titre_x = "") |>
        plotly::layout(legend = list(orientation = "h", x = 0, y = -0.12),
                       yaxis = list(tickformat = ".0%", range = c(0, 1), gridcolor = COUL$grille, zeroline = FALSE, fixedrange = TRUE, title = ""))
    })
    output$g_conf <- plotly::renderPlotly(pl_barres(serie_cible(ids_S()$donnees, "PALUDISME CONF", S()), "cas", "cas confirmés", app$cfg))

    output$g_chaleur <- plotly::renderPlotly({
      x <- palu_large(ids_S()$donnees) |>
        dplyr::mutate(p = dplyr::if_else(!is.na(susp) & susp > 0, 100 * pmin(dplyr::coalesce(conf, 0) / susp, 1), NA_real_)) |>
        dplyr::select(zs, semaine, p)
      ord <- ids_S()$ruptures |> dplyr::arrange(dplyr::coalesce(debut_rupture, 99L), zs) |> dplyr::pull(zs)
      ord <- c(ord, setdiff(app$zones, ord))
      x <- tidyr::complete(x, zs = app$zones, semaine = seq_len(S()))
      pl_carte_chaleur(x, "p", "Confirmés / suspects", app$cfg, zmax = 100, ordre = ord, ticksuffix = " %")
    })

    output$ruptures <- reactable::renderReactable({
      m <- app$expo |> dplyr::select(zs, mve)
      x <- ids_S()$ruptures |>
        dplyr::filter(!is.na(debut_rupture) | conf_egal_susp) |>
        dplyr::left_join(m, by = "zs") |>
        dplyr::arrange(debut_rupture) |>
        dplyr::transmute(Zone = nom_zone(zs), Depuis = ifelse(is.na(debut_rupture), "–", paste0("S", debut_rupture)),
                         `Base` = pct_fr(positivite_base, 0), `Conf. = susp.` = ifelse(conf_egal_susp, "Oui", "Non"), `Cas Ebola` = mve)
      col <- reactable::colDef(minWidth = 62)
      tableau(x, lignes = 30, columns = list(Depuis = col, Base = col, `Conf. = susp.` = col, `Cas Ebola` = col))
    })
  })
}
