mod_ebola_ui <- function(id) {
  ns <- NS(id)
  page(
    entete_page("Ebola", "Cas confirmés d'après les sitreps, et concordance avec la surveillance IDS", "shield-exclamation", uiOutput(ns("semaine"))),
    uiOutput(ns("message")),
    uiOutput(ns("kpis")),
    ligne(c(4, 3, 5),
      carte_graphe("Cas confirmés par zone", plotly::plotlyOutput(ns("g_zones"), height = "100%"), 620),
      carte_graphe("Sitrep (cas confirmés) et IDS (FHA) par zone", plotly::plotlyOutput(ns("g_concord"), height = "100%"), 620),
      carte_tableau("Provinces", reactable::reactableOutput(ns("provinces")), 620)
    )
  )
}

mod_ebola_server <- function(id, app, S, ids_S) {
  moduleServer(id, function(input, output, session) {
    sit <- reactive(dernier_sitrep(S(), app$cfg))
    output$semaine <- rendu_puce_semaine(S, app$cfg)

    output$message <- renderUI({
      s <- sit()
      if (is.null(s)) return(div(class = "attention critique", bsicons::bs_icon("exclamation-octagon-fill"), div(strong("Aucun sitrep disponible pour cette période."))))
      niv <- if (s$age_sem >= 2) "attention" else "ok"
      div(class = paste("attention", niv), bsicons::bs_icon(if (s$age_sem >= 2) "info-circle-fill" else "check-circle-fill"),
          div(strong(sprintf("Sitrep %s du %s", s$provinces$sitrep[1], format(s$date, "%d/%m/%Y"))),
              div(class = "detail", paste0(if (s$age_sem >= 2) sprintf("Ancien de %d semaines par rapport à S%d : ajouter le dernier sitrep dans data/external/. ", s$age_sem, S()),
                                           "Chiffres transcrits manuellement, à vérifier dans le document original."))))
    })

    output$kpis <- renderUI({
      s <- sit()
      req(s)
      p <- s$provinces
      it <- p[p$province == "Ituri", ]
      layout_column_wrap(
        width = "190px", fill = FALSE,
        kpi("Cas confirmés, RDC", n_fr(sum(p$cas_confirmes)), paste0(nrow(p), " provinces"), "clipboard2-pulse", "orange"),
        kpi("Décès confirmés, RDC", n_fr(sum(p$deces_confirmes)), paste0("Létalité : ", pct_fr(sum(p$deces_confirmes) / sum(p$cas_confirmes))), "heart-pulse", "rouge"),
        kpi("Cas confirmés, Ituri", n_fr(it$cas_confirmes), paste0("Létalité : ", pct_fr(it$deces_confirmes / it$cas_confirmes)), "geo-alt", "orange"),
        kpi("Zones touchées, Ituri", paste0(it$zs_touchees, " / ", it$zs_total), pct_fr(it$zs_touchees / it$zs_total), "pin-map", "ardoise")
      )
    })

    output$g_zones <- plotly::renderPlotly({
      s <- sit()
      req(s)
      x <- s$zones |> dplyr::filter(zs != "A_VENTILER", cas > 0) |> dplyr::mutate(zone = nom_zone(zs))
      pl_barres_h(x, "cas", "zone", "cas confirmés", COUL$serie2) |> plotly::layout(yaxis = list(tickfont = list(size = 10)))
    })

    output$g_concord <- plotly::renderPlotly({
      x <- app$expo |> dplyr::mutate(zone = nom_zone(zs))
      plotly::plot_ly(x, x = ~mve + 1, y = ~fha + 1, type = "scatter", mode = "markers", text = ~zone,
                      customdata = ~paste0(mve, " cas confirmés ; ", fha, " FHA"),
                      marker = list(color = COUL$serie2, size = 9, opacity = 0.8, line = list(color = "#ffffff", width = 1)),
                      hovertemplate = "<b>%{text}</b><br>%{customdata}<extra></extra>") |>
        plotly::layout(
          font = list(family = POLICE, size = 11.5, color = COUL$encre2), showlegend = FALSE,
          margin = list(l = 10, r = 12, t = 10, b = 10),
          xaxis = list(title = list(text = "Cas confirmés + 1 (log)", font = list(size = 11, color = COUL$muet)), type = "log", tickvals = c(1, 10, 100, 1000), gridcolor = COUL$grille, griddash = "dot", zeroline = FALSE, fixedrange = TRUE),
          yaxis = list(title = list(text = "FHA IDS + 1 (log)", font = list(size = 11, color = COUL$muet)), type = "log", tickvals = c(1, 3, 10, 30, 100, 300, 1000), gridcolor = COUL$grille, griddash = "dot", zeroline = FALSE, fixedrange = TRUE),
          hoverlabel = list(bgcolor = "#ffffff", bordercolor = COUL$axe, font = list(color = COUL$encre))
        ) |>
        plotly::config(displayModeBar = FALSE, locale = "fr")
    })

    output$provinces <- reactable::renderReactable({
      s <- sit()
      req(s)
      x <- s$provinces |>
        dplyr::arrange(dplyr::desc(cas_confirmes)) |>
        dplyr::transmute(Province = province, Cas = cas_confirmes, `Décès` = deces_confirmes, `Létalité` = deces_confirmes / cas_confirmes,
                         `ZS touchées` = paste0(zs_touchees, " / ", zs_total), `24 h` = nouveaux_24h)
      cas_total <- sum(x$Cas)
      deces_total <- sum(x$`Décès`)
      cas_max <- max(x$Cas)
      tableau(x, columns = list(
        Province = col_libelle(100, footer = "Total"),
        Cas = reactable::colDef(minWidth = 96, footer = n_fr(cas_total), footerStyle = list(textAlign = "right"),
                                cell = function(v) barre_cellule(n_fr(v), v / cas_max, "orange")),
        `Décès` = col_deces(58, footer = n_fr(deces_total), footerStyle = list(textAlign = "right")),
        `Létalité` = reactable::colDef(minWidth = 64, align = "right", cell = function(v) pct_fr(v),
                                       footer = pct_fr(deces_total / cas_total), footerStyle = list(textAlign = "right")),
        `ZS touchées` = reactable::colDef(minWidth = 74, align = "center"),
        `24 h` = col_nombre(50, footer = n_fr(sum(x$`24 h`, na.rm = TRUE)), footerStyle = list(textAlign = "right"))
      ))
    })
  })
}
