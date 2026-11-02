mod_apercu_ui <- function(id) {
  ns <- NS(id)
  page(
    uiOutput(ns("kpis")),
    ligne(c(4, 8),
      bslib::card(height = 350, card_header(bsicons::bs_icon("bell"), " Points d'attention"), card_body(uiOutput(ns("attention")))),
      carte_tableau("Maladies à potentiel épidémique", reactable::reactableOutput(ns("table")), 350)
    ),
    ligne(c(4, 4, 4),
      carte_graphe("Paludisme suspect : cas", plotly::plotlyOutput(ns("g_palu"), height = "100%"), 280),
      carte_graphe("Infections respiratoires aiguës : cas", plotly::plotlyOutput(ns("g_ira"), height = "100%"), 280),
      carte_graphe("Fièvre hémorragique aiguë : notifications IDS", plotly::plotlyOutput(ns("g_fha"), height = "100%"), 280)
    )
  )
}

mod_apercu_server <- function(id, app, S, ids_S) {
  moduleServer(id, function(input, output, session) {
    d <- reactive(ids_S()$donnees)

    output$kpis <- renderUI({
      s <- S()
      x <- d()
      n <- dplyr::n_distinct(x$zs[x$semaine == s])
      pal <- evolution_comparable(x, "PALUDISME SUSP", s)
      ira <- evolution_comparable(x, "IRA", s)
      fha <- x[x$maladie == "FHA", ]
      pos <- positivite_province(ids_S(), s)
      pos_s <- pos[pos$semaine == s, ]
      nb_rup <- sum(!is.na(ids_S()$ruptures$debut_rupture))
      layout_column_wrap(
        width = "190px", fill = FALSE, heights_equal = "row",
        kpi("Zones ayant rapporté", paste0(n, " / ", length(app$zones)), paste0("Semaine ", s), "broadcast"),
        kpi("Paludisme suspect", n_fr(pal$valeur), texte_evolution(pal, s), "droplet"),
        kpi("Infections respiratoires", n_fr(ira$valeur), texte_evolution(ira, s), "lungs"),
        kpi("FHA notifiées", n_fr(sum(fha$cas[fha$semaine == s])), paste0(n_fr(sum(fha$deces[fha$semaine == s])), " décès ; ", n_fr(sum(fha$cas)), " cas cumulés"), "shield-exclamation"),
        kpi("Confirmés / suspects", pct_fr(pos_s$corrigee), sprintf("Hors %d zones sans confirmation (brut : %s)", nb_rup, pct_fr(pos_s$brute)), "percent")
      )
    })

    output$attention <- renderUI({
      icones <- c(critique = "exclamation-octagon-fill", serieux = "exclamation-triangle-fill", attention = "info-circle-fill", ok = "check-circle-fill")
      lapply(points_attention(ids_S(), S(), app), function(p) {
        div(class = paste("attention", p$niveau),
            bsicons::bs_icon(icones[[p$niveau]]),
            div(strong(p$titre), div(class = "detail", p$detail)))
      })
    })

    output$table <- reactable::renderReactable({
      codes <- unlist(app$cfg$tableau_principal)
      t <- tableau_maladies(d(), codes, S(), app$cfg, n_sem = 4) |>
        dplyr::mutate(Maladie = libelle(maladie, app$cfg)) |>
        dplyr::select(Maladie, dplyr::matches("^S[0-9]+$"), !!paste0("Décès S", S()) := dc_S, `Cumul 2026` = cumul, `Décès 2026` = cumul_dc) |>
        dplyr::filter(`Cumul 2026` > 0)
      num <- setdiff(names(t), "Maladie")
      tableau(t, lignes = 30, columns = c(
        list(Maladie = reactable::colDef(minWidth = 170)),
        stats::setNames(lapply(num, function(.) reactable::colDef(format = reactable::colFormat(separators = TRUE, locales = "fr-FR"), minWidth = 62, align = "right")), num)
      ))
    })

    trace <- function(code, nom, couleur = COUL$serie1) {
      plotly::renderPlotly(pl_barres(serie_cible(d(), code, S()), "cas", nom, app$cfg, couleur))
    }
    output$g_palu <- trace("PALUDISME SUSP", "cas suspects")
    output$g_ira <- trace("IRA", "cas")
    output$g_fha <- trace("FHA", "cas notifiés", COUL$serie2)
  })
}
