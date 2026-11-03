mod_apercu_ui <- function(id) {
  ns <- NS(id)
  page(
    entete_page("Vue d'ensemble", "Situation de la province pour la semaine de référence", "speedometer2", uiOutput(ns("semaine"))),
    uiOutput(ns("kpis")),
    ligne(c(4, 8),
      bslib::card(height = 440, card_header(bsicons::bs_icon("bell"), " Points d'attention"), card_body(class = "corps-alertes", uiOutput(ns("attention")))),
      carte_tableau("Maladies à potentiel épidémique", reactable::reactableOutput(ns("table")), 440,
                    note = "Évol. : variation par rapport à la semaine précédente, sur les zones ayant rapporté les deux semaines et à partir de 10 cas.")
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
    output$semaine <- rendu_puce_semaine(S, app$cfg)

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
        width = "170px", fill = FALSE, heights_equal = "row",
        kpi("Zones ayant rapporté", paste0(n, " / ", length(app$zones)), paste0("Semaine ", s), "broadcast", "ardoise"),
        kpi("Paludisme suspect", n_fr(pal$valeur), evolution_tag(pal, s), "droplet", "bleu"),
        kpi("Infections respiratoires", n_fr(ira$valeur), evolution_tag(ira, s), "lungs", "vert"),
        kpi("FHA notifiées", n_fr(sum(fha$cas[fha$semaine == s])), paste0(n_fr(sum(fha$deces[fha$semaine == s])), " décès ; ", n_fr(sum(fha$cas)), " cas cumulés"), "shield-exclamation", "orange"),
        kpi("Confirmés / suspects", pct_fr(pos_s$corrigee), sprintf("Hors %d zones sans confirmation (brut : %s)", nb_rup, pct_fr(pos_s$brute)), "percent", "bleu")
      )
    })

    output$attention <- renderUI({
      icones <- c(critique = "exclamation-octagon-fill", serieux = "exclamation-triangle-fill", attention = "info-circle-fill", ok = "check-circle-fill")
      div(class = "pile-alertes", lapply(points_attention(ids_S(), S(), app), function(p) {
        div(class = paste("attention", p$niveau),
            bsicons::bs_icon(icones[[p$niveau]]),
            div(strong(p$titre), div(class = "detail", p$detail)))
      }))
    })

    output$table <- reactable::renderReactable({
      codes <- unlist(app$cfg$tableau_principal)
      s <- S()
      t <- tableau_maladies(d(), codes, s, app$cfg, n_sem = 4) |>
        dplyr::mutate(Maladie = libelle(maladie, app$cfg),
                      `Évol.` = vapply(maladie, function(m) {
                        e <- evolution_comparable(d(), m, s)
                        if (e$base >= 10) e$variation else NA_real_
                      }, numeric(1))) |>
        dplyr::select(Maladie, dplyr::matches("^S[0-9]+$"), `Évol.`, !!paste0("Décès S", s) := dc_S, `Cumul 2026` = cumul, `Décès 2026` = cumul_dc) |>
        dplyr::filter(`Cumul 2026` > 0)
      tableau(t, lignes = 30, columns = colonnes_maladies(t, s, largeur_libelle = 132, largeur_nombre = 50))
    })

    trace <- function(code, nom, couleur = COUL$serie1) {
      plotly::renderPlotly(pl_barres(serie_cible(d(), code, S()), "cas", nom, app$cfg, couleur))
    }
    output$g_palu <- trace("PALUDISME SUSP", "cas suspects", COUL$serie1)
    output$g_ira <- trace("IRA", "cas", COUL$serie3)
    output$g_fha <- trace("FHA", "cas notifiés", COUL$serie2)
  })
}
