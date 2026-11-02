mod_zones_ui <- function(id) {
  ns <- NS(id)
  page(
    uiOutput(ns("filtre")),
    uiOutput(ns("kpis")),
    ligne(c(7, 5),
      carte_tableau(textOutput(ns("titre"), inline = TRUE), reactable::reactableOutput(ns("table")), 556),
      pile(
        carte_graphe("Paludisme suspect : cas", plotly::plotlyOutput(ns("g_palu"), height = "100%"), 270),
        carte_graphe("Fièvre typhoïde : cas", plotly::plotlyOutput(ns("g_typh"), height = "100%"), 270)
      )
    )
  )
}

mod_zones_server <- function(id, app, S, ids_S) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$filtre <- renderUI(div(class = "filtres", selectInput(ns("zone"), "Zone de santé", choix_zones(app), width = "340px")))
    z <- reactive({ req(input$zone); input$zone })
    dz <- reactive(dplyr::filter(ids_S()$donnees, zs == z()))

    output$titre <- renderText(paste0("Situation à ", nom_zone(z()), ", 6 dernières semaines"))

    output$kpis <- renderUI({
      s <- S()
      pop <- app$ids$pop$pop_ref[app$ids$pop$zs == z()]
      r <- rapportage_zones(ids_S()$donnees, s)
      r <- r[r$zs == z(), ]
      cs <- cumul_maladie(dz(), "PALUDISME SUSP", s)
      cc <- cumul_maladie(dz(), "PALUDISME CONF", s)
      mve <- app$expo$mve[app$expo$zs == z()]
      rup <- ids_S()$ruptures[ids_S()$ruptures$zs == z(), ]
      msg <- if (nrow(rup) && !is.na(rup$debut_rupture)) paste0("Confirmation interrompue depuis S", rup$debut_rupture) else "Cumul depuis S1"
      layout_column_wrap(
        width = "190px", fill = FALSE,
        kpi("Population de référence", n_fr(pop), "Dénominateur à valider", "people"),
        kpi("Semaines rapportées", paste0(r$sem_rapportees, " / ", s), paste0(pct_fr(r$couverture_4s, 0), " sur 4 semaines"), "broadcast"),
        kpi("Paludisme : confirmés / suspects", pct_fr(if (cs$cas > 0) cc$cas / cs$cas else NA), msg, "droplet"),
        kpi("Cas confirmés d'Ebola", n_fr(mve), paste0("Sitrep ", app$cfg$epidemie$sitrep_exposition), "shield-exclamation")
      )
    })

    output$table <- reactable::renderReactable({
      codes <- c(unlist(app$cfg$tableau_principal), unlist(app$cfg$tableau_autres))
      s <- S()
      t <- tableau_maladies(dz(), codes, s, app$cfg) |> dplyr::filter(cumul > 0)
      sc <- grep("^S[0-9]+$", names(t), value = TRUE)
      # Une ligne absente n'est pas un zéro confirmé.
      pres <- dz() |> dplyr::filter(semaine %in% as.integer(sub("S", "", sc))) |> dplyr::mutate(col = paste0("S", semaine)) |>
        dplyr::select(maladie, col, cas) |> tidyr::pivot_wider(names_from = col, values_from = cas)
      x <- dplyr::select(t, maladie, !!paste0("Décès S", s) := dc_S, `Cumul 2026` = cumul, `Décès 2026` = cumul_dc) |> dplyr::left_join(pres, by = "maladie")
      for (cl in setdiff(sc, names(x))) x[[cl]] <- NA_real_
      x <- x |> dplyr::mutate(Maladie = libelle(maladie, app$cfg)) |>
        dplyr::select(Maladie, dplyr::all_of(sc), dplyr::all_of(paste0("Décès S", s)), `Cumul 2026`, `Décès 2026`)
      num <- setdiff(names(x), "Maladie")
      tableau(x, lignes = 30, columns = c(
        list(Maladie = reactable::colDef(minWidth = 170)),
        stats::setNames(lapply(num, function(.) reactable::colDef(format = reactable::colFormat(separators = TRUE, locales = "fr-FR"), na = "–", minWidth = 58, align = "right")), num)
      ))
    })
    output$g_palu <- plotly::renderPlotly(pl_barres(serie_province(dz(), "PALUDISME SUSP", S()), "cas", "cas suspects", app$cfg))
    output$g_typh <- plotly::renderPlotly(pl_barres(serie_province(dz(), "FIEVRE TYPHOIDE", S()), "cas", "cas", app$cfg))
  })
}
