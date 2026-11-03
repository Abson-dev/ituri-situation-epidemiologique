# Comparer une maladie sur plusieurs années, à période égale (S1 à la semaine de référence).

COUL_ANNEES <- c("#b7c4d4", "#7f95ad", "#475f7b", "#3a2f6b", "#1f6f78")

couleurs_annees <- function(annees, courante) {
  passees <- sort(setdiff(annees, courante))
  c(stats::setNames(utils::tail(COUL_ANNEES[seq_len(min(length(passees), 5))], length(passees)), passees),
    stats::setNames("#c40000", courante))
}

mod_annees_ui <- function(id, app) {
  ns <- NS(id)
  entete <- entete_page("Années", "Comparer une maladie sur plusieurs années, à période égale", "calendar-range", uiOutput(ns("semaine")))
  if (!length(app$historique)) {
    return(page(entete, div(class = "attention", bsicons::bs_icon("info-circle-fill"),
      div(strong("Les données des années passées ne sont pas encore chargées."),
          div(class = "detail", "Déposer les bases reçues dans data/raw/historique/, sous la forme Base_DPS_Ituri_2024.xlsx, puis relancer. Aucune donnée synthétique n'est utilisée avec les données réelles.")))))
  }
  page(
    entete,
    uiOutput(ns("filtres")),
    uiOutput(ns("kpis")),
    ligne(c(6, 6),
      carte_graphe(textOutput(ns("titre_canal"), inline = TRUE), plotly::plotlyOutput(ns("g_canal"), height = "100%"), 380),
      carte_tableau("Maladies, cumul de S1 à la semaine de référence", reactable::reactableOutput(ns("table")), 380,
                    note = "– : aucune ligne dans la base. Écart : année en cours par rapport à la moyenne des années passées, à partir de 10 cas. Comparaison brute, sensible au rapportage.")
    ),
    carte_graphe(textOutput(ns("titre_annees"), inline = TRUE), plotly::plotlyOutput(ns("g_annees"), height = "100%"), 340),
    carte_graphe(textOutput(ns("titre_zones"), inline = TRUE), plotly::plotlyOutput(ns("g_zones"), height = "100%"), 760)
  )
}

mod_annees_server <- function(id, app, S) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$semaine <- rendu_puce_semaine(S, app$cfg)
    if (!length(app$historique)) return(invisible(NULL))
    courante <- annee_courante(app$cfg)
    annees <- sort(unique(app$multi$annee))
    couleurs <- couleurs_annees(annees, courante)
    passees <- setdiff(annees, courante)

    output$filtres <- renderUI({
      layout_columns(
        col_widths = c(5, 4, 3), class = "filtres",
        selectInput(ns("maladie"), "Maladie", choix_maladies(app, toutes_annees = TRUE), selected = "PALUDISME SUSP"),
        selectInput(ns("zone"), "Territoire", c(list(Province = stats::setNames(PROVINCE, "Toute la province")), choix_zones(app))),
        radioButtons(ns("mesure"), "Mesure", c("Cas" = "cas", "Pour 100 000 hab." = "taux"), inline = TRUE)
      )
    })
    code <- reactive({ req(input$maladie); input$maladie })
    zone <- reactive(input$zone %||% PROVINCE)
    mesure <- reactive(input$mesure %||% "cas")
    lib <- reactive(libelle(code(), app$cfg))
    lieu <- reactive(if (zone() == PROVINCE) "province" else nom_zone(zone()))
    # Les semaines de l'année en cours postérieures à la référence ne sont pas encore connues.
    visibles <- reactive(dplyr::filter(app$multi, annee != courante | semaine <= S()))
    pops <- reactive({
      x <- dplyr::distinct(app$multi, annee, zs, pop)
      if (zone() != PROVINCE) x <- x[x$zs == zone(), ]
      dplyr::summarise(dplyr::group_by(x, annee), pop = sum(pop), .groups = "drop")
    })

    # Une semaine sans aucune ligne reste vide : elle n'est pas tracée comme un zéro.
    serie <- reactive({
      x <- dplyr::filter(visibles(), maladie == code())
      if (zone() != PROVINCE) x <- x[x$zs == zone(), ]
      x <- dplyr::summarise(dplyr::group_by(x, annee, semaine), cas = sum(cas), deces = sum(deces), .groups = "drop")
      tidyr::expand_grid(annee = annees, semaine = 1:52) |>
        dplyr::filter(annee != courante | semaine <= S()) |>
        dplyr::left_join(x, by = c("annee", "semaine")) |>
        dplyr::left_join(pops(), by = "annee") |>
        dplyr::mutate(taux = cas / pop * 1e5, valeur = if (mesure() == "taux") taux else cas)
    })
    nom_mesure <- reactive(if (mesure() == "taux") "pour 100 000 hab." else "cas")
    format_y <- reactive(if (mesure() == "taux") ".1f" else ",.0f")

    output$kpis <- renderUI({
      s <- S()
      x <- dplyr::filter(app$multi, maladie == code(), semaine <= s)
      r <- dplyr::filter(app$multi, semaine <= s)
      if (zone() != PROVINCE) {
        x <- x[x$zs == zone(), ]
        r <- r[r$zs == zone(), ]
      }
      n_attendu <- s * if (zone() == PROVINCE) length(app$zones) else 1L
      valeur_annee <- function(a) sum(x$cas[x$annee == a])
      # Une année sans aucune ligne ne compte pas comme un zéro dans la moyenne.
      avec_ligne <- intersect(passees, unique(x$annee))
      moyenne <- if (length(avec_ligne)) mean(vapply(avec_ligne, function(a) as.numeric(valeur_annee(a)), numeric(1))) else NA_real_
      cartes <- lapply(annees, function(a) {
        presentes <- x[x$annee == a, ]
        cumul <- valeur_annee(a)
        rapporte <- nrow(dplyr::distinct(r[r$annee == a, c("zs", "semaine")])) / n_attendu
        base <- sprintf("Rapportage : %s", pct_fr(rapporte, 0))
        detail <- if (a == courante && !is.na(moyenne) && moyenne >= 10) {
          e <- cumul / moyenne - 1
          tagList(span(class = if (e >= 0) "hausse" else "baisse", paste0(if (e >= 0) "▲ +" else "▼ −", pct_fr(abs(e), 0))),
                  sprintf(" par rapport à la moyenne %d-%d. %s", min(passees), max(passees), base))
        } else base
        kpi(paste0(a, ", S1 à S", s), if (nrow(presentes)) n_fr(cumul) else "Aucune ligne", detail, "calendar3", if (a == courante) "rouge" else "ardoise")
      })
      do.call(layout_column_wrap, c(list(width = "190px", fill = FALSE, heights_equal = "row"), cartes))
    })

    output$titre_canal <- renderText(sprintf("%s, %s : %d par rapport à %d-%d", lib(), lieu(), courante, min(passees), max(passees)))
    output$g_canal <- plotly::renderPlotly({
      x <- serie()
      p <- dplyr::filter(x, annee %in% passees) |>
        dplyr::group_by(semaine) |>
        dplyr::summarise(n = sum(!is.na(valeur)),
                         bas = if (n) min(valeur, na.rm = TRUE) else NA_real_, haut = if (n) max(valeur, na.rm = TRUE) else NA_real_,
                         moyenne = if (n) mean(valeur, na.rm = TRUE) else NA_real_, .groups = "drop")
      a <- dplyr::filter(x, annee == courante)
      fmt <- format_y()
      validate(need(any(!is.na(p$moyenne)) || any(!is.na(a$valeur)), "Aucune ligne pour cette maladie et ce territoire."))
      plotly::plot_ly(p, x = ~semaine) |>
        plotly::add_ribbons(ymin = ~bas, ymax = ~haut, name = sprintf("Plage %d-%d", min(passees), max(passees)),
                            fillcolor = "rgba(127,149,173,.28)", line = list(width = 0), hoverinfo = "skip") |>
        plotly::add_lines(y = ~moyenne, name = "Moyenne des années passées", line = list(color = "#475f7b", width = 1.5, dash = "dot"),
                          hovertemplate = paste0("<b>S%{x}</b><br>Moyenne : %{y:", fmt, "}<extra></extra>")) |>
        plotly::add_lines(data = a, x = ~semaine, y = ~valeur, name = as.character(courante), line = list(color = couleurs[[as.character(courante)]], width = 2.5),
                          hovertemplate = paste0("<b>S%{x}</b><br>", courante, " : %{y:", fmt, "}<extra></extra>")) |>
        pl_style(app$cfg, titre_y = nom_mesure(), titre_x = "Semaine épidémiologique", legende = TRUE, nom_declaration = paste("Déclaration", courante)) |>
        plotly::layout(legend = list(orientation = "h", x = 0, y = -0.22))
    })

    output$titre_annees <- renderText(sprintf("%s, %s : chaque année, semaine par semaine", lib(), lieu()))
    output$g_annees <- plotly::renderPlotly({
      x <- serie()
      validate(need(any(!is.na(x$valeur)), "Aucune ligne pour cette maladie et ce territoire."))
      fmt <- format_y()
      p <- plotly::plot_ly()
      for (a in annees) {
        v <- x[x$annee == a, ]
        p <- plotly::add_lines(p, data = v, x = ~semaine, y = ~valeur, name = as.character(a),
                               line = list(color = couleurs[[as.character(a)]], width = if (a == courante) 2.5 else 1.5),
                               hovertemplate = paste0("<b>", a, ", S%{x}</b><br>%{y:", fmt, "}<extra></extra>"))
      }
      pl_style(p, app$cfg, titre_y = nom_mesure(), titre_x = "Semaine épidémiologique", legende = TRUE, nom_declaration = paste("Déclaration", courante)) |>
        plotly::layout(legend = list(orientation = "h", x = 0, y = -0.26))
    })

    output$table <- reactable::renderReactable({
      s <- S()
      x <- dplyr::filter(app$multi, semaine <= s)
      if (zone() != PROVINCE) x <- x[x$zs == zone(), ]
      x <- x |>
        dplyr::filter(maladie %in% choix_maladies(app, toutes_annees = TRUE)) |>
        dplyr::group_by(maladie, annee) |>
        dplyr::summarise(cas = sum(cas), .groups = "drop") |>
        tidyr::pivot_wider(names_from = annee, values_from = cas)
      noms <- as.character(annees)
      for (n in setdiff(noms, names(x))) x[[n]] <- NA_real_
      m <- rowMeans(as.matrix(x[as.character(passees)]), na.rm = TRUE)
      cur <- x[[as.character(courante)]]
      ecart <- ifelse(!is.na(m) & m >= 10 & !is.na(cur), cur / m - 1, NA_real_)
      t <- x |>
        dplyr::mutate(ordre = match(maladie, choix_maladies(app, toutes_annees = TRUE)), Maladie = libelle(maladie, app$cfg), `Écart` = ecart) |>
        dplyr::arrange(ordre) |>
        dplyr::select(Maladie, dplyr::all_of(noms), `Écart`)
      cols <- list(Maladie = col_libelle(130), `Écart` = col_evolution())
      for (n in noms) cols[[n]] <- if (n == as.character(courante)) col_semaine_courante(66) else col_nombre(66)
      tableau(t, lignes = 30, columns = cols)
    })

    output$titre_zones <- renderText(sprintf("%s : cumul pour 100 000 hab. de S1 à S%d, par zone et par année (gris : aucune ligne)", lib(), S()))
    output$g_zones <- plotly::renderPlotly({
      x <- dplyr::filter(app$multi, maladie == code(), semaine <= S()) |>
        dplyr::group_by(annee, zs) |>
        dplyr::summarise(taux = sum(cas) / dplyr::first(pop) * 1e5, .groups = "drop")
      validate(need(nrow(x) > 0, "Aucune ligne pour cette maladie."))
      zones <- rev(sort(app$zones))
      grille <- tidyr::expand_grid(zs = zones, annee = annees) |> dplyr::left_join(x, by = c("zs", "annee"))
      m <- matrix(grille$taux, nrow = length(zones), byrow = TRUE)
      plotly::plot_ly(x = as.character(annees), y = nom_zone(zones), z = m, type = "heatmap", colorscale = SEQ_BLEU, xgap = 2, ygap = 1, zmin = 0,
                      colorbar = list(title = list(text = "Pour 100 000", side = "right"), thickness = 10, outlinewidth = 0, tickformat = ",.0f"),
                      hovertemplate = "<b>%{y}</b>, %{x}<br>Pour 100 000 : %{z:,.1f}<extra></extra>") |>
        plotly::layout(separators = ", ", font = list(family = POLICE, size = 11.5, color = COUL$encre2),
                       paper_bgcolor = COUL$surface, plot_bgcolor = COUL$grille, margin = list(l = 10, r = 12, t = 8, b = 10),
                       xaxis = list(title = "", type = "category", side = "top", fixedrange = TRUE),
                       yaxis = list(title = "", dtick = 1, fixedrange = TRUE, automargin = TRUE, tickfont = list(size = 10.5),
                                    categoryorder = "array", categoryarray = nom_zone(zones))) |>
        plotly::config(displayModeBar = FALSE, locale = "fr")
    })
  })
}
