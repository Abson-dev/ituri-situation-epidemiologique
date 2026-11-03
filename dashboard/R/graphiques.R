# Graphiques et tableaux : une mesure par graphique, jamais de double axe.

SEQ_BLEU <- list(c(0, "#cde2fb"), c(0.25, "#86b6ef"), c(0.5, "#2a78d6"), c(0.75, "#184f95"), c(1, "#0d366b"))
COUL_DECES <- "#a32424"
POLICE <- "Segoe UI, system-ui, sans-serif"

assombrir <- function(couleur, facteur = 0.62) {
  v <- grDevices::col2rgb(couleur)[, 1] * facteur
  grDevices::rgb(v[1], v[2], v[3], maxColorValue = 255)
}

pl_style <- function(p, cfg, titre_y = NULL, titre_x = "Semaine épidémiologique 2026", declaration = TRUE, legende = FALSE) {
  x_decl <- cfg$epidemie$semaine_rupture - 0.5
  titre_axe <- function(texte) list(text = texte %||% "", font = list(size = 11, color = COUL$muet))
  p |>
    plotly::layout(
      separators = ", ",
      font = list(family = POLICE, size = 11.5, color = COUL$encre2),
      paper_bgcolor = COUL$surface, plot_bgcolor = COUL$surface,
      margin = list(l = 10, r = 12, t = 22, b = 10),
      xaxis = list(title = titre_axe(titre_x), showgrid = FALSE, zeroline = FALSE, linecolor = COUL$axe, tick0 = 4, dtick = 4,
                   fixedrange = TRUE, automargin = TRUE),
      yaxis = list(title = titre_axe(titre_y), gridcolor = COUL$grille, griddash = "dot", zeroline = FALSE, rangemode = "tozero", fixedrange = TRUE,
                   automargin = TRUE, tickformat = ",.2~f"),
      hoverlabel = list(bgcolor = "#ffffff", bordercolor = COUL$axe, font = list(color = COUL$encre)),
      showlegend = legende,
      legend = list(orientation = "h", x = 0, y = -0.28),
      shapes = if (declaration) list(list(type = "line", x0 = x_decl, x1 = x_decl, yref = "paper", y0 = 0, y1 = 1,
                                          line = list(color = COUL$encre2, width = 1, dash = "dot"))),
      annotations = if (declaration) list(list(x = x_decl, y = 1, yref = "paper", text = "Déclaration", showarrow = FALSE,
                                               xanchor = "center", yanchor = "bottom", font = list(size = 10, color = COUL$muet)))
    ) |>
    plotly::config(displayModeBar = FALSE, locale = "fr")
}

pl_barres <- function(df, y, nom, cfg, couleur = COUL$serie1, titre_y = NULL) {
  df$valeur <- df[[y]]
  # Le nombre de zones n'est utile qu'à l'échelle de la province.
  df$note <- if ("zones" %in% names(df)) paste0("<br>", df$zones, " zones rapportées") else ""
  # Semaine de référence plus foncée : c'est la barre que l'on lit en premier.
  teintes <- ifelse(df$semaine == max(df$semaine), assombrir(couleur), couleur)
  plotly::plot_ly(df, x = ~semaine, y = ~valeur, type = "bar", marker = list(color = teintes), customdata = ~note,
                  hovertemplate = paste0("<b>S%{x}</b><br>%{y} ", nom, "%{customdata}<extra></extra>")) |>
    pl_style(cfg, titre_y = titre_y) |>
    plotly::layout(bargap = 0.25)
}

pl_barres_h <- function(df, x, y, nom, couleur = COUL$serie1) {
  df <- df[order(df[[x]]), ]
  df$yy <- factor(df[[y]], levels = df[[y]])
  df$xx <- df[[x]]
  plotly::plot_ly(df, x = ~xx, y = ~yy, type = "bar", orientation = "h", marker = list(color = couleur),
                  hovertemplate = paste0("<b>%{y}</b><br>%{x} ", nom, "<extra></extra>")) |>
    plotly::layout(
      separators = ", ", font = list(family = POLICE, size = 11.5, color = COUL$encre2),
      margin = list(l = 10, r = 12, t = 8, b = 10), bargap = 0.25, showlegend = FALSE,
      xaxis = list(title = list(text = nom, font = list(size = 11, color = COUL$muet)), gridcolor = COUL$grille, griddash = "dot", zeroline = FALSE, fixedrange = TRUE, rangemode = "tozero", tickformat = ",.2~f"),
      yaxis = list(title = "", fixedrange = TRUE, automargin = TRUE, dtick = 1),
      hoverlabel = list(bgcolor = "#ffffff", bordercolor = COUL$axe, font = list(color = COUL$encre))
    ) |>
    plotly::config(displayModeBar = FALSE, locale = "fr")
}

pl_carte_chaleur <- function(df, valeur, nom, cfg, format_z = ",.0f", zmax = NULL, ordre = NULL, ticksuffix = "", echelle = SEQ_BLEU) {
  zones <- if (is.null(ordre)) sort(unique(df$zs), decreasing = TRUE) else rev(ordre)
  semaines <- sort(unique(df$semaine))
  grille <- tidyr::expand_grid(zs = zones, semaine = semaines) |> dplyr::left_join(df, by = c("zs", "semaine"))
  m <- matrix(grille[[valeur]], nrow = length(zones), byrow = TRUE)
  plotly::plot_ly(x = semaines, y = nom_zone(zones), z = m, type = "heatmap", colorscale = echelle, xgap = 1, ygap = 1,
                  zmin = 0, zmax = zmax,
                  colorbar = list(title = list(text = nom, side = "right"), thickness = 10, outlinewidth = 0, ticksuffix = ticksuffix),
                  hovertemplate = paste0("<b>%{y}</b>, S%{x}<br>", nom, " : %{z:", format_z, "}<extra></extra>")) |>
    pl_style(cfg) |>
    plotly::layout(plot_bgcolor = COUL$grille, xaxis = list(tick0 = 4, dtick = 4, showgrid = FALSE, fixedrange = TRUE),
                   yaxis = list(title = "", dtick = 1, fixedrange = TRUE, automargin = TRUE, gridcolor = "rgba(0,0,0,0)", tickfont = list(size = 10.5),
                                tickformat = "", categoryorder = "array", categoryarray = nom_zone(zones)))
}

# Ratio de ratios avec IC : l'échelle log rend symétriques une baisse et une hausse équivalentes.
pl_foret <- function(df) {
  df$y <- factor(df$issue, levels = rev(df$issue))
  plotly::plot_ly(df, x = ~rrr, y = ~y, type = "scatter", mode = "markers",
                  marker = list(color = COUL$serie1, size = 10),
                  error_x = list(type = "data", symmetric = FALSE, array = ~haut - rrr, arrayminus = ~rrr - bas, color = COUL$serie1, thickness = 1.6, width = 0),
                  customdata = ~paste0(sprintf("%.2f", bas), " – ", sprintf("%.2f", haut)),
                  hovertemplate = "<b>%{y}</b><br>Ratio : %{x:.2f}<br>IC 95 % : %{customdata}<extra></extra>") |>
    plotly::layout(
      separators = ", ", font = list(family = POLICE, size = 11.5, color = COUL$encre2),
      margin = list(l = 10, r = 16, t = 10, b = 10), showlegend = FALSE,
      xaxis = list(title = list(text = "Ratio de ratios de taux (échelle log)", font = list(size = 11, color = COUL$muet)), type = "log", tickvals = c(0.4, 0.6, 0.8, 1, 1.25), gridcolor = COUL$grille, griddash = "dot", zeroline = FALSE, fixedrange = TRUE),
      yaxis = list(title = "", fixedrange = TRUE, automargin = TRUE),
      shapes = list(list(type = "line", x0 = 1, x1 = 1, yref = "paper", y0 = 0, y1 = 1, line = list(color = COUL$encre2, width = 1))),
      hoverlabel = list(bgcolor = "#ffffff", bordercolor = COUL$axe, font = list(color = COUL$encre))
    ) |>
    plotly::config(displayModeBar = FALSE, locale = "fr")
}

# Tableaux -------------------------------------------------------------------------------------

tableau <- function(df, ..., hauteur = NULL, recherche = FALSE, lignes = 15) {
  reactable::reactable(
    df, ..., compact = TRUE, highlight = TRUE, striped = TRUE, searchable = recherche, height = if (is.null(hauteur)) "auto" else hauteur,
    pagination = nrow(df) > lignes, defaultPageSize = lignes, wrap = TRUE,
    defaultColDef = reactable::colDef(headerClass = "tete-tableau", vAlign = "center", headerVAlign = "center"),
    language = reactable::reactableLang(
      searchPlaceholder = "Rechercher…", noData = "Aucune donnée", pageInfo = "{rowStart}–{rowEnd} sur {rows}",
      pagePrevious = "Précédent", pageNext = "Suivant", pageSizeOptions = "{rows} lignes")
  )
}

format_nombre <- function() reactable::colFormat(separators = TRUE, locales = "fr-FR")

col_libelle <- function(largeur = 150, ...) reactable::colDef(minWidth = largeur, class = "cell-libelle", ...)

col_nombre <- function(largeur = 62, ...) {
  reactable::colDef(format = format_nombre(), na = "–", minWidth = largeur, align = "right", ...)
}

col_semaine_courante <- function(largeur = 62) {
  reactable::colDef(format = format_nombre(), na = "–", minWidth = largeur, align = "right", class = "cell-courante",
                    headerClass = "tete-tableau tete-courante")
}

# Un décès est toujours visible, y compris dans un long tableau.
col_deces <- function(largeur = 62, ...) {
  col_nombre(largeur, style = function(v) if (!is.na(v) && v > 0) list(color = COUL_DECES, fontWeight = 650), ...)
}

col_evolution <- function(largeur = 68) {
  reactable::colDef(
    minWidth = largeur, align = "right", html = FALSE,
    cell = function(v) {
      if (is.na(v)) return(shiny::span(class = "muet", "–"))
      shiny::span(class = if (v >= 0) "hausse" else "baisse", paste0(if (v >= 0) "▲ +" else "▼ −", pct_fr(abs(v), 0)))
    }
  )
}

pastille <- function(texte, niveau) shiny::span(class = paste("pastille", niveau), texte)

barre_cellule <- function(texte, fraction, ton = "bleu") {
  largeur <- if (is.na(fraction)) 0 else round(100 * min(max(fraction, 0), 1), 1)
  shiny::div(class = "barre-cellule",
             shiny::span(class = "barre-valeur", texte),
             shiny::div(class = "barre-fond", shiny::div(class = paste0("barre-plein ton-", ton), style = paste0("width:", largeur, "%"))))
}

# Colonnes communes aux tableaux par maladie (province et zone) : S-n, évolution, décès, cumuls.
colonnes_maladies <- function(x, s, largeur_libelle = 170, largeur_nombre = 58) {
  semaines <- grep("^S[0-9]+$", names(x), value = TRUE)
  courante <- paste0("S", s)
  cols <- list(Maladie = col_libelle(largeur_libelle))
  for (n in semaines) cols[[n]] <- if (n == courante) col_semaine_courante(largeur_nombre) else col_nombre(largeur_nombre)
  if ("Évol." %in% names(x)) cols[["Évol."]] <- col_evolution()
  cols[[paste0("Décès S", s)]] <- col_deces(largeur_nombre)
  cols[["Cumul 2026"]] <- col_nombre(76, class = "cell-cumul")
  cols[["Décès 2026"]] <- col_deces(largeur_nombre)
  cols
}

# Cartes et mise en page ---------------------------------------------------------------------

carte_graphe <- function(titre, sortie, hauteur = 300) {
  bslib::card(full_screen = TRUE, height = hauteur, bslib::card_header(titre), bslib::card_body(padding = 6, sortie))
}

carte_tableau <- function(titre, sortie, hauteur = 300, note = NULL) {
  bslib::card(full_screen = TRUE, height = hauteur, bslib::card_header(titre), bslib::card_body(padding = 0, sortie),
              if (!is.null(note)) bslib::card_footer(class = "pied-tableau", note))
}

page <- function(...) shiny::div(class = "page", ...)
ligne <- function(largeurs, ...) bslib::layout_columns(col_widths = largeurs, fill = FALSE, ...)
pile <- function(...) shiny::div(class = "pile", ...)
