# Figures partagées. Jamais de double axe : une mesure par panneau, à semaines alignées.

COUL <- list(serie1 = "#2a78d6", serie2 = "#eb6834", serie3 = "#1baf7a",
             encre = "#0b0b0b", encre2 = "#52514e", muet = "#898781",
             grille = "#e1e0d9", axe = "#c3c2b7", surface = "#ffffff", neutre = "#f0efec")

theme_rapport <- function(base = 9) {
  ggplot2::theme_minimal(base_size = base, base_family = "sans") +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = COUL$surface, colour = NA),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = COUL$grille, linewidth = 0.3),
      axis.line.x = ggplot2::element_line(colour = COUL$axe, linewidth = 0.4),
      axis.text = ggplot2::element_text(colour = COUL$muet),
      axis.title = ggplot2::element_text(colour = COUL$encre2),
      strip.text = ggplot2::element_text(colour = COUL$encre, hjust = 0, face = "bold"),
      plot.title = ggplot2::element_text(colour = COUL$encre, face = "bold", size = base + 1),
      plot.subtitle = ggplot2::element_text(colour = COUL$encre2),
      legend.position = "top", legend.justification = "left",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(colour = COUL$encre2),
      plot.title.position = "plot"
    )
}

ech_y <- function(...) ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = " "),
                                                   expand = ggplot2::expansion(mult = c(0, 0.08)), ...)
ech_sem <- function(S) ggplot2::scale_x_continuous(breaks = seq(0, 52, by = if (S > 26) 4 else 2),
                                                   limits = c(0.4, S + 0.6), expand = c(0, 0))

ligne_declaration <- function(cfg) {
  ggplot2::geom_vline(xintercept = cfg$epidemie$semaine_rupture - 0.5, colour = COUL$encre2,
                      linewidth = 0.35, linetype = "22")
}

barres_semaine <- function(df, y, S, titre, cfg, coul = COUL$serie1, axe_x = FALSE, entier = FALSE) {
  g <- ggplot2::ggplot(df, ggplot2::aes(semaine, {{ y }})) +
    ggplot2::geom_col(fill = coul, width = 0.72) +
    ligne_declaration(cfg) +
    ech_sem(S) +
    ggplot2::labs(title = titre, x = if (axe_x) "Semaine épidémiologique 2026" else NULL, y = NULL) +
    theme_rapport()
  g <- g + if (entier) {
    ggplot2::scale_y_continuous(breaks = scales::breaks_pretty(n = 3), labels = function(x) ifelse(x %% 1 == 0, x, ""),
                                expand = ggplot2::expansion(mult = c(0, 0.08)))
  } else ech_y()
  if (!axe_x) g <- g + ggplot2::theme(axis.text.x = ggplot2::element_blank())
  g
}

serie_province <- function(d, code, S) {
  d |>
    dplyr::filter(maladie == code, semaine <= S) |>
    dplyr::group_by(semaine) |>
    dplyr::summarise(cas = sum(cas), deces = sum(deces), .groups = "drop") |>
    tidyr::complete(semaine = seq_len(S), fill = list(cas = 0, deces = 0))
}

fig_rougeole <- function(d, S, cfg) {
  x <- serie_province(d, "ROUGEOLE", S)
  barres_semaine(x, cas, S, "Cas suspects de rougeole par semaine", cfg, entier = TRUE, axe_x = TRUE)
}

fig_paludisme <- function(ids, S, cfg) {
  d <- ids$donnees
  x <- serie_province(d, "PALUDISME CONF", S)
  pos <- positivite_province(ids, S) |>
    tidyr::pivot_longer(c(brute, corrigee), names_to = "serie", values_to = "pos") |>
    dplyr::mutate(serie = dplyr::recode(serie, brute = "Toutes les zones",
                                        corrigee = "Hors zones en rupture de confirmation"))
  g1 <- barres_semaine(x, cas, S, "Paludisme confirmé : cas par semaine", cfg)
  g2 <- barres_semaine(x, deces, S, "Décès notifiés parmi les cas confirmés", cfg, coul = COUL$serie2, entier = TRUE)
  g3 <- ggplot2::ggplot(pos, ggplot2::aes(semaine, pos, colour = serie, linetype = serie)) +
    ggplot2::geom_line(linewidth = 0.7) +
    ligne_declaration(cfg) +
    ggplot2::scale_colour_manual(values = c("Toutes les zones" = COUL$muet,
                                            "Hors zones en rupture de confirmation" = COUL$serie1)) +
    ggplot2::scale_linetype_manual(values = c("Toutes les zones" = "22",
                                              "Hors zones en rupture de confirmation" = "solid")) +
    ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1), limits = c(0, 1),
                                expand = ggplot2::expansion(mult = c(0, 0.02))) +
    ech_sem(S) +
    ggplot2::labs(title = "Positivité (confirmés / suspects)", x = "Semaine épidémiologique 2026", y = NULL) +
    theme_rapport()
  patchwork::wrap_plots(g1, g2, g3, ncol = 1, heights = c(1, 0.65, 1))
}

fig_fha <- function(d, S, cfg) {
  x <- serie_province(d, "FHA", S)
  g1 <- barres_semaine(x, cas, S, "Fièvre hémorragique aiguë : cas notifiés (IDS)", cfg)
  g2 <- barres_semaine(x, deces, S, "Décès notifiés", cfg, coul = COUL$serie2, axe_x = TRUE)
  patchwork::wrap_plots(g1, g2, ncol = 1, heights = c(1, 0.65))
}
