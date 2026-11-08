# Indicateurs du rapport hebdomadaire et formatage.

n_fr <- function(x, d = 0) {
  ifelse(is.na(x), "–", formatC(round(x, d), format = "f", digits = d, big.mark = " ", decimal.mark = ","))
}
pct_fr <- function(x, d = 1) ifelse(is.na(x), "–", paste0(n_fr(100 * x, d), " %"))
libelle <- function(code, cfg, langue = "fr") {
  vapply(code, function(k) {
    v <- cfg$libelles[[k]]
    if (is.null(v)) k else v[[langue]]
  }, character(1), USE.NAMES = FALSE)
}
# Conversion sur les valeurs uniques : toTitleCase est lent, et la base compte plus de 10 000 lignes.
nom_zone <- function(z) {
  u <- unique(z)
  tools::toTitleCase(tolower(u))[match(z, u)]
}
enumerer <- function(x, vide = "aucune") {
  if (!length(x)) return(vide)
  if (length(x) == 1) return(x)
  paste0(paste(x[-length(x)], collapse = ", "), " et ", x[length(x)])
}

tableau_maladies <- function(d, codes, S, cfg, n_sem = 6) {
  semaines <- seq(max(1L, S - n_sem + 1L), S)
  hebdo <- d |>
    dplyr::filter(maladie %in% codes, semaine %in% semaines) |>
    dplyr::group_by(maladie, semaine) |>
    dplyr::summarise(cas = sum(cas), .groups = "drop") |>
    tidyr::complete(maladie = codes, semaine = semaines, fill = list(cas = 0)) |>
    dplyr::mutate(semaine = paste0("S", semaine)) |>
    tidyr::pivot_wider(names_from = semaine, values_from = cas)
  dc <- d |>
    dplyr::filter(maladie %in% codes, semaine %in% c(S - 1L, S)) |>
    dplyr::group_by(maladie) |>
    dplyr::summarise(dc_prec = sum(deces[semaine == S - 1L]), dc_S = sum(deces[semaine == S]), .groups = "drop")
  cumul <- d |>
    dplyr::filter(maladie %in% codes, semaine <= S) |>
    dplyr::group_by(maladie) |>
    dplyr::summarise(cumul = sum(cas), cumul_dc = sum(deces), .groups = "drop")
  hebdo |>
    dplyr::left_join(dc, by = "maladie") |>
    dplyr::left_join(cumul, by = "maladie") |>
    dplyr::mutate(dplyr::across(c(dc_prec, dc_S, cumul, cumul_dc), ~ dplyr::coalesce(.x, 0)),
                  ordre = match(maladie, codes)) |>
    dplyr::arrange(ordre) |>
    dplyr::select(-ordre)
}

# Cumuls de S1 à S, dans l'ordre de codes ; une maladie sans ligne compte zéro.
cumul_annee <- function(d, codes, S) {
  x <- d[d$maladie %in% codes & d$semaine <= S, ]
  cas <- tapply(x$cas, factor(x$maladie, levels = codes), sum)
  deces <- tapply(x$deces, factor(x$maladie, levels = codes), sum)
  tibble::tibble(maladie = codes, cas = unname(dplyr::coalesce(as.numeric(cas), 0)),
                 deces = unname(dplyr::coalesce(as.numeric(deces), 0)))
}

cumul_maladie <- function(d, code, S, zone = NULL) {
  x <- d[d$maladie == code & d$semaine <= S, ]
  if (!is.null(zone)) x <- x[x$zs == zone, ]
  list(cas = sum(x$cas), deces = sum(x$deces))
}

# Rougeole : la semaine S atteint au moins le double de la moyenne des semaines précédentes.
alertes_rougeole <- function(d, S, cfg) {
  s <- cfg$seuils
  fen <- seq(max(1L, S - s$rougeole_fenetre), S - 1L)
  d |>
    dplyr::filter(maladie == "ROUGEOLE", semaine %in% c(fen, S)) |>
    dplyr::group_by(zs) |>
    dplyr::summarise(cas_S = sum(cas[semaine == S]), moy = sum(cas[semaine %in% fen]) / length(fen), .groups = "drop") |>
    dplyr::filter(cas_S >= s$rougeole_min_cas, cas_S >= s$rougeole_facteur * moy)
}

alertes_meningite <- function(d, S, cfg) {
  s <- cfg$seuils
  d |>
    dplyr::filter(maladie == "MENINGITE", semaine == S) |>
    dplyr::group_by(zs, pop) |>
    dplyr::summarise(cas = sum(cas), .groups = "drop") |>
    dplyr::mutate(taux = cas / pop * 1e5,
                  statut = dplyr::case_when(taux >= s$meningite_epidemique ~ "épidémique",
                                            taux >= s$meningite_alerte ~ "alerte", TRUE ~ NA_character_)) |>
    dplyr::filter(!is.na(statut))
}

zones_recentes <- function(d, code, S, n = 4) {
  d |>
    dplyr::filter(maladie == code, semaine > S - n, semaine <= S, cas > 0) |>
    dplyr::group_by(zs) |>
    dplyr::summarise(cas = sum(cas), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(cas))
}
texte_zones <- function(z, vide = "aucune zone de santé") {
  if (!nrow(z)) return(vide)
  enumerer(paste0(nom_zone(z$zs), " (", z$cas, ")"))
}

# Proxy calculé pour toutes les zones : une zone-semaine compte si la zone a soumis une donnée.
rapportage_zones <- function(d, S) {
  d |>
    dplyr::filter(semaine <= S) |>
    dplyr::distinct(zs, semaine) |>
    dplyr::group_by(zs) |>
    dplyr::summarise(
      sem_rapportees = dplyr::n(),
      couverture = sem_rapportees / S,
      couverture_4s = sum(semaine > S - 4L) / min(4L, S),
      manquantes_4s = paste(setdiff(seq(max(1L, S - 3L), S), semaine), collapse = ", "),
      .groups = "drop"
    ) |>
    dplyr::arrange(couverture_4s, couverture, zs)
}

positivite_province <- function(ids, S) {
  positivite_zone(ids, S) |>
    dplyr::group_by(semaine) |>
    dplyr::summarise(
      brute = sum(conf, na.rm = TRUE) / sum(susp, na.rm = TRUE),
      corrigee = sum(conf[valide]) / sum(susp[valide]),
      zones_valides = sum(valide),
      .groups = "drop"
    )
}

# Tendance sur les zones ayant rapporté aux deux semaines : une zone absente de l'une d'elles ne la déforme pas.
evolution_palu <- function(d, S1, S2) {
  zones <- intersect(d$zs[d$semaine == S1], d$zs[d$semaine == S2])
  somme <- function(code, s) sum(d$cas[d$maladie == code & d$semaine == s & d$zs %in% zones], na.rm = TRUE)
  list(n_zones = length(zones),
       conf1 = somme("PALUDISME CONF", S1), conf2 = somme("PALUDISME CONF", S2),
       susp1 = somme("PALUDISME SUSP", S1), susp2 = somme("PALUDISME SUSP", S2))
}

# Sépare les zones sans cas récent : une semaine sans donnée ne permet pas de conclure à l'absence de cas.
zones_sans_cas <- function(d, code, zones, S, n = 4) {
  semaines <- seq(max(1L, S - n + 1L), S)
  sans <- setdiff(zones, zones_recentes(d, code, S, n)$zs)
  manquantes <- lapply(sans, function(z) setdiff(semaines, d$semaine[d$zs == z]))
  list(completes = sans[lengths(manquantes) == 0L],
       incompletes = stats::setNames(manquantes[lengths(manquantes) > 0L], sans[lengths(manquantes) > 0L]))
}

dernier_sitrep <- function(S, cfg) {
  fin <- date_semaine(S, cfg) + 6L + 7L
  p <- readr::read_csv(chemin_donnees("external/mve_provinces.csv"), show_col_types = FALSE)
  z <- readr::read_csv(chemin_donnees("external/mve_zones.csv"), show_col_types = FALSE)
  dates <- sort(unique(p$date_sitrep))
  ok <- dates[dates <= fin]
  if (!length(ok)) return(NULL)
  dd <- max(ok)
  list(date = dd, age_sem = as.numeric(fin - dd) %/% 7,
       provinces = p[p$date_sitrep == dd, ], zones = z[z$date_sitrep == dd, ])
}

ligne_journal <- function(ids, motif) {
  j <- ids$journal
  j$n[grepl(motif, j$regle, fixed = TRUE)][1]
}

# Rapprochement avec le tableau du rapport de référence (S32).
rapprocher_reference <- function(ids, cfg) {
  r <- readr::read_csv(chemin_donnees("external/rapport_s32_reference.csv"), show_col_types = FALSE)
  base <- ids$donnees |>
    dplyr::filter(semaine <= 32) |>
    dplyr::group_by(maladie) |>
    dplyr::summarise(cumul_base = sum(cas), deces_base = sum(deces), .groups = "drop")
  s32 <- ids$donnees |>
    dplyr::filter(semaine == 32) |>
    dplyr::group_by(maladie) |>
    dplyr::summarise(S32_base = sum(cas), .groups = "drop")
  r |>
    dplyr::select(maladie, S32, cumul_2026, deces_2026) |>
    dplyr::left_join(s32, by = "maladie") |>
    dplyr::left_join(base, by = "maladie") |>
    dplyr::mutate(dplyr::across(c(S32_base, cumul_base, deces_base), ~ dplyr::coalesce(.x, 0)),
                  ecart_pct = dplyr::if_else(cumul_2026 > 0, 100 * (cumul_base - cumul_2026) / cumul_2026, NA_real_))
}
