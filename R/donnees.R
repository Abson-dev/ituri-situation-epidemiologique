# Lecture, validation et nettoyage de la base IDS. Chaque règle alimente un journal
# auditable : la base brute n'est jamais modifiée.

# Mode démonstration : données synthétiques et moins de permutations, sans toucher aux vraies données.
mode_demo <- function() identical(Sys.getenv("ITURI_DEMO"), "1")
chemin_donnees <- function(...) here::here(if (mode_demo()) "data/synthetic" else "data", ...)

lire_config <- function(chemin = here::here("config.yml")) {
  cfg <- yaml::read_yaml(chemin)
  local <- here::here("config.local.yml")
  if (!mode_demo() && file.exists(local)) cfg <- utils::modifyList(cfg, yaml::read_yaml(local))
  if (mode_demo()) cfg$analyse$n_permutations <- cfg$analyse$n_permutations_demo
  cfg
}

# Année et semaine lues dans le nom d'un classeur, NA si absentes :
#   Base_DPS_Ituri_Week43.xlsx : NA, 43     IDSR 2025_WEEK52.xls : 2025, 52
#   IDSR Week44.xlsx           : NA, 44     Base_DPS_Ituri_2024.xlsx : 2024, NA
lire_nom_base <- function(noms) {
  noms <- noms[grepl("\\.xlsx?$", noms, ignore.case = TRUE) & !startsWith(noms, "~$")]
  motif_sem <- "^.*week[ _-]?([0-9]{1,2})\\.xlsx?$"
  semaine <- ifelse(grepl(motif_sem, noms, ignore.case = TRUE), sub(motif_sem, "\\1", noms, ignore.case = TRUE), NA)
  m <- regmatches(noms, regexpr("(?<![0-9])20[0-9]{2}(?![0-9])", noms, perl = TRUE), invert = NA)
  annee <- vapply(m, function(x) if (length(x) == 3L) x[2] else NA_character_, character(1))
  tibble::tibble(fichier = noms, annee = as.integer(annee), semaine = as.integer(semaine))
}

bases_dossier <- function(dossier) {
  x <- lire_nom_base(list.files(dossier))
  x$chemin <- file.path(dossier, x$fichier)
  x
}

# Base hebdomadaire : un nom avec une semaine, sans année ou avec l'année en cours. Une base d'une année
# passée (IDSR 2025_WEEK52.xls) ne peut jamais passer pour la semaine la plus récente.
trouver_base <- function(cfg, semaine = NULL, dossier = chemin_donnees("raw")) {
  x <- bases_dossier(dossier)
  x <- x[!is.na(x$semaine) & (is.na(x$annee) | x$annee == annee_courante(cfg)), ]
  if (!nrow(x)) stop("Aucune base de l'année en cours trouvée dans ", dossier, " (nom attendu : IDSR Week44.xlsx)")
  n <- x$semaine
  # La base la plus récente couvrant la semaine demandée.
  i <- if (is.null(semaine)) which.max(n) else {
    ok <- which(n >= semaine)
    if (!length(ok)) stop("Aucune base ne couvre la semaine ", semaine)
    ok[which.max(n[ok])]
  }
  x$chemin[i]
}

# Certains .xls reçus sont en réalité des .xlsx : le format est lu dans le fichier, pas dans son nom.
lire_classeur <- function(chemin) {
  format <- readxl::format_from_signature(chemin)
  if (is.na(format)) stop("Fichier illisible comme classeur Excel : ", basename(chemin), call. = FALSE)
  if (format == "xlsx") readxl::read_xlsx(chemin, guess_max = 20000) else readxl::read_xls(chemin, guess_max = 20000)
}

COL_CAS <- c("c328tnn", "c011mois", "c1259mois", "c515ans", "cp15ans")
COL_DC <- c("dtnn", "d011mois", "d1259mois", "d515ans", "dp15ans")
COL_ATTENDUES <- c("num", "zs", "pop", "numsem", "debutsem", "maladie", COL_CAS, COL_DC, "totalcas", "totaldeces")

date_semaine <- function(s, cfg) as.Date(cfg$donnees$origine_semaine1) + 7L * (s - 1L)

appliquer_corrections <- function(brut, chemin) {
  if (!file.exists(chemin)) return(list(brut = brut, n = 0L))
  cor <- readr::read_csv(chemin, show_col_types = FALSE)
  for (i in seq_len(nrow(cor))) {
    ligne <- which(brut$zs == cor$zs[i] & brut$numsem == cor$semaine[i] & brut$maladie == cor$maladie[i])
    if (!length(ligne)) stop("Correction sans ligne correspondante : ", paste(cor[i, 1:3], collapse = "/"))
    brut[ligne, cor$champ[i]] <- cor$valeur[i]
  }
  list(brut = brut, n = nrow(cor))
}

# regle_doublons : "max" (par défaut), "somme" ou "retirer" (sensibilité).
nettoyer_ids <- function(chemin, cfg, regle_doublons = cfg$nettoyage$regle_doublons,
                         chemin_corrections = chemin_donnees("corrections_manuelles.csv")) {
  brut <- lire_classeur(chemin)
  manquantes <- setdiff(COL_ATTENDUES, names(brut))
  if (length(manquantes)) stop("Colonnes absentes : ", paste(manquantes, collapse = ", "))
  n_brut <- nrow(brut)
  journal <- list()
  noter <- function(regle, n, action) {
    journal[[length(journal) + 1L]] <<- tibble::tibble(regle = regle, n = as.integer(n), action = action)
  }

  res <- appliquer_corrections(brut, chemin_corrections)
  brut <- res$brut
  noter("Manual corrections (data/corrections_manuelles.csv)", res$n, "Applied before all other rules")

  d <- brut |>
    dplyr::mutate(zs = toupper(trimws(zs)), maladie = toupper(trimws(maladie)))

  vides <- is.na(d$maladie) | is.na(d$zs)
  noter("Rows without disease or zone", sum(vides), "Removed (no data recorded)")
  d <- d[!vides, ]

  ecarts_date <- sum(as.Date(d$debutsem) != date_semaine(d$numsem, cfg), na.rm = TRUE)
  noter("Week-start date inconsistent with week number", ecarts_date, "Date recomputed from week number")

  n0 <- nrow(d)
  d <- dplyr::distinct(d, zs, numsem, maladie, dplyr::across(dplyr::all_of(c(COL_CAS, COL_DC, "totalcas", "totaldeces"))),
                       .keep_all = TRUE)
  noter("Exact duplicate rows", n0 - nrow(d), "Removed")

  # Conflits : même zone-semaine-maladie avec des valeurs différentes.
  cle <- c("zs", "numsem", "maladie")
  d <- dplyr::add_count(d, dplyr::across(dplyr::all_of(cle)), name = "n_cle")
  conflits <- d |>
    dplyr::filter(n_cle > 1) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(cle))) |>
    dplyr::summarise(valeurs = paste(totalcas, collapse = " / "), .groups = "drop")
  noter("Conflicting zone-week-disease records", nrow(conflits),
        paste0("Rule applied: ", regle_doublons, " (alternatives tested in sensitivity analyses)"))

  simples <- dplyr::filter(d, n_cle == 1)
  multiples <- dplyr::filter(d, n_cle > 1)
  if (regle_doublons == "retirer") {
    multiples <- multiples[0, ]
  } else if (regle_doublons == "max") {
    multiples <- multiples |>
      dplyr::group_by(dplyr::across(dplyr::all_of(cle))) |>
      dplyr::mutate(totaldeces = max(totaldeces, na.rm = TRUE)) |>
      dplyr::slice_max(totalcas, n = 1, with_ties = FALSE) |>
      dplyr::ungroup()
  } else if (regle_doublons == "somme") {
    multiples <- multiples |>
      dplyr::group_by(dplyr::across(dplyr::all_of(cle))) |>
      dplyr::summarise(dplyr::across(dplyr::all_of(c(COL_CAS, COL_DC, "totalcas", "totaldeces")), ~ sum(.x, na.rm = TRUE)),
                       num = dplyr::first(num), pop = dplyr::first(pop), .groups = "drop")
  } else stop("Règle de doublons inconnue : ", regle_doublons)
  d <- dplyr::bind_rows(simples, multiples) |> dplyr::select(-n_cle)

  somme_age <- rowSums(d[COL_CAS], na.rm = TRUE)
  ecart <- !is.na(d$totalcas) & somme_age != d$totalcas
  noter("Case total differs from sum of age groups", sum(ecart), "Total replaced by sum of age groups")
  d$totalcas[ecart] <- somme_age[ecart]

  # Deux jeux de population coexistent (S1-2 puis S3+) : une seule référence par zone.
  ref_pop <- d |>
    dplyr::filter(numsem >= cfg$nettoyage$semaine_pop_min) |>
    dplyr::count(zs, pop) |>
    dplyr::group_by(zs) |>
    dplyr::slice_max(n, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::transmute(zs, pop_ref = pop)
  noter("Zones with more than one population denominator",
        sum(tapply(d$pop, d$zs, dplyr::n_distinct) > 1),
        "Modal value from week 3 onward used as single denominator")

  propre <- d |>
    dplyr::left_join(ref_pop, by = "zs") |>
    dplyr::transmute(zs, semaine = as.integer(numsem), date = date_semaine(semaine, cfg), maladie,
                     cas = totalcas, deces = totaldeces, pop = pop_ref) |>
    dplyr::arrange(zs, semaine, maladie)

  zones <- sort(unique(propre$zs))
  if (length(zones) != cfg$donnees$nb_zs_attendu) {
    warning("Nombre de zones inattendu : ", length(zones))
  }
  conf_sup <- propre |>
    dplyr::filter(maladie %in% c("PALUDISME CONF", "PALUDISME SUSP")) |>
    tidyr::pivot_wider(id_cols = c(zs, semaine), names_from = maladie, values_from = cas) |>
    dplyr::filter(`PALUDISME CONF` > `PALUDISME SUSP`)
  noter("Confirmed malaria exceeds suspected malaria", nrow(conf_sup), "Week excluded from positivity")

  ruptures <- detecter_ruptures_palu(propre, cfg)
  noter("Zones with abrupt loss of malaria confirmation", sum(!is.na(ruptures$debut_rupture)),
        "Confirmed malaria set to missing from onset of break")
  noter("Zones where confirmed equals suspected malaria at baseline", sum(ruptures$conf_egal_susp),
        "Confirmed malaria set to missing for those weeks")

  list(
    donnees = propre,
    journal = dplyr::bind_rows(journal),
    conflits = conflits,
    ruptures = ruptures,
    pop = ref_pop,
    n_brut = n_brut,
    fichier = basename(chemin),
    sha256 = digest::digest(file = chemin, algo = "sha256"),
    semaine_max = max(propre$semaine)
  )
}

palu_large <- function(d) {
  d |>
    dplyr::filter(maladie %in% c("PALUDISME CONF", "PALUDISME SUSP")) |>
    dplyr::select(zs, semaine, maladie, cas) |>
    tidyr::pivot_wider(names_from = maladie, values_from = cas) |>
    dplyr::rename_with(~ sub("PALUDISME ", "", .x)) |>
    dplyr::rename(conf = dplyr::any_of("CONF"), susp = dplyr::any_of("SUSP"))
}

# Une zone qui confirmait normalement le paludisme et dont la positivité s'effondre
# durablement pendant que les suspects persistent : le signal mesure alors le dépistage
# ou la déclaration, pas la maladie.
detecter_ruptures_palu <- function(d, cfg) {
  p <- cfg$paludisme
  base_fin <- cfg$epidemie$semaine_rupture - 1L
  palu_large(d) |>
    dplyr::filter(!is.na(susp), susp > 0) |>
    dplyr::mutate(conf = dplyr::coalesce(conf, 0), pos = conf / susp) |>
    dplyr::arrange(zs, semaine) |>
    dplyr::group_by(zs) |>
    dplyr::group_modify(function(z, ...) {
      base <- z[z$semaine <= base_fin, ]
      pos0 <- stats::median(base$pos)
      egal <- mean(base$conf == base$susp) >= p$part_conf_egal_susp
      debut <- NA_integer_
      if (pos0 >= p$positivite_base_min && nrow(z) >= p$fenetre) {
        sous <- z$pos < p$positivite_rupture
        for (i in seq_len(nrow(z) - p$fenetre + 1L)) {
          if (sous[i] && sum(sous[i:(i + p$fenetre - 1L)]) >= p$min_sous_seuil) {
            debut <- z$semaine[i]
            break
          }
        }
      }
      tibble::tibble(positivite_base = pos0, conf_egal_susp = egal, debut_rupture = debut)
    }) |>
    dplyr::ungroup()
}

# Panel équilibré zone × semaine pour une maladie.
# NA = semaine non rapportée, ou zéro/ligne absente invraisemblable pour une zone à fort volume.
construire_panel <- function(ids, maladie_cible, cfg, semaine_fin = ids$semaine_max, masquer_palu = TRUE) {
  d <- ids$donnees
  rapporte <- dplyr::distinct(d, zs, semaine) |> dplyr::mutate(rapporte = 1L)
  x <- d |> dplyr::filter(maladie == maladie_cible) |> dplyr::select(zs, semaine, y = cas)
  mediane <- x |> dplyr::group_by(zs) |> dplyr::summarise(med = stats::median(y), .groups = "drop")
  p <- tidyr::expand_grid(zs = sort(unique(d$zs)), semaine = seq_len(semaine_fin)) |>
    dplyr::left_join(rapporte, by = c("zs", "semaine")) |>
    dplyr::mutate(rapporte = dplyr::coalesce(rapporte, 0L)) |>
    dplyr::left_join(x, by = c("zs", "semaine")) |>
    dplyr::left_join(mediane, by = "zs") |>
    dplyr::left_join(ids$pop, by = "zs") |>
    dplyr::rename(pop = pop_ref) |>
    dplyr::mutate(
      med = dplyr::coalesce(med, 0),
      y_raw = y,
      y = dplyr::case_when(
        rapporte == 0L ~ NA_real_,
        med >= cfg$nettoyage$mediane_zone_pour_zero & (is.na(y) | y == 0) ~ NA_real_,
        is.na(y) ~ 0,
        TRUE ~ y
      )
    )
  if (masquer_palu && maladie_cible == "PALUDISME CONF") {
    p <- masquer_confirmes(p, ids)
  }
  dplyr::select(p, zs, semaine, pop, rapporte, y, y_raw)
}

masquer_confirmes <- function(p, ids) {
  r <- ids$ruptures
  p |>
    dplyr::left_join(dplyr::select(r, zs, debut_rupture, conf_egal_susp), by = "zs") |>
    dplyr::mutate(
      y = dplyr::if_else(!is.na(debut_rupture) & semaine >= debut_rupture, NA_real_, y),
      y = dplyr::if_else(dplyr::coalesce(conf_egal_susp, FALSE) &
                           (is.na(debut_rupture) | semaine < debut_rupture), NA_real_, y)
    ) |>
    dplyr::select(-debut_rupture, -conf_egal_susp)
}

# Positivité (proxy) : confirmés / suspects, avec et sans semaines de rupture.
positivite_zone <- function(ids, semaine_fin = ids$semaine_max) {
  r <- ids$ruptures
  palu_large(ids$donnees) |>
    dplyr::filter(semaine <= semaine_fin) |>
    dplyr::left_join(dplyr::select(r, zs, debut_rupture, conf_egal_susp), by = "zs") |>
    dplyr::mutate(
      valide = !is.na(susp) & susp > 0 & !is.na(conf) & conf <= susp &
        (is.na(debut_rupture) | semaine < debut_rupture) & !dplyr::coalesce(conf_egal_susp, FALSE)
    )
}
