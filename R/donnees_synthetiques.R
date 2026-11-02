# Génère une base factice de même structure et avec les mêmes défauts que la base réelle,
# pour exécuter tout le pipeline sans aucune donnée réelle. Aucune valeur n'est tirée de la vraie base.
generer_donnees_synthetiques <- function(cfg, graine = 1L) {
  set.seed(graine)
  racine <- here::here("data/synthetic")
  for (d in c("raw", "external", "notes")) dir.create(file.path(racine, d), recursive = TRUE, showWarnings = FALSE)
  zones <- c("ADI", "ADJA", "ANGUMU", "ARIWARA", "ARU", "AUNGBA", "BAMBU", "BIRINGI", "BOGA", "BUNIA", "DAMAS", "DRODRO",
             "FATAKI", "GETHY", "JIBA", "KAMBALA", "KILO", "KOMANDA", "LAYBO", "LINGA", "LITA", "LOGO", "LOLWA", "MAHAGI",
             "MAMBASSA", "MANDIMA", "MANGALA", "MONGBWALU", "NIA-NIA", "NIZI", "NYAKUNDE", "NYARAMBE", "RETHY", "RIMBA", "RWAMPARA", "TCHOMIA")
  S <- 43L
  rupture <- cfg$epidemie$semaine_rupture
  z <- tibble::tibble(zs = zones, pop = round(stats::runif(36, 8e4, 4e5)),
                      mve = c(rep(0, 8), round(stats::rlnorm(28, 3, 1.5)))[sample(36)],
                      niveau = stats::rlnorm(36, 0, 0.3))
  z$dose <- log1p(z$mve / z$pop * 1e4)

  taux <- c("PALUDISME SUSP" = 300, "IRA" = 80, "PNEUMONIE" = 20, "DIARRHEE DHY M5" = 15, "FIEVRE TYPHOIDE" = 40, "GRIPPE" = 40)
  rares <- c("ROUGEOLE", "DIARR SANGLANTE", "MONKEYPOX", "MENINGITE", "PFA", "DIPHTERIE", "COQUELUCHE",
             "FIEVRE JAUNE", "DECES MATERNELS", "RAGE", "PESTE")
  grille <- tidyr::expand_grid(z, semaine = seq_len(S)) |>
    dplyr::mutate(post = semaine >= rupture, attendu = pop / 1e5 * niveau * exp(-0.1 * post * dose))
  frequents <- lapply(names(taux), function(m) {
    dplyr::mutate(grille, maladie = m, cas = stats::rnbinom(dplyr::n(), size = 20, mu = attendu * taux[[m]]),
                  deces = stats::rbinom(dplyr::n(), cas, 2e-4))
  }) |> dplyr::bind_rows()
  conf <- dplyr::filter(frequents, maladie == "PALUDISME SUSP") |>
    dplyr::mutate(maladie = "PALUDISME CONF", cas = stats::rbinom(dplyr::n(), cas, 0.6))
  fha <- dplyr::filter(grille, post) |>
    dplyr::mutate(maladie = "FHA", cas = stats::rpois(dplyr::n(), 2 * dose), deces = stats::rbinom(dplyr::n(), cas, 0.2)) |>
    dplyr::filter(cas > 0)
  rare <- lapply(rares, function(m) {
    dplyr::mutate(grille, maladie = m, cas = stats::rpois(dplyr::n(), 0.05), deces = 0L) |> dplyr::filter(cas > 0)
  }) |> dplyr::bind_rows()
  long <- dplyr::bind_rows(frequents, conf, fha, rare)

  # Défauts volontaires : ruptures de confirmation, semaines manquantes.
  casse <- sample(zones, 3)
  manquantes <- sample(unique(paste(long$zs, long$semaine)), 40)
  long <- long |>
    dplyr::filter(!(maladie == "PALUDISME CONF" & zs %in% casse & semaine >= 28L)) |>
    dplyr::filter(!paste(zs, semaine) %in% manquantes)

  repartir <- function(n) as.numeric(stats::rmultinom(1, n, c(0.15, 0.25, 0.25, 0.35)))
  ages <- t(vapply(long$cas, repartir, numeric(4)))
  base <- long |>
    dplyr::transmute(num = paste0("RDCITU", substr(gsub("-", "", zs), 1, 3), sprintf("%02d", semaine)),
                     pays = "RDC", prov = "ITURI", zs, pop, numsem = semaine,
                     debutsem = date_semaine(semaine, cfg), maladie,
                     c328tnn = 0, dtnn = 0, c011mois = ages[, 1], d011mois = 0, c1259mois = ages[, 2], d1259mois = 0,
                     c515ans = ages[, 3], d515ans = 0, cp15ans = ages[, 4], dp15ans = deces,
                     totalcas = cas, totaldeces = deces)
  # Doublons exacts, conflits, ligne vide, date fausse.
  exact <- base[sample(nrow(base), 5), ]
  conflit <- base[sample(which(base$totalcas > 20), 5), ]
  conflit$cp15ans <- conflit$cp15ans + 7
  conflit$totalcas <- conflit$totalcas + 7
  vide <- base[1, ]
  vide[, setdiff(names(base), c("num", "pays", "prov", "zs", "pop", "numsem", "debutsem"))] <- NA
  base <- dplyr::bind_rows(base, exact, conflit, vide)
  base$debutsem[base$numsem == 32] <- as.Date("2026-08-12")
  writexl::write_xlsx(base, file.path(racine, "raw", "Base_DPS_Ituri_Week43.xlsx"))

  touchees <- dplyr::filter(z, mve > 0)
  deces <- stats::rbinom(nrow(touchees), touchees$mve, 0.45)
  readr::write_csv(dplyr::bind_rows(
    tibble::tibble(sitrep = "n092", date_sitrep = as.Date("2026-08-14"), zs = touchees$zs, cas = touchees$mve, deces = deces),
    tibble::tibble(sitrep = "n092", date_sitrep = as.Date("2026-08-14"), zs = "A_VENTILER", cas = 0, deces = 5)),
    file.path(racine, "external", "mve_zones.csv"))
  readr::write_csv(tibble::tibble(sitrep = "n092", date_sitrep = as.Date("2026-08-14"), province = c("Ituri", "Province B"),
                                  cas_confirmes = c(sum(touchees$mve), 50), deces_confirmes = c(sum(deces), 20),
                                  zs_touchees = c(nrow(touchees), 3), zs_total = c(36, 10), nouveaux_24h = c(5, 1), note = NA),
                   file.path(racine, "external", "mve_provinces.csv"))
  codes <- unlist(cfg$tableau_principal)
  readr::write_csv(tibble::tibble(maladie = codes, S27 = 0, S28 = 0, S29 = 0, S30 = 0, S31 = 0, deces_S31 = 0, S32 = 0, deces_S32 = 0,
                                  cumul_2026 = 0, deces_2026 = 0, cumul_2025 = 0, deces_2025 = 0, fiable_2025 = "oui", note_2025 = NA),
                   file.path(racine, "external", "rapport_s32_reference.csv"))
  readr::write_csv(tibble::tibble(semaine = S, zs = unlist(cfg$zones_msf), aires_sante = 10, completude_pct = 95,
                                  promptitude_pct = 90, as_non_rapportees_pct = 5),
                   file.path(racine, "external", "completude_as.csv"))
  writeLines("zs,semaine,maladie,champ,valeur,motif,valide_par,date_validation", file.path(racine, "corrections_manuelles.csv"))
  writeLines("Note de terrain factice pour la démonstration.", file.path(racine, "notes", paste0("S", S, ".md")))
  invisible(racine)
}
