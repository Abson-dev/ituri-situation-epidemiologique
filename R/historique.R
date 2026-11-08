# Années antérieures : une base par année, de même structure que la base hebdomadaire. L'année est lue
# dans le nom du fichier, par exemple :
#   Données réelles        : data/raw/IDSR 2025_WEEK52.xls ou data/raw/historique/Base_DPS_Ituri_2024.xlsx
#   Données synthétiques   : data/synthetic/raw/historique/Base_DPS_Ituri_2024.xlsx
# chemin_donnees() choisit le dossier selon le mode : un historique synthétique n'est jamais lu avec
# des données réelles, car des chiffres inventés passeraient pour une référence.

fichiers_historique <- function(cfg) {
  courante <- annee_courante(cfg)
  hist <- bases_dossier(chemin_donnees("raw", "historique"))
  hist <- hist[!is.na(hist$annee), ]
  if (any(hist$annee >= courante)) {
    stop("Historique : année postérieure aux années passées : ", paste(hist$fichier[hist$annee >= courante], collapse = ", "), call. = FALSE)
  }
  raw <- bases_dossier(chemin_donnees("raw"))
  x <- rbind(hist, raw[!is.na(raw$annee) & raw$annee < courante, ])
  doubles <- x$annee[duplicated(x$annee)]
  if (length(doubles)) {
    stop("Historique : plusieurs bases pour ", paste(unique(doubles), collapse = ", "), " : ",
         paste(x$fichier[x$annee %in% doubles], collapse = ", "), ". En garder une seule.", call. = FALSE)
  }
  x[order(x$annee), ]
}

# Lundi de la semaine ISO 1, comme la semaine 1 de 2026 dans config.yml. À confirmer pour les vraies bases.
origine_annee <- function(annee) {
  j4 <- as.Date(sprintf("%d-01-04", annee))
  j4 - (as.integer(format(j4, "%u")) - 1L)
}

annee_courante <- function(cfg) as.integer(format(as.Date(cfg$donnees$origine_semaine1) + 3L, "%Y"))

charger_historique <- function(cfg) {
  x <- fichiers_historique(cfg)
  fichiers <- x$chemin
  annees <- x$annee
  res <- lapply(seq_along(fichiers), function(i) {
    cfg_a <- cfg
    cfg_a$donnees$origine_semaine1 <- as.character(origine_annee(annees[i]))
    # Les corrections de 2026 ne s'appliquent pas aux autres années, même zone-semaine-maladie.
    nettoyer_ids(fichiers[i], cfg_a, chemin_corrections = chemin_donnees(sprintf("corrections_manuelles_%d.csv", annees[i])))
  })
  stats::setNames(res, annees)
}

# Une table zone-semaine-maladie sur toutes les années disponibles.
donnees_multi_annees <- function(ids, historique, cfg) {
  ajouter <- function(x, a) dplyr::mutate(x$donnees, annee = a, .before = 1)
  dplyr::bind_rows(
    lapply(names(historique), function(a) ajouter(historique[[a]], as.integer(a))),
    ajouter(ids, annee_courante(cfg))
  )
}

# Une base factice par année, à partir des niveaux de la base 2026 synthétique. Saisonnalité, tendance et
# flambée sont inventées pour illustrer les pages : aucune valeur ne vient de la vraie base.
generer_historique_synthetique <- function(cfg, annees = 2023:2025, graine = 20232025L) {
  ids <- nettoyer_ids(trouver_base(cfg), cfg)
  set.seed(graine)
  dossier <- chemin_donnees("raw", "historique")
  dir.create(dossier, recursive = TRUE, showWarnings = FALSE)
  saison <- list(`PALUDISME SUSP` = c(0.25, 20), IRA = c(0.20, 30), PNEUMONIE = c(0.20, 30), `DIARRHEE DHY M5` = c(0.15, 10),
                 `FIEVRE TYPHOIDE` = c(0.10, 40), GRIPPE = c(0.25, 28))
  niveaux <- ids$donnees |>
    dplyr::filter(maladie %in% names(saison), semaine < cfg$epidemie$semaine_rupture) |>
    dplyr::group_by(zs, maladie) |>
    dplyr::summarise(moyenne = mean(cas), .groups = "drop")
  rares <- c("ROUGEOLE", "CHOLERA", "DIARR SANGLANTE", "MONKEYPOX", "MENINGITE", "PFA", "DIPHTERIE", "COQUELUCHE",
             "FIEVRE JAUNE", "DECES MATERNELS", "RAGE", "PESTE", "FHA")
  zones <- sort(unique(ids$donnees$zs))
  flambee <- sample(zones, 3)

  for (a in annees) {
    pop <- tibble::tibble(zs = zones, pop = round(ids$pop$pop_ref[match(zones, ids$pop$zs)] / 1.03^(annee_courante(cfg) - a)))
    grille <- tidyr::expand_grid(niveaux, semaine = 1:52) |> dplyr::left_join(pop, by = "zs")
    grille$effet <- stats::rlnorm(length(saison), 0, 0.05)[match(grille$maladie, names(saison))]
    grille$s <- vapply(saison, `[`, numeric(1), 1)[grille$maladie] * cos(2 * pi * (grille$semaine - vapply(saison, `[`, numeric(1), 2)[grille$maladie]) / 52)
    frequents <- grille |>
      dplyr::mutate(cas = stats::rnbinom(dplyr::n(), size = 20, mu = moyenne * effet * (1 + s) / 1.03^(annee_courante(cfg) - a)),
                    deces = stats::rbinom(dplyr::n(), cas, 2e-4)) |>
      dplyr::select(zs, pop, semaine, maladie, cas, deces)
    conf <- dplyr::filter(frequents, maladie == "PALUDISME SUSP") |>
      dplyr::mutate(maladie = "PALUDISME CONF", cas = stats::rbinom(dplyr::n(), cas, 0.6), deces = stats::rbinom(dplyr::n(), deces, 0.5))
    # Comme dans la base 2026, une maladie rare n'a une ligne que si des cas sont notifiés.
    rare <- tidyr::expand_grid(pop, semaine = 1:52, maladie = rares) |>
      dplyr::mutate(lambda = dplyr::case_when(maladie == "FHA" ~ 0.002, maladie == "CHOLERA" & a != 2024L ~ 0, TRUE ~ 0.05),
                    cas = stats::rpois(dplyr::n(), lambda), deces = 0L) |>
      dplyr::filter(cas > 0) |>
      dplyr::select(-lambda)
    long <- dplyr::bind_rows(frequents, conf, rare)
    if (a == 2024L) {
      pic <- tidyr::expand_grid(dplyr::filter(pop, zs %in% flambee), semaine = 12:20, maladie = "ROUGEOLE") |>
        dplyr::mutate(cas = stats::rpois(dplyr::n(), 8), deces = 0L) |>
        dplyr::filter(cas > 0)
      long <- dplyr::bind_rows(dplyr::anti_join(long, pic, by = c("zs", "semaine", "maladie")), pic)
    }
    # Mêmes défauts que la base hebdomadaire : semaines manquantes, doublons, conflits, ligne vide.
    manquantes <- sample(unique(paste(long$zs, long$semaine)), 30)
    long <- dplyr::filter(long, !paste(zs, semaine) %in% manquantes)
    base <- long |>
      dplyr::transmute(num = paste0("RDCITU", substr(gsub("-", "", zs), 1, 3), sprintf("%02d", semaine)),
                       pays = "RDC", prov = "ITURI", zs, pop, numsem = semaine,
                       debutsem = origine_annee(a) + 7L * (semaine - 1L), maladie,
                       c328tnn = 0, dtnn = 0, c011mois = floor(cas * 0.15), d011mois = 0, c1259mois = floor(cas * 0.25), d1259mois = 0,
                       c515ans = floor(cas * 0.25), d515ans = 0, cp15ans = cas - floor(cas * 0.15) - 2 * floor(cas * 0.25), dp15ans = deces,
                       totalcas = cas, totaldeces = deces)
    exact <- base[sample(nrow(base), 3), ]
    conflit <- base[sample(which(base$totalcas > 20), 3), ]
    conflit$cp15ans <- conflit$cp15ans + 7
    conflit$totalcas <- conflit$totalcas + 7
    vide <- base[1, ]
    vide[, setdiff(names(base), c("num", "pays", "prov", "zs", "pop", "numsem", "debutsem"))] <- NA
    writexl::write_xlsx(dplyr::bind_rows(base, exact, conflit, vide), file.path(dossier, sprintf("Base_DPS_Ituri_%d.xlsx", a)))
  }
  invisible(dossier)
}
