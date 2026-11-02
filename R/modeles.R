# Analyse pour publication : effet indirect de la flambée d'Ebola sur la morbidité notifiée.
# Estimand principal : ratio de ratios de taux (RRR) après/avant la déclaration, pour une zone à
# incidence cumulée de MVE de référence (10/10 000) comparée à une zone sans MVE.
# Estimateur principal : Poisson à pseudo-vraisemblance avec effets fixes zone et semaine
# (consistant quelle que soit la surdispersion) et erreurs types groupées par zone.

exposition_zones <- function(ids, cfg) {
  mve <- readr::read_csv(chemin_donnees("external/mve_zones.csv"), show_col_types = FALSE) |>
    dplyr::filter(sitrep == cfg$epidemie$sitrep_exposition, zs != "A_VENTILER") |>
    dplyr::select(zs, mve = cas, mve_dc = deces)
  fha <- ids$donnees |>
    dplyr::filter(maladie == "FHA", semaine <= cfg$epidemie$semaine_exposition) |>
    dplyr::group_by(zs) |>
    dplyr::summarise(fha = sum(cas), .groups = "drop")
  ids$pop |>
    dplyr::rename(pop = pop_ref) |>
    dplyr::left_join(mve, by = "zs") |>
    dplyr::left_join(fha, by = "zs") |>
    dplyr::mutate(
      mve = dplyr::coalesce(mve, 0), mve_dc = dplyr::coalesce(mve_dc, 0), fha = dplyr::coalesce(fha, 0),
      inc = mve / pop * 1e4,
      dose = log1p(inc),
      dose_cas = log1p(mve),
      dose_fha = log1p(fha / pop * 1e4),
      groupe = factor(dplyr::case_when(mve == 0 ~ "None", mve < cfg$analyse$seuil_groupe_fort ~ "Lower", TRUE ~ "Higher"),
                      levels = c("None", "Lower", "Higher"))
    )
}

preparer <- function(panel, expo, cfg, var_dose = "dose", rupture = cfg$epidemie$semaine_rupture) {
  panel |>
    dplyr::filter(!is.na(y)) |>
    dplyr::left_join(expo, by = "zs", suffix = c("", ".e")) |>
    dplyr::mutate(post = as.integer(semaine >= rupture), x = post * .data[[var_dose]])
}

# Mesure de ratio au contraste de référence, IC avec G - 1 degrés de liberté.
contraste <- function(b, se, g, cfg, facteur = log1p(cfg$analyse$incidence_reference)) {
  t <- stats::qt(0.975, g - 1L)
  tibble::tibble(rrr = exp(b * facteur), bas = exp((b - t * se) * facteur), haut = exp((b + t * se) * facteur),
                 beta = b, se = se)
}

ajuster <- function(df, cfg, formule = y ~ x | zs + semaine) {
  m <- fixest::fepois(formule, data = df, vcov = ~zs, notes = FALSE, warn = FALSE)
  ct <- fixest::coeftable(m)["x", ]
  dplyr::bind_cols(contraste(ct[["Estimate"]], ct[["Std. Error"]], dplyr::n_distinct(df$zs), cfg),
                   tibble::tibble(n_obs = stats::nobs(m), n_zs = dplyr::n_distinct(df$zs)))
}

# Inférence par permutation de l'exposition entre zones : valide avec 36 grappes.
permuter <- function(df, expo, cfg, B, var_dose = "dose") {
  obs <- stats::coef(fixest::fepois(y ~ x | zs + semaine, df, notes = FALSE, warn = FALSE, vcov = "iid"))[["x"]]
  zones <- expo$zs
  doses <- expo[[var_dose]]
  stat <- vapply(seq_len(B), function(i) {
    tir <- stats::setNames(sample(doses), zones)
    df$x <- df$post * tir[df$zs]
    stats::coef(fixest::fepois(y ~ x | zs + semaine, df, notes = FALSE, warn = FALSE, vcov = "iid"))[["x"]]
  }, numeric(1))
  (1 + sum(abs(stat) >= abs(obs))) / (B + 1)
}

etude_evenement <- function(df, cfg) {
  S0 <- cfg$epidemie$semaine_rupture
  df$k <- floor((df$semaine - S0) / 4)
  m <- fixest::fepois(y ~ i(k, dose, ref = -1) | zs + semaine, df, vcov = ~zs, notes = FALSE, warn = FALSE)
  ct <- as.data.frame(fixest::coeftable(m))
  ct$k <- as.numeric(sub("k::(-?[0-9]+):dose", "\\1", rownames(ct)))
  g <- dplyr::n_distinct(df$zs)
  f <- log1p(cfg$analyse$incidence_reference)
  t <- stats::qt(0.975, g - 1L)
  V <- stats::vcov(m)
  av <- which(ct$k < -1)
  W <- as.numeric(t(ct$Estimate[av]) %*% solve(V[av, av]) %*% ct$Estimate[av]) / length(av)
  p_pre <- stats::pf(W, length(av), g - 1L, lower.tail = FALSE)
  tab <- tibble::tibble(k = ct$k, rrr = exp(ct$Estimate * f),
                        bas = exp((ct$Estimate - t * ct$`Std. Error`) * f),
                        haut = exp((ct$Estimate + t * ct$`Std. Error`) * f)) |>
    dplyr::bind_rows(tibble::tibble(k = -1, rrr = 1, bas = 1, haut = 1)) |>
    dplyr::arrange(k) |>
    dplyr::mutate(sem_deb = pmax(1, S0 + 4 * k), sem_fin = S0 + 4 * k + 3)
  list(table = tab, p_pre = p_pre)
}

# Cas non consultés/non notifiés estimés dans les zones exposées, semaines observées uniquement.
deficit <- function(df, res, cfg) {
  set.seed(cfg$analyse$graine)
  post <- df[df$post == 1 & df$dose > 0, ]
  tirages <- stats::rnorm(cfg$analyse$n_tirages_deficit, res$beta, res$se)
  calc <- function(b) sum(post$y * (exp(-b * post$dose) - 1))
  v <- vapply(tirages, calc, numeric(1))
  tibble::tibble(deficit = calc(res$beta), bas = unname(stats::quantile(v, 0.025)), haut = unname(stats::quantile(v, 0.975)),
                 observe = sum(post$y))
}

gamm_nb <- function(df, cfg) {
  df$zs_f <- factor(df$zs)
  m <- mgcv::bam(y ~ x + s(semaine, k = 15) + s(zs_f, bs = "re") + offset(log(pop)),
                 family = mgcv::nb(), data = df, method = "fREML", discrete = TRUE)
  s <- summary(m)$p.table["x", ]
  # Résidus quantiles randomisés (Dunn & Smyth 1996).
  set.seed(cfg$analyse$graine)
  mu <- stats::fitted(m)
  th <- m$family$getTheta(TRUE)
  a <- stats::pnbinom(pmax(df$y - 1, -1), mu = mu, size = th)
  b <- stats::pnbinom(df$y, mu = mu, size = th)
  rq <- stats::qnorm(pmin(pmax(stats::runif(length(a), a, b), 1e-10), 1 - 1e-10))
  ac <- tibble::tibble(zs = df$zs, semaine = df$semaine, r = rq) |>
    dplyr::arrange(zs, semaine) |>
    dplyr::group_by(zs) |>
    dplyr::mutate(r1 = dplyr::lag(r)) |>
    dplyr::ungroup()
  list(contraste = contraste(s[["Estimate"]], s[["Std. Error"]], dplyr::n_distinct(df$zs), cfg),
       theta = th, ac1 = stats::cor(ac$r, ac$r1, use = "complete.obs"), rq = rq)
}

analyses_sensibilite <- function(code, ids, panel, expo, cfg) {
  a <- cfg$analyse
  base <- preparer(panel, expo, cfg)
  couv <- panel |> dplyr::group_by(zs) |> dplyr::summarise(c = mean(rapporte), .groups = "drop")
  gardees <- couv$zs[couv$c >= a$couverture_min_zone]
  deux_max <- expo$zs[order(-expo$mve)][1:2]
  fin_pre <- cfg$epidemie$semaine_rupture - 1L

  alt_doublons <- function(regle) {
    ids2 <- nettoyer_ids(file.path(chemin_donnees("raw"), ids$fichier), cfg, regle_doublons = regle)
    preparer(construire_panel(ids2, code, cfg, semaine_fin = max(panel$semaine)), exposition_zones(ids2, cfg), cfg)
  }
  specs <- list(
    "Exposure: case count, not incidence" = list(df = preparer(panel, expo, cfg, "dose_cas"), facteur = log1p(100)),
    "Exposure: IDS haemorrhagic fever incidence" = list(df = preparer(panel, expo, cfg, "dose_fha")),
    "Excluding two highest-burden zones" = list(df = dplyr::filter(base, !zs %in% deux_max)),
    "Excluding transition weeks 20–21" = list(df = dplyr::filter(base, !semaine %in% unlist(a$exclusion_transition))),
    "Zones reporting at least 90% of weeks" = list(df = dplyr::filter(base, zs %in% gardees)),
    "Duplicates: conflicting records removed" = list(df = alt_doublons("retirer")),
    "Duplicates: conflicting records summed" = list(df = alt_doublons("somme")),
    "Zone-specific linear trends" = list(df = base, formule = y ~ x | zs[semaine_n] + semaine),
    "Placebo: false break at week 10, pre-period only" = list(
      df = base |> dplyr::filter(semaine <= fin_pre) |>
        dplyr::mutate(post = as.integer(semaine >= a$semaine_placebo), x = post * dose))
  )
  out <- lapply(names(specs), function(nom) {
    s <- specs[[nom]]
    df <- s$df
    df$semaine_n <- df$semaine
    f <- if (is.null(s$formule)) y ~ x | zs + semaine else s$formule
    r <- ajuster(df, cfg, f)
    if (!is.null(s$facteur)) {
      r2 <- contraste(r$beta, r$se, r$n_zs, cfg, s$facteur)
      r[c("rrr", "bas", "haut")] <- r2[c("rrr", "bas", "haut")]
    }
    dplyr::mutate(r, analyse = nom, .before = 1)
  })
  # Influence : retrait d'une zone à la fois.
  loo <- vapply(unique(base$zs), function(z) ajuster(dplyr::filter(base, zs != z), cfg)$rrr, numeric(1))
  cat_m <- base |> dplyr::mutate(xbas = post * (groupe == "Lower"), xhaut = post * (groupe == "Higher"))
  m <- fixest::fepois(y ~ xbas + xhaut | zs + semaine, cat_m, vcov = ~zs, notes = FALSE, warn = FALSE)
  ct <- fixest::coeftable(m)
  t <- stats::qt(0.975, dplyr::n_distinct(cat_m$zs) - 1L)
  categ <- tibble::tibble(groupe = c("Lower", "Higher"), rrr = exp(ct[, 1]), bas = exp(ct[, 1] - t * ct[, 2]),
                          haut = exp(ct[, 1] + t * ct[, 2]))
  list(table = dplyr::bind_rows(out), loo = range(loo), categories = categ)
}

analyser_article <- function(ids, cfg) {
  a <- cfg$analyse
  expo <- exposition_zones(ids, cfg)
  res <- list()
  set.seed(a$graine)
  codes <- c(a$resultats_principaux, a$resultats_secondaires)
  for (code in codes) {
    message("  - ", code)
    panel <- construire_panel(ids, code, cfg)
    df <- preparer(panel, expo, cfg)
    principal <- ajuster(df, cfg)
    principal$p_perm <- permuter(df, expo, cfg, a$n_permutations)
    res[[code]] <- list(
      principal = principal,
      evenement = etude_evenement(df, cfg),
      deficit = deficit(df, principal, cfg),
      gamm = gamm_nb(df, cfg),
      sensibilite = analyses_sensibilite(code, ids, panel, expo, cfg)
    )
  }
  prim <- a$resultats_principaux
  p_holm <- stats::p.adjust(vapply(prim, function(k) res[[k]]$principal$p_perm, numeric(1)), method = "holm")
  for (k in prim) res[[k]]$principal$p_holm <- p_holm[[k]]

  # Résultats secondaires non causaux : positivité et rapportage.
  pos <- positivite_zone(ids) |>
    dplyr::filter(valide) |>
    dplyr::left_join(expo, by = "zs") |>
    dplyr::mutate(post = as.integer(semaine >= cfg$epidemie$semaine_rupture), x = post * dose, p = conf / susp)
  m_pos <- fixest::feglm(p ~ x | zs + semaine, pos, family = stats::quasibinomial(), weights = ~susp, vcov = ~zs,
                         notes = FALSE, warn = FALSE)
  ct <- fixest::coeftable(m_pos)["x", ]
  positivite <- contraste(ct[["Estimate"]], ct[["Std. Error"]], dplyr::n_distinct(pos$zs), cfg)

  rep <- tidyr::expand_grid(zs = expo$zs, semaine = seq_len(ids$semaine_max)) |>
    dplyr::left_join(dplyr::distinct(ids$donnees, zs, semaine) |> dplyr::mutate(r = 1L), by = c("zs", "semaine")) |>
    dplyr::mutate(r = dplyr::coalesce(r, 0L)) |>
    dplyr::left_join(expo, by = "zs") |>
    dplyr::mutate(post = as.integer(semaine >= cfg$epidemie$semaine_rupture), x = post * dose)
  m_rep <- fixest::feols(r ~ x | zs + semaine, rep, vcov = ~zs, notes = FALSE)
  crep <- fixest::coeftable(m_rep)["x", ]
  f <- log1p(a$incidence_reference)
  t <- stats::qt(0.975, nrow(expo) - 1L)
  rapportage <- tibble::tibble(dif = crep[["Estimate"]] * f,
                               bas = (crep[["Estimate"]] - t * crep[["Std. Error"]]) * f,
                               haut = (crep[["Estimate"]] + t * crep[["Std. Error"]]) * f)

  concord <- list(
    rho = stats::cor(expo$mve, expo$fha, method = "spearman"),
    rho_inc = stats::cor(expo$inc, expo$fha / expo$pop, method = "spearman")
  )
  list(resultats = res, expo = expo, positivite = positivite, rapportage = rapportage,
       rapportage_groupes = rep |> dplyr::group_by(groupe, post) |> dplyr::summarise(r = mean(r), .groups = "drop"),
       concordance = concord, semaine_max = ids$semaine_max, sha256 = ids$sha256, fichier = ids$fichier,
       date_calcul = Sys.Date(), n_perm = a$n_permutations)
}

# Cache piloté par le contenu : recalcul uniquement si données, config ou code changent.
resultats_article <- function(ids, cfg, forcer = FALSE) {
  cle <- digest::digest(list(
    ids$sha256, cfg$analyse, cfg$nettoyage, cfg$paludisme, cfg$epidemie,
    digest::digest(file = here::here("R/modeles.R")), digest::digest(file = here::here("R/donnees.R")),
    digest::digest(file = chemin_donnees("external/mve_zones.csv")),
    mode_demo(),
    if (file.exists(chemin_donnees("corrections_manuelles.csv"))) digest::digest(file = chemin_donnees("corrections_manuelles.csv"))
  ))
  dossier <- here::here("outputs/modeles")
  dir.create(dossier, recursive = TRUE, showWarnings = FALSE)
  f <- file.path(dossier, paste0("resultats_", substr(cle, 1, 12), ".rds"))
  if (file.exists(f) && !forcer) return(readRDS(f))
  message("Calcul des modèles (quelques minutes)...")
  r <- analyser_article(ids, cfg)
  saveRDS(r, f)
  r
}
