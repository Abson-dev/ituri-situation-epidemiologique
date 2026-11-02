library(testthat)
for (f in list.files(here::here("R"), full.names = TRUE)) source(f, encoding = "UTF-8")
cfg <- lire_config()
ids <- nettoyer_ids(trouver_base(cfg), cfg)
d <- ids$donnees

test_that("la clé zone-semaine-maladie est unique après nettoyage", {
  expect_equal(anyDuplicated(d[c("zs", "semaine", "maladie")]), 0L)
})

test_that("toutes les zones ont une population unique et aucun champ essentiel n'est vide", {
  expect_equal(dplyr::n_distinct(d$zs), cfg$donnees$nb_zs_attendu)
  expect_true(all(tapply(d$pop, d$zs, dplyr::n_distinct) == 1))
  expect_false(anyNA(d[c("zs", "semaine", "maladie", "cas", "deces", "pop")]))
})

test_that("les décès ne dépassent jamais les cas", {
  expect_true(all(d$deces <= d$cas))
})

test_that("les dates sont cohérentes avec les semaines", {
  expect_equal(d$date, date_semaine(d$semaine, cfg))
  expect_equal(date_semaine(20L, cfg), as.Date("2026-05-11"))
  expect_true(as.numeric(as.Date(cfg$epidemie$date_declaration) - date_semaine(cfg$epidemie$semaine_rupture, cfg)) %in% 0:6)
})

test_that("la rupture de confirmation est détectée", {
  expect_gte(sum(!is.na(ids$ruptures$debut_rupture)), 1L)
})

test_that("une semaine non rapportée reste manquante, pas zéro", {
  p <- construire_panel(ids, "PALUDISME SUSP", cfg)
  expect_true(all(is.na(p$y[p$rapporte == 0L])))
  expect_equal(nrow(p), cfg$donnees$nb_zs_attendu * ids$semaine_max)
})

test_that("une ligne absente d'une maladie rare vaut zéro si la zone a rapporté", {
  p <- construire_panel(ids, "ROUGEOLE", cfg)
  expect_true(all(!is.na(p$y[p$rapporte == 1L])))
})

test_that("les règles de doublons sont ordonnées et les inconnues refusées", {
  chemin <- trouver_base(cfg)
  somme <- nettoyer_ids(chemin, cfg, regle_doublons = "somme")
  retire <- nettoyer_ids(chemin, cfg, regle_doublons = "retirer")
  total <- function(x) sum(x$donnees$cas[x$donnees$maladie == "PALUDISME SUSP"])
  expect_gte(total(somme), total(ids))
  expect_gte(total(ids), total(retire))
  expect_error(nettoyer_ids(chemin, cfg, regle_doublons = "x"), "inconnue")
})

test_that("le modèle retrouve un effet simulé connu", {
  set.seed(1)
  expo <- tibble::tibble(zs = sprintf("Z%02d", 1:36), dose = rep(c(0, 1, 2), 12))
  sim <- tidyr::expand_grid(expo, semaine = 1:43) |>
    dplyr::mutate(post = as.integer(semaine >= 20), x = post * dose,
                  y = stats::rpois(dplyr::n(), exp(5 + 0.3 * as.numeric(factor(zs)) / 36 + 0.1 * sin(semaine / 5) - 0.2 * x)))
  r <- ajuster(sim, cfg)
  expect_lt(abs(r$beta - (-0.2)), 0.02)
  expect_equal(r$rrr, exp(r$beta * log1p(cfg$analyse$incidence_reference)))
})
