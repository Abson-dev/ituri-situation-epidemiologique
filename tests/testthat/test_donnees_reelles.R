library(testthat)
for (f in list.files(here::here("R"), full.names = TRUE)) source(f, encoding = "UTF-8")

# Valeurs attendues conservées localement : ignoré en mode démo et sans les données réelles.
chemin_controles <- here::here("data/external/controles_reels.yml")
skip_if(mode_demo() || !file.exists(chemin_controles), "Données réelles absentes")

cfg <- lire_config()
ids <- nettoyer_ids(trouver_base(cfg), cfg)
d <- ids$donnees
ctrl <- yaml::read_yaml(chemin_controles)

test_that("la base reproduit les cumuls publiés à S32", {
  for (c in ctrl$cumuls_s32) {
    expect_equal(sum(d$cas[d$zs == c$zs & d$maladie == c$maladie & d$semaine <= 32]), c$cas)
  }
})

test_that("les cumuls provinciaux restent à moins de 2 % du rapport de référence", {
  r <- rapprocher_reference(ids, cfg)
  expect_true(all(abs(r$ecart_pct) < 2, na.rm = TRUE))
})

test_that("les ruptures attendues sont détectées, sans faux positif", {
  rompues <- ids$ruptures$zs[!is.na(ids$ruptures$debut_rupture)]
  expect_true(all(unlist(ctrl$ruptures_attendues) %in% rompues))
  expect_false(any(unlist(ctrl$sans_rupture) %in% rompues))
})
