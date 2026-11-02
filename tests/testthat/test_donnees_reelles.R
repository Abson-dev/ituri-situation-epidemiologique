library(testthat)
for (f in list.files(here::here("R"), full.names = TRUE)) source(f, encoding = "UTF-8")

# Valeurs issues des vraies données : ignoré en mode démo et sur un clone sans données.
skip_if(mode_demo() || !length(list.files(here::here("data/raw"), pattern = "xlsx$")), "Base réelle absente")

cfg <- lire_config()
ids <- nettoyer_ids(trouver_base(cfg), cfg)
d <- ids$donnees
s <- function(z, m, S = 32) sum(d$cas[d$zs == z & d$maladie == m & d$semaine <= S])

test_that("la base reproduit les cumuls publiés de Mongbwalu à S32", {
  expect_equal(s("MONGBWALU", "PALUDISME SUSP"), 11693)
  expect_equal(s("MONGBWALU", "PALUDISME CONF"), 6829)
  expect_equal(s("MONGBWALU", "FIEVRE TYPHOIDE"), 3802)
  expect_equal(s("MONGBWALU", "FHA"), 117)
})

test_that("les cumuls provinciaux restent à moins de 2 % du rapport S32", {
  r <- rapprocher_reference(ids, cfg)
  expect_true(all(abs(r$ecart_pct) < 2, na.rm = TRUE))
})

test_that("les ruptures évidentes de confirmation sont détectées et Bunia n'est pas un faux positif", {
  r <- ids$ruptures
  rompues <- r$zs[!is.na(r$debut_rupture)]
  expect_true(all(c("NYARAMBE", "ANGUMU", "LAYBO") %in% rompues))
  expect_false("BUNIA" %in% rompues)
})
