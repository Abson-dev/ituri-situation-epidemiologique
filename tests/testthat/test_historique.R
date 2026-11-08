library(testthat)
for (f in list.files(here::here("R"), full.names = TRUE)) source(f, encoding = "UTF-8")
mode_initial <- Sys.getenv("ITURI_DEMO")
Sys.setenv(ITURI_DEMO = "1")
withr::defer(Sys.setenv(ITURI_DEMO = mode_initial), teardown_env())

cfg <- lire_config()
if (!dir.exists(chemin_donnees("raw"))) generer_donnees_synthetiques(cfg)
empreinte_2026 <- digest::digest(file = trouver_base(cfg), algo = "sha256")
generer_historique_synthetique(cfg)
hist <- suppressWarnings(charger_historique(cfg))
ids <- nettoyer_ids(trouver_base(cfg), cfg)

test_that("la semaine 1 de chaque année est un lundi, cohérent avec la configuration 2026", {
  expect_equal(origine_annee(2026L), as.Date(cfg$donnees$origine_semaine1))
  expect_equal(origine_annee(2025L), as.Date("2024-12-30"))
  expect_equal(origine_annee(2024L), as.Date("2024-01-01"))
  expect_equal(origine_annee(2023L), as.Date("2023-01-02"))
  expect_equal(annee_courante(cfg), 2026L)
})

test_that("l'historique synthétique couvre 2023 à 2025 sans modifier la base 2026", {
  expect_named(hist, c("2023", "2024", "2025"))
  expect_identical(digest::digest(file = trouver_base(cfg), algo = "sha256"), empreinte_2026)
  for (a in names(hist)) {
    d <- hist[[a]]$donnees
    expect_equal(anyDuplicated(d[c("zs", "semaine", "maladie")]), 0L)
    expect_equal(sort(unique(d$semaine)), 1:52)
    expect_equal(dplyr::n_distinct(d$zs), 36)
    expect_equal(min(d$date), origine_annee(as.integer(a)))
    expect_false(anyNA(d[c("cas", "deces", "pop")]))
    expect_gt(hist[[a]]$journal$n[grepl("Conflicting", hist[[a]]$journal$regle)], 0)
  }
})

test_that("la génération est reproductible", {
  chemin <- file.path(chemin_donnees("raw", "historique"), "Base_DPS_Ituri_2024.xlsx")
  avant <- readxl::read_excel(chemin)
  generer_historique_synthetique(cfg)
  expect_equal(readxl::read_excel(chemin), avant)
})

test_that("la table multi-années conserve chaque année sans perte", {
  m <- donnees_multi_annees(ids, hist, cfg)
  expect_equal(sort(unique(m$annee)), 2023:2026)
  expect_equal(anyDuplicated(m[c("annee", "zs", "semaine", "maladie")]), 0L)
  expect_equal(sum(m$cas[m$annee == 2026]), sum(ids$donnees$cas))
  expect_equal(sum(m$cas[m$annee == 2024]), sum(hist[["2024"]]$donnees$cas))
})

test_that("une maladie sans ligne pour une année n'a pas de ligne créée", {
  m <- donnees_multi_annees(ids, hist, cfg)
  expect_gt(sum(m$annee == 2024 & m$maladie == "CHOLERA"), 0)
  expect_equal(sum(m$annee == 2023 & m$maladie == "CHOLERA"), 0)
})

test_that("les corrections de 2026 ne touchent pas les autres années", {
  fichier <- trouver_base(cfg)
  sans <- ids$donnees
  ligne <- sans[sans$maladie == "IRA" & sans$semaine == 5, ][1, ]
  corrections <- tempfile(fileext = ".csv")
  readr::write_csv(tibble::tibble(zs = ligne$zs, semaine = 5L, maladie = "IRA", champ = "cp15ans", valeur = 123456), corrections)
  cfg_a <- cfg
  cfg_a$donnees$origine_semaine1 <- "2024-01-01"
  x <- nettoyer_ids(file.path(chemin_donnees("raw", "historique"), "Base_DPS_Ituri_2024.xlsx"), cfg_a,
                    chemin_corrections = chemin_donnees("corrections_manuelles_2024.csv"))
  expect_false(any(x$donnees$cas >= 123456))
})

test_that("en mode réel, l'historique synthétique n'est jamais lu", {
  Sys.setenv(ITURI_DEMO = "0")
  withr::defer(Sys.setenv(ITURI_DEMO = "1"))
  dossier <- chemin_donnees("raw", "historique")
  expect_false(grepl("synthetic", dossier, fixed = TRUE))
  cfg_r <- lire_config()
  attendus <- fichiers_historique(cfg_r)
  expect_false(any(grepl("synthetic", attendus$chemin, fixed = TRUE)))
  expect_true(all(attendus$annee < annee_courante(cfg_r)))
  charges <- charger_historique(cfg_r)
  expect_equal(names(charges), as.character(attendus$annee))
  for (x in charges) expect_false(x$sha256 %in% vapply(hist, function(h) h$sha256, character(1)))
})
