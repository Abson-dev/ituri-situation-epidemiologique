library(testthat)
for (f in list.files(here::here("R"), full.names = TRUE)) source(f, encoding = "UTF-8")

ligne <- function(zs, semaine, maladie, cas) tibble::tibble(zs = zs, semaine = semaine, maladie = maladie, cas = cas)

test_that("le motif de fichier lit la semaine des deux noms, et ignore les bases annuelles", {
  motif <- yaml::read_yaml(here::here("config.yml"))$donnees$motif_fichier
  noms <- c("Base_DPS_Ituri_Week43.xlsx", "IDSR Week44.xlsx", "IDSR week 45.xlsx")
  expect_true(all(grepl(motif, noms)))
  expect_equal(as.integer(sub(motif, "\\1", noms)), 43:45)
  expect_false(any(grepl(motif, c("Base_DPS_Ituri_2024.xlsx", "IDSR Week44.xlsx.bak", "~$IDSR Week44.xlsx"))))
})

test_that("une zone absente d'une des deux semaines ne compte pas dans la tendance", {
  d <- dplyr::bind_rows(
    ligne("A", 1L, "PALUDISME CONF", 100), ligne("A", 1L, "PALUDISME SUSP", 200),
    ligne("A", 4L, "PALUDISME CONF", 80), ligne("A", 4L, "PALUDISME SUSP", 200),
    ligne("B", 4L, "PALUDISME CONF", 50), ligne("B", 4L, "PALUDISME SUSP", 60),
    ligne("C", 1L, "PALUDISME CONF", 30), ligne("C", 1L, "PALUDISME SUSP", 40)
  )
  e <- evolution_palu(d, 1L, 4L)
  expect_equal(e$n_zones, 1L)
  expect_equal(c(e$conf1, e$conf2, e$susp1, e$susp2), c(100, 80, 200, 200))
})

test_that("une zone ayant rapporté sans ligne pour la maladie compte pour zéro", {
  d <- dplyr::bind_rows(
    ligne("A", 1L, "PALUDISME SUSP", 10), ligne("A", 1L, "PALUDISME CONF", 5),
    ligne("A", 4L, "PALUDISME SUSP", 10)
  )
  e <- evolution_palu(d, 1L, 4L)
  expect_equal(e$n_zones, 1L)
  expect_equal(e$conf2, 0)
})

test_that("aucune zone commune donne des totaux nuls", {
  d <- dplyr::bind_rows(ligne("A", 1L, "PALUDISME CONF", 5), ligne("B", 4L, "PALUDISME CONF", 5))
  e <- evolution_palu(d, 1L, 4L)
  expect_equal(e$n_zones, 0L)
  expect_equal(e$conf1, 0)
})

test_that("une zone avec des semaines manquantes n'est pas déclarée sans cas", {
  d <- dplyr::bind_rows(
    # A : rapporte chaque semaine, sans rougeole.
    ligne("A", 1:4, "IRA", 1),
    # B : ne rapporte pas en semaines 1 et 4.
    ligne("B", 2:3, "IRA", 1),
    # C : un cas de rougeole malgré une semaine manquante.
    ligne("C", 1:3, "IRA", 1), ligne("C", 2L, "ROUGEOLE", 2),
    # D : rapporte chaque semaine, rougeole à zéro.
    ligne("D", 1:4, "ROUGEOLE", 0)
  )
  r <- zones_sans_cas(d, "ROUGEOLE", c("A", "B", "C", "D", "E"), 4L)
  expect_equal(r$completes, c("A", "D"))
  expect_equal(names(r$incompletes), c("B", "E"))
  expect_equal(r$incompletes$B, c(1L, 4L))
  expect_equal(r$incompletes$E, 1:4)
})

test_that("sans zone concernée, les deux listes sont vides", {
  r <- zones_sans_cas(ligne("A", 1:4, "ROUGEOLE", 1), "ROUGEOLE", "A", 4L)
  expect_length(r$completes, 0)
  expect_length(r$incompletes, 0)
})
