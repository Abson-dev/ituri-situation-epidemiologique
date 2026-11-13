library(testthat)
for (f in list.files(here::here("R"), full.names = TRUE)) source(f, encoding = "UTF-8")

# Petit classeur Excel 2.x écrit octet par octet : BOF, cellules, EOF.
enregistrement <- function(type, donnees) c(writeBin(as.integer(type), raw(), size = 2, endian = "little"),
                                            writeBin(length(donnees), raw(), size = 2, endian = "little"), donnees)
cellule <- function(ligne, col) c(writeBin(as.integer(c(ligne, col)), raw(), size = 2, endian = "little"), as.raw(c(0, 0, 0)))
nombre <- function(ligne, col, x) enregistrement(0x0003, c(cellule(ligne, col), writeBin(x, raw(), size = 8, endian = "little")))
texte <- function(ligne, col, x) enregistrement(0x0004, c(cellule(ligne, col), as.raw(nchar(x, "bytes")), charToRaw(x)))
ecrire_biff2 <- function(...) {
  chemin <- tempfile(fileext = ".xls")
  writeBin(c(enregistrement(0x0009, as.raw(c(0, 0, 0x10, 0))), ..., enregistrement(0x000A, raw())), chemin)
  chemin
}

test_that("un classeur Excel 2.x se lit avec les noms, les types et les dates", {
  chemin <- ecrire_biff2(
    texte(0, 0, "zs"), texte(0, 1, "numsem"), texte(0, 2, "debutsem"), texte(0, 3, "totalcas"),
    texte(1, 0, "  BUNIA "), nombre(1, 1, 1), nombre(1, 2, 46020), nombre(1, 3, 12.5),
    nombre(2, 0, NaN), nombre(2, 1, 2), nombre(2, 2, 46027), texte(2, 3, ""),
    texte(3, 0, "ARU"), nombre(3, 1, 3), nombre(3, 2, 46034)
  )
  x <- lire_classeur(chemin)
  expect_equal(names(x), c("zs", "numsem", "debutsem", "totalcas"))
  expect_equal(x$zs, c("BUNIA", NA, "ARU"))
  expect_equal(x$numsem, c(1, 2, 3))
  expect_equal(as.Date(x$debutsem), as.Date(c("2025-12-29", "2026-01-05", "2026-01-12")))
  expect_equal(x$totalcas, c(12.5, NA, NA))
})

test_that("un type de cellule inconnu arrête la lecture plutôt que de perdre des valeurs", {
  chemin <- ecrire_biff2(texte(0, 0, "zs"), texte(1, 0, "ADI"), enregistrement(0x0006, as.raw(rep(0, 20))))
  expect_error(lire_classeur(chemin), "non prises en charge")
})

test_that("un fichier ni xls, ni xlsx, ni Excel 2.x est refusé avec une consigne", {
  chemin <- tempfile(fileext = ".xls")
  writeLines("pas un classeur", chemin)
  expect_error(lire_classeur(chemin), "enregistrer au format .xlsx")
})
