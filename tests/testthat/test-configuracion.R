library(testthat)
library(tibble)

source(testthat::test_path("..", "..", "R", "05_estabilidad.R"))
source(testthat::test_path("..", "..", "R", "10_configuracion.R"))

test_that("leer_configuracion exige referencia y motores", {
  ruta <- withr::local_tempfile(fileext = ".yml")
  writeLines("titulo: x", ruta)
  expect_error(leer_configuracion(ruta), "referencia, motores")
})

test_that("cargar_corpus arma un documento por línea y motor", {
  dir <- withr::local_tempdir()
  writeLines(c("Uno", "Dos", "Tres"), file.path(dir, "ref.txt"))
  writeLines(c("Uno", "Dos", "Tres"), file.path(dir, "a.txt"))
  writeLines(c("Uno", "Dós", "Tres"), file.path(dir, "b.txt"))
  writeLines(c(
    "referencia: ref.txt",
    "motores:",
    "  - {nombre: a, salida: a.txt}",
    "  - {nombre: b, salida: b.txt}"
  ), file.path(dir, "cfg.yml"))

  cfg <- leer_configuracion(file.path(dir, "cfg.yml"))
  corpus <- cargar_corpus(cfg, dir_base = dir)

  expect_equal(nrow(corpus), 6)
  expect_equal(sort(unique(corpus$motor)), c("a", "b"))
  expect_false(cfg$unir_continuaciones)
})
