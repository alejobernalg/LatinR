library(testthat)
library(tibble)

source(testthat::test_path("..", "..", "R", "12_tokens.R"))

corpus_prueba <- tibble(
  motor = "m", doc_id = c("1", "2"),
  referencia = c("El dia es Bueno, el 23.", "Indigena y otro"),
  hipotesis = c("El día es bueno el 23", "Indígena y y otro")
)

test_that("tokens_corpus da un token por fila, en minúscula, sin puntuación y con tildes", {
  t <- tokens_corpus(corpus_prueba)
  ref1 <- t$token[t$fuente == "referencia" & t$doc_id == "1"]
  ocr1 <- t$token[t$fuente == "ocr" & t$doc_id == "1"]

  expect_equal(ref1, c("el", "dia", "es", "bueno", "el", "23"))
  expect_equal(ocr1, c("el", "día", "es", "bueno", "el", "23"))
})

test_that("volumen_tokens compara tokens totales y únicos con la referencia", {
  v <- volumen_tokens(tokens_corpus(corpus_prueba))

  expect_equal(v$tokens_referencia, 9)
  expect_equal(v$tokens_ocr, 10)
  expect_equal(v$razon_tokens, 10 / 9)
  expect_equal(v$unicos_referencia, 8)
})

test_that("palabras_diferentes lista las añadidas y las perdidas", {
  p <- palabras_diferentes(tokens_corpus(corpus_prueba))

  expect_true(all(c("día", "indígena") %in% p$token[p$tipo == "Añadidas por el ICR"]))
  expect_true(all(c("dia", "indigena") %in% p$token[p$tipo == "Perdidas por el ICR"]))
})

test_that("diferencias_solo_tilde cuenta las añadidas que solo cambian de tilde", {
  d <- diferencias_solo_tilde(tokens_corpus(corpus_prueba))

  expect_equal(d$anadidas, 2)
  expect_equal(d$solo_tilde, 2)
})
