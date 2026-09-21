library(testthat)
library(tibble)

source(testthat::test_path("..", "..", "R", "01_alineamiento.R"))
source(testthat::test_path("..", "..", "R", "08_tipos_error.R"))
source(testthat::test_path("..", "..", "R", "11_figuras_reporte.R"))

errores_ejemplo <- function() {
  errores_por_tipo(tibble(
    motor = "m", doc_id = "1",
    referencia = "casa blanca",
    hipotesis = "cosa blancca"
  ))
}

test_that("matriz_confusion_completa registra sustituciones, omisiones e inserciones", {
  mc <- matriz_confusion_completa(errores_ejemplo(), "m")

  expect_true(any(mc$ref == "a" & mc$hip == "o"))
  expect_true(any(mc$ref == "∅" & mc$hip == "c"))
  expect_equal(sum(mc$n), nrow(errores_ejemplo()))
})

test_that("top_omisiones_inserciones separa lo omitido de lo insertado", {
  df <- errores_por_tipo(tibble(
    motor = "m", doc_id = "1", referencia = "abcd", hipotesis = "abxcdd"
  ))
  res <- top_omisiones_inserciones(df, "m")

  expect_true(all(res$operacion %in% c("omisión", "inserción")))
  expect_equal(sum(res$n[res$operacion == "inserción"]), 2)
  expect_equal(sum(res$n[res$operacion == "omisión"]), 0)
})

test_that("grafico_matriz_confusion y grafico_contraste_entidades devuelven un ggplot", {
  mc <- matriz_confusion_completa(errores_ejemplo(), "m")
  expect_s3_class(grafico_matriz_confusion(mc), "ggplot")

  tabla <- tibble(
    categoria = rep(c("fecha", "numero"), 2),
    corpus = rep(c("OCR", "ICR"), each = 2),
    preservadas = c(4, 90, 21, 9), total = c(5, 127, 37, 34)
  )
  expect_s3_class(grafico_contraste_entidades(tabla), "ggplot")
})
