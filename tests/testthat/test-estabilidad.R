library(testthat)
library(tibble)

source(testthat::test_path("..", "..", "R", "01_alineamiento.R"))
source(testthat::test_path("..", "..", "R", "05_estabilidad.R"))

test_that("cer_documento da 0 cuando referencia e hipótesis son iguales", {
  expect_equal(cer_documento("abc", "abc"), 0)
})

test_that("cer_documento cuenta una sustitución sobre el largo de la referencia", {
  expect_equal(cer_documento("abc", "abd"), 1 / 3)
})

test_that("cer_documento cuenta una eliminación sobre el largo de la referencia", {
  expect_equal(cer_documento("abc", "ab"), 1 / 3)
})

test_that("cer_documento divide una inserción por los caracteres de referencia, no de hipótesis", {
  expect_equal(cer_documento("ab", "abc"), 1 / 2)
})

test_that("tasa_error_documento produce una fila por documento con columnas motor, doc_id, cer", {
  df <- tibble(
    motor = c("tesseract", "tesseract"),
    doc_id = c("1", "2"),
    referencia = c("abc", "abc"),
    hipotesis = c("abc", "abd")
  )

  res <- tasa_error_documento(df)

  expect_equal(names(res), c("motor", "doc_id", "cer"))
  expect_equal(res$cer, c(0, 1 / 3))
})

test_that("resumen_estabilidad distingue un motor estable de uno con un documento atípico", {
  tabla_cer <- tibble(
    motor = c(rep("estable", 4), rep("inestable", 4)),
    doc_id = as.character(1:8),
    cer = c(0.10, 0.11, 0.09, 0.10, 0.05, 0.05, 0.05, 0.40)
  )

  res <- resumen_estabilidad(tabla_cer)
  estable <- res[res$motor == "estable", ]
  inestable <- res[res$motor == "inestable", ]

  expect_equal(estable$n_documentos, 4)
  expect_lt(estable$de, inestable$de)
  expect_equal(estable$mediana, median(c(0.10, 0.11, 0.09, 0.10)))
})

test_that("resumen_estabilidad devuelve NA de asimetría con menos de 3 documentos", {
  tabla_cer <- tibble(
    motor = c("olmo", "olmo"),
    doc_id = c("1", "2"),
    cer = c(0.1, 0.2)
  )

  res <- resumen_estabilidad(tabla_cer)

  expect_true(is.na(res$asimetria))
})
