library(testthat)
library(tibble)

source(testthat::test_path("..", "..", "R", "01_alineamiento.R"))

test_that("alinear_pares detecta sustituciones r->m y e->c en 'Correos' -> 'Comercio'", {
  res <- alinear_pares("Correos", "Comercio")
  sustituciones <- res[res$operacion == "S", ]

  expect_true(any(sustituciones$ref_char == "r" & sustituciones$hip_char == "m"))
  expect_true(any(sustituciones$ref_char == "e" & sustituciones$hip_char == "c"))
})

test_that("alinear_pares detecta la sustitución ú->u en 'número' -> 'numero'", {
  res <- alinear_pares("número", "numero")
  sustituciones <- res[res$operacion == "S", ]

  expect_true(any(sustituciones$ref_char == "ú" & sustituciones$hip_char == "u"))
})

test_that("alinear_pares detecta la sustitución ñ->h en 'Año 1943' -> 'Aho 1943'", {
  res <- alinear_pares("Año 1943", "Aho 1943")
  sustituciones <- res[res$operacion == "S", ]

  expect_true(any(sustituciones$ref_char == "ñ" & sustituciones$hip_char == "h"))
})

test_that("matriz_confusion cuenta sustituciones agrupadas por motor", {
  df <- tibble(
    motor = c("tesseract", "tesseract", "easyocr"),
    referencia = c("número", "Año 1943", "Correos"),
    hipotesis = c("numero", "Aho 1943", "Comercio")
  )

  mc <- matriz_confusion(df)

  expect_true(all(c("motor", "ref_char", "hip_char", "n") %in% names(mc)))
  expect_true(any(mc$motor == "tesseract" & mc$ref_char == "ú" & mc$hip_char == "u"))
  expect_true(any(mc$motor == "tesseract" & mc$ref_char == "ñ" & mc$hip_char == "h"))
  expect_true(any(mc$motor == "easyocr" & mc$ref_char == "r" & mc$hip_char == "m"))
  expect_true(all(mc$n == sort(mc$n, decreasing = TRUE)))
})
