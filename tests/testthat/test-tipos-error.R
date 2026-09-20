library(testthat)
library(tibble)

source(testthat::test_path("..", "..", "R", "01_alineamiento.R"))
source(testthat::test_path("..", "..", "R", "08_tipos_error.R"))

test_that("tipo_sustitucion distingue acento añadido, perdido y cambiado", {
  expect_equal(tipo_sustitucion("i", "í"), "acento añadido")
  expect_equal(tipo_sustitucion("é", "e"), "acento perdido")
  expect_equal(tipo_sustitucion("ñ", "n"), "acento perdido")
  expect_equal(tipo_sustitucion("á", "à"), "acento cambiado")
})

test_that("tipo_sustitucion distingue mayúscula/minúscula, letra/dígito, espacio y puntuación", {
  expect_equal(tipo_sustitucion("s", "S"), "mayúscula/minúscula")
  expect_equal(tipo_sustitucion("É", "e"), "acento perdido")
  expect_equal(tipo_sustitucion("l", "1"), "letra/dígito")
  expect_equal(tipo_sustitucion("0", "O"), "letra/dígito")
  expect_equal(tipo_sustitucion("n", " "), "espacio")
  expect_equal(tipo_sustitucion(".", ","), "puntuación/símbolo")
  expect_equal(tipo_sustitucion("a", "o"), "letra por otra letra")
})

test_that("errores_por_tipo clasifica sustituciones, omisiones e inserciones", {
  df <- tibble(
    motor = "m", doc_id = "1",
    referencia = "dia, casa",
    hipotesis = "día casa."
  )
  e <- errores_por_tipo(df)

  expect_true(any(e$operacion == "sustitución" & e$tipo == "acento añadido"))
  expect_true(any(e$operacion == "omisión" & e$tipo == "puntuación/símbolo" & e$ref_char == ","))
  expect_true(any(e$operacion == "inserción" & e$tipo == "puntuación/símbolo" & e$hip_char == "."))
})

test_that("resumen_tipos_error suma el total de errores del alineamiento", {
  df <- tibble(motor = "m", doc_id = "1", referencia = "Correos y otros", hipotesis = "Comercio y otras")
  e <- errores_por_tipo(df)
  r <- resumen_tipos_error(e)

  expect_equal(sum(r$n), nrow(e))
  expect_equal(sum(r$pct_errores), 1)
})

test_that("grupo_sustitucion agrupa comillas, tildes, letra por dígito, letras y puntuación", {
  expect_equal(grupo_sustitucion('"', "„"), "Comillas y apóstrofos")
  expect_equal(grupo_sustitucion("'", "’"), "Comillas y apóstrofos")
  expect_equal(grupo_sustitucion("i", "í"), "Tildes y diacríticos")
  expect_equal(grupo_sustitucion("l", "1"), "Letra por dígito")
  expect_equal(grupo_sustitucion("a", "o"), "Confusión entre letras")
  expect_equal(grupo_sustitucion("s", "S"), "Confusión entre letras")
  expect_equal(grupo_sustitucion(".", ","), "Puntuación")
  expect_equal(grupo_sustitucion("n", " "), "Otras")
})

test_that("nombre_caracter da nombre legible a comillas y deja las letras igual", {
  expect_equal(nombre_caracter(c('"', "'", "“", "”", " ", "a")),
               c("comilla recta", "apóstrofo recto", "comilla curva izq.", "comilla curva der.", "espacio", "a"))
})
