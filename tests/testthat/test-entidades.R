library(testthat)
library(tibble)

source(testthat::test_path("..", "..", "R", "03_entidades.R"))

test_that("clasificar_entidades detecta una fecha completa", {
  res <- clasificar_entidades("Cartagena 23. de Marzo de 1793.")
  fechas <- res[res$categoria == "fecha", ]

  expect_true(any(grepl("23\\. de Marzo de 1793", fechas$entidad)))
})

test_that("clasificar_entidades no cuenta como número suelto un año ya capturado en una fecha", {
  res <- clasificar_entidades("Cartagena 23. de Marzo de 1793.")
  numeros <- res[res$categoria == "numero", ]

  expect_false(any(grepl("1793", numeros$entidad)))
})

test_that("clasificar_entidades detecta números fuera de fechas", {
  res <- clasificar_entidades("el año de mil novecientos, folio 45 del expediente")
  numeros <- res[res$categoria == "numero", ]

  expect_true(any(numeros$entidad == "45"))
})

test_that("clasificar_entidades detecta códigos administrativos tipo abreviatura", {
  res <- clasificar_entidades("D.n Francisco Sandoval y V.S. se sirva citar")
  codigos <- res[res$categoria == "codigo_administrativo", ]

  expect_true(any(codigos$entidad == "D.n"))
  expect_true(any(codigos$entidad == "V.S."))
})

test_that("clasificar_entidades detecta nombres propios y excluye vocabulario institucional", {
  res <- clasificar_entidades("En Junta de Marina se leyó un Oficio de Francisco Sandoval")
  nombres <- res[res$categoria == "nombre_propio", ]

  expect_true(any(nombres$entidad == "Francisco"))
  expect_true(any(nombres$entidad == "Sandoval"))
  expect_false(any(nombres$entidad == "Junta"))
  expect_false(any(nombres$entidad == "Marina"))
  expect_false(any(nombres$entidad == "Oficio"))
})

test_that("clasificar_entidades no marca la primera palabra de una oración como nombre propio", {
  res <- clasificar_entidades("El Tribunal mayor de Cuentas me está apurando.")
  nombres <- res[res$categoria == "nombre_propio", ]

  expect_false(any(nombres$entidad == "El"))
})

test_that("preservacion_entidades calcula la tasa de preservación por motor y categoría", {
  df <- tibble(
    motor = "olmo",
    referencia = "D.n Francisco Sandoval, Cartagena 23. de Marzo de 1793.",
    hipotesis = "Dn. Francisco Sandoval, Cartagena 23 de Marzo de 1793."
  )

  tabla <- preservacion_entidades(df)

  expect_true(all(c("motor", "categoria", "tokens_totales", "tokens_preservados", "tasa_preservacion") %in% names(tabla)))

  fila_nombre <- tabla[tabla$categoria == "nombre_propio", ]
  expect_equal(fila_nombre$tasa_preservacion, 1)

  fila_codigo <- tabla[tabla$categoria == "codigo_administrativo", ]
  expect_equal(fila_codigo$tasa_preservacion, 0)
})
