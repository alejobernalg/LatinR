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

  fila_fecha <- tabla[tabla$categoria == "fecha", ]
  expect_equal(fila_fecha$tasa_preservacion, 1)
})

test_that("clasificar_entidades detecta fechas con abreviaturas y años con puntos o guiones", {
  txt <- "Medellín oct. 28 - 1940. Carta. N.Y. Marzo 4 1.941 Hola. Bogotá Abril 28 de 1941 Adiós. San Agustin - 6 de sept. 1-43 Fin."
  fechas <- clasificar_entidades(txt)
  fechas <- fechas$entidad[fechas$categoria == "fecha"]
  expect_equal(fechas, c("oct. 28 - 1940", "Marzo 4 1.941", "Abril 28 de 1941", "6 de sept. 1-43"))
})

test_that("preservacion_entidades exige que día, mes y año de la fecha coincidan en su línea", {
  df <- tibble(
    motor = "olmo",
    referencia = "Bogotá Abril 28 de 1941 Sr Juan\nMedellin Nov. 1941 Hola",
    hipotesis = "Bogotá, Abril 28 de 1951 Sr. Juan\nMedellín Nov. 1941 Hola"
  )
  tabla <- preservacion_entidades(df)
  fila <- tabla[tabla$categoria == "fecha", ]
  # 1951 en la línea 1 no se salva por el "1941" de la línea 2
  expect_equal(fila$tokens_totales, 2)
  expect_equal(fila$tokens_preservados, 1)
})

test_that("clasificar_entidades no toma palabras comunes con punto final como códigos", {
  res <- clasificar_entidades("No dijo nada. Vivo bien. Ud. y V.S. etc. Sr. D. Juan, E.S.M. y S.or")
  codigos <- res$entidad[res$categoria == "codigo_administrativo"]

  expect_false(any(c("nada.", "bien.") %in% codigos))
  expect_true(all(c("Ud.", "V.S.", "etc.", "Sr.", "D.", "E.S.M.", "S.or") %in% codigos))
})

test_that("clasificar_entidades detecta fechas sin año y números con ordinal", {
  res <- clasificar_entidades("Contesto su carta del 28 de agosto. Bogotá Dic - 21 Muy bien. Fiesta del 1º de Mayo")
  fechas <- res$entidad[res$categoria == "fecha"]
  numeros <- res$entidad[res$categoria == "numero"]

  expect_true(all(c("28 de agosto", "Dic - 21") %in% fechas))
  expect_true("1º" %in% numeros)
  expect_false("28" %in% numeros)
})

test_that("clasificar_entidades no cuenta como nombre propio saludos, tratamientos ni inicios de oración", {
  res <- clasificar_entidades("Bogotá Muy estimado amigo: Recibí su carta. Realmente, Sr. Juan Friede, Ud. Dijo: Nada. Vi a Eduardo Zalamea.")
  nombres <- res$entidad[res$categoria == "nombre_propio"]

  expect_true(all(c("Juan", "Friede", "Eduardo", "Zalamea") %in% nombres))
  expect_false(any(c("Muy", "Recibí", "Nada", "Realmente", "Ud", "Sr") %in% nombres))
})

test_that("preservacion_entidades: los acentos y la puntuación cuentan, y no hay coincidencia dentro de otra palabra", {
  df <- tibble(
    motor = "olmo",
    referencia = "Medellin Nov. 1940. Señor Juan Gomez, V.S. y Ud. Bogotá Dic - 21 Muy bien.",
    hipotesis = "Medellín Nov. 1940. Señor Juan Gómez, V.S. y Ud. Bogotá Muy bien 1940."
  )
  tabla <- preservacion_entidades(df)
  fila <- function(cat) tabla[tabla$categoria == cat, ]

  # Medellin (lugar del encabezado) -> Medellín y Gomez -> Gómez: el acento
  # cuenta como error; Juan y Bogotá se preservan
  expect_equal(fila("nombre_propio")$tokens_totales, 4)
  expect_equal(fila("nombre_propio")$tokens_preservados, 2)
  # "Dic - 21" no aparece en la salida: no preservada; Nov. 1940 sí
  expect_equal(fila("fecha")$tokens_preservados, 1)
  expect_equal(fila("fecha")$tokens_totales, 2)
  # V. no se da por preservado dentro de V.S.
  expect_equal(fila("codigo_administrativo")$tasa_preservacion, 1)
})

test_that("clasificar_entidades detecta fechas con 'del', meses abreviados con cifra, ':' y palabras", {
  txt <- paste(
    "Bogotá, Julio 10 del 935 Hola. Bogotá, 6 de 9bre. de 1935 Adiós.",
    "Cali Julio 14.-1935 Fin. Y Enrique Martinez Z. Septiembre 8 : 1949 En 4 fojas.",
    "hoy seis de Septiembre de mil novecientos cuarenta y nueve, con el fin.",
    "Hasta mayo de mil novecientos cuarenta y siete formaron."
  )
  fechas <- clasificar_entidades(txt)
  fechas <- fechas$entidad[fechas$categoria == "fecha"]

  expect_true(all(c(
    "Julio 10 del 935", "6 de 9bre. de 1935", "Julio 14.-1935", "Septiembre 8 : 1949",
    "seis de Septiembre de mil novecientos cuarenta y nueve",
    "mayo de mil novecientos cuarenta y siete"
  ) %in% fechas))
})

test_that("clasificar_entidades toma como nombre el lugar del encabezado, antes de la primera fecha", {
  res <- clasificar_entidades("Cali, Julio 6 de 1935. Señor Doctor Enrique Olaya.")
  nombres <- res$entidad[res$categoria == "nombre_propio"]

  expect_true(all(c("Cali", "Enrique", "Olaya") %in% nombres))
  expect_false(any(c("Señor", "Doctor") %in% nombres))
})

test_that("clasificar_entidades excluye como nombre las palabras que el corpus escribe también en minúscula", {
  texto <- "Le dije que Si atiende, Sea de un modo. Vino Pedro Paz."
  voc <- vocabulario_minusculas(c("si vienes, sea como sea", texto))
  nombres <- clasificar_entidades(texto, vocabulario_comun = voc)
  nombres <- nombres$entidad[nombres$categoria == "nombre_propio"]

  expect_false(any(c("Si", "Sea") %in% nombres))
  expect_true(all(c("Pedro", "Paz") %in% nombres))
})

test_that("clasificar_entidades no toma como nombre una palabra cortada por guion de fin de línea", {
  res <- clasificar_entidades("Fue el último Ca- bildo indígena y Asterio Vi- llota no fue")
  nombres <- res$entidad[res$categoria == "nombre_propio"]

  expect_false(any(c("Ca", "Vi") %in% nombres))
  expect_true("Asterio" %in% nombres)
})

test_that("clasificar_entidades detecta ordinales femeninos y la abreviatura D.ª", {
  res <- clasificar_entidades("a la 2ª.- Señora D.ª Teresa, Calle 3ª #13-87")
  expect_true(all(c("2ª", "3ª", "13", "87") %in% res$entidad[res$categoria == "numero"]))
  expect_true("D.ª" %in% res$entidad[res$categoria == "codigo_administrativo"])
})

test_that("fecha_preservada ignora los espacios alrededor del guion pero no cambia año ni día", {
  expect_true(fecha_preservada("Julio 14.-1935", "Julio 14.-1935 Dr.", "Julio 14 - 1935 Dr."))
  expect_false(fecha_preservada("Julio 10 del 935", "Julio 10 del 935 Mi", "Julio 10 del 925 Mi"))
})
