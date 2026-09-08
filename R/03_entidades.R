library(tidyverse)
library(quanteda)

#' Meses del español, para el patrón de fecha ("23 de Marzo de 1793")
meses_regex <- paste(
  c(
    "enero", "febrero", "marzo", "abril", "mayo", "junio", "julio",
    "agosto", "septiembre", "setiembre", "octubre", "noviembre", "diciembre"
  ),
  collapse = "|"
)

#' Diccionario de exclusión para nombres propios
#'
#' Vocabulario institucional/administrativo que aparece capitalizado a mitad
#' de oración pero no es un nombre propio de persona o lugar (p.ej. "Junta",
#' "Ministerio", "Tribunal"). Se construye leyendo el corpus de referencia y
#' separando manualmente instituciones/cargos de nombres reales; es la
#' alternativa a un gazetteer externo, que cubre mal nombres de documentos
#' administrativos históricos.
#'
#' El resto de las categorías (fecha, número, código administrativo) se
#' detectan con patrones estructurales (regex) y no requieren diccionario,
#' salvo el listado cerrado de meses de arriba.
dic_exclusion_nombres <- dictionary(list(
  institucional = c(
    "Marina", "Oficio", "Señor", "Presidente", "Ministro", "Ministerio",
    "Cuenta", "Cuentas", "Tribunal", "Junta", "Contaduria", "Contaduría",
    "Esquadra", "Escuadra", "Ordenanza", "Dios", "Oficial", "Tierra",
    "Guarda-Costas"
  )
))

#' Reemplaza en `texto` las coincidencias de `patron` por "#" (mismo largo)
#'
#' Sirve para que un extractor no vuelva a capturar lo que ya clasificó un
#' extractor anterior (p.ej. que "1793" dentro de una fecha ya extraída no se
#' cuente después como número suelto), preservando las posiciones del resto
#' de la cadena para los patrones que corren a continuación.
#'
#' @return list(texto = cadena enmascarada, coincidencias = subcadenas
#'   originales encontradas).
enmascarar_spans <- function(texto, patron) {
  m <- stringr::str_locate_all(texto, patron)[[1]]
  if (nrow(m) == 0) {
    return(list(texto = texto, coincidencias = character(0)))
  }

  coincidencias <- stringr::str_sub(texto, m[, "start"], m[, "end"])
  texto_enmascarado <- texto
  for (i in seq_len(nrow(m))) {
    largo <- m[i, "end"] - m[i, "start"] + 1
    stringr::str_sub(texto_enmascarado, m[i, "start"], m[i, "end"]) <- strrep("#", largo)
  }

  list(texto = texto_enmascarado, coincidencias = coincidencias)
}

#' Clasifica las entidades administrativas de un texto de referencia
#'
#' Extrae, en orden de prioridad (para evitar que una categoría capture lo
#' que ya capturó otra): fechas, códigos administrativos y números por
#' patrones estructurales (regex); y nombres propios tokenizando con
#' quanteda lo que queda sin clasificar, descartando la primera palabra de
#' cada oración (mayúscula por posición, no por ser nombre propio) y el
#' vocabulario institucional de `dic_exclusion_nombres`.
#'
#' Pensada para aplicarse solo sobre la referencia (transcripción manual):
#' la hipótesis de un motor OCR puede tener la capitalización corrompida,
#' que es justo la señal que usa la heurística de nombres propios.
#'
#' @param texto Cadena de referencia (transcripción manual).
#' @return Tibble con columnas `entidad` (subcadena encontrada) y
#'   `categoria` (fecha, numero, codigo_administrativo, nombre_propio).
clasificar_entidades <- function(texto) {
  patron_fecha <- regex(
    paste0("\\d{1,2}\\.?\\s+de\\s+(", meses_regex, ")\\s+de\\s+\\d{3,4}"),
    ignore_case = TRUE
  )
  patron_codigo <- regex("\\b(?:[A-Za-zÁÉÍÓÚÑ]{1,4}\\.){1,2}[A-Za-zÁÉÍÓÚÑ]{0,4}\\.?")
  patron_numero <- regex("\\b\\d+\\b")

  paso_fecha <- enmascarar_spans(texto, patron_fecha)
  paso_codigo <- enmascarar_spans(paso_fecha$texto, patron_codigo)
  paso_numero <- enmascarar_spans(paso_codigo$texto, patron_numero)

  entidades_regex <- bind_rows(
    tibble(entidad = paso_fecha$coincidencias, categoria = "fecha"),
    tibble(entidad = paso_codigo$coincidencias, categoria = "codigo_administrativo"),
    tibble(entidad = paso_numero$coincidencias, categoria = "numero")
  )

  oraciones <- as.character(tokens(paso_numero$texto, what = "sentence")[[1]])
  candidatos <- oraciones %>%
    map(function(oracion) {
      palabras <- as.character(tokens(oracion, what = "word", remove_punct = TRUE)[[1]])
      if (length(palabras) <= 1) character(0) else palabras[-1]
    }) %>%
    unlist()

  es_candidato <- grepl("^[A-ZÁÉÍÓÚÑ]", candidatos) &
    nchar(candidatos) > 1 &
    !grepl("^#+$", candidatos) &
    !(tolower(candidatos) %in% dic_exclusion_nombres[["institucional"]])
  candidatos <- candidatos[es_candidato]

  entidades_nombres <- tibble(entidad = candidatos, categoria = "nombre_propio")

  bind_rows(entidades_regex, entidades_nombres) %>%
    filter(!is.na(entidad), entidad != "")
}

#' Calcula la tasa de preservación de entidades administrativas por motor
#'
#' Para cada documento clasifica las entidades de la referencia con
#' `clasificar_entidades()` y verifica, para cada una, si aparece
#' literalmente (sin distinguir mayúsculas/minúsculas) en la hipótesis del
#' motor. La tasa de preservación por categoría es la proporción de
#' entidades de la referencia que se encuentran así en la hipótesis.
#'
#' @param df Data frame con al menos las columnas `motor`, `referencia`,
#'   `hipotesis` (una fila por segmento/documento).
#' @return Tibble con columnas `motor`, `categoria`, `tokens_totales`,
#'   `tokens_preservados`, `tasa_preservacion`.
preservacion_entidades <- function(df) {
  df %>%
    mutate(entidades = map(referencia, clasificar_entidades)) %>%
    select(motor, hipotesis, entidades) %>%
    unnest(entidades) %>%
    mutate(
      preservado = str_detect(str_to_lower(hipotesis), fixed(str_to_lower(entidad)))
    ) %>%
    group_by(motor, categoria) %>%
    summarise(
      tokens_totales = n(),
      tokens_preservados = sum(preservado),
      tasa_preservacion = mean(preservado),
      .groups = "drop"
    )
}

#' Grafica la tasa de preservación de entidades administrativas por motor
#'
#' @param tabla Tibble producido por `preservacion_entidades()`, con
#'   columnas `motor`, `categoria`, `tasa_preservacion`.
#' @return Un objeto ggplot de barras, una barra por motor dentro de cada
#'   categoría de entidad.
graficar_preservacion_entidades <- function(tabla) {
  ggplot(tabla, aes(x = categoria, y = tasa_preservacion, fill = motor)) +
    geom_col(position = "dodge") +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    scale_fill_viridis_d() +
    labs(
      title = "Preservación de entidades administrativas por motor",
      x = "Categoría de entidad",
      y = "Tasa de preservación",
      fill = "Motor"
    ) +
    theme_minimal()
}
