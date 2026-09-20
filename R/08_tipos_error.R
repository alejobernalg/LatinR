library(tidyverse)

# Depende de `alinear_pares()` (R/01_alineamiento.R); quien use este archivo
# debe cargarlo antes (mismo criterio que R/05_estabilidad.R).

#' Base sin diacríticos de un carácter ("á" -> "a", "ñ" -> "n", "ü" -> "u")
base_sin_acento <- function(x) {
  stringi::stri_trans_general(x, "Latin-ASCII")
}

#' Clase de un carácter, para clasificar omisiones e inserciones
clase_caracter <- function(x) {
  case_when(
    x == " " ~ "espacio",
    str_detect(x, "^\\p{N}$") ~ "dígito",
    str_detect(x, "^\\p{L}$") & x != base_sin_acento(x) ~ "letra acentuada",
    str_detect(x, "^\\p{L}$") ~ "letra",
    TRUE ~ "puntuación/símbolo"
  )
}

#' Tipo de una sustitución de carácter (referencia -> hipótesis)
#'
#' Categorías, en este orden de prioridad:
#' - `mayúscula/minúscula`: la misma letra con otra capitalización.
#' - `acento perdido`, `acento añadido`, `acento cambiado`: la misma letra
#'   base (ignorando mayúsculas) con y sin diacrítico, o con otro diacrítico.
#' - `letra/dígito`: un dígito por una letra o al revés (p.ej. l/1, O/0).
#' - `espacio`: un espacio por otro carácter o al revés.
#' - `puntuación/símbolo`: alguno de los dos es signo o símbolo.
#' - `letra por otra letra`: dos letras distintas.
tipo_sustitucion <- function(ref_char, hip_char) {
  r <- str_to_lower(ref_char)
  h <- str_to_lower(hip_char)
  br <- base_sin_acento(r)
  bh <- base_sin_acento(h)
  letra_r <- str_detect(ref_char, "^\\p{L}$")
  letra_h <- str_detect(hip_char, "^\\p{L}$")
  digito_r <- str_detect(ref_char, "^\\p{N}$")
  digito_h <- str_detect(hip_char, "^\\p{N}$")

  case_when(
    letra_r & letra_h & r == h ~ "mayúscula/minúscula",
    letra_r & letra_h & br == bh & r == br & h != bh ~ "acento añadido",
    letra_r & letra_h & br == bh & r != br & h == bh ~ "acento perdido",
    letra_r & letra_h & br == bh ~ "acento cambiado",
    (letra_r & digito_h) | (digito_r & letra_h) ~ "letra/dígito",
    ref_char == " " | hip_char == " " ~ "espacio",
    letra_r & letra_h ~ "letra por otra letra",
    TRUE ~ "puntuación/símbolo"
  )
}

#' Comillas y apóstrofos (rectos, curvos y bajos), para agrupar sus confusiones
caracteres_comilla <- c('"', "'", "‘", "’", "“", "”", "„", "«", "»", "´", "`", "′", "″")

#' Grupo de una sustitución, para leer las confusiones más frecuentes
#'
#' Reagrupa `tipo_sustitucion()` en seis grupos: confusión entre letras,
#' tildes y diacríticos, letra por dígito, comillas y apóstrofos, puntuación y
#' otras (espacios). Solo cambia la lectura; los conteos son los mismos.
grupo_sustitucion <- function(ref_char, hip_char) {
  tipo <- tipo_sustitucion(ref_char, hip_char)
  case_when(
    ref_char %in% caracteres_comilla & hip_char %in% caracteres_comilla ~ "Comillas y apóstrofos",
    str_detect(tipo, "^acento") ~ "Tildes y diacríticos",
    tipo == "letra/dígito" ~ "Letra por dígito",
    tipo %in% c("letra por otra letra", "mayúscula/minúscula") ~ "Confusión entre letras",
    tipo == "puntuación/símbolo" ~ "Puntuación",
    TRUE ~ "Otras"
  )
}

#' Nombre legible de un carácter ("\"" -> "comilla recta")
nombre_caracter <- function(x) {
  nombres <- c(
    '"' = "comilla recta", "'" = "apóstrofo recto",
    "“" = "comilla curva izq.", "”" = "comilla curva der.",
    "‘" = "apóstrofo curvo izq.", "’" = "apóstrofo curvo der.",
    "„" = "comilla baja", " " = "espacio", "-" = "guion",
    "–" = "semirraya", "—" = "raya"
  )
  out <- unname(nombres[x])
  if_else(is.na(out), x, out)
}

#' Errores de carácter de un corpus, clasificados por tipo
#'
#' Alinea cada documento con `alinear_pares()` y, para cada error (todo lo
#' que no es match), asigna la operación (sustitución, omisión, inserción) y
#' un tipo. En una omisión o inserción el tipo es la clase del carácter
#' omitido o insertado.
#'
#' @param df Data frame con `motor`, `doc_id`, `referencia`, `hipotesis`.
#' @return Tibble con una fila por error: `motor`, `doc_id`, `operacion`,
#'   `tipo`, `ref_char`, `hip_char`.
errores_por_tipo <- function(df) {
  df %>%
    mutate(alineado = map2(referencia, hipotesis, alinear_pares)) %>%
    select(motor, doc_id, alineado) %>%
    unnest(alineado) %>%
    filter(operacion != "M") %>%
    mutate(
      tipo = case_when(
        operacion == "S" ~ tipo_sustitucion(ref_char, hip_char),
        operacion == "D" ~ clase_caracter(ref_char),
        operacion == "I" ~ clase_caracter(hip_char)
      ),
      operacion = recode(
        operacion, S = "sustitución", D = "omisión", I = "inserción"
      )
    ) %>%
    select(motor, doc_id, operacion, tipo, ref_char, hip_char)
}

#' Resume los errores por operación y tipo
#'
#' @param errores Tibble producido por `errores_por_tipo()`.
#' @param n_ref Caracteres de referencia por motor (tibble `motor`, `n_ref`),
#'   para expresar cada tipo como tasa sobre los caracteres de la referencia.
#' @return Tibble con `motor`, `operacion`, `tipo`, `n`, `pct_errores`
#'   (porcentaje del total de errores del motor) y, si se da `n_ref`,
#'   `tasa_por_100_car` (errores de ese tipo por cada 100 caracteres de la
#'   referencia).
resumen_tipos_error <- function(errores, n_ref = NULL) {
  res <- errores %>%
    count(motor, operacion, tipo, name = "n") %>%
    group_by(motor) %>%
    mutate(pct_errores = n / sum(n)) %>%
    ungroup() %>%
    arrange(motor, desc(n))

  if (!is.null(n_ref)) {
    res <- res %>%
      left_join(n_ref, by = "motor") %>%
      mutate(tasa_por_100_car = 100 * n / n_ref) %>%
      select(-n_ref)
  }
  res
}

#' Confusiones más frecuentes de cada tipo
#'
#' Los espacios se muestran como "␣" y la ausencia (omisión o inserción)
#' como "∅", para que se vean en la tabla.
#'
#' @param errores Tibble producido por `errores_por_tipo()`.
#' @param top Cuántas confusiones conservar por operación y tipo.
confusiones_frecuentes <- function(errores, top = 5) {
  vis <- function(x) case_when(is.na(x) ~ "∅", x == " " ~ "␣", TRUE ~ x)

  errores %>%
    mutate(ref_char = vis(ref_char), hip_char = vis(hip_char)) %>%
    count(motor, operacion, tipo, ref_char, hip_char, name = "n") %>%
    group_by(motor, operacion, tipo) %>%
    slice_max(n, n = top, with_ties = FALSE) %>%
    ungroup() %>%
    arrange(motor, operacion, tipo, desc(n))
}

#' Grafica los errores por tipo, coloreados por operación
#'
#' @param resumen Tibble producido por `resumen_tipos_error()`.
graficar_tipos_error <- function(resumen) {
  resumen %>%
    mutate(tipo = fct_reorder(tipo, n, .fun = sum)) %>%
    ggplot(aes(x = n, y = tipo, fill = operacion)) +
    geom_col() +
    facet_wrap(~motor) +
    scale_fill_viridis_d(end = 0.85) +
    labs(
      title = "Errores de carácter por tipo",
      x = "Número de errores",
      y = NULL,
      fill = "Operación"
    ) +
    theme_minimal()
}
