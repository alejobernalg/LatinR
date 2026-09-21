library(tidyverse)
library(tidytext)

#' Tokeniza como el WER: secuencias de letras o dígitos, sin puntuación
palabras_lista <- function(x) {
  Encoding(x) <- "UTF-8"
  str_extract_all(x, "[\\p{L}\\p{N}]+")
}

#' Tabla de tokens de un corpus: un token (palabra) por fila
#'
#' Con `tidytext::unnest_tokens()` convierte la referencia y la salida de cada
#' motor en una tabla larga. Usa la misma definición de palabra que el WER
#' (letras y dígitos, sin puntuación) y pasa todo a minúscula; los acentos se
#' conservan.
#'
#' @param df Data frame con `motor`, `doc_id`, `referencia` e `hipotesis`.
#' @return Tibble con `motor`, `doc_id`, `fuente` ("referencia" u "ocr") y
#'   `token`.
tokens_corpus <- function(df) {
  bind_rows(
    df %>% transmute(motor, doc_id, fuente = "referencia", texto = referencia),
    df %>% transmute(motor, doc_id, fuente = "ocr", texto = hipotesis)
  ) %>%
    unnest_tokens(token, texto, token = palabras_lista, to_lower = FALSE) %>%
    mutate(token = str_to_lower(token))
}

#' Volumen de tokens totales y únicos por motor, frente a la referencia
#'
#' @param tokens Tibble producido por `tokens_corpus()`.
#' @return Tibble con `motor`, `tokens_referencia`, `tokens_ocr`,
#'   `razon_tokens`, `unicos_referencia`, `unicos_ocr`, `razon_unicos`.
volumen_tokens <- function(tokens) {
  tokens %>%
    group_by(motor, fuente) %>%
    summarise(total = n(), unicos = n_distinct(token), .groups = "drop") %>%
    pivot_wider(names_from = fuente, values_from = c(total, unicos)) %>%
    transmute(
      motor,
      tokens_referencia = total_referencia, tokens_ocr = total_ocr,
      razon_tokens = total_ocr / total_referencia,
      unicos_referencia = unicos_referencia, unicos_ocr = unicos_ocr,
      razon_unicos = unicos_ocr / unicos_referencia
    )
}

#' Palabras que el motor añade o pierde respecto de la referencia
#'
#' "Añadidas": palabras que aparecen en la salida y no existen en el
#' vocabulario de la referencia. "Perdidas": palabras de la referencia que la
#' salida nunca escribe. `n` es cuántas veces aparecen en su fuente.
#'
#' @param tokens Tibble producido por `tokens_corpus()`.
#' @param top Cuántas palabras conservar por motor y tipo.
#' @return Tibble con `motor`, `tipo`, `token`, `n`.
palabras_diferentes <- function(tokens, top = 10) {
  por_motor <- function(t) {
    ref <- filter(t, fuente == "referencia")
    ocr <- filter(t, fuente == "ocr")
    bind_rows(
      ocr %>% filter(!token %in% ref$token) %>% count(token, sort = TRUE) %>% mutate(tipo = "Añadidas por el ICR"),
      ref %>% filter(!token %in% ocr$token) %>% count(token, sort = TRUE) %>% mutate(tipo = "Perdidas por el ICR")
    )
  }
  tokens %>%
    group_by(motor) %>%
    group_modify(~ por_motor(.x)) %>%
    group_by(motor, tipo) %>%
    slice_max(n, n = top, with_ties = FALSE) %>%
    ungroup() %>%
    select(motor, tipo, token, n)
}

#' Cuántas palabras añadidas son una palabra perdida con otra tilde
#'
#' @return Tibble con `motor`, `anadidas` (palabras distintas añadidas),
#'   `perdidas` y `solo_tilde` (añadidas que coinciden con una perdida al
#'   quitar las tildes).
diferencias_solo_tilde <- function(tokens) {
  sin_tilde <- function(x) stringi::stri_trans_general(x, "Latin-ASCII")
  tokens %>%
    group_by(motor) %>%
    group_modify(function(t, k) {
      ref <- unique(t$token[t$fuente == "referencia"])
      ocr <- unique(t$token[t$fuente == "ocr"])
      anadidas <- setdiff(ocr, ref)
      perdidas <- setdiff(ref, ocr)
      n_solo_tilde <- sum(sin_tilde(anadidas) %in% sin_tilde(perdidas))
      tibble(
        anadidas = length(anadidas), perdidas = length(perdidas),
        solo_tilde = n_solo_tilde
      )
    }) %>%
    ungroup()
}
