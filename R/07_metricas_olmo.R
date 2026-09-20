library(tidyverse)

if (Sys.getlocale("LC_CTYPE") %in% c("C", "POSIX")) {
  tryCatch(
    Sys.setlocale("LC_CTYPE", "es_ES.UTF-8"),
    warning = function(w) Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
  )
}

source("R/01_alineamiento.R")
source("R/03_entidades.R")
source("R/02_confusion_olmo.R")

#' Errores de caracteres (S, D, I) y CER de un documento
#'
#' Reutiliza `alinear_pares()`, el mismo alineamiento de la matriz de
#' confusión y del CER de R/05_estabilidad.R.
#'
#' @return Tibble de una fila: `n_ref`, `sustituciones`, `eliminaciones`,
#'   `inserciones`, `cer`.
errores_caracter <- function(ref, hip) {
  a <- alinear_pares(ref, hip)
  n_ref <- sum(!is.na(a$ref_char))
  tibble(
    n_ref = n_ref,
    sustituciones = sum(a$operacion == "S"),
    eliminaciones = sum(a$operacion == "D"),
    inserciones = sum(a$operacion == "I"),
    cer = (sustituciones + eliminaciones + inserciones) / n_ref
  )
}

#' Divide un texto en palabras (secuencias de letras/dígitos, sin puntuación)
#' respetando mayúsculas y acentos.
palabras <- function(x) {
  Encoding(x) <- "UTF-8"
  stringr::str_extract_all(x, "[\\p{L}\\p{N}]+")[[1]]
}

#' Errores de palabra (S, D, I) y WER de un documento
#'
#' Cada palabra distinta se codifica como un carácter único para reutilizar
#' `adist(counts = TRUE)` a nivel de palabra.
#'
#' @return Tibble de una fila: `n_ref`, `sustituciones`, `eliminaciones`,
#'   `inserciones`, `wer`.
errores_palabra <- function(ref, hip) {
  p_ref <- palabras(ref)
  p_hip <- palabras(hip)
  vocab <- unique(c(p_ref, p_hip))
  codigo <- function(p) intToUtf8(0x4E00 + match(p, vocab) - 1L)
  s_ref <- paste0(vapply(p_ref, codigo, ""), collapse = "")
  s_hip <- paste0(vapply(p_hip, codigo, ""), collapse = "")

  d <- adist(s_ref, s_hip, counts = TRUE)
  cnt <- attr(d, "counts")[1, 1, ]
  tibble(
    n_ref = length(p_ref),
    sustituciones = unname(cnt["sub"]),
    eliminaciones = unname(cnt["del"]),
    inserciones = unname(cnt["ins"]),
    wer = (sustituciones + eliminaciones + inserciones) / n_ref
  )
}

if (sys.nframe() == 0) {
  # Cada línea de Datos/ es un documento
  df <- cargar_parrafos_olmo()

  cer <- df %>%
    mutate(m = map2(referencia, hipotesis, errores_caracter)) %>%
    select(motor, doc_id, m) %>% unnest(m)
  wer <- df %>%
    mutate(m = map2(referencia, hipotesis, errores_palabra)) %>%
    select(motor, doc_id, m) %>% unnest(m)

  # Total del motor: se suman los errores de todos los documentos
  total <- function(tabla, tasa) {
    tabla %>%
      group_by(motor) %>%
      summarise(
        n_documentos = n(),
        across(c(n_ref, sustituciones, eliminaciones, inserciones), sum),
        "{tasa}" := (sustituciones + eliminaciones + inserciones) / n_ref,
        .groups = "drop"
      )
  }

  cat("\n== CER por documento ==\n"); print(cer, n = Inf)
  cat("\n== CER total ==\n"); print(total(cer, "cer"))
  cat("\n== WER por documento ==\n"); print(wer, n = Inf)
  cat("\n== WER total ==\n"); print(total(wer, "wer"))

  cat("\n== Preservación de entidades ==\n")
  print(preservacion_entidades(df), n = Inf)
}
