library(tidyverse)
library(stringdist)

#' Alinea carácter a carácter una referencia y una hipótesis OCR/ICR
#'
#' Usa el edit-script de Levenshtein expuesto por `adist(..., counts = TRUE)`
#' para reconstruir, carácter a carácter, la operación que transforma la
#' referencia en la hipótesis. Antes de dividir las cadenas en caracteres se
#' fuerza la codificación a UTF-8: en locale no-UTF-8 (o si el string no trae
#' marcada su codificación), `adist()` y `strsplit(x, "")` tratan los
#' caracteres multibyte (á, é, í, ó, ú, ñ) como bytes sueltos en vez de un
#' solo carácter, lo que rompe silenciosamente la alineación justo en los
#' caracteres más relevantes para español administrativo.
#'
#' @param ref Cadena de referencia (transcripción manual).
#' @param hip Cadena de hipótesis (salida del motor OCR/ICR).
#' @return Un tibble con columnas `ref_char`, `hip_char`, `operacion`
#'   (M = match, S = sustitución, D = eliminación, I = inserción). En D,
#'   `hip_char` es NA; en I, `ref_char` es NA.
alinear_pares <- function(ref, hip) {
  Encoding(ref) <- "UTF-8"
  Encoding(hip) <- "UTF-8"

  ref_chars <- strsplit(ref, "")[[1]]
  hip_chars <- strsplit(hip, "")[[1]]

  d <- adist(ref, hip, counts = TRUE)
  trafos <- attr(d, "trafos")[1, 1]
  ops <- strsplit(trafos, "")[[1]]

  n <- length(ops)
  ref_char <- character(n)
  hip_char <- character(n)
  operacion <- character(n)

  i <- 1L
  j <- 1L

  for (k in seq_len(n)) {
    op <- ops[k]
    if (op %in% c("M", "S")) {
      ref_char[k] <- ref_chars[i]
      hip_char[k] <- hip_chars[j]
      operacion[k] <- op
      i <- i + 1L
      j <- j + 1L
    } else if (op == "D") {
      ref_char[k] <- ref_chars[i]
      hip_char[k] <- NA_character_
      operacion[k] <- "D"
      i <- i + 1L
    } else if (op == "I") {
      ref_char[k] <- NA_character_
      hip_char[k] <- hip_chars[j]
      operacion[k] <- "I"
      j <- j + 1L
    }
  }

  tibble(ref_char = ref_char, hip_char = hip_char, operacion = operacion)
}

#' Construye la matriz de confusión a nivel de carácter de un corpus OCR/ICR
#'
#' Alinea cada par (referencia, hipótesis) del data frame con
#' `alinear_pares()`, se queda solo con las sustituciones y cuenta cuántas
#' veces ocurre cada par (ref_char, hip_char) por motor.
#'
#' @param df Data frame con al menos las columnas `motor`, `referencia`,
#'   `hipotesis` (una fila por segmento/documento).
#' @return Tibble largo con columnas `motor`, `ref_char`, `hip_char`, `n`,
#'   ordenado por `n` descendente.
matriz_confusion <- function(df) {
  df %>%
    mutate(alineado = map2(referencia, hipotesis, alinear_pares)) %>%
    unnest(alineado) %>%
    filter(operacion == "S") %>%
    count(motor, ref_char, hip_char, name = "n") %>%
    arrange(desc(n))
}

#' Grafica la matriz de confusión a nivel de carácter como un heatmap
#'
#' @param mc Tibble producido por `matriz_confusion()`, con columnas
#'   `motor`, `ref_char`, `hip_char`, `n`.
#' @param motor_filtro Nombre de un motor para restringir el gráfico a un
#'   solo panel; si es NULL (default) se dibuja un `facet_wrap` por motor.
#' @return Un objeto ggplot con `ref_char` en el eje y, `hip_char` en el eje
#'   x, y color según `n`.
graficar_matriz_confusion <- function(mc, motor_filtro = NULL) {
  if (!is.null(motor_filtro)) {
    mc <- filter(mc, motor == motor_filtro)
  }

  titulo <- if (is.null(motor_filtro)) {
    "Matriz de confusión de caracteres"
  } else {
    paste("Matriz de confusión de caracteres —", motor_filtro)
  }

  p <- ggplot(mc, aes(x = hip_char, y = ref_char, fill = n)) +
    geom_tile() +
    scale_fill_viridis_c(option = "viridis", direction = -1) +
    labs(
      title = titulo,
      x = "Carácter en hipótesis",
      y = "Carácter en referencia",
      fill = "n"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

  if (is.null(motor_filtro)) {
    p <- p + facet_wrap(~motor)
  }

  p
}
