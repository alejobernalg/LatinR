library(tidyverse)

# Depende de `alinear_pares()` (R/01_alineamiento.R); quien use este archivo
# debe cargarlo antes. No se hace `source()` acá adentro para que este
# archivo se pueda `source()`ar de forma segura tanto desde un script en la
# raíz del proyecto como desde los tests de testthat, que corren con otro
# working directory.

#' Tasa de error de caracteres (CER) de un documento
#'
#' CER = (sustituciones + eliminaciones + inserciones) / caracteres de la
#' referencia. Reutiliza el alineamiento de `alinear_pares()` (el mismo que
#' arma la matriz de confusión del primer producto) en vez de recalcular la
#' distancia de edición aparte, para que ambos productos cuenten los mismos
#' errores.
#'
#' @param ref Cadena de referencia (transcripción manual).
#' @param hip Cadena de hipótesis (salida del motor OCR/ICR).
#' @return Un número entre 0 y 1 (puede superar 1 si hay muchas más
#'   inserciones que caracteres de referencia).
cer_documento <- function(ref, hip) {
  alineado <- alinear_pares(ref, hip)
  errores <- sum(alineado$operacion != "M")
  n_ref <- sum(!is.na(alineado$ref_char))
  errores / n_ref
}

#' Calcula la tasa de error de caracteres de cada documento de un corpus
#'
#' @param df Data frame con al menos las columnas `motor`, `doc_id`,
#'   `referencia`, `hipotesis` (una fila por documento).
#' @return Tibble con columnas `motor`, `doc_id`, `cer`.
tasa_error_documento <- function(df) {
  df %>%
    mutate(cer = map2_dbl(referencia, hipotesis, cer_documento)) %>%
    select(motor, doc_id, cer)
}

#' Asimetría muestral (Fisher-Pearson, sin corregir) de un vector numérico
#'
#' No depende del paquete `moments` (no está entre las dependencias del
#' proyecto). Con menos de 3 documentos no hay forma robusta de estimar
#' forma de una distribución, así que devuelve `NA` en ese caso.
asimetria <- function(x) {
  if (length(x) < 3) {
    return(NA_real_)
  }
  m <- mean(x)
  s <- sd(x)
  mean((x - m)^3) / s^3
}

#' Resume la estabilidad del error entre documentos, por motor
#'
#' Para cada motor compara la dispersión (desviación estándar, rango
#' intercuartílico) y la forma (asimetría) de la distribución de `cer` entre
#' documentos: dos motores con la misma media pueden tener muy distinta
#' confiabilidad operativa si uno es consistente y el otro tiene documentos
#' ocasionales con error mucho más alto.
#'
#' @param tabla_cer Tibble producido por `tasa_error_documento()`, con
#'   columnas `motor`, `doc_id`, `cer`.
#' @return Tibble con columnas `motor`, `n_documentos`, `media`, `mediana`,
#'   `de` (desviación estándar), `iqr`, `asimetria`.
resumen_estabilidad <- function(tabla_cer) {
  tabla_cer %>%
    group_by(motor) %>%
    summarise(
      n_documentos = n(),
      media = mean(cer),
      mediana = median(cer),
      de = sd(cer),
      iqr = IQR(cer),
      asimetria = asimetria(cer),
      .groups = "drop"
    )
}

#' Grafica la distribución del error de caracteres por documento, por motor
#'
#' @param tabla_cer Tibble producido por `tasa_error_documento()`, con
#'   columnas `motor`, `doc_id`, `cer`.
#' @return Un objeto ggplot con un boxplot por motor y los documentos
#'   individuales superpuestos como puntos, para no ocultar la forma real de
#'   la distribución detrás del resumen de caja.
graficar_estabilidad <- function(tabla_cer) {
  ggplot(tabla_cer, aes(x = motor, y = cer, fill = motor)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.6) +
    geom_jitter(width = 0.1, height = 0, alpha = 0.7) +
    scale_fill_viridis_d() +
    labs(
      title = "Estabilidad del error de caracteres entre documentos",
      x = "Motor",
      y = "Tasa de error de caracteres (CER) por documento",
      fill = "Motor"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
}
