library(tidyverse)

# Depende de `segmentar_parrafos()` (R/05_estabilidad.R); quien use este
# archivo debe cargarlo antes.

#' Lee el archivo de configuración del reporte (YAML)
#'
#' @param ruta Ruta del YAML; por defecto la variable de entorno
#'   `LATINR_CONFIG` o, si no existe, "config.yml".
#' @return Lista con `titulo`, `referencia`, `motores` (lista de `nombre` y
#'   `salida`) y `unir_continuaciones`.
leer_configuracion <- function(ruta = Sys.getenv("LATINR_CONFIG", "config.yml")) {
  cfg <- yaml::read_yaml(ruta)

  faltan <- setdiff(c("referencia", "motores"), names(cfg))
  if (length(faltan) > 0) {
    stop("Faltan campos en la configuración: ", paste(faltan, collapse = ", "))
  }
  cfg$unir_continuaciones <- isTRUE(cfg$unir_continuaciones)
  cfg
}

#' Carga el corpus de todos los motores de la configuración
#'
#' Lee la referencia una vez y la salida de cada motor, y arma una fila por
#' documento y motor con `segmentar_parrafos()`.
#'
#' @param cfg Lista producida por `leer_configuracion()`.
#' @param dir_base Carpeta contra la que se resuelven las rutas relativas.
#' @return Tibble con `doc_id`, `motor`, `referencia`, `hipotesis`.
cargar_corpus <- function(cfg, dir_base = ".") {
  ruta <- function(x) file.path(dir_base, x)
  ref <- leer_lineas_csv(ruta(cfg$referencia))

  cfg$motores %>%
    map(function(m) {
      segmentar_parrafos(
        ref,
        leer_lineas_csv(ruta(m$salida)),
        motor = m$nombre,
        unir_continuaciones = cfg$unir_continuaciones
      )
    }) %>%
    bind_rows()
}

#' Lee las cifras de referencia de un análisis OCR externo (contraste)
#'
#' El archivo es un CSV largo con columnas `indicador`, `detalle`, `valor`,
#' `total` y `fuente`: una fila por cifra tomada del reporte OCR (métricas
#' por documento, operaciones de edición y entidades preservadas). Estas
#' cifras no se recalculan en este proyecto; solo se leen para contrastarlas
#' con el análisis ICR.
#'
#' @param ruta Ruta del CSV.
#' @return Tibble con las columnas del archivo; `valor` y `total` numéricos.
leer_contraste <- function(ruta) {
  tabla <- readr::read_csv(ruta, show_col_types = FALSE, col_types = readr::cols(.default = "c"))
  faltan <- setdiff(c("indicador", "detalle", "valor", "total", "fuente"), names(tabla))
  if (length(faltan) > 0) {
    stop("Faltan columnas en el archivo de contraste: ", paste(faltan, collapse = ", "))
  }
  tabla %>% mutate(across(c(valor, total), as.numeric))
}
