library(tidyverse)

# En locale "C" (p.ej. Rscript lanzado desde una shell sin LANG/LC_ALL) los
# caracteres acentuados se leen bien pero los dispositivos gráficos (ragg,
# png, pdf) no encuentran el glyph correcto y lo dibujan como "..". Forzar
# LC_CTYPE a un locale UTF-8 antes de graficar evita ese problema.
if (Sys.getlocale("LC_CTYPE") %in% c("C", "POSIX")) {
  tryCatch(
    Sys.setlocale("LC_CTYPE", "es_ES.UTF-8"),
    warning = function(w) Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
  )
}

source("R/01_alineamiento.R")
source("R/05_estabilidad.R")

#' Carga los datos de olmOCR segmentados por párrafo (una fila por documento)
#'
#' Lee las líneas de ambos archivos y las agrupa con `segmentar_parrafos()`
#' (R/05_estabilidad.R, que este archivo carga).
#'
#' @return Tibble con `doc_id`, `motor`, `referencia`, `hipotesis`.
cargar_parrafos_olmo <- function(dir_datos = "Datos", unir_continuaciones = FALSE) {
  segmentar_parrafos(
    leer_lineas_csv(file.path(dir_datos, "groundtruth.csv")),
    leer_lineas_csv(file.path(dir_datos, "salida_olmo.csv")),
    motor = "olmo",
    unir_continuaciones = unir_continuaciones
  )
}

if (sys.nframe() == 0) {
  df_olmo <- cargar_parrafos_olmo()
  mc_olmo <- matriz_confusion(df_olmo)

  print(mc_olmo, n = Inf)

  p <- graficar_matriz_confusion(mc_olmo, motor_filtro = "olmo")
  ggsave(
    "Datos/matriz_confusion_olmo.png", p,
    width = 8, height = 6, dpi = 150, device = ragg::agg_png
  )
}
