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

#' Carga groundtruth.csv y salida_olmo.csv y arma el data frame de entrada
#' esperado por matriz_confusion(): una fila por documento, con `motor`,
#' `referencia` (transcripción manual) e `hipotesis` (salida de olmOCR).
#'
#' Ambos archivos en Datos/ son texto plano (un documento completo por
#' archivo, sin encabezado ni columnas), así que se leen con
#' readr::read_file() en vez de read_csv().
cargar_datos_olmo <- function(dir_datos = "Datos") {
  referencia <- readr::read_file(file.path(dir_datos, "groundtruth.csv"))
  hipotesis <- readr::read_file(file.path(dir_datos, "salida_olmo.csv"))

  tibble(
    doc_id = "1",
    motor = "olmo",
    referencia = referencia,
    hipotesis = hipotesis
  )
}

if (sys.nframe() == 0) {
  df_olmo <- cargar_datos_olmo()
  mc_olmo <- matriz_confusion(df_olmo)

  print(mc_olmo, n = Inf)

  p <- graficar_matriz_confusion(mc_olmo, motor_filtro = "olmo")
  ggsave(
    "Datos/matriz_confusion_olmo.png", p,
    width = 8, height = 6, dpi = 150, device = ragg::agg_png
  )
}
