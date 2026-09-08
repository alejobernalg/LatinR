library(tidyverse)

if (Sys.getlocale("LC_CTYPE") %in% c("C", "POSIX")) {
  tryCatch(
    Sys.setlocale("LC_CTYPE", "es_ES.UTF-8"),
    warning = function(w) Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
  )
}

source("R/01_alineamiento.R")
source("R/05_estabilidad.R")
source("R/02_confusion_olmo.R")

if (sys.nframe() == 0) {
  # Datos/ solo trae un documento de ejemplo, así que este resumen no es
  # informativo sobre estabilidad (se necesitan varios documentos por motor
  # para que dispersión/asimetría signifiquen algo); el script queda listo
  # para correr tal cual sobre un corpus con más de un documento por motor.
  df_olmo <- tasa_error_documento(cargar_datos_olmo())
  resumen_olmo <- resumen_estabilidad(df_olmo)

  print(df_olmo, n = Inf)
  print(resumen_olmo, n = Inf)

  p <- graficar_estabilidad(df_olmo)
  ggsave(
    "Datos/estabilidad_olmo.png", p,
    width = 8, height = 6, dpi = 150, device = ragg::agg_png
  )
}
