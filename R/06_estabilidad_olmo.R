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
  # Cada línea del archivo es un documento.
  # Con `unir_continuaciones = TRUE` las líneas que continúan la anterior se
  # unen (10 cartas).
  df_olmo <- tasa_error_documento(cargar_parrafos_olmo())
  resumen_olmo <- resumen_estabilidad(df_olmo)

  print(df_olmo, n = Inf)
  print(resumen_olmo, n = Inf)

  p <- graficar_estabilidad(df_olmo)
  dir.create("figuras", showWarnings = FALSE)
  ggsave(
    "figuras/estabilidad_olmo.png", p,
    width = 8, height = 6, dpi = 150, device = ragg::agg_png
  )
}
