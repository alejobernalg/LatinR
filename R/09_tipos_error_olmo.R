library(tidyverse)

if (Sys.getlocale("LC_CTYPE") %in% c("C", "POSIX")) {
  tryCatch(
    Sys.setlocale("LC_CTYPE", "es_ES.UTF-8"),
    warning = function(w) Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
  )
}

source("R/01_alineamiento.R")
source("R/08_tipos_error.R")
source("R/02_confusion_olmo.R")

if (sys.nframe() == 0) {
  # Cada línea de Datos/ es un documento
  df_olmo <- cargar_parrafos_olmo()
  errores <- errores_por_tipo(df_olmo)

  n_ref <- df_olmo %>%
    mutate(n_ref = nchar(referencia)) %>%
    group_by(motor) %>%
    summarise(n_ref = sum(n_ref), .groups = "drop")

  resumen <- resumen_tipos_error(errores, n_ref)
  cat("\n== Errores por operación y tipo ==\n")
  print(resumen, n = Inf)

  cat("\n== Confusiones más frecuentes por tipo ==\n")
  print(confusiones_frecuentes(errores), n = Inf)

  p <- graficar_tipos_error(resumen)
  dir.create("figuras", showWarnings = FALSE)
  ggsave(
    "figuras/tipos_error_olmo.png", p,
    width = 8, height = 6, dpi = 150, device = ragg::agg_png
  )
}
