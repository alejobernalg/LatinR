library(tidyverse)

if (Sys.getlocale("LC_CTYPE") %in% c("C", "POSIX")) {
  tryCatch(
    Sys.setlocale("LC_CTYPE", "es_ES.UTF-8"),
    warning = function(w) Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
  )
}

source("R/03_entidades.R")
source("R/02_confusion_olmo.R")

if (sys.nframe() == 0) {
  # Cada línea de Datos/ es un documento
  df_olmo <- cargar_parrafos_olmo()
  tabla_olmo <- preservacion_entidades(df_olmo)

  print(tabla_olmo, n = Inf)
}
