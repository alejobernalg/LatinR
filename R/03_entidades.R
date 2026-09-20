library(tidyverse)
library(quanteda)

#' Meses del español, para el patrón de fecha ("23 de Marzo de 1793")
meses <- c(
  "enero", "febrero", "marzo", "abril", "mayo", "junio", "julio",
  "agosto", "septiembre", "setiembre", "octubre", "noviembre", "diciembre",
  "ene", "feb", "febr", "mar", "abr", "may", "jun", "jul", "ago", "sept",
  "set", "oct", "nov", "dic"
)
# "7bre", "8bre", "9bre", "10bre": setiembre a diciembre abreviados con cifra
meses_regex <- paste(c(meses, "\\d{1,2}bre"), collapse = "|")

#' Números escritos con palabras, para fechas como "seis de septiembre de mil
#' novecientos cuarenta y siete"
numerales <- c(
  "mil", "novecientos", "ochocientos", "setecientos", "seiscientos",
  "quinientos", "cuatrocientos", "trescientos", "doscientos", "ciento",
  "cien", "noventa", "ochenta", "setenta", "sesenta", "cincuenta", "cuarenta",
  "treinta", "veinte", "veinti[a-záéíóúñü]+", "dieciséis", "dieciseis",
  "diecisiete", "dieciocho", "diecinueve", "quince", "catorce", "trece",
  "doce", "once", "diez", "nueve", "ocho", "siete", "seis", "cinco", "cuatro",
  "tres", "dos", "uno", "una", "un"
)
numerales_regex <- paste(numerales, collapse = "|")

#' Diccionario de exclusión para nombres propios
#'
#' Vocabulario institucional/administrativo que aparece capitalizado a mitad
#' de oración pero no es un nombre propio de persona o lugar (p.ej. "Junta",
#' "Ministerio", "Tribunal"). Se construye leyendo el corpus de referencia y
#' separando manualmente instituciones/cargos de nombres reales; es la
#' alternativa a un gazetteer externo, que cubre mal nombres de documentos
#' administrativos históricos.
#'
#' El resto de las categorías (fecha, número, código administrativo) se
#' detectan con patrones estructurales (regex) y no requieren diccionario,
#' salvo el listado cerrado de meses de arriba.
dic_exclusion_nombres <- dictionary(list(
  institucional = c(
    "Marina", "Oficio", "Señor", "Presidente", "Ministro", "Ministerio",
    "Cuenta", "Cuentas", "Tribunal", "Junta", "Contaduria", "Contaduría",
    "Esquadra", "Escuadra", "Ordenanza", "Dios", "Oficial", "Tierra",
    "Guarda-Costas", "Exposición", "Pintura", "Biblioteca", "Galería",
    "Facultad", "Minas", "Hospital", "Salón", "Alcalde", "Guerra",
    "Auto-retrato", "Relaciones", "Exteriores", "Gobierno", "Gobernador",
    "Juzgado", "Juez", "Justicia", "Municipal", "Nacional", "Nación", "Nacion",
    "Oficina", "Oficinas", "Secretario", "Presidencia", "Policía", "Correos",
    "Telégrafos", "Cabildo", "Estado", "República", "Resolución",
    "Resolucion", "Administrador", "Capitán", "Jefe", "Gral", "Gbn",
    "Excelencia", "Sede", "Dirección", "Calle", "Dtto", "Dto"
  ),
  # tratamientos y fórmulas de saludo/despedida, capitalizados por cortesía
  tratamiento = c(
    "Don", "Doña", "Dn", "Sr", "Sra", "Ud", "Uds", "Ued", "Usd", "Herr",
    "Padre", "Estimado", "Amigo", "Afectísimo", "Afectuosos", "Doctor",
    "Doctora", "Señora", "Señores", "Señorita", "Excelentísimo",
    "Excelentisimo", "Servidor", "Servidora", "Admiración", "Ti"
  ),
  # palabras comunes capitalizadas tras un encabezado sin puntuación
  # ("Bogotá Muy estimado amigo", "Su afectisimo")
  comun = c("Muy", "Su", "Creo", "Realmente"),
  # sustantivos y verbos que la referencia escribe con mayúscula inicial a
  # mitad de frase y que no aparecen en minúscula en ningún otro lugar del
  # corpus, por lo que el filtro de vocabulario no los detecta
  mayuscula_comun = c(
    "Colocar", "Colocarnos", "Sacar", "Servirme", "Súplica", "Sueldos",
    "Ocupados", "Oficiales", "Gobernantes", "Directores", "Colonos",
    "Telegrafista", "Concerbadores", "Concervadora", "Cumunidad", "Economia",
    "Gendarmería", "Librería", "Boliviana", "Patria", "Señ"
  )
))

#' Reemplaza en `texto` las coincidencias de `patron` por "#" (mismo largo)
#'
#' Sirve para que un extractor no vuelva a capturar lo que ya clasificó un
#' extractor anterior (p.ej. que "1793" dentro de una fecha ya extraída no se
#' cuente después como número suelto), preservando las posiciones del resto
#' de la cadena para los patrones que corren a continuación.
#'
#' @return list(texto = cadena enmascarada, coincidencias = subcadenas
#'   originales encontradas).
enmascarar_spans <- function(texto, patron) {
  m <- stringr::str_locate_all(texto, patron)[[1]]
  if (nrow(m) == 0) {
    return(list(texto = texto, coincidencias = character(0)))
  }

  coincidencias <- stringr::str_sub(texto, m[, "start"], m[, "end"])
  texto_enmascarado <- texto
  for (i in seq_len(nrow(m))) {
    largo <- m[i, "end"] - m[i, "start"] + 1
    stringr::str_sub(texto_enmascarado, m[i, "start"], m[i, "end"]) <- strrep("#", largo)
  }

  list(texto = texto_enmascarado, coincidencias = coincidencias)
}

#' Clasifica las entidades administrativas de un texto de referencia
#'
#' Extrae, en orden de prioridad (para evitar que una categoría capture lo
#' que ya capturó otra): fechas (con año y sin año), códigos administrativos
#' y números por patrones estructurales (regex); y nombres propios como las
#' palabras capitalizadas que no abren una oración, sin el vocabulario de
#' `dic_exclusion_nombres`.
#'
#' Una palabra abre oración si está al inicio del texto o si lo anterior
#' termina en `. ! ? : ; = "` u otro signo de corte. Esta regla propia
#' reemplaza al segmentador de oraciones, que no entiende abreviaturas
#' ("Ud.", "D.n") ni los cortes con ":" y "=" de estos documentos.
#'
#' Los códigos administrativos son abreviaturas: siglas con puntos internos
#' ("V.S.", "E.S.M.", "D.n", "S.or"), iniciales sueltas ("V.") y una lista
#' cerrada ("Ud.", "Sr.", "etc."). Una palabra común seguida de punto al final
#' de una oración ("nada.") no es un código.
#'
#' Pensada para aplicarse solo sobre la referencia (transcripción manual):
#' la hipótesis de un motor OCR puede tener la capitalización corrompida,
#' que es justo la señal que usa la heurística de nombres propios.
#'
#' @param texto Cadena de referencia (transcripción manual).
#' @param vocabulario_comun Palabras en minúscula que aparecen en el corpus de
#'   referencia (ver `vocabulario_minusculas()`). Una palabra capitalizada que
#'   también aparece en minúscula es una palabra común escrita con mayúscula
#'   (en estos manuscritos abundan: "Si", "Un", "Sea"), no un nombre propio.
#' @return Tibble con columnas `entidad` (subcadena encontrada) y
#'   `categoria` (fecha, numero, codigo_administrativo, nombre_propio).
clasificar_entidades <- function(texto, vocabulario_comun = character()) {
  # Formatos con año: "23. de Marzo de 1793", "oct. 28 - 1940", "Nov. 1940",
  # "Marzo 4 1.941", "Abril 28 de 1941", "6 de sept. 1-43", "Julio 10 del
  # 935", "Julio 14.-1935", "Septiembre 8 : 1949", "6 de 9bre. de 1935" y
  # fechas con palabras ("seis de septiembre de mil novecientos cuarenta y
  # siete"). El año admite puntos o guiones internos (así aparece en la
  # referencia).
  anio <- paste0(
    "(?:\\d(?:[\\d.\\-]{1,3}\\d)\\b",
    "|mil(?:\\s+(?:y\\s+)?\\b(?:", numerales_regex, ")\\b)+)"
  )
  patron_fecha <- regex(
    paste0(
      "(?:\\b(?:\\d{1,2}|", numerales_regex, ")\\.?\\s+(?:de\\s+)?)?",
      "\\b(?:", meses_regex, ")\\b\\.?\\s*",
      "(?:\\d{1,2}\\s*[,.:]?\\s*(?:-\\s*)?)?(?:del?\\s+)?",
      anio
    ),
    ignore_case = TRUE
  )
  # Fecha sin año: "28 de agosto", "Dic - 21", "julio 23."
  patron_fecha_sin_anio <- regex(
    paste0(
      "\\b\\d{1,2}\\s+de\\s+(?:", meses_regex, ")\\b",
      "|\\b(?:", meses_regex, ")\\b\\.?\\s*-?\\s*\\d{1,2}\\b"
    ),
    ignore_case = TRUE
  )
  letra <- "A-Za-záéíóúñüÁÉÍÓÚÑªº"
  mayus <- "A-ZÁÉÍÓÚÑ"
  minus <- "a-záéíóúñü"
  patron_codigo <- regex(paste0(
    # siglas con punto interno: V.S., E.S.M., N.Y., P.N., D.n, S.or, D.ª
    "\\b[", mayus, "][", minus, "]{0,3}\\.(?:[", mayus, "][", minus, "]{0,3}\\.)?[", letra, "]{1,4}\\.?",
    # inicial suelta: V., D., T.
    "|\\b[", mayus, "]\\.(?![", letra, "])",
    # lista cerrada de abreviaturas de tratamiento
    "|\\b(?:Ud|Uds|Sr|Sra|Srta|Dn|Da|Dr|Dra|Sta|etc|Vd|Vds)\\.",
    "|\\bDª\\.?"
  ))
  # Número suelto, con ordinal opcional ("1º", "3ª", "3°")
  patron_numero <- regex("(?<![\\p{L}\\p{N}])\\d+[ºª°]?(?![\\p{L}\\p{N}])")

  paso_fecha <- enmascarar_spans(texto, patron_fecha)
  paso_fecha2 <- enmascarar_spans(paso_fecha$texto, patron_fecha_sin_anio)
  paso_codigo <- enmascarar_spans(paso_fecha2$texto, patron_codigo)
  paso_numero <- enmascarar_spans(paso_codigo$texto, patron_numero)

  entidades_regex <- bind_rows(
    tibble(
      entidad = c(paso_fecha$coincidencias, paso_fecha2$coincidencias),
      categoria = "fecha"
    ),
    tibble(entidad = paso_codigo$coincidencias, categoria = "codigo_administrativo"),
    tibble(entidad = paso_numero$coincidencias, categoria = "numero")
  )

  # Encabezado: "Cali, Julio 6 de 1935". Lo que precede a la primera fecha,
  # si está al inicio del texto, es el lugar de la carta y no una oración.
  posicion_fecha <- stringr::str_locate(
    texto, paste0("(?i)(?:", patron_fecha, "|", patron_fecha_sin_anio, ")")
  )[1, "start"]
  encabezado_hasta <- if (!is.na(posicion_fecha) && posicion_fecha <= 70) posicion_fecha else 0
  entidades_nombres <- extraer_nombres_propios(paso_numero$texto, encabezado_hasta, vocabulario_comun)

  bind_rows(entidades_regex, entidades_nombres) %>%
    filter(!is.na(entidad), entidad != "")
}

#' Palabras que el corpus de referencia escribe con minúscula inicial
#'
#' @param textos Vector con los textos de referencia.
#' @return Vector de palabras únicas en minúscula.
vocabulario_minusculas <- function(textos) {
  textos %>%
    str_extract_all("(?<![\\p{L}\\p{N}])[a-záéíóúñü]+") %>%
    unlist() %>%
    unique()
}

#' Nombres propios de un texto ya enmascarado: capitalizadas que no abren oración
#'
#' `encabezado_hasta` es la posición donde empieza la primera fecha, si está
#' al inicio del texto: antes de ella no se aplica la regla de inicio de
#' oración (el lugar de la carta suele abrir el texto).
extraer_nombres_propios <- function(texto, encabezado_hasta = 0, vocabulario_comun = character()) {
  patron <- paste0(
    "(?<![\\p{L}\\p{N}#-])[A-ZÁÉÍÓÚÑ][a-záéíóúñü]+(?:-[A-Za-záéíóúñü]+)?(?![\\p{L}-])"
  )
  m <- stringr::str_locate_all(texto, patron)[[1]]
  if (nrow(m) == 0) {
    return(tibble(entidad = character(), categoria = character()))
  }

  candidatos <- stringr::str_sub(texto, m[, "start"], m[, "end"])
  previo <- stringr::str_sub(texto, 1, m[, "start"] - 1) %>% str_remove("\\s+$")
  abre_oracion <- (previo == "" | str_detect(previo, "([.!?:;=\"„¿¡—–]|\\s-|\\.-)$")) &
    m[, "start"] >= max(encabezado_hasta, 1)

  excluidos <- c(
    unlist(dic_exclusion_nombres, use.names = FALSE),
    meses
  )
  es_nombre <- !abre_oracion &
    nchar(candidatos) > 1 &
    !(tolower(candidatos) %in% tolower(excluidos)) &
    !(tolower(candidatos) %in% vocabulario_comun)

  tibble(entidad = candidatos[es_nombre], categoria = "nombre_propio")
}

#' Normaliza una fecha para compararla: sin puntos ni comas
#'
#' La referencia conserva el punto ordinal de la época ("23. de Marzo") y los
#' motores suelen omitirlo ("23 de Marzo"); ese punto no cambia la fecha. El
#' día, el mes y el año deben coincidir exactamente (los acentos cuentan).
normalizar_fecha <- function(x) {
  x %>%
    str_remove_all("[.,]") %>%
    str_replace_all("\\s*-\\s*", "-") %>%
    str_squish() %>%
    str_to_lower()
}

#' ¿Se preservó una fecha de la referencia en la hipótesis?
#'
#' Busca la fecha (normalizada) en la misma línea de la hipótesis que la
#' contiene en la referencia, para que un año que aparece en otra parte del
#' texto no dé por buena una fecha mal transcrita. Si la hipótesis tiene un
#' número distinto de líneas, busca en todo el texto.
fecha_preservada <- function(entidad, referencia, hipotesis) {
  lineas_ref <- str_split(referencia, "\\r?\\n")[[1]]
  lineas_hip <- str_split(hipotesis, "\\r?\\n")[[1]]
  destino <- hipotesis
  if (length(lineas_ref) == length(lineas_hip)) {
    idx <- which(str_detect(lineas_ref, fixed(entidad)))[1]
    if (!is.na(idx)) destino <- lineas_hip[idx]
  }
  str_detect(normalizar_fecha(destino), fixed(normalizar_fecha(entidad)))
}

#' ¿Aparece `entidad` como palabra completa en `texto`?
#'
#' Sin distinguir mayúsculas/minúsculas, pero sí acentos y puntuación. No
#' cuenta si la entidad está pegada a una letra o dígito (un "4" dentro de
#' "1940", o "V." dentro de "V.S.").
esta_en_texto <- function(entidad, texto) {
  escapada <- str_replace_all(entidad, "([\\^$.|?*+()\\[\\]{}])", "\\\\\\1")
  patron <- regex(
    paste0("(?<![\\p{L}\\p{N}])", escapada, "(?![\\p{L}\\p{N}])"),
    ignore_case = TRUE
  )
  str_detect(texto, patron)
}

#' Calcula la tasa de preservación de entidades administrativas por motor
#'
#' Para cada documento clasifica las entidades de la referencia con
#' `clasificar_entidades()` y verifica, para cada una, si aparece como palabra
#' completa en la hipótesis del mismo documento (`esta_en_texto()`): sin
#' distinguir mayúsculas, pero los acentos y la puntuación deben coincidir.
#' Las fechas se comparan con `fecha_preservada()` (mismo criterio, salvo el
#' punto ordinal). La tasa de preservación por categoría es la proporción de
#' entidades de la referencia que se encuentran así en la hipótesis.
#'
#' @param df Data frame con al menos las columnas `motor`, `referencia`,
#'   `hipotesis` (una fila por segmento/documento).
#' @return Tibble con columnas `motor`, `categoria`, `tokens_totales`,
#'   `tokens_preservados`, `tasa_preservacion`.
preservacion_entidades <- function(df) {
  detalle_entidades(df) %>%
    group_by(motor, categoria) %>%
    summarise(
      tokens_totales = n(),
      tokens_preservados = sum(preservado),
      tasa_preservacion = mean(preservado),
      .groups = "drop"
    )
}

#' Detalle de la preservación: una fila por entidad de la referencia
#'
#' Es la base de `preservacion_entidades()`; sirve para ver qué entidades
#' concretas se preservan y cuáles no.
#'
#' @param df Data frame con `motor`, `referencia`, `hipotesis` (y, si existe,
#'   `doc_id`).
#' @return Tibble con `motor`, `doc_id` (si estaba), `entidad`, `categoria` y
#'   `preservado`.
detalle_entidades <- function(df) {
  vocabulario <- vocabulario_minusculas(unique(df$referencia))

  df %>%
    mutate(entidades = map(referencia, clasificar_entidades, vocabulario_comun = vocabulario)) %>%
    select(any_of(c("motor", "doc_id")), referencia, hipotesis, entidades) %>%
    unnest(entidades) %>%
    mutate(
      preservado = if_else(
        categoria == "fecha",
        pmap_lgl(list(entidad, referencia, hipotesis), fecha_preservada),
        map2_lgl(entidad, hipotesis, esta_en_texto)
      )
    ) %>%
    select(-referencia, -hipotesis)
}

#' Grafica la tasa de preservación de entidades administrativas por motor
#'
#' @param tabla Tibble producido por `preservacion_entidades()`, con
#'   columnas `motor`, `categoria`, `tasa_preservacion`.
#' @return Un objeto ggplot de barras, una barra por motor dentro de cada
#'   categoría de entidad.
graficar_preservacion_entidades <- function(tabla) {
  ggplot(tabla, aes(x = categoria, y = tasa_preservacion, fill = motor)) +
    geom_col(position = "dodge") +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    scale_fill_viridis_d() +
    labs(
      title = "Preservación de entidades administrativas por motor",
      x = "Categoría de entidad",
      y = "Tasa de preservación",
      fill = "Motor"
    ) +
    theme_minimal()
}
